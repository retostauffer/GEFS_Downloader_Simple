# -------------------------------------------------------------------
# - NAME:        GEFS_interpolate.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-10-16
# -------------------------------------------------------------------
# - DESCRIPTION: Based on a set of NetCDF files and a station list
#                this script will perform the bilinear interpolation.
#                The output is stored in an easy to read ASCII format.
# -------------------------------------------------------------------
# - EDITORIAL:   2018-10-16, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-11-12 17:31 on marvin
# -------------------------------------------------------------------

    rm(list = ls())
    Sys.setenv("TZ" = "UTC")

    # Using argparser here
    suppressPackageStartupMessages(library("argparse"))
    usage <- paste("Small script to read the grib files (via raster)",
                   "and bilinearely interpolate the GEFS forecasts to",
                   "a set of airports/stations.",
                   "This script does the interpolation and data",
                   "preparation and stores the forecasts into a set",
                   "of CSV files.")
    parser <- ArgumentParser(description = usage)
    parser$add_argument("-d", "--date", metavar = "date", type = "character",
                        help = "Date (initial date), format YYYY-mm-dd")
    parser$add_argument("-r", "--runhour", metavar = "runhour", type = "integer",
                        help = "Model run hour (UTC), integer [0, 6, 12, 18].")

    if ( interactive() ) {
        args <- parser$parse_args(c("-d", "2018-11-12", "-r", 0))
    } else { args <- parser$parse_args() }

    if ( is.null(args$date) | is.null(args$runhour) ) {
        parser$print_help(); stop("Wrong usage.")
    }


    # Convert input "date".
    args$date <- try(as.Date(strptime(args$date, "%Y-%m-%d")))
    if ( inherits(args$date, "try-error") | is.na(args$date) ) {
        parser$print_help(); stop("Wrong format for -d/--date") }
    if ( ! args$runhour %in% seq(0, 18, by = 6) ) {
        parser$print_help(); stop("Wrong format for -r/--runhour") }

    # Create full POSIXct of the model initialization
    initialization <- as.POSIXlt(args$date) + 3600 * args$runhour
    cat(sprintf("\n* Processing GEFS forecast for %s\n",
                strftime(initialization, "%Y-%m-%d %H:%M UTC")))

    # Looking for avialable PARAMETER files, t2m
    fdir  <- sprintf("data_t2m/%s%02d00", strftime(args$date, "%Y%m%d"), args$runhour)
    all_files <- list.files(fdir)

    for ( mem in 0:20 ) {
        files <- all_files[grep(sprintf("^GEFS_%s_%02d00_%02d_.*_subset.grb2$",
                                    strftime(args$date, "%Y%m%d"), args$runhour, mem), all_files)]
        if ( length(files) == 0 ) stop("Could not find any files")
        files <- sprintf("%s/%s", fdir, files) 
        cmd <- sprintf("cat %s > tmp_%02d.grb2", paste(files, collapse = " "), mem)
        system(cmd)
        cmd <- sprintf("wgrib2 tmp_%1$02d.grb2 -netcdf tmp_%1$02d.nc && rm tmp_%1$02d.grb2", mem)
        system(cmd)
    }
    nc_files <- list.files(".")[grep("^tmp_[0-9]{2}\\.nc$", list.files("."))]


# -------------------------------------------------------------------
# Use ncdf4 to read the data and raster to create geodata which
# makes it easy to perform the bilinear interpolation.
# -------------------------------------------------------------------
    library("ncdf4")
    library("raster")

    # sp package for hanlding the stations/interpolation
    library("sp")
    proj <- "+proj=longlat +ellps=WGS84 +no_defs"
    stations <- SpatialPointsDataFrame(data.frame(lon = 11.38, lat = 47.26),
                                       data = data.frame(Station = "Innsbruck"),
                                       proj4string = CRS(proj))


# -------------------------------------------------------------------
# Processing the netcdf files
# -------------------------------------------------------------------

    # Helper function to convert NetCDF data to raster
    nc_to_raster <- function(nc, var, newname = NULL) {
        stopifnot(inherits(var, "character"))
        vardims <- sapply(nc$var[["GUST_surface"]]$dim, function(x) x$name)
        if ( is.null(newname) ) newname <- var
        if ( ! all(vardims == c("longitude", "latitude", "time")) )
            stop("Got unexpected dimensions!")
        if ( ! grepl("seconds since 1970-01-01 00", nc$dim$tim$units) )
            stop("Got unexpected time units from NetCDF! Please check.")
        times <- as.POSIXlt(ncvar_get(nc, "time"), origin = "1970-01-01 00")
        lons <- ncvar_get(nc, "longitude")
        lats <- ncvar_get(nc, "latitude")
        dlon <- unique(diff(lons)); stopifnot(length(dlon) == 1)
        dlat <- unique(diff(lats)); stopifnot(length(dlat) == 1)

        # Create raster template
        empty <- raster(nrows = length(lats), ncol = length(lons),
                        xmn = as.numeric(min(lons) - dlon/2.),
                        xmx = as.numeric(max(lons) + dlon/2.),
                        ymn = as.numeric(min(lats) - dlat/2.),
                        ymx = as.numeric(max(lats) + dlat/2.))

        # Looping over data, create RasterStack object 
        data <- ncvar_get(nc, var)
        res <- list()
        for ( i in seq_along(times) ) {
            tmp <- empty
            values(tmp) <- t(data[,order(lats, decreasing = TRUE),i])
            res[[sprintf("%s_%s", newname, strftime(times[i], "%Y%m%d%H%M"))]] <- tmp
        }
        return(stack(res))
    }


    all_data <- list()
    for ( file in nc_files ) {
        cat(sprintf("Interpolating %s\n", file))
        nc <- nc_open(file)
        # Convert NetCDF fields to RasterStack objects
        data <- list()
        data[["t2m"]]  <- nc_to_raster(nc, "TMP_2maboveground",    "t2m")

        # Extract member from file name and append as attribute
        attr(data, "member") <- as.integer(substr(regmatches(file, regexpr("[0-9]{2}\\.nc$", file)), 0, 2))

        hash <- regmatches(file, regexpr("[0-9]{2}", file))
        all_data[[hash]] <- data

        nc_close(nc)
    }

# -------------------------------------------------------------------
# Interpolate data
# -------------------------------------------------------------------
    bilinear <- function(x, stations) {
        x <- t(extract(x, stations, method = "bilinear"))
        colnames(x) <- as.character(stations@data$Station)
        datetime    <- regmatches(rownames(x), regexpr("[0-9]{12}$", rownames(x)))
        datetime    <- as.POSIXct(strptime(datetime, "%Y%m%d%H%M"))
        varnames    <- sapply(rownames(x), function(x) substr(x, 0, nchar(x) - 13))
        # Combine
        x <- cbind(data.frame(var = varnames, datetime = datetime), x)
        rownames(x) <- NULL
        return(x)
    }

    bifun <- function(x, var, stations) bilinear(x[[var]], stations)
    prepare <- function(x) {
        for ( i in seq_along(x) ) {
            mem <- as.integer(regmatches(names(x)[i],
                              regexpr("[0-9]{2}$", names(x)[i])))
            x[[i]]$member <- mem
        }
        return(subset(do.call(rbind, x), select = -c(var)))
    }

    prepared <- list()
    for ( var in names(all_data[[1]]) )
        prepared[[var]] <- prepare(lapply(all_data, bifun, var = var, stations = stations))


# -------------------------------------------------------------------
# Reshpae data
# -------------------------------------------------------------------
    reshapefun <- function(x, stations) {
        res <- list()
        for ( stn in stations@data$Station ) {
            res[[stn]] <- reshape(x, timevar = "member", idvar = "datetime", direction = "wide",
                                  drop = names(x)[! names(x) %in% c("datetime", "member", stn)])
            names(res[[stn]])    <- gsub("^.*\\.", "member_", names(res[[stn]]))
            rownames(res[[stn]]) <- NULL
        }
        return(res)
    }
    data <- lapply(prepared, reshapefun, stations = stations)

    # Picking innsbruck
    library("zoo")
    data <- data$t2m$Innsbruck
    data <- zoo(data[,-1] - 273.15, data[,1])

    cat("... reached the end of the script, good night!\n")


    plot(data, screen = 1)
    tmp <- zoo(apply(data, 1, mean), index(data))
    lines(tmp, col = 2, lwd = 5)

    idx <- which(index(data) == as.POSIXct("2018-11-22 12:00"))
    if ( length(idx) == 1 ) {
        abline(v = index(data)[idx], col = 2, lty = 3, lwd = 4)
        # Expected temperature 90%
        tmp <- quantile(as.numeric(data[idx,]), probs = c(.1, .9))
        text(index(data)[idx], tmp[1L], sprintf("%.1f", tmp[1L]), cex = 3, col = 4)
        text(index(data)[idx], tmp[2L], sprintf("%.1f", tmp[2L]), cex = 3, col = 4)
    }


    saveRDS(data, file = sprintf("data_t2m/hearing_ibkt2m_%s_%02d00.rds", strftime(args$date, "%Y%m%d"), args$runhour))

















