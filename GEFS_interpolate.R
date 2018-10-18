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
# - L@ST MODIFIED: 2018-10-18 09:48 on marvin
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
        args <- parser$parse_args(c("-d", "2018-10-5", "-r", 6))
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


    # Looking for available files
    fhash <- sprintf("GEFS_%s_.*.nc$", strftime(initialization, "%Y%m%d%H%M"))
    files <- list.files("prepared")
    files <- sprintf("prepared/%s", files[grep(fhash, files)])
    if ( length(files) == 0 ) stop("Sorry, no files found matching the search pattern.")
    cat(sprintf("* Found %d different files\n\n", length(files)))


# -------------------------------------------------------------------
# Use ncdf4 to read the data and raster to create geodata which
# makes it easy to perform the bilinear interpolation.
# -------------------------------------------------------------------
    library("ncdf4")
    library("raster")

    # sp package for hanlding the stations/interpolation
    library("sp")
    proj <- "+proj=longlat +ellps=WGS84 +no_defs"
    stations <- read.table("Stations.csv", header = TRUE, sep = ";")
    stations <- SpatialPointsDataFrame(subset(stations, select = c(lon, lat)),
                                       data = subset(stations, select = -c(lon, lat)),
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
    for ( file in files ) {
        nc <- nc_open(file)
        cat(sprintf("* %s: %s\n", file,
            paste(sapply(nc$var, function(x) x$name), collapse = ", ")))

        # Convert NetCDF fields to RasterStack objects
        data <- list()
        data[["fg10m"]]  <- nc_to_raster(nc, "GUST_surface",         "fg10m")
        data[["u10m"]]   <- nc_to_raster(nc, "UGRD_10maboveground",  "u10m")
        data[["v10m"]]   <- nc_to_raster(nc, "VGRD_10maboveground",  "v10m")
        data[["u80m"]]   <- nc_to_raster(nc, "UGRD_80maboveground",  "u80m")
        data[["v80m"]]   <- nc_to_raster(nc, "VGRD_80maboveground",  "v80m")
        data[["u100m"]]  <- nc_to_raster(nc, "UGRD_100maboveground", "u100m")
        data[["v100m"]]  <- nc_to_raster(nc, "VGRD_100maboveground", "v100m")
        data[["ff10m"]]  <- sqrt(data[["u10m"]]^2  + data[["v10m"]]^2)
        data[["ff80m"]]  <- sqrt(data[["u80m"]]^2  + data[["v80m"]]^2)
        data[["ff100m"]] <- sqrt(data[["u100m"]]^2 + data[["v100m"]]^2)

        # Extract member from file name and append as attribute
        attr(data, "member") <- as.integer(substr(regmatches(file, regexpr("[0-9]{2}\\.nc$", file)), 0, 2))

        hash <- regmatches(file, regexpr("[0-9]{12}_[0-9]{2}", file))
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

    data <- prepare(lapply(all_data, bifun, var = "fg10m", stations = stations))

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
    #var <- "ff100m"
    #for ( i in seq_along(x[[var]]) ) {
    #    boxplot(t(x[[var]][[i]][,-1]), main = names(x[[var]])[i])
    #}

# -------------------------------------------------------------------
# Quick check: ensemble median
# -------------------------------------------------------------------
    var <- "ff100m"
    medianfun <- function(x) {
        require("zoo")
        return(zoo(apply(x[,-1], 1, median), x$datetime))
    }
    tmp <- do.call(merge, lapply(data[[var]], medianfun))
    plot(tmp, screen = 1, col = 1:ncol(tmp), lty = 1:ncol(tmp),
         xlim = range(index(tmp) + c(0, 6*3600)))
    text(max(index(tmp)), tail(tmp, 1), names(tmp), pos = 4)

# -------------------------------------------------------------------
# Save forecasts
# -------------------------------------------------------------------
    save_ASCII <- function(x, var, stn, initialization, dir = "ASCII") {
        # Ouput directory as a combination of "dir" and model
        # initialization
        dir <- sprintf("%s/%s", dir, strftime(initialization, "%Y%m%d%H%M"))
        # Output file
        if ( ! dir.exists(dir) ) dir.create(dir, recursive = TRUE)
        # Prepare data
        x$datetime <- as.numeric(x$datetime)
        names(x)[which(names(x) == "datetime")] <- "timestamp"

        # Ouptut file name
        require("gdata")
        outfile <- sprintf("%s/GEFS_%s_%s_%s.dat", dir,
                           strftime(initialization, "%Y%m%d%H%M"), var, stn)
        head <- c("# Bilinearely interpolated GEFS forecasts",
                  sprintf("# for station \"%s\", parameter \"%s\".", stn, var),
                  sprintf("# GEFS initialization was %s", strftime(initialization, "%Y-%m-%d %H UTC")),
                  "# The \"timestamp\" column contains the date/time\"",
                  "# for which the forecasts have been computed, UNIX time",
                  "# stamp (seconds since 1970-01-01 00:00:00 UTC).",
                  "# the rest is wind speed/gust speed in meters per second.")
        write(paste(head, collapse = "\n"), file = outfile)
        write.fwf(x, file = outfile, append = TRUE)
    }


    for ( var in names(data) ) {
        for ( stn in names(data[[var]]) ) {
            save_ASCII(data[[var]][[stn]], var, stn, initialization)
        }
    }

    cat("... reached the end of the script, good night!\n")

















