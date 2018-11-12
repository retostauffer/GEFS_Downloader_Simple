# -------------------------------------------------------------------
# - NAME:        GEFS_check.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-10-16
# -------------------------------------------------------------------
# - DESCRIPTION: Checking ASCII files (devel purposes).
# -------------------------------------------------------------------
# - EDITORIAL:   2018-10-16, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-10-18 13:21 on marvin
# -------------------------------------------------------------------

    rm(list = ls())
    Sys.setenv("TZ" = "UTC")

    param   <- "fg10m"
    param <- "ff100m"
    param <- "ff10m"

    # Station list
    #library("maps"); library("sp")
    #stations <- read.table("Stations.csv", header = TRUE, sep = ";")
    #stations <- SpatialPointsDataFrame(subset(stations, select = c("lon", "lat")),
    #                               data = subset(stations, select = c("Station")))
    #plot(stations)
    #map(add = TRUE)
    #text(coordinates(stations)[,"lon"], coordinates(stations)[,"lat"],
    #     as.character(stations@data$Station), col = 2)

    station <- "KPNS"
    # Looking for available files
    fdir  <- "ASCII"
    fhash <- sprintf("GEFS_.*_%s_%s.dat$", param, station)
    files <- list.files(fdir, recursive = TRUE)
    files <- sprintf("%s/%s", fdir, files[grep(fhash, files)])
    if ( length(files) == 0 ) stop("Sorry, no files found matching the search pattern.")
    cat(sprintf("* Found %d different files\n\n", length(files)))

# -------------------------------------------------------------------
# Loading data
# -------------------------------------------------------------------

    # Extract date/time information from path
    extract_date <- function(x) {
        x <- regmatches(x, regexpr("\\/[0-9]{12}\\/", x))
        as.POSIXct(strptime(substr(x, 2, 12), "%Y%m%d%H%M"))
    }

    # Plotting the data
    plotpoly <- function(x, ...) {
        px <- c(as.numeric(index(x)), rev(as.numeric(index(x))))
        py <- c(apply(x, 1, min), rev(apply(x, 1, max)))
        plot(NA, type = "n", xlim = range(px),
             xlab = NA, ylab = NA, main = NA, ...)
        polygon(px, py, border = NA, col = "gray80")
        abline(v = as.POSIXct("2018-10-10 18:00"), col = 2, lwd = 3)
        abline(h = seq(10, 50, by = 10), lty = 3, col = "gray50")
        lines(index(x), apply(x, 1, median))
        box()
    }

    # Getting the initialization date/time information from file name
    dates <- extract_date(files)

    # Reading the data
    library("zoo")
    data <- list()
    for ( i in seq_along(dates) ) {
        file <- files[grep(strftime(dates[i], "%Y%m%d%H%M"), files)]
        cat(sprintf("Reading %s\n", file))
        hash <- strftime(dates[i], "%Y%m%d%H%M")
        if ( length(file) != 1 ) next
        # Reading data and convert to time series object
        tmp          <- read.table(file, header = TRUE, comment = "#")
        data[[hash]] <- zoo(tmp[, -1], as.POSIXct(tmp[, 1], origin = "1970-01-01"))
    }

    # Spanning the grid for the plot
    grid  <- expand.grid(date = unique(as.Date(dates)), runhour = seq(0, 18, by = 6))
    dates <- as.POSIXct(grid$date) + grid$runhour * 3600; rm(grid)

    pdf(file = sprintf("check_%s_%s.pdf", station, param), width  = 12, height = 8)
        # Setting up the plot
        par(mfcol = c(length(unique(as.Date(dates))), 4),
            mar = rep(0, 4), oma = c(6,7,4,5))
    
    
        # Looping over the dates and make plots
        ylim <- c(0, max(sapply(data, max))) * c(1, 1.03)
        print(ylim)
        library("zoo")
        for ( i in seq_along(dates) ) {
            # Loading data
            hash <- strftime(dates[i], "%Y%m%d%H%M")
            if ( ! hash %in% names(data) ) {
                plot(NA, xlim = c(-1,1), ylim = c(-1,1), type = "n", xaxt = "n", yaxt = "n")
                text(0, 0, col = 2, "MISSING")
                next
            }
            # Else plotting the data
            plotpoly(data[[hash]], xaxt = "n", yaxt = "n", xaxs = "i", ylim = ylim)
            if ( i %in% seq(1, length(dates), by = par()$mfcol[1L]) )
                mtext(side = 3, sprintf("%s UTC Run", strftime(dates[i], "%H")))
            # x-Axis
            if ( i %in% seq(par()$mfcol[1L], length(dates), by = par()$mfcol[1L]) ) {
                at <- seq(24, 300, by = 24)
                axis(side = 1, at = dates[i] + at * 3600, labels = sprintf("+%d", at), las = 2)
            }
            # y-Axis
            if ( i > (length(dates) - par()$mfcol[1L]) )
                axis(side = 4, at = seq(10, max(ylim), by = 10), las = 1)
            # Date
            if ( i <= par()$mfcol[1L] )
                mtext(side = 2, line = 1, strftime(dates[i], "%a\n%Y\n%m-%d"))
        }
        mtext(side = 3, line = 2, outer = TRUE, font = 2, cex = 1.2,
              sprintf("GEFS Forecasts %s for Station %s", param, station))
        mtext(side = 4, line = 3, cex = .8,
              sprintf("%s in meters per second", param), outer = TRUE)
        mtext(side = 1, line = 4, outer = TRUE,
              cex = .8, "lead time or forecast step in hours")
    dev.off()








