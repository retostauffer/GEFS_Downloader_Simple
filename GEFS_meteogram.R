# -------------------------------------------------------------------
# - NAME:        GEFS_meteogram.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-10-16
# -------------------------------------------------------------------
# - DESCRIPTION: Plotting some meteograms based on the bilinearely
#                interpolated GEFS forecasts.
# -------------------------------------------------------------------
# - EDITORIAL:   2018-10-16, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-10-16 15:15 on marvin
# -------------------------------------------------------------------

    rm(list = ls())
    Sys.setenv("TZ" = "UTC")

    # Using argparser here
    suppressPackageStartupMessages(library("argparse"))
    usage <- paste("Creates some meteograms based on the interpolated",
                   "GEFS forecasts.")
    parser <- ArgumentParser(description = usage)
    parser$add_argument("-d", "--date", metavar = "date", type = "character",
                        help = "Date (initial date), format YYYY-mm-dd")
    parser$add_argument("-r", "--runhour", metavar = "runhour", type = "integer",
                        help = "Model run hour (UTC), integer [0, 6, 12, 18].")

    if ( interactive() ) {
        args <- parser$parse_args(c("-d", "2018-10-09", "-r", 6))
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
    fhash <- sprintf("^GEFS_%s_.*.dat$", strftime(initialization, "%Y%m%d%H%M"))
    fdir  <- sprintf("%s/%s", "ASCII", strftime(initialization, "%Y%m%d%H%M"))
    files <- list.files(fdir)
    files <- sprintf("%s/%s", fdir, files[grep(fhash, files)])
    if ( length(files) == 0 ) stop("Sorry, no files found matching the search pattern.")
    cat(sprintf("* Found %d different files\n\n", length(files)))

    # Station list
    stations <- read.table("Stations.csv", header = TRUE, sep = ";")

# -------------------------------------------------------------------
# Loading data
# -------------------------------------------------------------------
    extract_param_name <- function(x) {
        x <- regmatches(x, regexpr("[a-z0-9]+_[A-Za-z]+\\.dat", x))
        strsplit(x, "_")[[1]][1]
    }

    library("zoo")
    params <- c("ff100m", "ff80m", "ff10m", "fg10m")
    data <- list()
    for ( stn in as.character(stations$Station) ) {
        data[[stn]] <- list()
        tmpfiles <- files[grep(sprintf("(%s)_%s\\.dat$", paste(params, collapse = "|"), stn), files)]
        for ( file in tmpfiles ) {
            param <- extract_param_name(file)
            tmp   <- read.table(file, header = TRUE, comment = "#")
            tmp   <- zoo(tmp[, -1], as.POSIXct(tmp[, 1], origin = "1970-01-01"))
            data[[stn]][[param]] <- tmp
        }
    }


    plot_meteogram <- function(x, main = NA, ...) {
        par(mfrow = c(length(x), 1))
        for ( i in seq_along(x) ) {
            boxplot(t(x[[i]]), main = main, ylab = names(x)[i], ...)
        }
    }
    plot_meteogram(data[[5]], names(data)[1])








