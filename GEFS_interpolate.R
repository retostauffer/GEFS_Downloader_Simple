# -------------------------------------------------------------------
# - NAME:        GEFS_read.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-10-12
# -------------------------------------------------------------------
# - DESCRIPTION:
# -------------------------------------------------------------------
# - EDITORIAL:   2018-10-12, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-10-12 07:48 on marvin
# -------------------------------------------------------------------



# -------------------------------------------------------------------
# Use ncdf4 to read the data and raster to create geodata which
# makes it easy to perform the bilinear interpolation.
# -------------------------------------------------------------------
    library("ncdf4")
    library("raster")

# -------------------------------------------------------------------
# Processing the netcdf files
# -------------------------------------------------------------------
    file = "prepared/GEFS_201810050600_10.nc"
    nc <- nc_open(file)

    cat(sprintf("* %s: %s\n", file,
        paste(sapply(nc$var, function(x) x$name), collapse = ", ")))

    nc_to_raster <- function(nc, var) {
        if ( inherits(var, c("integer", "numeric")) ) {
            print('take message by integer')
        } else {
            stop('need to search for variable in nc')
        }

        times <- ncvar_get(nc, "time")
        lons <- ncvar_get(nc, "longitude")
        lats <- ncvar_get(nc, "latitude")
        dlon <- unique(diff(lons)); stopifnot(length(dlon) == 1)
        dlat <- unique(diff(lats)); stopifnot(length(dlat) == 1)

        # Create raster template
        empty <- raster(nrows = length(lons), ncol = length(lats),
                        xmn = as.numeric(min(lons) - dlon/2.),
                        xmx = as.numeric(max(lons) + dlon/2.),
                        ymn = as.numeric(min(lats) - dlat/2.),
                        ymx = as.numeric(max(lats) + dlat/2.))

        # Looping over data, create RasterStack object 
        data <- ncvar_get(nc, "GUST_surface")
        for ( i in seq_along(times) ) {
            tmp <- empty
            values(tmp) <- as.vector(t(data[,,i]))
            library("maps")
            plot(tmp); map(add = TRUE)
        }
    }

    x <- nc_to_raster(nc, 1)
    plot(x)
    nc_close(nc)
