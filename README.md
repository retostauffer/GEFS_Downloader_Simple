
Downloading GEFS Forecast Data
==============================

* Script: `GEFS.py`

Used to download grib2 forecast files from the nomads servers.
Note that they only provide public access to a rolling archive
which contains the most recent 7 days of forecasts or so.
Forecasts become available four times a day (initialized at 00UTC,
06UTC, 12UTC, and 18UTC).

Usage:

* `python GEFS.py -d <date> -r <runhour>`

The script provides some usage, see `python GEFS.py --help`.
If `wgrib2` is installed this script performs spatial subsetting
using the `wgrib2 -small_grib` option. The subset domain is hardcoded
within the `GEFS.py` script if changes are needed.


Prepare grib2 files
===================

* Script: `GEFS_prepare.sh`

Small bash script which checks for available grib files and converts
them to NetCDF.

* Usage: `bash GEFS_prepare.sh`

Uses the files in `data/*.grb2`, converts them to NetCDF and stores
the converted files in `prepared/*.nc`. Combines the different forecast
steps from one GEFS run and one member into one NetCDF file.

Interpolate Data
================

* Script: `GEFS_interpolare.R`

I've quickly written an R-based interpolation script (most efficient
way for me for today). 

* Usage: `Rscript GEFS_interpolate.R -d <date> -r <runhour>`

The script comes with a usage, please check `Rscript GEFS_interpolate.R --help`
for more information. Reads the NetCDF files from the `prepared` folder and
creates ouptut in `ASCII`.



