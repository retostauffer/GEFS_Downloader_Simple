#!/bin/bash
# -------------------------------------------------------------------
# - NAME:        GEFS_prepare.sh
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-10-12
# -------------------------------------------------------------------
# - DESCRIPTION: Split/recombine/convert the downloaded grib2 files.
#                Creates a set of netCDF file each containing one
#                specific time ..
# -------------------------------------------------------------------
# - EDITORIAL:   2018-10-12, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-10-12 08:20 on marvin
# -------------------------------------------------------------------

set -u

# Data directory
datadir="data"
prepdir="prepared"
tmpgrib=".grb"

if [ ! -d ${prepdir} ] ; then mkdir -p ${prepdir}; fi

# Getting available model runs based on output directory names
dirs=(`find data -maxdepth 1 -type d -regextype posix-egrep -regex "^${datadir}/[0-9]{12}$"`)

# Looping over the directories
for dir in ${dirs[@]}; do

    printf "* Preparing %s\n" ${dir}

    # Extracting date/time (used for output file name)
    datetime=`echo $dir | grep -P "[0-9]{12}$" -o`

    # Find available files, member by member
    mem=0
    while [ $mem -le 20 ] ; do
        printf "  Date/Time %s, member %02d\n" $datetime $mem

        # Find available files for this member
        pattern=`printf "^%s/GEFS_[0-9_]{13}_%02d_f[0-9]{3}_subset\.grb2$" ${dir} ${mem}`
        files=(`find ${dir} -type f -regextype posix-egrep -regex "${pattern}" | sort`)
        tmp=""

        # Create a temporary grib file with all forecast steps
        for file in ${files[@]}; do tmp=`printf "%s %s" "${tmp}" "${file}"`; done
        cat ${tmp} > $tmpgrib

        # Create combined NetCDF (per model initialization
        # and ensemble member, we cannot handle multiple ensemble
        # members with wgrib2->netCDF).
        out=`printf "%s/GEFS_%s_%02d.nc" ${prepdir} ${datetime} $mem`
        wgrib2 $tmpgrib -netcdf $out

        # Remove temporary file
        if [ -f $tmpgrib ] ; then rm $tmpgrib; fi 

        let mem=$mem+1
    done


done
