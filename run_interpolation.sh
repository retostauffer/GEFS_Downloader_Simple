# -------------------------------------------------------------------
# - NAME:        run_interpolation.sh
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-10-18
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-10-18 09:54 on marvin
# -------------------------------------------------------------------


TMP=".tmplist"
TMP2=".tmplist2"
ls prepared/* > $TMP
sed -i "s/_/\ /g" $TMP
cat $TMP | awk '{print $2}' | sort | uniq > $TMP2 

while read line ; do

    date=`printf "%s-%s-%s" ${line:0:4} ${line:4:2} ${line:6:2}`
    hour=`printf "%s" ${line:8:2}`

    # Executing
    echo "Rscript GEFS_interpolate.R -d ${date} -r ${hour}"
    Rscript GEFS_interpolate.R -d ${date} -r ${hour}
    

done < $TMP2

rm $TMP
rm $TMP2
