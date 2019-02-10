$in unix

# TRMM
find /auto/sc06_data1/data/TRMM/Ver7/3B42 -type f -name "*.HDF.Z" > TRMM3B42list.txt
# Era Interim
find /auto/sc05_data2/data/ERA-Interim/v2/ERA-interim.TMP/0.75grid/6hourly/pressure/air -type f -name "*.nc" > air_eralist.txt
find /auto/sc05_data2/data/ERA-Interim/v2/ERA-interim.TMP/0.75grid/6hourly/pressure.bk2/clwc -type f -name "*.nc" > clwc_eralist.txt
find /auto/sc05_data2/data/ERA-Interim/v2/ERA-interim.TMP/0.75grid/6hourly/pressure/div -type f -name "*.nc" > div_eralist.txt
find /auto/sc05_data2/data/ERA-Interim/v2/ERA-interim.TMP/0.75grid/6hourly/pressure/gph -type f -name "*.nc" > gph_eralist.txt
find /auto/sc05_data2/data/ERA-Interim/v2/ERA-interim.TMP/0.75grid/6hourly/pressure/omega -type f -name "*.nc" > omega_eralist.txt
find /auto/sc05_data2/data/ERA-Interim/v2/ERA-interim.TMP/0.75grid/6hourly/pressure/rhum -type f -name "*.nc" > rhum_eralist.txt
find /auto/sc05_data2/data/ERA-Interim/v2/ERA-interim.TMP/0.75grid/6hourly/pressure/rvor -type f -name "*.nc" > rvor_eralist.txt
find /auto/sc05_data2/data/ERA-Interim/v2/ERA-interim.TMP/0.75grid/6hourly/pressure/shum -type f -name "*.nc" > shum_eralist.txt
find /auto/sc05_data2/data/ERA-Interim/v2/ERA-interim.TMP/0.75grid/6hourly/pressure/uwnd -type f -name "*.nc" > uwnd_eralist.txt
find /auto/sc05_data2/data/ERA-Interim/v2/ERA-interim.TMP/0.75grid/6hourly/pressure/velp -type f -name "*.nc" > velp_eralist.txt
find /auto/sc05_data2/data/ERA-Interim/v2/ERA-interim.TMP/0.75grid/6hourly/pressure/vwnd -type f -name "*.nc" > vwnd_eralist.txt
# Cloudsat
find /sc01_data/Cloudsat/2B-GEOPROF.R04 -type f -name ".zip" > cloudsatGprofList.txt
find /sc03_data1/Cloudsat/2B-CWC-RO.R04 -type f -name ".zip" > cloudsatCwcList.txt



$in windows
dir/s/b *.mat > grided2A12.txt
