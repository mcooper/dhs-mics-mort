sudo chown mattcoop /mnt
cd /mnt

for i in {1980..2016..1}
do
  wget http://hydrology.princeton.edu/data/pgf/v3/0.25deg/daily/prcp_daily_$i-$i.nc
  wget http://hydrology.princeton.edu/data/pgf/v3/0.25deg/daily/tas_daily_$i-$i.nc





done

gdalbuildvrt -separate -a_srs 'EPSG:4326' myvrt $(ls | grep prcp)




wget http://hydrology.princeton.edu/data/pgf/v3/0.25deg/daily/tas_daily_1981-1981.nc -o tas_daily_1981-1981.p1.nc 

wget http://hydrology.princeton.edu/data/pgf/v3/0.25deg/daily/prcp_daily_1981-1981.nc
START=$(date +%s)
gdal_calc.py -A prcp_daily_1981-1981.nc --A_band=10 --calc='A' --outfile=prcp_daily_1981-1981.p1.nc --format='netCDF'
gdal_calc.py -A prcp_daily_1981-1981.nc --A_band=10 --calc='A**2' --outfile=prcp_daily_1981-1981.p2.nc --format='netCDF'
gdal_calc.py -A prcp_daily_1981-1981.nc --A_band=10 --calc='A**3' --outfile=prcp_daily_1981-1981.p3.nc --format='netCDF'
gdal_calc.py -A prcp_daily_1981-1981.nc --A_band=10 --calc='A**4' --outfile=prcp_daily_1981-1981.p4.nc --format='netCDF'
END=$(date +%s)

gdal_calc.py -A tas_daily_1981-1981.p1.nc --calc='A**2' --outfile=tas_daily_1981-1981.p2.nc --format='netCDF'
gdal_calc.py -A tas_daily_1981-1981.p1.nc --calc='A**3' --outfile=tas_daily_1981-1981.p3.nc --format='netCDF'
gdal_calc.py -A tas_daily_1981-1981.p1.nc --calc='A**4' --outfile=tas_daily_1981-1981.p4.nc --format='netCDF'

gdaldimtranslate 



START=$(date +%s);
gdal_calc.py -A prcp_daily_1980-1980.nc --A_band=11 --calc='A**4' --outfile=prcp_daily_1980-1980.p4.11.tif
END=$(date +%s);
echo $((END-START)) | awk '{print int($1/60)":"int($1%60)}'

for i in {1..365..1}
do
  echo $i
  cp my.tif my$i.tif
done

