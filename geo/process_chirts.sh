cd ~/mortalityblob/mortnew/chirts/

# Take a tif, convert it to xyz, clean it with awk
for f in Tmin*.tif;
do
  echo $f
  dt=${f:5:7}
  new=${f%.tif}.xyz
  gdal_translate -of XYZ $f $new
  gawk -i inplace '{ if ($3 > -1000) { printf "'$dt' %.3f %.3f %.3f\n", $1, $2, $3 } }' $new
done

for f in Tmax*.tif;
do
  echo $f
  dt=${f:5:7}
  new=${f%.tif}.xyz
  gdal_translate -of XYZ $f $new
  gawk -i inplace '{ if ($3 > -1000) { printf "'$dt' %.3f %.3f %.3f\n", $1, $2, $3 } }' $new
done

