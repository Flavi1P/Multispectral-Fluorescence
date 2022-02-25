chmod 640 *.asc
chmod 640 *.txt

gfortran ecoconvert.for
echo "pressure CN1 CN2 CN3" > header

j=1
while [ "$j" -le 5 ] ; do

if [ $j -lt 10 ]; then
file="TONGA_CTD_00"$j
elif [ $j -lt 100 ]; then
file="TONGA_CTD_0"$j
else
file="TONGA_CTD_"$j
fi

if [ -f $file".txt" ] ; then

echo $file

sed 's/^.........//' $file.txt > titi
sed 's///g' titi > toto
sed 's/	/ /g' toto > titi
sed 's/2019\] 99\/99\/99 99\:99\:99 440/C3 440/g' titi > toto
sed 's/2019\] 99\/99\/99 99\:99\:99 695/C2 695/g' toto > titi
sed 's/2019\]/C1/g' titi > toto
sed 's/\:/ /g' toto > output

sed -n '/C1/p' output > toto
sed 's/C1 //g' toto > C1
sed -n '/C2/p' output > toto
sed 's/C2 //g' toto > C2
sed -n '/C3/p' output > toto
sed 's/C3 //g' toto > C3

./a.out

rm -f toto titi C1 C2 C3 output ECO1.txt ECO2.txt
cat header ECO1.txt > $file-ECO1.txt
cat header ECO2.txt > $file-ECO2.txt

fi

j=`expr $j + 1`

done
rm -f a.out header
