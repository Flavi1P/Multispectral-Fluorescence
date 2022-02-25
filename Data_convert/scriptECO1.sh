chmod 640 *.txt

j=36
while [ "$j" -le 50 ] ; do

if [ $j -lt 10 ]; then
file="TONGA_CTD_00"$j
elif [ $j -lt 100 ]; then
file="TONGA_CTD_0"$j
else
file="TONGA_CTD_"$j
fi

if [ -f $file".txt"  -o -f $file"_ECO1.txt" ] ; then
echo $file

if [ -f $file"_ECO1.txt" ] ; then
echo $file "ALREADY DECODED"

else
sed 's/^.........//' $file.txt > toto
sed 's/ 2019.*99\:99//' toto > titi
sed 's/\:/ /g' titi > toto
sed 's/	/ /g' toto > titi
sed 's///g' titi > output
rm -f toto titi
cp output $file"_ECO1.txt"

fi

fi	
j=`expr $j + 1`

done