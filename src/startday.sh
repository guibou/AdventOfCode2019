dayNumber=$(printf "%02d" $1)
echo $dayNumber

cp DayX.hs Day$dayNumber.hs
sed -i "s/DayX/Day$dayNumber/" Day$dayNumber.hs
