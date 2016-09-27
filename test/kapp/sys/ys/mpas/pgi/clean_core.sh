#!/bin/bash

rm -f ./coreJob.*
rm -f ./run/output.nc
rm -f ./run/restart*
cd ./core_atmosphere
rm -f atmosphere_model
find . -name *.o -exec rm {} \;
find . -name *.mod -exec rm {} \;
make clean CORE=atmosphere
