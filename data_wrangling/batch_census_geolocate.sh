#!/bin/bash

# Get directory information
cd "$(dirname "$0")"
cd ".."

# Split file from Intterra into max size for Census API
echo "- SPLITTING AND FORMATTING DATA (may take some time on poor hardware)"
Rscript data_wrangling/chunk_select_export_for_census.R

# Submit split files to Census API
echo "- GEOCODING DATA WITH CENSUS API"
for i in {1..119..1}
	do
		echo "- geocoding part $i of 119"
		filename="data_source/incidents_pre_${i}.csv"
		fileout="data_source/with_census_${i}.csv"
		curl --form addressFile=@$filename --form benchmark=Public_AR_Current --form vintage=ACS2018_Current https://geocoding.geo.census.gov/geocoder/geographies/addressbatch --output $fileout
	done

# Combine returned files into single file (without having to read the entire file into memory -- bashiscool)
echo "- TIDYING UP"
for f in data_source/with_census_*; do mv "$f" "$f.csv"; done
Out="./data_source/stacked_geolocated_complete.csv"
i=0   
for filename in ./data_source/with_census_*.csv; do 
 if [ "$filename"  != "$Out" ] ;
 then 
   if [[ $i -eq 0 ]] ; then 
      head -1  "$filename" >   "$Out"
   fi
   tail -n +2  "$filename" >>  "$Out"
   i=$(( $i + 1 ))
 fi
done