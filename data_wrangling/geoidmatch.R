#############################################################################
# Code Purpose: Combine patrick's data with demographic data
# Outputs: csv files
# Last updated: 10/13/2020
#############################################################################

library(sp)
library(rgeos)
library(spdep)
library(rgdal)
library(data.table)
library(dplyr)


# import shapefile and census data ----------------------------------------
filename <- "datacopy/nhgis_shape"
layername <- "US_tract_2018"


# Import the shapefile
blocks <- readOGR(dsn=filename, layer=layername,
                  stringsAsFactors=FALSE, encoding="latin1")


# gjoin <- blocks@data[, c("GISJOIN", "STATEFP", "COUNTYFP", "TRACTCE")]
# gjoin$sct <- paste0(gjoin$STATEFP, gjoin$COUNTYFP, gjoin$TRACTCE)

# census data
cdat <- read.csv("data_source/nhgis0008_ds239_20185_2018_tract.csv", 
                 stringsAsFactors = F)

getcode <- function(x){
  #G 01 0 055 0 000600
  # State and county are padded by 0s at the end, so remove those
  return(paste0(substr(x, 2, 3), substr(x, 5, 7), substr(x, 9, 14)))
  }

cdat$sct <- sapply(cdat$GISJOIN, getcode)


# Intterra data that already has gisjoin code -----------------------------
# Merging the data that we already have block group data

# Merging census and sample data
# alldat <- fread("isub.csv")
alldat <- fread("data_source/incidents_for_d2k.csv")
nrow(alldat)
sum(alldat$nfirs_group_final != "")
sum(alldat$nfirs_category_final != "")


withtract <- alldat[alldat$tract_block_group != "", ]

#15000US 41 043 030800 3
# Padded with stuff + state + county + tract + block group
withtract$sct <- sapply(withtract$tract_block_group, substr, 
                        start = 8, stop = 18)

mdf <- merge(x = withtract, y = cdat, by = "sct", all.x = TRUE)

# Check number matched
sum(withtract$sct %in% cdat$sct)
nrow(withtract$sct)
#100 % match 
# Intterra data that already has gisjoin code
write.csv(mdf, "data_source/temp_census_matched.csv")


# reading and cleaning patrick input --------------------------------------
# Merging the data for the ones we didn't have blockgroup data
# First run
# pat <- fread("stacked_with_census_new.csv")
# Second run
pat <- fread("data_source/stacked_geolocated_complete.csv")

colnames(pat) <- c("id", "add", "match", "exact", "matchedadd", "coord",
                   "tigerline", "tigerline_side", 
                   "state", "county", "tract")
pat <- pat[state != "fips1"]

str(pat)
padThis <- function(input, padnum){
  if (is.na(input)){
    return(NA)
  }
  
  thislen <- nchar(toString(input))
  if (thislen > padnum) {
    return("bad pad")
  } else {
    return(paste0(paste(rep("0", padnum - thislen), collapse = ""), 
                  toString(input)))
  }
}

pat$fstate <- sapply(pat$state, padThis, padnum = 2)
sum(pat$fstate == "bad pad", na.rm = T)
pat$fcount <- sapply(pat$county, padThis, padnum = 3)
sum(pat$fcount == "bad pad", na.rm = T)
pat$ftract <- sapply(pat$tract, padThis, padnum = 6)
sum(pat$ftract == "bad pad", na.rm = T)

pat$sct <- paste0(pat$fstate, pat$fcount, pat$ftract)
pat$sct <- na_if(pat$sct, "NANANA")

pat <- pat[sct != "00000000000"]
# Check if every non NA value in pat is in block data
which(!(pat$sct %in% cdat$sct) & !is.na(pat$sct))


mdf <- merge(x = pat, y = cdat, by = "sct", all.x = TRUE)
f1 <- mdf
# tail(mdf)
# write.csv(mdf, "stacked_census_matched.csv")

sum(!c(f1$id %in% alldat$id))

# f1 <- fread("stacked_census_matched.csv")
#alldat
colnames(f1)
final1 <- merge(x = f1, y = alldat, by = "id", all.x = TRUE)

# First run
# write.csv(final1, "merged_stacked_census_matched.csv")
# Second run
write.csv(final1, "data_source/merged_part_3_final_pls.csv")

# irrelevant code ---------------------------------------------------------
# These are my notes that I'm keeping for archive purposes. 


# head(withtract$tract_block_group)

#G 01 0 055 0 000600

#15000US 41 043 030800 3
#15000US410430308003

# "030800" %in% blocks@data$TRACTCE

#STATE+COUNTY+TRACT+BLOCK GROUP
# Need state-count-tract to merge

#2+3+6+1

# View(cdat[c(5000:5010), c(1:37)])



# How do i join this????
# tail(gisjoin_geoid)

# "144995715" %in% blocks$GEOID



# Where do i join files what

# test <- read.csv("census_sub.csv")
# nrow(test[test < 100000000])
# format(100000000, scientific = F)
# 
# test1 <- sapply(test$tract, format, scientific = F)
# test2 <- sapply(test1, nchar)
# table(test2)

