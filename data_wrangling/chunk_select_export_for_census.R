install.packages("pacman", repos = "http://cran.us.r-project.org")
library("pacman")

#define a macro for negative logic on vectors
`%notin%` <- Negate(`%in%`)

pacman::p_load(data.table, qwraps2, ggmap, RJSONIO, RCurl)
incidents <- fread("data_source/incidents_for_d2k.csv")
incidents <- data.table(incidents)
#library(qwraps2)
#library(ggmap)
#library(RJSONIO)
#library(RCurl)

# Prune states with fewer than 100 incidents, faster to do this manually than 
# to have to read and sum the file
incidents <- incidents[state_lookup %notin% c("Delaware", "Florida", "Indiana",
                                             "Iowa", "Kansas", "Kentucky",
                                             "Maine", "Massachusetts",
                                             "Montana", "Nevada", "New York",
                                             "North Carolina", "Ohio",
                                             "Rhode Island", "Texas",
                                             "West Virginia")]

# Select those incidents missing geolocation data from Intterra
incidents_with_no_tract <- incidents[tract_block_group == ""]
columnns_for_census_transmission_no_tract <- incidents_with_no_tract[, c("id", "full_address", "city",
                                                  "state_lookup", "zip_code_lookup")]

# Set the rows expected by the Census API
column_other = c("id", "full_address", "city", "state_lookup", 
                 "zip_code_lookup")

# Read in 10k line chunks to data.table elements and use the sweet yummy power of 
# C to get them out in less than 2% of the cycles pandas takes #notmypython
for (n_chunk in seq(from = 1, 
                    to = nrow(columnns_for_census_transmission_no_tract) / 9999)) {
  chunk = sample(columnns_for_census_transmission_no_tract
                 [((n_chunk - 1)*9999 + 1):(n_chunk*9999)],)
  setcolorder(chunk, column_other)
  fwrite(chunk, paste("data_source/incidents_pre_", n_chunk, ".csv", sep=""))
}

