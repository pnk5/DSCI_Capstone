#############################################################################
# Code Purpose: Spatial plots of the different demographic variables
# Outputs: Cleaned SVI Data
# Download SVI from 
# https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html
# where Year: 2018, Geography: United States, Geography Type: Census Tracts,
# File Type: CSV
# downloaded file should be named "SVI2018_US.csv"
#############################################################################

svi <- read.csv("data_source/SVI2018_US.csv")

# Changing FIPS code to GISJOIN code for merging
FIPStoGIS <- function(input){
  if (is.na(input)){
    return(NA)
  }
  
  thislen <- nchar(toString(input))
  if (thislen > 11) {
    return("Invalid Fips")
  } else {
    fips <- paste0(paste(rep("0", 11 - thislen), collapse = ""), 
                   toString(input))
    #G 01 0 055 0 000600
    return(paste0("G", 
                  substr(fips, 1, 2), "0", 
                  substr(fips, 3, 5), "0",
                  substr(fips, 6, 11)))
  }
}

svi$GISJOIN <- sapply(svi$FIPS, FIPStoGIS)
colnames(svi)

# E_LIMENG - % persons who speak english less than well age 5+
# EP_GROUPQ - % persons in group quarters
exportThis <- svi[, c("GISJOIN", "EP_LIMENG", "EP_GROUPQ", 
                      "RPL_THEME1", "RPL_THEME2", "RPL_THEME3", "RPL_THEME4", 
                      "SPL_THEME1", "SPL_THEME2", "SPL_THEME3", "SPL_THEME4", 
                      "RPL_THEMES", "SPL_THEMES"
                      )]
write.csv(exportThis, "data_source/svi_edited.csv")
