library(sp)
library(rgeos)
library(spdep)
library(rgdal)
library(data.table)
library(dplyr)
library(RColorBrewer)
library(tidyr)
library(ggplot2)

# Reading in data ---------------------------------------------------------

# Import the shapefile
filename <- "data_source/nhgis_shape"
layername <- "US_tract_2018"
blocks <- readOGR(dsn=filename, layer=layername,
                  stringsAsFactors=FALSE, encoding="latin1")

# census data
cdat <- read.csv("data_source/nhgis0008_ds239_20185_2018_tract.csv", 
                 stringsAsFactors = F)

# intterra data -----------------------------------------------------------
prelim <- fread("data_source/temp_census_matched.csv", stringsAsFactors = F)
colnames(prelim)

# Data cleaning and reformating -------------------------------------------

# Every tract has population data, so the NA therefore doesn't have data matched
# sum(is.na(cdat[, "AJWNE001"]))
# sum(is.na(prelim[, "AJWNE001"]))

# Some incidents don't have group numbers?
prelim[prelim$nfirs_group_final != "" & prelim$nfirs_code_final == "", ]

presub <- na.omit(prelim, cols = "AJWNE001")[nfirs_group_final != "",
                 c("GISJOIN", "nfirs_group_final")]

# nrow(presub)
# nrow(prelim)

# First, aggregate and pivot incident count per unique GIS location

gisinc <- presub %>% 
  group_by (GISJOIN) %>% 
  count(nfirs_group_final) %>% 
  ungroup %>% 
  pivot_wider(names_from = nfirs_group_final, values_from = n,
              values_fill = list(n = 0)) %>% 
  data.frame

# str(gisinc)



# merging relevant data ---------------------------------------------------
sort(table(prelim$state_lookup))
state_code <-  "06" # for california
pal_nodat <- "#cbc9e2"

# Filter shapefile data for state only, 
shapesub <- blocks[blocks$STATEFP == state_code, ]

# get relevant demographic data 
# Data I need:
# total population (AJWNM001)
# median income (AJZAE001), 
# without insurance (AJ35M017 + AJ35M033 + AJ35M050 + AJ35M066 / AJ35M001 total), 
# vacant houses (AJ1TE003 / AJ1TE001 total)

demodat <- cdat[, c("GISJOIN", "AJWNM001", "AJZAE001", "AJ1TE003", "AJ1TE001")]
colnames(demodat) <- c("GISJOIN", "totalpop", "medinc", "v", "t")
demodat$vacantp <- demodat$v / demodat$t



# Merging data with shapefile
rownames(demodat) <- demodat$GISJOIN
rownames(gisinc) <- gisinc$GISJOIN
democols <- c("totalpop", "medinc", "vacantp")
gcols <- c("X1", "X3")

shapesub@data[, democols] <- demodat[shapesub$GISJOIN, democols]
shapesub@data[, gcols] <- gisinc[shapesub$GISJOIN, gcols]

# Incident data
shapesub@data$fire <- shapesub@data$X1 / shapesub@data$totalpop
shapesub@data$med <- log(shapesub@data$X3 / shapesub@data$totalpop)

# My coloring function
get_colors <- function(in_num, pallete = NA, breaks = NA){
  # num Breaks = number of bars (as in bars and stars)
  # num Pallete = number of stars (as in bars and stars)
  if (length(pallete) - 1 != length(breaks)){
    warning("Incompatible number of breaks and palletes")
    return(NA)
  }
  for (i in 1:length(breaks)){
    if (in_num < breaks[i]){
      return(pallete[i])
    }
  }
  return(pallete[length(pallete)])
}


# graphing ----------------------------------------------------------------

windows()
par(mar = c(0,0,3,0))

sGraph <- function(mainTitle, sf, colname, 
                   pallete, breaks, legends,
                   pal_nodat = "#cbc9e2"){
  notNAshape <- sf[!is.na(sf@data[, colname]), ]
  NAshape <- sf[is.na(sf@data[, colname]), ]
  plot(notNAshape,
       col = sapply(notNAshape@data[, colname],
                    get_colors, 
                    pallete = pallete,
                    breaks = breaks),
       main = mainTitle,
       border = "transparent",
       lwd = 0.001)
  
  plot(NAshape, 
       col = pal_nodat, border = "transparent", add = T, lwd = 0.01)
  
  legend("bottomleft", 
         legend = c(legends, "No Data"),
         col = c(pallete, pal_nodat), pch = 15, cex = 1.5,
         bty = "n")
}

quantile(shapesub$medinc, probs = seq(0,1,0.20), na.rm = T)
sGraph("Distribution of Median Income in California", 
       sf = shapesub,
       colname = "medinc", 
       pallete = brewer.pal(5, "Greens"),
       breaks = c(20000, 50000, 70000, 90000),
       legends = c("<20k", "20k-50k", "50k-70k", "70k-90k", "90k<"))

quantile(shapesub$vacantp, probs = seq(0,1,0.20), na.rm = T)
sGraph("Distribution of Proportion of Vacant Houses in CA",
       sf = shapesub,
       colname = "vacantp",
       pallete = brewer.pal(5, "Blues"),
       breaks = c(0.025, 0.04, 0.065, 0.095),
       legends = c("<2.5%", "2.5%-4%", "4%-6.5%", "6.5%-9.5%", "9.5%<"))


quantile(shapesub$fire, probs = seq(0,1,0.20), na.rm = T)
sGraph("Distribution of Fire Incidents per Capita in CA",
       sf = shapesub,
       colname = "fire",
       pallete = brewer.pal(5, "Reds"),
       breaks = c(0.002, 0.005, 0.015, 0.035),
       legends = c("<0.2%", "0.2%-0.05%", "0.05%-1.5%", "1.5%-3.5%", "3.5%<"))

quantile(shapesub$med, probs = seq(0,1,0.20), na.rm = T)
sGraph("Distribution of Medical Incidents per Capita in CA",
       sf = shapesub,
       colname = "fire",
       pallete = brewer.pal(5, "Reds"),
       breaks = c(0.003, 0.006, 0.35, 0.6),
       legends = c(""))

# counts and proportion ----------------------------------------------------------------
prelim2<-cbind(prelim,dt_ind=as.Date(paste(prelim$year,prelim$month_of_year,prelim$day_of_month,sep='-'))) #add combined date column

nfirscounts<- prelim2 %>% group_by(dt_ind,nfirs_group_final) %>% summarize(count=n()) %>% mutate(percentage = count / sum(count)) #get incident counts by day, grouped by NFIRS group

codekey<-c("N/A","Fire",
           "Overpressure Rupture, Explosion, Overheat (No Fire)",
           "Rescue and EMS",
           "Hazardous Condition (No Fire)",
           "Service Call",
           "Good Intent Call",
           "False Alarm and False Call",
           "Severe Weather and Natural Disaster",
           "Special Incident Type",
           "Unknown") #Order of NFIRS groups

ggplot(nfirscounts)+aes(x=dt_ind,y=count,group=nfirs_group_final,color=nfirs_group_final)+
geom_line()+ggtitle("Incident Counts by NFIRS Group Over Time")+
labs(color="NFIRS Group",y="Count",x="Date")+
scale_colour_discrete(labels = codekey) #Make plot of daily incident counts

nfirscounts2 <- subset(nfirscounts,nfirs_group_final!='') %>%
  group_by(dt_ind, nfirs_group_final) %>%
  summarise(count = sum(count)) %>%
  mutate(percentage = count / sum(count)) #Calculate percentage of each day's incidents comprised of each NFIRS group

ggplot(nfirscounts2)+
aes(x=dt_ind,y=percentage,fill=nfirs_group_final)+
geom_area(alpha=0.6 , size=.1, colour="black")+
ggtitle("Proportion of Incidents by NFIRS Group Over Time")+
labs(fill="NFIRS Group Final",y="Proportion",x="Date")+
scale_fill_discrete(labels = codekey[2:11]) #Plots percentages that each NFIRS group makes up every day