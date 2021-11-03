# This script is used to reorganize and calculate distance between household IDs and a manmade geographic phenomenon like the Aswan Dam or Nasser Lake

library(geosphere)
library(tidyverse)
library(highcharter)
library(broom)

setwd('C:\\Users\\murad\\Documents\\DHS_Egypt_Surveys')

file <- read.csv("Export_GPS.csv") #Import the exported long/lat data file gathered from CSPRO DHS

keeps <- c('DHSCLUST','ADM1FIPSNA','DHSREGNA','LONGNUM','LATNUM') #We only need these variables from the exported data file

df <- file[keeps] #Making a new df

#Some GPS data points are 0,0 for whatever reason, so we need to remove these rows

df_pre_length <- nrow(df) #Getting length of df before I eliminate all clusters with no GPS data
cat('pre length is ', df_pre_length) 

df[df == 0] <- NA_character_
nas1 <- rownames(df[rowSums(is.na(df)) > 0,])
df4 <- df[nas1,]
df[!(row.names(df) %in% nas1),]
df <- na.omit(df)




df_post_length <-nrow(df) #Getting length of df after I elimate all clusters with no GPS data
cat('post length is ', df_post_length) 

if (df_pre_length - df_post_length != length(nas1)){
  
  
  stop()
}



if (df_pre_length != df_post_length){
  print("Clusters with invalid GPS data were removed here")
}

number_of_clusters1 <- length(df$DHSCLUST)
cat("There are", number_of_clusters1, "clusters in the GPS dataset")


target_gps <- c(31.749997,22.416665) #GPS coordinates for Lake Nasser

df <- df %>% mutate(CTD = distGeo(target_gps, cbind.data.frame(LONGNUM, LATNUM))) #Calculating geographic distance between target and DHSID using surface ellipsoid calculations

dist <- df$CTD #Gather distances 

dist_km <- dist/1000 #Convert to km from m


# Now we need to gather all the DHSID's that were confirmed positive for HEPC antibodies in the 2008 DHS survey
file2 <- read.csv("Export_DHS.csv")

keeps2 <- c('ï..OBCLUST','OB03A')

df2 <- file2[keeps2]
df2 <- rename(df2, antibody_count = OB03A)
df2 <- rename(df2, DHSCLUST = ï..OBCLUST)

df2_pre_length <- nrow(df2)

double_check <- as.data.frame(table(df2$antibody_count))

df2[df2 == 7] <- NA_character_ #This is to check if there are any indeterminate blood antibody test results, there were none for this dataset but I am keeping it to be reproducible

df2<- na.omit(df2)

df2_post_length <- nrow(df2)

if (df2_pre_length != df2_post_length){
  print("Clusters with indeterminate antibody results were removed here")
}


df2 <- aggregate(antibody_count ~ DHSCLUST, FUN=sum, data=df2)  #The data was organized such that each cluster was broken into households for Hep C blood tests, but since we are drawing correlations between GPS and HEP C, we need to aggregate by cluster

number_of_clusters <- length(df2$DHSCLUST)
cat("There are", number_of_clusters,"clusters in the household survey dataset")


length(df2$DHSCLUST) #Checking the difference in lengths between the two datasets, they vary due to missing GPS data from some of the clusters, 16 clusters eliminated here
length(df$DHSCLUST)

df3 <- merge(df, df2, by = 'DHSCLUST')  #Mostly just some cosmetic changes here, I change the names of all columns and convert the distance variable from meters to km
df3 <- rename(df3, distance = CTD)
df3 <- rename(df3, longitude = LONGNUM)
df3 <- rename(df3, latitude = LATNUM)
df3 <- rename(df3, region = DHSREGNA)
df3 <- rename(df3, governorate = ADM1FIPSNA)
df3 <- rename(df3, cluster = DHSCLUST)
df3$distance <- df3$distance/1000
df3 <- rename(df3, lat = latitude)
df3 <- rename(df3, lon = longitude)

df3_pre_length <- nrow(df3)


df3[df3 == 0] <- NA  #    THIS IS FOR REMOVING ALL POINTS WITH 0 ANTIBODY PREVALENCE
df3<- na.omit(df3)

df3_post_length <- nrow(df3)
 

if (df3_pre_length != df3_post_length){
  print("Clusters with no antibody prevalence were removed here")
}

length(df3$cluster) #Something weird happens here, we are losing about 20 more clusters in the merge, presumably because there are clusters that don't align? Very difficult to figure out why

write.csv(df3, file = "Murads cool data export.csv", row.names=F)

# The next bit is for a Loess regression

modlss <- loess(antibody_count ~ distance, data = df3)

fit <- arrange(augment(modlss), distance) %>% 
  mutate(.se = predict(modlss, se = TRUE)$se.fit)

hc <- hchart(
  df3,
  type = "scatter",
  hcaes(x = distance, y = antibody_count),
  name = "Hep C Antibody Prevalence as a Function of Distance from Lake Nasser",
  showInLegend = TRUE
)

qtint <- qt(0.975, predict(modlss, se = TRUE)$df)

hc %>%
  hc_add_series(
    fit,
    type = "spline",
    hcaes(x = distance, y = .fitted),
    name = "Fit",
    id = "fit", # this is for link the arearange series to this one and have one legend
    lineWidth = 1,
    showInLegend = TRUE
  ) %>% 
  hc_add_series(
    fit,
    type = "arearange",
    name = "SE",
    hcaes(x = distance, low = .fitted - qtint*.se, high = .fitted + qtint*.se),
    linkedTo = "fit", # here we link the legends in one.
    showInLegend = FALSE,
    color = hex_to_rgba("gray", 0.2),  # put a semi transparent color
    zIndex = -3 # this is for put the series in a back so the points are showed first
  )


hcmap(
  map = 'countries/eg/eg-all.js', 
  data = df3,
  type = "mappoint",
  name = "Localization of Hep C Prevalence in DHS Clusters", 
  value = "antibody_count", name = "Antibody Prevalence",
  borderWidth = 0,
  nullColor = "#d3d3d3",
) 

df3 <- rename(df3, z = antibody_count)

hcmap('countries/eg/eg-all.js', showInLegend = FALSE) %>%
  hc_add_series(
    data = df3, 
    type = "mapbubble",
    minsize = "0.1%",
    maxsize = "0.2%",
    value = "z",
    name = "Antibody Prevalence", 
    tooltip = list(
      pointFormat = "{point.z}: {point.governorate:,.0f}
      ({point.lat:,.2f}, {point.lon:,.2f})"
    )
  ) %>%
  hc_colorAxis(minColor = "pink",
               maxColor = "purple") %>%
  hc_mapNavigation(enabled = TRUE)

df3 <- rename(df3, antibody_count = z)

