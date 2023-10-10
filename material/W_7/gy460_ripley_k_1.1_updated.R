########################################################
# Title: GY460 Ripley K and point pattern analysis
# Author: SG
# Date: 16/01/2022
########################################################

# This exercise uses spatstat to do the point pattern analysis
# sp is required for some spatial data transformations
# ggmap is for plotting
# https://cran.r-project.org/web/packages/spatstat/vignettes/getstart.pdf

# install packages as needed
#install.packages("ggmap")
#install.packages("spatstat")
#install.packages("sp")

library(spatstat) 
library(sp)
library(ggmap)
library(sf)

# load data - change dat_dir as necessary
data_dir <- "C:/Users/aavil/OneDrive/Documentos/A - Trabajo/GTA/GY460/Descriptive indicators of spatial clusters and concentration/Data for Practical Descriptive/"
crimes2021 <- read.csv(paste0(data_dir,"2021-11-metropolitan-street.csv"))

#inspect data
head(crimes2021)




### HERE STARTS THE CHANGED CODE

crimes2021points <- sf::st_as_sf(crimes2021, coords =  c("Longitude", "Latitude"))   # Transform to spatial data, specifying which variables are the coordinates
crimes2021points <- st_set_crs(crimes2021points, 4326) # specify that the coordinate system of (WGS84). You can look up epsg codes here https://epsg.org/home.html

#spatstat point patter analysis requires coordinate system in cartesian coordinates  
# transform coordinate system to OSGB36


crimes2021osgb <- st_transform(crimes2021points, 27700) #~ before: #crimes2021osgb = spTransform(crimes2021, CRS("+init=epsg:27700"))

# add the x an y coordinates (eastins and northings) to the crimes data set
#crimes2021$eastings <- coordinates(crimes2021osgb)[,1]
#crimes2021$northings <- coordinates(crimes2021osgb)[,2]
crime_coord <- crimes2021osgb$geometry # take the spatial representation of each observation and store it into a vector 
crime_coord <- st_coordinates(crime_coord) # extract the coordinates of that vector
crime_coord <- as.data.frame(crime_coord) # from a matrix to a data.frame


#  join it with the crimes data (adds the coordinates in the new projections to the original data)
crimes2021$eastings <- crime_coord$X
crimes2021$northings <- crime_coord$Y

#### HERE ENDS THE CHANGED CODE




# view the different crime types
crimes2021$Crime.type <- factor(crimes2021$Crime.type)
levels(crimes2021$Crime.type) #can only do levels on factor variables

# view the different crime types
table(crimes2021$Crime.type)
unique(crimes2021$Crime.type)

# extract specific crimes in a new dataset 
crime2021 <- crimes2021[crimes2021$Crime.type == "Burglary",]

# define a bounding box (study area) 
# owin just creates an window object needed by spatstat and requires x min max and y min max
bbox <- owin(c(528000, 535000 ),c(182000, 189000))

# transform data frame to point pattern for spatstat
crime2021p <- ppp(crime2021$eastings, crime2021$northings, bbox)

# Check that the point pattern looks right by plotting it:
plot(crime2021p, clipwin = bbox)

# ripley's K-function with border correction (buffer zone)
# ?Kest to view Help
plot(Kest(crime2021p, correction = "border", rmax = 2000))

# Envelopes of K-function - top an bottome 5% under CSR:
# ?envelope to view Help
plot(envelope(crime2021p, fun = Kest, nrank = 5, funargs = list(correction = "border", rmax = 2000)))

#KERNEL DENSITY

# kernel smoother of point density:
plot(density(crime2021p, kernel = "epanechnikov", sigma = 500), axes = TRUE)
