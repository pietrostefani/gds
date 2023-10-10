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
#install.packages("sf")
#install.packages("raster")
#install.packages("rgdal")
#install.packages("ggplot2")
#install.packages("PROJ")

library(spatstat) 
library(sp)
library(ggmap)
library(rgdal)
library(sf)

# load data - change dat_dir as necessary
data_dir <- "D:\\Program Files\\Dropbox\\LSE\\LT\\GY460\\Week2\\"
crimes2021 <- read.csv(paste0(data_dir,"2021-11-metropolitan-street.csv"))

#inspect data
head(crimes2021)


# specify that the coordinate system of (WGS84)
# you can look up epsg codes here https://epsg.org/home.html
crimes2021points <- crimes2021  %>% 
  sf::st_as_sf(coords =  c("Longitude", "Latitude"))  %>%  # create pts from coordinates
  st_set_crs(4326)

#spatstat point patter analysis requires coordinate system in cartesian coordinates  
# transform coordinate system to OSGB36

crimes2021osgb <- st_transform(crimes2021points, 27700)



# add the x an y coordinates (eastins and northings) to the crimes data set
crime_coord <- crimes2021osgb['geometry']
crime_coord <- st_coordinates(crime_coord)
crimes2021 <- cbind(crimes2021, crime_coord)

# view the different crime types
levels(crimes2021$Crime.type)

# extract specific crimes in a new dataset 
crime2021 <- crimes2021[crimes2021$Crime.type == "Burglary",]

# define a bounding box (study area) 
# owin just creates an window object needed by spatstat and requires x min max and y min max
bbox <- owin(c(528000, 535000 ),c(182000, 189000))

# transform data frame to point pattern for spatstat
crime2021p <- ppp(crime2021$X, crime2021$Y, bbox)

# Check that the point pattern looks right by plotting it:
plot(crime2021p, clipwin = bbox )

# ripley's K-function with border correction (buffer zone)
# ?Kest to view Help
plot(Kest(crime2021p, correction = "border", rmax = 2000))

# Envelopes of K-function - top an bottome 5% under CSR:
# ?envelope to view Help
plot(envelope(crime2021p, fun = Kest, nrank = 5, funargs = list(correction = "border", rmax = 2000)))

#KERNEL DENSITY

# kernel smoother of point density:
plot(density(crime2021p, kernel = "epanechnikov", sigma = 500), axes = TRUE)



## unused
# define bounding box based on wgs84 google maps coordinates
#limit <- as.data.frame(cbind(c(-0.142,-0.062),c(51.53, 51.57)))
#coordinates(limit) <- c("V1","V2")
#proj4string(limit) <- CRS("+init=epsg:4326")

#convert bounding box to osgb36
#limitosgb = spTransform(limit, CRS("+init=epsg:27700"))
#xlim <- coordinates(limitosgb)[,1]
#ylim <- coordinates(limitosgb)[,2]