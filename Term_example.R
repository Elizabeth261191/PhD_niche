#####Terminalia example

#Clean data and georeference - Terminalia

setwd("C:/Users/etelford.IC.002/Dropbox/Phd/R")

library(dismo)
library(maptools)
library(magrittr)
library("CoordinateCleaner")
library("sp")

Terminalia <- gbif("Terminalia", geo = TRUE)

write.csv(Terminalia, file = "Terminalia.csv")

data("wrld_simpl")
par(mfrow=c(1,1))
plot(wrld_simpl, axes=TRUE, col="light blue")
box()
points(Terminalia$lon, Terminalia$lat, col='orange', pch=20, cex=0.75)
click()
#Explore dataset
dim(Terminalia)
colnames(Terminalia)

TTgeo <- subset(Terminalia, !is.na(lon), !is.na(lat))
dim(TTgeo)
TTgeos <- TTgeo[, c(109, 122, 3)]
dim(TTgeos)

#1. Check coordinate errors ----

#common mistakes might come from sign typos or errorneous copies to either
#latitude or longitude. So, map and then check! 
TTSen <- subset(TTgeo,TTgeo$country == "Peru") #the most eastwards possible in the dataset
TTSen[, c(109,122,3)]

#First issue: check if any coordinates are (0,0). The 'gbif' function sets
#coordinates that are (0,0) to NA. Let's check
lonzero <- subset(TTgeos, lon==0)
which(TTgeos$lon==0)
lonzero

latzero <- subset(TTgeos, lat==0)
which(TTgeos$lat==0)
latzero

lonlatzero <- subset(TTgeos, TTgeos$lon == 0, TTgeos$lat ==0)
lonlatzero

#2. Duplicate records ----

#records from different people or years within the same species
dups <- duplicated(TTgeos[,c(lon','lat','acceptedScientificName'))
TTgeos[,c('acceptedScientificName','lon','lat')]
#ignoring subspecies and other naming variation
dups2 <- duplicated(TTgeos[,c('lon','lat')])
sum(dups)
sum(dups2)

# #clean

TTgeo_cl <- subset(TTgeos, TTgeos$lon > -13)
TTgeo_cl

load("buffland_1deg.rda")

TTgeo_cl <- clean_coordinates(TTgeo_cl, lon = "lon", lat = "lat",
                              species = "acceptedScientificName", tests = c("seas", "zeros"), 
                              seas_ref = buffland, value = "clean")
                              
#clean1: duplicated records (keep the records that are not duplicated)

dups2 <- duplicated(TTgeo_cl[,c('lon','lat')])
sum(dups2)
TTgeo_cl <- TTgeo_cl[!dups2,]

TTgeo_cl

data("wrld_simpl")
par(mfrow=c(1,1))
plot(wrld_simpl, axes=TRUE, col="light blue")
box()
points(TTgeo_cl$lon, TTgeo_cl$lat, col='orange', pch=20, cex=0.75)

write.csv(TTgeo_cl, file = "Terminalia_clean.csv")

####
#make a separate map - Terminalia
library("ggplot2")
library("ggthemes")
library("mapdata")
library("maps")

Terminalia_clean <- read.csv("./Terminalia_clean.csv", header = TRUE)
dim(Terminalia_clean)


pdf(file = "Ttriandra_cleanmap2.pdf", width = 11.7, height = 6.27)

par(mar = rep(0,4))

data("wrld_simpl")
par(mfrow=c(1,1))
plot(wrld_simpl, axes=TRUE, col="black")
box()
points(Terminalia_clean$lon, Terminalia_clean$lat, col='yellow', pch=20, cex=0.2)

#Spatial data and maps
#Using R as a GIS software tool and creating informative maps

install.packages("ggsn")
install.packages("rworldmap")
install.packages("tidyverse")
install.packages("rworldxtra")

library(ggplot2)  # ggplot() fortify()
library(dplyr)  # %>% select() filter() bind_rows()
library(rgdal)  # readOGR() spTransform()
library(rgeos)
library(raster)  # intersect()
library(ggsn)  # north2() scalebar()
library(rworldmap)  # getMap()
library(maps)
library(maptools)
library(tidyverse)
library(rworldxtra)

#1. Heteropogon contortus ----

setwd("C:/Users/etelford.IC.002/Dropbox/Phd/R/Terminalia")

Term <- read.csv("Terminalia_clean.csv", header = TRUE)
colnames(Term)

Term$lat <- as.numeric(Term$lat)
Term$lon <- as.numeric(Term$lon)
Term$species <- Term$acceptedScientificName
  
####sub("Heteropogon contortus Beauv. ex Roem. & Schult.", "Heteropogon contortus", Hcontortus$Species)

is.numeric(Term$lon)
is.numeric(Term$lat)

(Term_plot <- ggplot(Term, aes(x = lon, y = lat, colour = species)) + geom_point())
Term_plot + theme(legend.position = "none ")

world <- getMap(resolution = "low") 
#a SpatialPolygonsDataFrame, a complex object type with specific slots for different types of data.

(term_world <- ggplot() +
    geom_polygon(data = world, 
                 aes(x = long, y = lat, group = group),
                 fill = "grey", colour = "black") + 
    geom_point(data = Term,  # Add and plot species data
               aes(x = lon, y = lat, 
                   colour = species), size = 0.3) +
    coord_quickmap() +  # Prevents stretching when resizing
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") + 
    guides(colour=guide_legend(title="species")))

term_world + theme(legend.position = "none ")

#get rid of some continents and countries not relevant to the species

continent <- c("Eurasia", "Africa", "North America", 
               "South America", "Australia") #select continents

#select countries
country <- world@data$ADMIN
country <- as.data.frame(country)
index <- which(world@data$ADMIN != "Greenland")
country <- country[index,]
country <- as.character(as.vector(country))
country

world_saf <- world[world@data$continent %in% continent, ]
world_saf <- world_saf[world_saf@data$ADMIN %in% country, ]



(term_world <- ggplot() +
    geom_polygon(data = world, 
                 aes(x = long, y = lat, group = group),
                 fill = "grey", colour = "black") + 
    geom_point(data = Term,  # Add and plot species data
               aes(x = lon, y = lat, 
                   colour = species), size = 0.3) +
    coord_quickmap() +  # Prevents stretching when resizing
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") + 
    guides(colour=guide_legend(title="species")))

term_world + 
  theme(plot.title = element_text(face = "bold", size = "12"),
        axis.title.x = element_text(size = "10", hjust = 0.5),
        axis.title.y = element_text(size = "10", hjust = 0.5),
        panel.background = element_rect(fill = "aliceblue", colour = "aliceblue"),
        axis.line = element_line(size = 1)) +
  ggtitle(expression(paste("Occurrence map of ", italic("Terminalia sp")))) +
  xlab("Longitude") +
  ylab("Latitude") +
  geom_hline(yintercept = c(-23.43, 0, 23.43), linetype = c("solid", "dotted", "solid"), color = "black", size = 0.1) + theme(legend.position = "none ")


pdf(file = "Terminalia.pdf", width = 11.7, height = 6.27)

par(mar = rep(0,4))

term_world + 
  coord_equal(xlim = c(-180, 180), ylim = c(-90, 90), expand = TRUE) +
  annotate("text", x = -130, y = -70, label = "Equator line") +
  annotate("text", x = -100, y = -80, label = "Tropic of Cancer and Capricorn lines") +
  annotate("text", x = -110, y = -87, label = "(Latitude: -23.43 and 23.43)") +
  annotate("segment", x = -175, xend = -145, y= -70, yend = -70, colour = "black",
           linetype = "dotted") +
  annotate("segment", x = -175, xend = -145, y= -80, yend = -80, colour = "black",
           linetype = "solid") +
  theme(plot.title = element_text(face = "bold", size = "12"),
        axis.title.x = element_text(size = "10", hjust = 0.5),
        axis.title.y = element_text(size = "10", hjust = 0.5),
        panel.background = element_rect(fill = "aliceblue", colour = "aliceblue"),
        axis.line = element_line(size = 1)) +
  ggtitle(expression(paste("Occurrence map of ", italic("Terminalia sp.")))) +
  xlab("Longitude") +
  ylab("Latitude") +
  geom_hline(yintercept = c(-23.43, 0, 23.43), linetype = c("solid", "dotted", "solid"), color = "black", size = 0.1)+ theme(legend.position = "none ")

library("raster")
library("rgdal")
library("dismo")

setwd("C:/Users/etelford.IC.002/Dropbox/Phd/R/niche")

#Example: monthly precipitation, mean, minimum and maximum monthly
#temperature and other 19 variables with WorldClim version 2.0

#resolution: '30 seconds' file - highest resolution

#data type: WGS84

#1.1 Download data from WorldClim and read in ----
Bio1_0.5m <- raster("./wc2.0_bio_30s_01.tif")
Bio2_0.5m <- raster("./wc2.0_bio_30s_02.tif")
Bio3_0.5m <- raster("./wc2.0_bio_30s_03.tif")
Bio4_0.5m <- raster("./wc2.0_bio_30s_04.tif")
Bio5_0.5m <- raster("./wc2.0_bio_30s_05.tif")
Bio6_0.5m <- raster("./wc2.0_bio_30s_06.tif")
Bio7_0.5m <- raster("./wc2.0_bio_30s_07.tif")
Bio8_0.5m <- raster("./wc2.0_bio_30s_08.tif")
Bio9_0.5m <- raster("./wc2.0_bio_30s_09.tif")
Bio10_0.5m <- raster("./wc2.0_bio_30s_10.tif")
Bio11_0.5m <- raster("./wc2.0_bio_30s_11.tif")
Bio12_0.5m <- raster("./wc2.0_bio_30s_12.tif")
Bio13_0.5m <- raster("./wc2.0_bio_30s_13.tif")
Bio14_0.5m <- raster("./wc2.0_bio_30s_14.tif")
Bio15_0.5m <- raster("./wc2.0_bio_30s_15.tif")
Bio16_0.5m <- raster("./wc2.0_bio_30s_16.tif")
Bio17_0.5m <- raster("./wc2.0_bio_30s_17.tif")
Bio18_0.5m <- raster("./wc2.0_bio_30s_18.tif")
Bio19_0.5m <- raster("./wc2.0_bio_30s_19.tif")

par(mar = rep(0,4))
plot(Bio1_0.5m)
box()

RBio_0.5m <- stack(x=c(Bio1_0.5m, Bio2_0.5m, Bio3_0.5m, Bio4_0.5m, Bio5_0.5m, Bio6_0.5m, Bio7_0.5m, Bio8_0.5m, 
               Bio9_0.5m, Bio10_0.5m, Bio11_0.5m, Bio12_0.5m, Bio13_0.5m, Bio14_0.5m, Bio15_0.5m, Bio16_0.5m,
               Bio17_0.5m, Bio18_0.5m, Bio19_0.5m))
RBio_0.5m

#1.2 Download environmental data via 'raster' ----
RBio_30s <- getData("worldclim", var = "bio", res = 0.5, lat = 0, lon=0)
#lon and lat are needed to specify when using res of 0.50
RBio_2.5m <- getData("worldclim", var = "bio", res = 2.5)
RBio_5m <- getData("worldclim", var = "bio", res = 5)
RBio_10m <- getData("worldclim", var = "bio", res = 10)

#2. bounding box of study area using 'drawExtent()' ----

ext1 <- drawExtent()
bio6.ext1 <- crop(RBio2$bio6, ext1)
plot(bio6.ext1)

#or focus on a given region by fixing the coordinates of the bounding box
#for example, around Central America

ext2 <- c(-100, -75, 0, 25)
bio6.ext2 <- crop(RBio2$bio6, ext2)
plot(bio6.ext2)

#3. Extracting macroclimate data for analysing the distribution of 
#individual species ----

#Resolution 10m ----

#3.1 Heteropogon contortus ----
#plot coordinate points onto the environmental map
hcontortus <- read.csv("../../Heteropogon contortus/GBIF and herbarium occurrence data/Hcontortus_combined3.csv")
points(hcontortus$Lon, hcontortus$Lat, col = 'blue', pch = 20, cex = 0.15)

#extract the values of the climatic variables at the locations of the "points" where
#the species is found using 'extract()'
coordinates(hcontortus) <- c("Lon", "Lat")
RBio.hcontortus <- raster::extract(RBio2, hcontortus, df = TRUE)
RBio.hcontortus

write.csv(RBio.hcontortus, file = "./RBio_Hcontortus.csv")


