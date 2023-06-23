


library(ecmwfr)
library(raster)
library(rgdal)
library(tis)
library(dplyr)
library(sf)

dir<-"C:/Users/david.munoz/OneDrive - ctfc.cat/predictors_ebalife/"
proj_final<-CRS("EPSG:3035")



### Errors to warnings
# If we see that some warnings are turned to errors, run the following:
options(warn=1)

### Install and Load libraries
list.of.packages <- c("raster", "car", "shapefiles", "rgdal", "dplyr", "rworldmap", "stringr", "graphics", "plyr", "beepr",
                      "xlsx", "earth", "mda", "caret", "tidyverse", "mgcv", "maptools", "pbapply", "zonator", "parallel",
                      "sf", "rgeos", "spdep", "paletteer", "ncdf4", "ecmwfr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
install.packages(new.packages)
invisible(lapply(list.of.packages, library, character.only = T, logical.return=T))
rm(list.of.packages, new.packages)

### Number of cores of the computer
numCores <- detectCores()
numCores # 16 for serverbio and 4 for CTFC local
numCores <- numCores - 1 # recommended to leave one free core for other tasks

### Directories
dir <- "C:/Users/david.munoz/Documents/EBBA" # G: local directories

dir_raw <- file.path(dir, 'raw')  # G: new directories
ref_r <- raster(paste0(dir_raw, "/grids/reference_predictors_raster/reference_predictors_raster.tif"))
dir_static   <- file.path(dir, '2_Model_predictors', 'new_predictors', 'static')  # G: directory for static environmental variables
dir_EBBA2   <- file.path(dir, '2_Model_predictors', 'new_predictors', 'EBBA2')  # G: directory for dynamic environmental variables in period EBBA2
dir_EBBA3   <- file.path(dir, '2_Model_predictors', 'new_predictors', 'EBBA3')  # G: directory for dynamic environmental variables in period EBBA3

dir_envi <- file.path('C:/Users/david.munoz/Documents/R/ENVIRONMENTS/EBBA') # G: where environments are stored


dir.create(file.path(dir_EBBA2, 'raw'), showWarnings = F)
dir.create(file.path(dir_EBBA2, 'clean'), showWarnings = F)

dir.create(file.path(dir_EBBA3, 'raw'), showWarnings = F)
dir.create(file.path(dir_EBBA3, 'clean'), showWarnings = F)


for (i in c(dir_EBBA2, dir_EBBA3)) {
  
  dir.create(file.path(i, 'raw', 'climate'), showWarnings = F)
  dir.create(file.path(i, 'raw', 'landcover'), showWarnings = F)
  dir.create(file.path(i, 'raw', 'climate'), showWarnings = F)
  dir.create(file.path(i, 'raw', 'anthropogenic'), showWarnings = F)
  dir.create(file.path(i, 'raw', 'dhi'), showWarnings = F)
  dir.create(file.path(i, 'raw', 'habitatestruc'), showWarnings = F)
  
}

# Climate ----
## Download data. It comes from https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview. DM: #It is necessary to have an account in ECMWF. ----
  
#DM: Then, download the package ecmwfr, and run the following function:
  # wf_set_key(user = "david.munoz@ctfc.cat", #put the user
  #            key = ,
  #            service = "cds")
  
  #DM: Then browser will be opened, put the UID and the API key. To download, copy the code in python in the web
  #DM: and in RSTUDIO "addins"-> python to list. Maybe you have to remove some characters from dataset_short_name
  #DM: finally, we obtain for each variable, the values for EBBA2 and EBBA3. Variable name will be the original name+_EBBA2/3

variable_list<-c("2m_temperature", "total_evaporation", "total_precipitation", "surface_net_solar_radiation", "snow_cover", "potential_evaporation") #evaporation_from_vegetation_transpiration  #List of variables to download

for (i in variable_list) {
  
  request <- list(
    product_type = "monthly_averaged_reanalysis",
    variable = i,
    year = c("2013", "2014", "2015", "2016", "2017"), #Years of interest (in this case, EBBA2)
    month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
    time = "00:00",
    area = c(88, -40, 20, 80),
    format = "grib",
    dataset_short_name = "reanalysis-era5-land-monthly-means",
    target = paste0(i, "_EBBA2.grib")
  )
  
  wf_request(request, user = "201975", transfer = TRUE, path = file.path(dir_EBBA2, "raw", "climate"),
             time_out = 300, verbose = TRUE) #Download the request using your user and a path. 
  
  grib_file <- file.path(dir_EBBA2, "raw", "climate",paste0(i,"_EBBA2.grib")) #Open the grib and read as rasterbrick
  grib_file <- brick(grib_file)
  
  assign(paste0(i,"_EBBA2"), grib_file)
  
  
  request <- list(
    product_type = "monthly_averaged_reanalysis",
    variable = i,
    year = c("2018", "2019", "2020", "2021", "2022"), #Years of interest (in this case, EBBA3)
    month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), #This includes the months to download
    time = "00:00",
    area = c(88, -40, 20, 80),
    format = "grib",
    dataset_short_name = "reanalysis-era5-land-monthly-means",
    target = paste0(i, "_EBBA3.grib") #We give the variable a name and the EBBA related. 
  )
  
  wf_request(request, user = "201975", transfer = TRUE, path = file.path(dir_EBBA3, "raw", "climate"), #Change user
             time_out = 300, verbose = TRUE) #Download the request using your user and a path. 
  
  grib_file <- file.path(dir_EBBA3, "raw", "climate",paste0(i,"_EBBA3.grib")) #Open the grib and read as rasterbrick
  grib_file <- brick(grib_file)
  
  assign(paste0(i,"_EBBA3"), grib_file)
  
}


### Modifications of each variable for EBBA 2 and EBBA 3----
#1: Evapotranspiration
#2: Annual temperature (K)
#3: Breeding temperature (K)
#4: Mean temperature of the warmest month (K)
#5: Mean temperature of the coldest month (K)
#6: Annual precipitation (mm)
#7: Breeding precipitation (mm)
#8: Surface net solar radiation (J*m-2)

#DM: If you see the following error: Warning: Inside GRIB2Inventory, Message # 61; ERROR: Ran out of file reading SECT0, do not worry. It works properly.

##Evapotranspiration in breeding period (APR-JUL)
ETR<-raster::subset(total_evaporation_EBBA2, c(4:7, 16:19, 28:31, 40:43, 52:55)) #select apr-jul from year 1 to 5
ETR<-calc(ETR, fun = function (x) sum(x)/5*1000) #UNITS ARE NOT CORRECT

ETP<-raster::subset(potential_evaporation_EBBA2, c(4:7, 16:19, 28:31, 40:43, 52:55)) #select apr-jul from year 1 to 5
ETP<-calc(ETP, fun = function (x) sum(x)/5*1000) #UNITS ARE NOT CORRECT


kk<-total_evaporation_EBBA2[[1]]/potential_evaporation_EBBA2[[1]]


EVAPObree_EBBA2<-ETR/ETP

EVAPObree_EBBA3<-raster::subset(total_evaporation_EBBA3, c(4:7, 16:19, 28:31, 40:43, 52:55)) #select apr-jul from year 1 to 5
EVAPObree_EBBA3<-calc(EVAPObree_EBBA3, fun = function(x) sum(x)/5) 

writeRaster(ETP, "C:/Users/david.munoz/Downloads/EVAPOPOTENCIAL.tif")

##Mean annual temperature
Tannual_EBBA2<-calc(`2m_temperature_EBBA2`, fun=mean)

Tannual_EBBA3<-calc(`2m_temperature_EBBA3`, fun=mean)



##Mean temperature in breeding period (APR-JUL)
TBreed_EBBA2<-raster::subset(`2m_temperature_EBBA2`, c(4:7, 16:19, 28:31, 40:43, 52:55))  #DM: select apr-jul from year 1 to 5
TBreed_EBBA2<-calc(TBreed_EBBA2, fun=mean) #DM: mean of the selected months

TBreed_EBBA3<-raster::subset(`2m_temperature_EBBA3`, c(4:7, 16:19, 28:31, 40:43, 52:55)) #DM: select apr-jul from year 1 to 5
TBreed_EBBA3<-calc(TBreed_EBBA3, fun=mean)



##Mean temperature in the warmest month
TMAX_EBBA2<-raster::subset(`2m_temperature_EBBA2`, c(7, 19, 31, 43, 55)) #DM: we select July as we found that it is the warmest month
TMAX_EBBA2<-calc(TMAX_EBBA2, fun=mean) #DM: mean of the 5 July's of the EBBA 2 period

TMAX_EBBA3<-raster::subset(`2m_temperature_EBBA3`, c(7, 19, 31, 43, 55))
TMAX_EBBA3<-calc(TMAX_EBBA3, fun=mean)



##Mean temperature in the coldest month
TMIN_EBBA2<-raster::subset(`2m_temperature_EBBA2`, c(1, 13, 25, 37, 49)) #DM: we select January as we found that it is the warmest month
TMIN_EBBA2<-calc(TMIN_EBBA2, fun=mean) #DM: mean of the 5 January's of the EBBA 2 period

TMIN_EBBA3<-raster::subset(`2m_temperature_EBBA3`, c(1, 13, 25, 37, 49))
TMIN_EBBA3<-calc(TMIN_EBBA3, fun=mean)

##Total annual precipitation. DM: unit is m/day, so we have to *1000*Ndaysmonth 
#https://confluence.ecmwf.int/pages/viewpage.action?pageId=197702790

# EBBA2
#First, calculate the number of days per month between 2013 and 2017
start_year <- 2013
end_year <- 2017

# Function to check if a year is a leap year
isLeapYear <- function(year) {
  year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0)
}

days_per_month <- c()
for (year in start_year:end_year) {
  for (month in 1:12) {
    if (month == 2 && isLeapYear(year)) {
      days_in_month <- 29
    } else {
      days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[month]
    }
    days_per_month <- c(days_per_month, days_in_month)
  }
}

for (i in seq_along(days_per_month)) {
  layer <- total_precipitation_EBBA2[[i]]
  days <- days_per_month[i]
  
  # Multiply each pixel value by the number of days to obtain m/month
  total_precipitation_EBBA2[[i]] <- layer * days
}

#DM: summarise, sum to obtain total accumulated precipitation in the period, if we divide the number of years, we obtain the precipitation/year. To have mm, we must *1000
PAnnual_EBBA2<-calc(total_precipitation_EBBA2, fun=function(x) sum(x)/5*1000)


# EBBA3
#First, calculate the number of days per month between 2018 and 2022
start_year <- 2018
end_year <- 2022

# Function to check if a year is a leap year
isLeapYear <- function(year) {
  year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0)
}
days_per_month <- c()
for (year in start_year:end_year) {
  for (month in 1:12) {
    if (month == 2 && isLeapYear(year)) {
      days_in_month <- 29
    } else {
      days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[month]
    }
    days_per_month <- c(days_per_month, days_in_month)
  }
}

for (i in seq_along(days_per_month)) {
  layer <- total_precipitation_EBBA3[[i]]
  days <- days_per_month[i]
  
  # Multiply each pixel value by the number of days. Now it is m for month
  total_precipitation_EBBA3[[i]] <- layer * days
}
#DM: summarise, sum to obtain total accumulated precipitation in the period, if we divide the number of years, we obtain the precipitation/year. To have mm, we must *1000
PAnnual_EBBA3<-calc(total_precipitation_EBBA3, fun=function(x) sum(x)/5*1000)



##Total precipitation in the breeding period
PBreed_EBBA2<-raster::subset(total_precipitation_EBBA2, c(4:7, 16:19, 28:31, 40:43, 52:55)) #select apr-jul from year 1 to 5
PBreed_EBBA2<-calc(PBreed_EBBA2, fun = function(x) sum(x)/5*1000)

PBreed_EBBA3<-raster::subset(total_precipitation_EBBA3, c(4:7, 16:19, 28:31, 40:43, 52:55)) #select apr-jul from year 1 to 5
PBreed_EBBA3<-calc(PBreed_EBBA3, fun = function(x) sum(x)/5*1000)



##DM: Days of snow cover. We do not use it at this moment. The NA value is 9999. We have to reclassify it.
#rcl<-matrix(c(102, 999999, NA, -999999, -0.001, NA), ncol = 3, byrow=T ) #Maybe if we mask the layer we will not have to do this. 

# #Snow_days_EBBA2<-raster::reclassify(snow_cover_EBBA2, rcl)
# Snow_days_EBBA2<-calc(snow_cover_EBBA2, fun=mean)
# 
# #Snow_days_EBBA3<-raster::reclassify(snow_cover_EBBA3, rcl)
# Snow_days_EBBA3<-calc(snow_cover_EBBA3, fun=mean)



##Surface net solar radiation
Rad_surf_EBBA2<-calc(surface_net_solar_radiation_EBBA2, fun=mean)

Rad_surf_EBBA3<-calc(surface_net_solar_radiation_EBBA3, fun=mean)

## Creation of lists

clim_EBBA2_r<-list(Tannual_EBBA2, TMAX_EBBA2, TMIN_EBBA2, TBreed_EBBA2, PAnnual_EBBA2, PBreed_EBBA2, Rad_surf_EBBA2, EVAPObree_EBBA2)
clim_EBBA3_r<-list(Tannual_EBBA3, TMAX_EBBA3, TMIN_EBBA3, TBreed_EBBA3, PAnnual_EBBA3, PBreed_EBBA3, Rad_surf_EBBA3, EVAPObree_EBBA3)

names(clim_EBBA2_r)<-c("Tannual", "TMAX", "TMIN", "TBreed", "PAnnual", "PBreed", "Rad_surf", "EVAPObree") 
names(clim_EBBA3_r)<-c("Tannual", "TMAX", "TMIN", "TBreed", "PAnnual", "PBreed", "Rad_surf", "EVAPObree") 

## Crop by rough extent
e <- extent(c(-36, 71, 25, 84))

clim_EBBA2_r <- pblapply(clim_EBBA2_r, raster::crop, e ) # G: takes two minutes
clim_EBBA3_r <- pblapply(clim_EBBA3_r, raster::crop, e ) # G: takes one minute

## Find the centroid and rasterize to good projection

### Function to find centroid and rasterize
fun_centroid_rasterize_clim<- function(x) {
  x <- raster::disaggregate(x, fact=2)
  ## Find value on raster centroids
  centroids <- xyFromCell(x, cell=1:ncell(x)) %>% as.data.frame # find centroides
  centroids$values <- values(x) # G: this are the values of the raster
  centroids_sf <- st_as_sf(centroids, coords=c('x', 'y'), crs= st_crs(x)) # G: centroids to sf object
  
  ## Transform projection of centroids
  centroids_sf <- st_transform(centroids_sf, crs=st_crs(ref_r))
  
  ## Rasterize points to reference raster
  y <- rasterize(st_coordinates(centroids_sf), ref_r, field=centroids_sf$values, fun=mean, na.rm=T) # G: Introduce MEAN values to reference raster
  
  ## Mask layer by reference raster
  y <- raster::mask(y, ref_r)
  
  return(y)
  
  
}

### Apply function to nested EBBA2 and EBBA3 pred lists
clim_EBBA2_r <- pblapply(clim_EBBA2_r, fun_centroid_rasterize_clim) 
clim_EBBA3_r <- pblapply(clim_EBBA3_r, fun_centroid_rasterize_clim) 


## Export raster
for (i in 1:length(clim_EBBA2_r)) {

  writeRaster(clim_EBBA2_r[[i]], file.path(dir_EBBA2, 'clean', names(clim_EBBA2_r)[i]), "GTiff", overwrite=T)
  writeRaster(clim_EBBA3_r[[i]], file.path(dir_EBBA3, 'clean', names(clim_EBBA3_r)[i]), "GTiff", overwrite=T)
  
}


### Find the warmest and the coldest month: ----
#EBBA2
june1<-raster::subset(`2m_temperature_EBBA2`, c(6, 18, 30, 42, 54))
june1<-calc(june1, fun=mean)
june1<-cellStats(june1, stat=mean)

july1<-raster::subset(`2m_temperature_EBBA2`, c(7, 19, 31, 43, 55))
july1<-calc(july1, fun=mean)
july1<-cellStats(july1, stat=mean)

august1<-raster::subset(`2m_temperature_EBBA2`, c(8, 20, 32, 44, 56))
august1<-calc(august1, fun=mean)
august1<-cellStats(august1, stat=mean)


#EBBA3
june2<-raster::subset(`2m_temperature_EBBA3`, c(6, 18, 30, 42, 54))
june2<-calc(june2, fun=mean)
june2<-cellStats(june2, stat=mean)

july2<-raster::subset(`2m_temperature_EBBA3`, c(7, 19, 31, 43, 55))
july2<-calc(july2, fun=mean)
july2<-cellStats(july2, stat=mean)

august2<-raster::subset(`2m_temperature_EBBA3`, c(8, 20, 32, 44, 56))
august2<-calc(august2, fun=mean)
august2<-cellStats(august2, stat=mean)

#Mean
august<-mean(august1, august2)
july<-mean(july1, july2)
june<-mean(june1, june2)



#Find the coldest month: 
#EBBA2
january1<-raster::subset(`2m_temperature_EBBA2`, c(1, 13, 25, 37, 49))
january1<-calc(january1, fun=mean)
january1<-cellStats(january1, stat=mean)

february1<-raster::subset(`2m_temperature_EBBA2`, c(2, 14, 26, 38, 50))
february1<-calc(february1, fun=mean)
february1<-cellStats(february1, stat=mean)

december1<-raster::subset(`2m_temperature_EBBA2`, c(12, 24, 36, 48, 60))
december1<-calc(december1, fun=mean)
december1<-cellStats(december1, stat=mean)

#EBBA3
january2<-raster::subset(`2m_temperature_EBBA3`, c(1, 13, 25, 37, 49))
january2<-calc(january2, fun=mean)
january2<-cellStats(january2, stat=mean)

february2<-raster::subset(`2m_temperature_EBBA3`, c(2, 14, 26, 38, 50))
february2<-calc(february2, fun=mean)
february2<-cellStats(february2, stat=mean)

december2<-raster::subset(`2m_temperature_EBBA3`, c(12, 24, 36, 48, 60))
december2<-calc(december2, fun=mean)
december2<-cellStats(december2, stat=mean)

january<-mean(january1, january2)
february<-mean(february1, february2)
december<-mean(december1, december2)


## Remove objects
rm(clim_EBBA2_r, clim_EBBA3_r, Tannual_EBBA2, TMAX_EBBA2, TMIN_EBBA2, TBreed_EBBA2, PAnnual_EBBA2, PBreed_EBBA2, Rad_surf_EBBA2, EVAPObree_EBBA2,
   Tannual_EBBA3, TMAX_EBBA3, TMIN_EBBA3, TBreed_EBBA3, PAnnual_EBBA3, PBreed_EBBA3, Rad_surf_EBBA3, EVAPObree_EBBA3)



#COMPARACIONS VS EBBA2 ----
library(mapview)
library(RColorBrewer)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)

#Fer model
valuesx<-values(x)
valuesy<-values(y)
model<-lm(valuesx ~ valuesy)
summary(model)



leaflet() |>
  addProviderTiles(providers$Esri.WorldImagery) |>
  addRasterImage(resta, opacity = 1, colors = hcl.colors(100, "Blue-Red")) |>
  addLegend( "bottomright",values = ~resta,title = "Temperatura",colors = hcl.colors(10, "Blue-Red"),labels = round(seq(min(values(resta), na.rm = TRUE), max(values(resta), na.rm = TRUE), length.out = 10))
  )


#tannual
abans<-raster("C:/Users/david.munoz/Downloads/wetransfer_predictors_corrected_2023-06-22_0817/predictors_corrected/Tannual.tif")
despres<-raster("C:/Users/david.munoz/Documents/EBBA/2_Model_predictors/new_predictors/EBBA2/clean/Tannual.tif")
resta<-despres-273.15-abans/10
mapview(resta, col.regions = hcl.colors(10, "Blue-Red"), legend = mapviewGetOption("legend"))

#tbreeding
TbreedAB<-rast("C:/Users/david.munoz/Downloads/wetransfer_predictors_corrected_2023-06-22_0817/predictors_corrected/Tbreed.tif")
TbreedDP<-rast("C:/Users/david.munoz/Documents/EBBA/2_Model_predictors/new_predictors/EBBA2/clean/Tbreed.tif")
restaTbreed<-TbreedDP-273.15-TbreedAB/10
mapview(restaTbreed, col.regions = hcl.colors(10, "Blue-Red")) #TÉ SENTIT QUE SIGUIN VALORS NEGATIUS. TMITJ=25(despres), tmax=30 (abans)


#precbreedingperiod
precab<-rast("C:/Users/david.munoz/Downloads/wetransfer_predictors_corrected_2023-06-22_0817/predictors_corrected/Pbreed.tif")
precdp<-rast("C:/Users/david.munoz/Documents/EBBA/2_Model_predictors/new_predictors/EBBA2/clean/Pbreed.tif")
restaprec<-precdp-precab
mapview(restaprec, col.regions = hcl.colors(100, "Blue-Red"))

#precannual
precab<-rast("C:/Users/david.munoz/Downloads/wetransfer_predictors_corrected_2023-06-22_0817/predictors_corrected/PAnnual.tif")
precdp<-rast("C:/Users/david.munoz/Documents/EBBA/2_Model_predictors/new_predictors/EBBA2/clean/PAnnual.tif")
restaprec<-precdp-precab
mapview(restaprec, col.regions = hcl.colors(100, "Blue-Red"))

#tmax
TMAXAB<-rast("C:/Users/david.munoz/Downloads/wetransfer_predictors_corrected_2023-06-22_0817/predictors_corrected/Tmaxjul.tif")
TMAXDP<-rast("C:/Users/david.munoz/Documents/EBBA/2_Model_predictors/new_predictors/EBBA2/clean/TMAX.tif")
restaTMAX<-TMAXDP-273.15-TMAXAB/10
mapview(restaTMAX, col.regions = hcl.colors(10, "Blue-Red")) #TÉ SENTIT QUE SIGUIN VALORS NEGATIUS. TMITJ=25(despres), tmax=30 (abans)

#tmin
TMINAB<-rast("C:/Users/david.munoz/Downloads/wetransfer_predictors_corrected_2023-06-22_0817/predictors_corrected/Tmingen.tif")
TMINDP<-rast("C:/Users/david.munoz/Documents/EBBA/2_Model_predictors/new_predictors/EBBA2/clean/TMIN.tif")
restaTMIN<-TMINDP-273.15-TMINAB/10
mapview(restaTMIN, col.regions = hcl.colors(10, "Blue-Red")) #TÉ SENTIT QUE SIGUIN VALORS POSITIUS. Si tmitj 7 (despres) i tmin 3 (abans)

rad<-rast("C:/Users/david.munoz/Documents/EBBA/2_Model_predictors/new_predictors/EBBA2/clean/Rad_surf.tif")
mapview(rad, col.regions = hcl.colors(10, "viridis"))
