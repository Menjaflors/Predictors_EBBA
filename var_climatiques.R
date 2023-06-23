
#ERA5 monthly averaged reanalysis
#https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview

library(ecmwfr)
library(raster)
library(rgdal)
library(tis)
library(dplyr)
library(sf)

dir<-"C:/Users/david.munoz/OneDrive - ctfc.cat/predictors_ebalife/"
proj_final<-CRS("EPSG:3035")

### Download and import variables. It is necessary to have an account in ECMWF. ----
#Then, download the package ecmwfr, and run the following function:

# wf_set_key(user = "david.munoz@ctfc.cat", #put the user
#            key = ,
#            service = "cds")

#Then browser will be opened, put the UID and the API key. To download, copy the code in python in the web
#and in RSTUDIO "addins"-> python to list. Maybe you have to remove some characters from sataset_short_name

variable_list<-c("2m_temperature", "total_evaporation", "total_precipitation", "surface_net_solar_radiation", "snow_cover") #evaporation_from_vegetation_transpiration  #List of variables to download

for (i in variable_list) {
  
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
  
  wf_request(request, user = "201975", transfer = TRUE, path = paste0(dir, "descarregues"), #Change user
             time_out = 300, verbose = TRUE) #Download the request using your user and a path. 
  
  grib_file <- paste0(dir, "descarregues/", i,"_EBBA3.grib") #Open the grib and read as rasterbrick
  grib_file <- brick(grib_file)
  
  assign(paste0(i,"_EBBA3"), grib_file)
  
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
  
  wf_request(request, user = "201975", transfer = TRUE, path = paste0(dir, "descarregues"),
             time_out = 300, verbose = TRUE) #Download the request using your user and a path. 
  
  grib_file <- paste0(dir, "descarregues/", i,"_EBBA2.grib") #Open the grib and read as rasterbrick
  grib_file <- brick(grib_file)

  assign(paste0(i,"_EBBA2"), grib_file)
  }
  

### Modifications of each variable----

##Evapotranspiration in breeding period (APR-JUL)
EVAPObree_EBBA2<-raster::subset(total_evaporation_EBBA2, c(4:7, 16:19, 28:31, 40:43, 52:55)) #select apr-jul from year 1 to 5
EVAPObree_EBBA2<-calc(EVAPObree_EBBA2, fun = function (x) sum(x)/5) #UNITS ARE NOT CORRECT

EVAPObree_EBBA3<-raster::subset(total_evaporation_EBBA3, c(4:7, 16:19, 28:31, 40:43, 52:55)) #select apr-jul from year 1 to 5
EVAPObree_EBBA3<-calc(EVAPObree_EBBA3, fun = function(x) sum(x)/5) 



##Mean annual temperature
Tannual_EBBA2<-calc(`2m_temperature_EBBA2`, fun=mean)

Tannual_EBBA3<-calc(`2m_temperature_EBBA3`, fun=mean)



##Mean temperature in breeding period (APR-JUL)
TBreed_EBBA2<-raster::subset(`2m_temperature_EBBA2`, c(4:7, 16:19, 28:31, 40:43, 52:55))
TBreed_EBBA2<-calc(TBreed_EBBA2, fun=mean)

TBreed_EBBA3<-raster::subset(`2m_temperature_EBBA3`, c(4:7, 16:19, 28:31, 40:43, 52:55))
TBreed_EBBA3<-calc(TBreed_EBBA3, fun=mean)



##Mean temperature in the warmest month (we found the warmest month at the bottom of the script)
TMAX_EBBA2<-raster::subset(`2m_temperature_EBBA2`, c(7, 19, 31, 43, 55))
TMAX_EBBA2<-calc(TMAX_EBBA2, fun=mean)

TMAX_EBBA3<-raster::subset(`2m_temperature_EBBA3`, c(7, 19, 31, 43, 55))
TMAX_EBBA3<-calc(TMAX_EBBA3, fun=mean)



##Mean temperature in the coldest month (we found the coldest month at the bottom of the script)
TMIN_EBBA2<-raster::subset(`2m_temperature_EBBA2`, c(1, 13, 25, 37, 49))
TMIN_EBBA2<-calc(TMIN_EBBA2, fun=mean)

TMIN_EBBA3<-raster::subset(`2m_temperature_EBBA3`, c(1, 13, 25, 37, 49))
TMIN_EBBA3<-calc(TMIN_EBBA3, fun=mean)

##Total annual precipitation: unit is m/day, so we have to *1000*Ndaysmonth  https://confluence.ecmwf.int/pages/viewpage.action?pageId=197702790 
#https://confluence.ecmwf.int/pages/viewpage.action?pageId=197702790

# EBBA2
#First, calculate the number of days per month between 2018 and 2022
start_year <- 2013
end_year <- 2017

# Function to check if a year is a leap year
isLeapYear <- function(year) {
  year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0)
}

# Initialize an empty vector to store the number of days per month
days_per_month <- c()

# Loop through each year and month to calculate the number of days
for (year in start_year:end_year) {
  for (month in 1:12) {
    # Check if it is a leap year
    if (month == 2 && isLeapYear(year)) {
      days_in_month <- 29
    } else {
      days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[month]
    }
    # Append the number of days to the vector
    days_per_month <- c(days_per_month, days_in_month)
  }
}


#Then, change units in the map
for (i in seq_along(days_per_month)) {
  layer <- total_precipitation_EBBA2[[i]]
  days <- days_per_month[i]
  
  # Multiply each pixel value by the number of days to obtain m/month
  total_precipitation_EBBA2[[i]] <- layer * days
}

#And summarise, firts we will obtain the total accumulated precipitation in the period, if we divide, we obtain the precipitation/year
PAnnual_EBBA2<-calc(total_precipitation_EBBA2, fun=function(x) sum(x)/5)



# EBBA3

#First, calculate the number of days per month between 2018 and 2022
start_year <- 2018
end_year <- 2022

# Function to check if a year is a leap year
isLeapYear <- function(year) {
  year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0)
}

# Initialize an empty vector to store the number of days per month
days_per_month <- c()

# Loop through each year and month to calculate the number of days
for (year in start_year:end_year) {
  for (month in 1:12) {
    # Check if it is a leap year
    if (month == 2 && isLeapYear(year)) {
      days_in_month <- 29
    } else {
      days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[month]
    }
    # Append the number of days to the vector
    days_per_month <- c(days_per_month, days_in_month)
  }
}


#Then, change units in the map
for (i in seq_along(days_per_month)) {
  layer <- total_precipitation_EBBA3[[i]]
  days <- days_per_month[i]
  
  # Multiply each pixel value by the number of days. Now it is m for month
  total_precipitation_EBBA3[[i]] <- layer * days
}

#And summarise, firts we will obtain the total accumulated precipitation in the period, if we divide, we obtain the precipitation/year
PAnnual_EBBA3<-calc(total_precipitation_EBBA3, fun=function(x) sum(x)/5)



##Total precipitation in the breeding period
PBreed_EBBA2<-raster::subset(total_precipitation_EBBA2, c(4:7, 16:19, 28:31, 40:43, 52:55)) #select apr-jul from year 1 to 5
PBreed_EBBA2<-calc(PBreed_EBBA2, fun = function(x) sum(x)/5)

PBreed_EBBA3<-raster::subset(total_precipitation_EBBA3, c(4:7, 16:19, 28:31, 40:43, 52:55)) #select apr-jul from year 1 to 5
PBreed_EBBA3<-calc(PBreed_EBBA3, fun = function(x) sum(x)/5)



##Days of snow cover. The first layer is good, but not the rest, as the NA values is 9999. We have to reclassify it
rcl<-matrix(c(102, 999999, NA, -999999, -0.001, NA), ncol = 3, byrow=T ) #Maybe if we mask the layer we will not have to do this. 

Snow_days_EBBA2<-raster::reclassify(snow_cover_EBBA2, rcl)
Snow_days_EBBA2<-calc(Snow_days_EBBA2, fun=mean)

Snow_days_EBBA3<-raster::reclassify(snow_cover_EBBA3, rcl)
Snow_days_EBBA3<-calc(Snow_days_EBBA3, fun=mean)



##Surface net solar radiation
Rad_surf_EBBA2<-calc(surface_net_solar_radiation_EBBA2, fun=mean)

Rad_surf_EBBA3<-calc(surface_net_solar_radiation_EBBA3, fun=mean)


##MASK, CROP, ETC. 
variable_list<-c("EVAPObree", "Tannual", "TBreed", "TMAX", "TMIN", "PAnnual", "PBreed","Snow_days","Rad_surf")
variable_list<-c("EVAPObree_EBBA2", "EVAPObree_EBBA3", "Tannual_EBBA2","Tannual_EBBA3", "TBreed_EBBA2", "TBreed_EBBA3",
                 "TMAX_EBBA2","TMAX_EBBA3", "TMIN_EBBA2", "TMIN_EBBA3", "PAnnual_EBBA2", "PAnnual_EBBA3", 
                 "PBreed_EBBA2", "PBreed_EBBA3", "Snow_days_EBBA2", "Snow_days_EBBA3", "Rad_surf_EBBA2", "Rad_surf_EBBA3")

## Crop the raster by extent
e <- extent(c(-36, 71, 25, 84)) # G: rought extent of Europe

ref_r<-raster("C:/Users/david.munoz/Downloads/wetransfer_ebba_2023-06-12_0855/EBBA/raw/grids/reference_predictors_raster/reference_predictors_raster.tif")

for (i in seq_along(variable_list)) {
  assign(variable_list[i], raster::crop(get(variable_list[i]), e))
  centroids <- as.data.frame(xyFromCell(get(variable_list[i]), cell = 1:ncell(get(variable_list[i]))))
  centroids$values <- values(get(variable_list[i]))
  assign(paste0("centroids_", variable_list[i]), centroids)
  assign(paste0("centroids_", variable_list[i]), st_as_sf(get(paste0("centroids_", variable_list[i])), coords=c('x', 'y'), crs= st_crs(TMIN_EBBA2))) # G: centroids to sf object
  assign(paste0("centroids_", variable_list[i]), st_transform(get(paste0("centroids_", variable_list[i])), crs=st_crs(ref_r)))
  assign(variable_list[i], raster::rasterize(st_coordinates(get(paste0("centroids_", variable_list[i]))), ref_r, field=get(paste0("centroids_", variable_list[i]))$values, fun=mean, na.rm=T))
  # writeRaster(get(paste0(variable_list[i], "_EBBA2")), file.path(dir, 'EBBA2', variable_list[i]), "GTiff", overwrite=TRUE)
  # writeRaster(get(paste0(variable_list[i], "_EBBA3")), file.path(dir, 'EBBA3', variable_list[i]), "GTiff", overwrite=TRUE)
  print(i)
}



#CODI GUILLEM:
centroids_EBBA2 <- xyFromCell(TMIN_EBBA2, cell=1:ncell(TMIN_EBBA2)) %>% as.data.frame # find centroides
centroids_EBBA2$values <- values(TMIN_EBBA2) # G: this are the values of the raster
centroids_EBBA2_sf <- st_as_sf(centroids_EBBA2, coords=c('x', 'y'), crs= st_crs(TMIN_EBBA2)) # G: centroids to sf object

## Find value on raster centroids
centroids_EBBA2 <- xyFromCell(popden_EBBA2, cell=1:ncell(popden_EBBA2)) %>% as.data.frame # find centroides
centroids_EBBA2$values <- values(popden_EBBA2) # G: this are the values of the raster
centroids_EBBA2_sf <- st_as_sf(centroids_EBBA2, coords=c('x', 'y'), crs= st_crs(popden_EBBA2)) # G: centroids to sf object

centroids_EBBA3 <- xyFromCell(popden_EBBA3, cell=1:ncell(popden_EBBA3)) %>% as.data.frame # find centroides
centroids_EBBA3$values <- values(popden_EBBA3) # G: this are the values of the raster
centroids_EBBA3_sf <- st_as_sf(centroids_EBBA3, coords=c('x', 'y'), crs= st_crs(popden_EBBA3)) # G: centroids to sf object

## Transform projection of centroids
centroids_EBBA2_sf <- st_transform(centroids_EBBA2_sf, crs=st_crs(ref_r))
centroids_EBBA3_sf <- st_transform(centroids_EBBA3_sf, crs=st_crs(ref_r))

## Rasterize points to reference raster
popden_EBBA2 <- rasterize(st_coordinates(centroids_EBBA2_sf), ref_r, field=centroids_EBBA2_sf$values, fun=mean, na.rm=T) # G: Introduce MEAN values to reference raster
popden_EBBA3 <- rasterize(st_coordinates(centroids_EBBA3_sf), ref_r, field=centroids_EBBA3_sf$values, fun=mean, na.rm=T) # G: Introduce MEAN values to reference raster

## Mask layer by reference raster
popden_EBBA2 <- raster::mask(popden_EBBA2, ref_r)
popden_EBBA3 <- raster::mask(popden_EBBA3, ref_r)


##Export


#EXPORT USING GUILLEM FORMULA:
for (i in seq_along(variable_list)) {
  writeRaster(get(paste0(variable_list[i], "_EBBA2")), file.path(dir, 'EBBA2', variable_list[i]), "GTiff", overwrite=TRUE)
  writeRaster(get(paste0(variable_list[i], "_EBBA3")), file.path(dir, 'EBBA3', variable_list[i]), "GTiff", overwrite=TRUE)
}


### Resample, mask in Europe----
# 
# grib_file<-projectRaster(grib_file, crs=proj_final) #Project the raster and change the name
# grid10<-readOGR(paste0(dir, "grid_10x10/grid_10x10_ebba2.shp"))
# rasgrid10<-raster(extent(grid10), resolution=c(10000, 10000), crs(grid10))
# rasgrid10<-rasterize(grid10, rasgrid10)
# rasgrid10<-mask(rasgrid10, grid10)
# 
# kk_resample <- resample(`2m_temperature`[[1]], rasgrid10, method = "bilinear")
# kk_resample <- raster::mask(kk_resample, rasgrid10)
kk<-raster::disaggregate(snow_cover_EBBA2[[1]], fact=2)



centroids_EBBA2 <- xyFromCell(kk, cell=1:ncell(kk)) %>% as.data.frame # find centroides
centroids_EBBA2$values <- values(kk) # G: this are the values of the raster
centroids_EBBA2_sf <- st_as_sf(centroids_EBBA2, coords=c('x', 'y'), crs= st_crs(kk)) # G: centroids to sf object

centroids_EBBA2_sf <- st_transform(centroids_EBBA2_sf, crs=st_crs(ref_r))


## Rasterize points to reference raster
popden_EBBA2 <- rasterize(st_coordinates(centroids_EBBA2_sf), ref_r, field=centroids_EBBA2_sf$values, fun=mean, na.rm=T) # G: Introduce MEAN values to reference raster

## Mask layer by reference raster
popden_EBBA2 <- raster::mask(popden_EBBA2, ref_r)


kk<-resample(TMIN_EBBA2, ref_r, method="bilinear")
writeRaster(popden_EBBA2, "C:/Users/david.munoz/Downloads/kk_resample1.tif")
writeRaster(snow_cover_EBBA2[[1]], "C:/Users/david.munoz/Downloads/snow_cover.tif" )
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
