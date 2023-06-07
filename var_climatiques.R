
#ERA5 monthly averaged reanalysis
#https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview

library(ecmwfr)
library(raster)
library(rgdal)
library(tis)

dir<-"C:/Users/david.munoz/OneDrive - ctfc.cat/predictors_ebalife/"
proj_final<-CRS("EPSG:3035")

### Download variables. It is necessary to have an account in ECMWF ----
#Then, download the package ecmwfr, and run the following function:
# wf_set_key(user = "david.munoz@ctfc.cat", #put the user
#            key = ,
#            service = "cds")

#Then browser will be opened, put the UID and the API key. To download, copy the code in python in the web
#and in RSTUDIO "addins"-> python to list. Maybe you have to remove some characters from sataset_short_name

variable_list<-c("2m_temperature", "evaporation_from_vegetation_transpiration", "total_precipitation", "surface_net_solar_radiation", "snow_cover")

for (i in variable_list) {
  
  request <- list(
    product_type = "monthly_averaged_reanalysis",
    variable = i,
    year = c("2018", "2019", "2020", "2021", "2022"),
    month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
    time = "00:00",
    area = c(88, -40, 20, 80),
    format = "grib",
    dataset_short_name = "reanalysis-era5-land-monthly-means",
    target = paste0(i, "_EBBA3.grib")
  )
  
  wf_request(request, user = "201975", transfer = TRUE, path = paste0(dir, "descarregues"),
             time_out = 300, verbose = TRUE) #Download the request using your user and a path. 
  
  grib_file <- paste0(dir, "descarregues/", i,"_EBBA3.grib") #Open the grib and read as rasterbrick
  grib_file <- brick(grib_file)
  
  assign(paste0(i,"_EBBA3"), grib_file)
  
  request <- list(
    product_type = "monthly_averaged_reanalysis",
    variable = i,
    year = c("2013", "2014", "2015", "2016", "2017"),
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
ETP_EBBA2<-raster::subset(evaporation_from_vegetation_transpiration_EBBA2, c(4:7, 16:19, 28:31, 40:43, 52:55)) #select apr-jul from year 1 to 5
ETP_EBBA2<-calc(ETP_EBBA2, fun = mean) #Maybe we can do a sum here?

ETP_EBBA3<-raster::subset(evaporation_from_vegetation_transpiration_EBBA3, c(4:7, 16:19, 28:31, 40:43, 52:55)) #select apr-jul from year 1 to 5
ETP_EBBA3<-calc(ETP_EBBA3, fun = mean) #Maybe we can do a sum here?

##Mean annual temperature
MAT_EBBA2<-calc(`2m_temperature_EBBA2`, fun=mean)

MAT_EBBA3<-calc(`2m_temperature_EBBA3`, fun=mean)

##Mean temperature in breeding period (APR-JUL)
MATBP_EBBA2<-raster::subset(`2m_temperature_EBBA2`, c(4:7, 16:19, 28:31, 40:43, 52:55))
MATBP_EBBA2<-calc(MATBP_EBBA2, fun=mean)

MATBP_EBBA3<-raster::subset(`2m_temperature_EBBA3`, c(4:7, 16:19, 28:31, 40:43, 52:55))
MATBP_EBBA3<-calc(MATBP_EBBA3, fun=mean)

##Total annual precipitation: unit is m/day, so we have to *1000*Ndaysmonth  https://confluence.ecmwf.int/pages/viewpage.action?pageId=197702790 

# EBBA2----

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
  
  # Multiply each pixel value by the number of days and 1000 (m to mm). Now it is l/m2 for month
  total_precipitation_EBBA2[[i]] <- layer * days * 1000
}

#And summarise, firts we will obtain the total accumulated precipitation in the period, if we divide, we obtain the precipitation/year
TAP_EBBA2<-calc(total_precipitation_EBBA2, fun=function(x) sum(x)/5)

# EBBA3 ----

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
  
  # Multiply each pixel value by the number of days and 1000 (m to mm). Now it is l/m2 for month
  total_precipitation_EBBA3[[i]] <- layer * days * 1000
}

#And summarise, firts we will obtain the total accumulated precipitation in the period, if we divide, we obtain the precipitation/year
TAP_EBBA3<-calc(total_precipitation_EBBA3, fun=function(x) sum(x)/5)


##Total precipitation in the breeding period
TAPBP_EBBA2<-raster::subset(total_precipitation_EBBA2, c(4:7, 16:19, 28:31, 40:43, 52:55)) #select apr-jul from year 1 to 5
TAPBP_EBBA2<-calc(TAPBP_EBBA2, fun = function(x) sum(x)/5)

TAPBP_EBBA3<-raster::subset(total_precipitation_EBBA3, c(4:7, 16:19, 28:31, 40:43, 52:55)) #select apr-jul from year 1 to 5
TAPBP_EBBA3<-calc(TAPBP_EBBA3, fun = function(x) sum(x)/5)

##Days of snow cover. The first layer is good, but not the rest, as the NA values is 9999. We have to reclassify it
rcl<-matrix(c(102, 999999, NA, -999999, -0.001, NA), ncol = 3, byrow=T ) #Maybe if we mask the layer we will not have to do this. 

days_snow_cover_EBBA2<-raster::reclassify(snow_cover_EBBA2, rcl)
SNWCVR_EBBA2<-calc(days_snow_cover_EBBA2, fun=mean)

days_snow_cover_EBBA3<-raster::reclassify(snow_cover_EBBA3, rcl)
SNWCVR_EBBA3<-calc(days_snow_cover_EBBA3, fun=mean)
##Surface net solar radiation
solar_radiation_EBBA_2<-calc(surface_net_solar_radiation_EBBA2, fun=mean)

solar_radiation_EBBA_3<-calc(surface_net_solar_radiation_EBBA3, fun=mean)



### Resample, mask in Europe----

grib_file<-projectRaster(grib_file, crs=proj_final) #Project the raster and change the name
grid10<-readOGR(paste0(dir, "grid_10x10/grid_10x10_ebba2.shp"))
rasgrid10<-raster(extent(grid10), resolution=c(10000, 10000), crs(grid10))
rasgrid10<-rasterize(grid10, rasgrid10)
rasgrid10<-mask(rasgrid10, grid10)

kk_resample <- resample(`2m_temperature`[[1]], rasgrid10, method = "bilinear")
kk_resample <- raster::mask(kk_resample, rasgrid10)


### Ohter things ----

# cat<-readOGR("C:/Users/david.munoz/Downloads/divisions-administratives-v2r1-20230511/divisions-administratives-v2r1-comarques-100000-20230511.shp")
# cat<-spTransform(cat, proj_final)
# 
# 
# grid10cat<-crop(grid10, cat)
# 
# rasgrid10cat<-raster(extent(grid10cat), resolution=c(10000, 10000), crs(grid10cat))
# rasgrid10cat<-rasterize(grid10cat, rasgrid10cat)
# rasgrid10cat<-mask(rasgrid10cat, cat)
# 
# kk<-crop(kk, cat)
# kk<-mask(kk, cat)
# 
# kk_resample <- resample(kk, rasgrid10cat, method = "bilinear")
# 
# 
# 
# 
# writeRaster(kk_resample, "C:/Users/david.munoz/Downloads/kk_resample.tif")
# writeRaster(kk, "C:/Users/david.munoz/Downloads/kk.tif")
# 
# 
# 
# 
# plot(cat, add=T)
