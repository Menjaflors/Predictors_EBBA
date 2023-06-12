
#ERA5 monthly averaged reanalysis
#https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview

library(ecmwfr)
library(raster)
library(rgdal)
library(tis)

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
ETP_EBBA2<-raster::subset(evaporation_from_vegetation_transpiration_EBBA2, c(4:7, 16:19, 28:31, 40:43, 52:55)) #select apr-jul from year 1 to 5
ETP_EBBA2<-calc(ETP_EBBA2, fun = function (x) sum(x)*1000/5) #Maybe we can do a sum here?

ETP_EBBA3<-raster::subset(evaporation_from_vegetation_transpiration_EBBA3, c(4:7, 16:19, 28:31, 40:43, 52:55)) #select apr-jul from year 1 to 5
ETP_EBBA3<-calc(ETP_EBBA3, fun = function(x) sum(x)*1000/5) #Maybe we can do a sum here?

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


### Find the warmest and the coldest month: ----
#Exploració mes més càlid
#EBBA2
juny1<-raster::subset(`2m_temperature_EBBA2`, c(6, 18, 30, 42, 54))
juny1<-calc(juny1, fun=mean)
juny1<-cellStats(juny1, stat=mean)

juliol1<-raster::subset(`2m_temperature_EBBA2`, c(7, 19, 31, 43, 55))
juliol1<-calc(juliol1, fun=mean)
juliol1<-cellStats(juliol1, stat=mean)

agost1<-raster::subset(`2m_temperature_EBBA2`, c(8, 20, 32, 44, 56))
agost1<-calc(agost1, fun=mean)
agost1<-cellStats(agost1, stat=mean)


#EBBA3
juny2<-raster::subset(`2m_temperature_EBBA3`, c(6, 18, 30, 42, 54))
juny2<-calc(juny2, fun=mean)
juny2<-cellStats(juny2, stat=mean)

juliol2<-raster::subset(`2m_temperature_EBBA3`, c(7, 19, 31, 43, 55))
juliol2<-calc(juliol2, fun=mean)
juliol2<-cellStats(juliol2, stat=mean)

agost2<-raster::subset(`2m_temperature_EBBA3`, c(8, 20, 32, 44, 56))
agost2<-calc(agost2, fun=mean)
agost2<-cellStats(agost2, stat=mean)

#Mean
agost<-mean(agost1, agost2)
juliol<-mean(juliol1, juliol2)
juny<-mean(juny1, juny2)





##Find the coldest month: 
#Exploració mes més fred
#EBBA2
gener1<-raster::subset(`2m_temperature_EBBA2`, c(1, 13, 25, 37, 49))
gener1<-calc(gener1, fun=mean)
gener1<-cellStats(gener1, stat=mean)

febrer1<-raster::subset(`2m_temperature_EBBA2`, c(2, 14, 26, 38, 50))
febrer1<-calc(febrer1, fun=mean)
febrer1<-cellStats(febrer1, stat=mean)

desembre1<-raster::subset(`2m_temperature_EBBA2`, c(12, 24, 36, 48, 60))
desembre1<-calc(desembre1, fun=mean)
desembre1<-cellStats(desembre1, stat=mean)



#EBBA3
gener2<-raster::subset(`2m_temperature_EBBA3`, c(1, 13, 25, 37, 49))
gener2<-calc(gener2, fun=mean)
gener2<-cellStats(gener2, stat=mean)

febrer2<-raster::subset(`2m_temperature_EBBA3`, c(2, 14, 26, 38, 50))
febrer2<-calc(febrer2, fun=mean)
febrer2<-cellStats(febrer2, stat=mean)

desembre2<-raster::subset(`2m_temperature_EBBA3`, c(12, 24, 36, 48, 60))
desembre2<-calc(desembre2, fun=mean)
desembre2<-cellStats(desembre2, stat=mean)

gener<-mean(gener1, gener2)
febrer<-mean(febrer1, febrer2)
desembre<-mean(desembre1, desembre2)
