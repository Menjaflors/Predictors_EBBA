
request <- list(
  format = "zip",
  variable = c("actual_evaporation", "potential_evaporation"),
  product_family = "evapotranspiration_indicators",
  year = c("2013", "2014", "2015", "2016", "2017"),
  month = c("04", "05", "06", "07"),
  day = c("01", "11", "21"),
  dataset_short_name = "sis-agroproductivity-indicators",
  target = "evapotranspiration.zip"
)

wf_request(request, user = "201975", transfer = TRUE, path = file.path(dir_EBBA2, "raw", "climate"),
           time_out = 300, verbose = TRUE) #Download the request using your user and a path. 


filenames <- list.files(path=file.path(dir_EBBA2, 'raw', 'climate', 'potential_evapotranspiration'), pattern="nc$", full.names=T) # G: I corrected the rasters because they were misplaced compared with the 10x10 EBBA grid. Open '.tif' files from a folder (10 km resolution)
EVAPOBREE_EBBA2_r <- pblapply(filenames, raster) # G: we open the rasters in a more efficient and clean way

filenames_year <- pblapply(EVAPOBREE_EBBA2_r, function(x) { x@z[[1]] } )
filenames_year <- do.call("c", filenames_year)
filenames_year <- paste0("EVAPO_", format(filenames_year,'%Y'))
names(EVAPOBREE_EBBA2_r) <- filenames_year
EVAPOBREE_EBBA2_r <- EVAPOBREE_EBBA2_r[order(names(EVAPOBREE_EBBA2_r))] # G: here we have to sort for EBBA2 only

kk<-mean(EVAPOBREE_EBBA2_r)



kk <- raster::brick(EVAPOBREE_EBBA2_r)

kk2<-calc(kk, fun=mean)
kk3<-crop(kk2, e)
writeRaster(kk3, "C:/Users/david.munoz/Downloads/nou_potential_evapo.tif")





potent<-rast("C:/Users/david.munoz/Downloads/nou_potential_evapo.tif")
real <-rast("C:/Users/david.munoz/Downloads/nou_evapo.tif")

divisio<-real/potent
writeRaster(divisio, "C:/Users/david.munoz/Downloads/nou_ETR_ETP.tif" )

library(terra)




request <- list(
  format = "zip",
  product_family = "crop_productivity_indicators",
  variable = c("crop_development_stage", "total_above_ground_production", "total_weight_storage_organs"),
  crop_type = c("maize", "soybean", "spring_wheat", "wet_rice", "winter_wheat"),
  year = "2019",
  month = "04",
  day = c("10", "20", "30"),
  growing_season = c("1st_season_per_campaign", "2nd_season_per_campaign"),
  harvest_year = "2019",
  dataset_short_name = "sis-agroproductivity-indicators",
  target = "download.zip"
)


wf_request(request, user = "201975", transfer = TRUE, path = "C:/Users/david.munoz/Downloads/kk",
           time_out = 300, verbose = TRUE) #Download the request using your user and a path. 


DIR_KK<-"C:/Users/david.munoz/Downloads/kk/download"

filenames <- list.files(DIR_KK, pattern="//.nc$", full.names=T) # G: I corrected the rasters because they were misplaced compared with the 10x10 EBBA grid. Open '.tif' files from a folder (10 km resolution)
suppressWarnings(EVAPOBREE_EBBA2_r <- pblapply(filenames, raster)) # G: we open the rasters in a more efficient and clean way

filenames_year <- pblapply(EVAPOBREE_EBBA2_r, function(x) { x@z[[1]] } )
filenames_year <- do.call("c", filenames_year)
filenames_year <- paste0("EVAPO_", format(filenames_year,'%Y'))
names(EVAPOBREE_EBBA2_r) <- filenames_year
EVAPOBREE_EBBA2_r <- EVAPOBREE_EBBA2_r[order(names(EVAPOBREE_EBBA2_r))] # G: here we have to sort for EBBA2 only



library(httr)
library(jsonlite)
params <- list(pretty = TRUE)
product_id <- 'SRTMGL1_NC.003'
response <- GET(paste("https://appeears.earthdatacloud.nasa.gov/api/product/",product_id, sep = ""), query = params)
product_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
product_response
