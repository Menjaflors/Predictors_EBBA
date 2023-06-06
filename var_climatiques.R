
#k
#ERA5 monthly averaged reanalysis
#https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview

library(ecmwfr)
library(raster)
library(rgdal)

dir<-"C:/Users/david.munoz/OneDrive - ctfc.cat/predictors_ebalife/"
proj_final<-CRS("EPSG:3035")

# set a key to the keychain
wf_set_key(user = "david.munoz@ctfc.cat",
           key = ,
           service = "cds")

variable_list<-c("2m_temperature", "evaporation_from_vegetation_transpiration", "total_precipitation")

for (i in variable_list) {
  
  
  request <- list(
    product_type = "monthly_averaged_reanalysis",
    variable = i,
    year = c("2018", "2019", "2020", "2021", "2022"),
    month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
    time = "00:00",
    area = c(44, -2, 39, 4),
    format = "grib",
    dataset_short_name = "reanalysis-era5-land-monthly-means",
    target = paste0(i,".grib")
  )
  
  
  wf_request(request, user = "201975", transfer = TRUE, path = paste0(dir, "descarregues"),
             time_out = 120, verbose = TRUE)
  
  ##https://confluence.ecmwf.int/pages/viewpage.action?pageId=197702790 unit is m/day, so we have to *1000*Ndaysmonth
  
  
  grib_file <- paste0(dir, "descarregues/", i,".grib")
  grib_file <- brick(grib_file)
  
  
  grib_file<-projectRaster(grib_file, crs=proj_final)
  assign(i, grib_file)
  
  
  
  
}

####### MÈTODE ANTIC

# dir<-"C:/Users/david.munoz/OneDrive - ctfc.cat/predictors_ebalife/"
# 
# # set a key to the keychain
# wf_set_key(user = "david.munoz@ctfc.cat",
#            key = ,
#            service = "cds")
# 
# 
# 
# request <- list(
#   product_type = "monthly_averaged_reanalysis",
#   variable = c("2m_temperature"),
#   year = c("2018", "2019", "2020", "2021", "2022"),
#   month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
#   time = "00:00",
#   area = c(44, -2, 39, 4),
#   format = "grib",
#   dataset_short_name = "reanalysis-era5-land-monthly-means",
#   target = paste0("2m_temperature", ".grib")
# )
# 
# 
# wf_request(request, user = "201975", transfer = TRUE, path = paste0(dir, "descarregues"),
#   time_out = 60, verbose = TRUE)
# 
# ##https://confluence.ecmwf.int/pages/viewpage.action?pageId=197702790 unit is m/day, so we have to *1000*Ndaysmonth
# 
# 
# grib_file <- "C:/Users/david.munoz/Downloads/prova_ebba/download.grib"
# raster_brick <- brick(grib_file)
# 
# 
# proj_final<-CRS("EPSG:3035")
# raster_brick<-projectRaster(raster_brick, crs=proj_final)
# kk<-raster_brick

cat<-readOGR("C:/Users/david.munoz/Downloads/divisions-administratives-v2r1-20230511/divisions-administratives-v2r1-comarques-100000-20230511.shp")
cat<-spTransform(cat, proj_final)

grid10<-readOGR("C:/Users/david.munoz/Downloads/wetransfer_grid_10x10_ebba2-shx_2023-06-05_1327/grid_10x10_ebba2.shp")

grid10cat<-crop(grid10, cat)

rasgrid10cat<-raster(extent(grid10cat), resolution=c(10000, 10000), crs(grid10cat))
rasgrid10cat<-rasterize(grid10cat, rasgrid10cat)
rasgrid10cat<-mask(rasgrid10cat, cat)

kk<-crop(kk, cat)
kk<-mask(kk, cat)

kk_resample <- resample(kk, rasgrid10cat, method = "bilinear")




writeRaster(kk_resample, "C:/Users/david.munoz/Downloads/kk_resample.tif")
writeRaster(kk, "C:/Users/david.munoz/Downloads/kk.tif")




plot(cat, add=T)
