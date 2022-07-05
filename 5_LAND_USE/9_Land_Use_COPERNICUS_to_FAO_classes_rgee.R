#######################################################
#
# Reclassify land use layer and obtain target points
# from GEE in Rstudio
#
# GSP-Secretariat
# Contact: Isabel.Luotto@fao.org
#
#######################################################

#Empty environment and cache ----
rm(list = ls());
gc()

#######################################################
#
#  User defined variables:

# Working directory
#wd <- 'C:/Users/luottoi/Documents/GitHub/Digital-Soil-Mapping'
wd <- 'C:/Users/hp/Documents/FAO/GSOCseq/rgee'


# Area of interest: either own shapefile or 3-digit ISO code to extract from UN 2020 boundaries
#AOI <- '01-Data/MKD.shp'
AOI <- 'AUS'

# GEE Resolution (CRS defined based on the first TerraClimate layer WGS84 )
res = 1000
crs <- 'EPSG:4326'
#
#
#######################################################


# Load libraries ----
library(data.table)
library(terra)
library(raster)
library(rgee)
library(sf)


# Set working directory ----


setwd(wd)

# Upload own AOI shapefile ----
#AOI <- read_sf(AOI)
# convert AOI to a box polygon
#AOI <- st_as_sfc(st_bbox(AOI))
#AOI <- st_as_sf(AOI)



#Initialize GEE ----
ee_Initialize()

#Initial setup either convert shp to gee geometry or extract from UN 2020 map----

#Convert shp to gee geometry
#region <- sf_as_ee(AOI)
#region = region$geometry()

#Extract from UN 2020 map using ISO code ----
region <-ee$FeatureCollection("projects/digital-soil-mapping-gsp-fao/assets/UN_BORDERS/BNDA_CTY")%>%
  ee$FeatureCollection$filterMetadata('ISO3CD', 'equals', AOI)

#AOI_shp <-ee_as_sf(region)
#write_sf(AOI_shp, paste0('01-Data/',AOI,'.shp'))

region = region$geometry()

# Mean annual temperature (daytime) ----
# Go to https://code.earthengine.google.com/
# browse for the dataset you're interested in
# find and copy/paste the path  

image1 <- ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global") %>%
  ee$ImageCollection$select("discrete_classification")%>%
  ee$ImageCollection$filterBounds(region)%>%
  ee$ImageCollection$toBands()


proj = image1$projection()$getInfo()

#crs = proj$crs
image1 = image1$resample('bilinear')$reproject(
  crs= crs,
  scale= res)



#Reclassify 
inList <- ee$List(c(0  ,20  ,30  ,40  ,50  ,60  ,70  ,80  ,90 ,100 ,111 ,112 ,113 ,114 ,115 ,116 ,121 ,122 ,123 ,124 ,125 ,126, 200))
outList <- ee$List(c(0,  5,  3,  2,  0,  0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0,  0,  0,  0))


# # Extract points for target classes
# FAO_lu<- image1$remap(inList, outList)
# FAO_lu <-FAO_lu$toDouble()
# FAO_lu =FAO_lu$clip(region)
# #Convert 0 to NA
# mask <- FAO_lu$neq(0)
# FAO_lu <- FAO_lu$updateMask(mask)
#Obtain points
# points <- FAO_lu$sample(
#   region=region,
#   geometries=T,
#   scale=res
# )
# 
# maxNum <- points$aggregate_count('remapped')
# maxNum <- maxNum$getInfo()
# 
# # Export vector points
# points_shp <-ee_as_sf(points
#                       ,maxFeatures = maxNum
# )
# write_sf(points_shp, paste0('target_points_',AOI,'.shp'))

# Solution for very large countries (split in croplands, grasslands and shrublands) 

#Reclassify 
#CROPLANDS
crops <- ee$List(c(0,  0,  0,  2,  0,  0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0,  0,  0,  0))
grasslands <- ee$List(c(0,  0, 3,  0,  0,  0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0,  0,  0,  0))
#shrublands <- ee$List(c(0,  5, 0,  0,  0,  0, 0, 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0,  0,  0,  0))


FAO_lu<- image1$remap(inList, crops)
FAO_lu <-FAO_lu$toDouble()
FAO_lu =FAO_lu$clip(region)
#Convert 0 to NA
mask <- FAO_lu$neq(0)
FAO_lu <- FAO_lu$updateMask(mask)

#Obtain points
points <- FAO_lu$sample(
  region=region,
  geometries=T,
  scale=res
)

maxNum <- points$aggregate_count('remapped')
maxNum <- maxNum$getInfo()

# Export vector points
points_crops <-ee_as_sf(points
                     ,maxFeatures = maxNum
                     , via = "drive" )

#GRASSLANDS
FAO_lu<- image1$remap(inList, grasslands)
FAO_lu <-FAO_lu$toDouble()
FAO_lu =FAO_lu$clip(region)
#Convert 0 to NA
mask <- FAO_lu$neq(0)
FAO_lu <- FAO_lu$updateMask(mask)

#Obtain points
points <- FAO_lu$sample(
  region=region,
  geometries=T,
  scale=res
)

maxNum <- points$aggregate_count('remapped')
maxNum <- maxNum$getInfo()

# Export vector points
points_grass <-ee_as_sf(points
                        ,maxFeatures = maxNum
                        , via = "drive"
)


points_shp <- c(points_crops,points_grass)
rm(points_crops)
rm(points_grass)

write_sf(points_shp, paste0('target_points_',AOI,'.shp'))

#Export raster will all FAO classes
#Reclassify 
outList <- ee$List(c(0,  5,  3,  2,  1,  9, 10, 11,  7,  0,  4,  4,  4,  4,  4,  4,  4,  4, 4,  4,  4,  4,  0))

FAO_lu<- image1$remap(inList, outList)
FAO_lu <-FAO_lu$toDouble()
FAO_lu =FAO_lu$clip(region)

# CTRL + shift + C to comment
FAO_lu <- ee_as_raster(
  image = FAO_lu,
  scale= res,
  region = region,
  via = "drive"
)
plot(FAO_lu)
last <-as.integer(length(dim(FAO_lu)))
FAO_lu <- as.factor(FAO_lu[[1]])

writeRaster(FAO_lu, paste0('LU_FAO_class_rgee_',AOI,'.tif'), overwrite=T)




