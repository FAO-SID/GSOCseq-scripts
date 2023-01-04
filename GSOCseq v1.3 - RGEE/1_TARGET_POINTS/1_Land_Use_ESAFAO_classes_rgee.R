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
wd <- 'C:/Users/hp/Documents/FAO/GSOCseq/Lebanon'


# Area of interest: either own shapefile or 3-digit ISO code to extract from UN 2020 boundaries
AOI <- 'LBN'
#AOI <-'C:/Users/hp/Documents/FAO/GSOCseq/Yemen'
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
#AOI='YEM'

#Extract from UN 2020 map using ISO code ----
region <-ee$FeatureCollection("projects/digital-soil-mapping-gsp-fao/assets/UN_BORDERS/BNDA_CTY")%>%
 ee$FeatureCollection$filterMetadata('ISO3CD', 'equals', AOI)

AOI_shp <-ee_as_sf(region)
write_sf(AOI_shp, paste0('INPUTS/AOI_POLYGON/',AOI,'.shp'))

region = region$geometry()

# Mean annual temperature (daytime) ----
# Go to https://code.earthengine.google.com/
# browse for the dataset you're interested in
# find and copy/paste the path  

image1 <- ee$Image("users/IsaLuotto/ESA_Land_Cover_12clases_FAO_World_2015_1km") %>%
  ee$Image$clip(region)


proj = image1$projection()$getInfo()



#   0 = 0	  No Data
#	190 = 1 Artificial
#	10 11 30 40 = 2 Croplands
#	130 = 3 Grassland
#	50 60 61 62 70 71 72 80 81 82 90 100 110 = 4 Tree Covered
#	120 121 122= 5 Shrubs Covered
#	160 180 = 6 Herbaceous vegetation flooded
#	170 = 7 Mangroves
#	150 151 152 153= 8 Sparse Vegetation
#	200 201 202 = 9 Baresoil
#	220 = 10 Snow and Glaciers
#	210 = 11 Waterbodies
#	12  = 12 Treecrops
# 20 = 13 Paddy fields(rice/ flooded crops)


#Reclassify 
inList <- ee$List(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13))
outList <- ee$List(c(0,0,2,3,0,5,0,0,0,0,0,0,12,13))


# Extract points for target classes
FAO_lu<- image1$remap(inList, outList)
#FAO_lu <-FAO_lu$toDouble()
FAO_lu =FAO_lu$clip(region)
#Convert 0 to NA
mask <- FAO_lu$neq(0)
FAO_lu <- FAO_lu$updateMask(mask)

FAO_lu = FAO_lu$resample()$reproject(
  crs= crs,
  scale= res)

lur <- ee_as_raster(
  image = FAO_lu,
  scale= res,
  region = region,
  via = "drive"
)
writeRaster(lur, paste0('INPUTS/LAND_USE/LU_',AOI,'.tif'),overwrite=TRUE)


#Obtain points
points <- FAO_lu$sample(
  region=region,
  geometries=T,
  scale=res
)

maxNum <- points$aggregate_count('remapped')
maxNum <- maxNum$getInfo()

## Export vector points
points_shp <-ee_as_sf(points
                      ,maxFeatures = maxNum
                      , via = "drive" 
)
write_sf(points_shp, paste0('INPUTS/TARGET_POINTS/target_points_',AOI,'.shp'))
write_sf(points_shp, paste0('INPUTS/LAND_USE/LU_points_',AOI,'.shp'))


