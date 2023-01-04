#######################################################
#
# Extract SOC and Clay from GEE
# MODIS
# 
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
#wd <- 'C:/Users/luottoi/Documents/GSOCseq/YEM'


# Area of interest: target points created in the previous scripts 
#AOI <- 'INPUTS/TARGET_POINTS/target_points_DNK.shp'
ISO <- 'LBN'

border <- "INPUTS/AOI_POLYGON/LBN.shp"

SOC <- 'INPUTS/SOC_MAP/ocs_0_30_QRF.tif'
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

border <- read_sf(border)



#Initialize GEE ----
ee_Initialize()
border <-  sf_as_ee(border)
region <- border$geometry()
#Initial setup either convert shp to gee geometry or extract from UN 2020 map----
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


SOC <- rast(SOC)
# #SOC map ----
#  soc <- ee$Image("users/IsaLuotto/GSOCmap161")%>%
#   ee$Image$clip(border)
# # socr <- ee_as_raster(
# #   image = soc,
# #   scale= res,
# #   region = border,
# #   via = "drive"
# # )
# # writeRaster(socr, paste0('INPUTS/SOC_MAP/SOC_MAP_',ISO,'.tif'))

# Clay map
clay <- ee$Image('OpenLandMap/SOL/SOL_CLAY-WFRACTION_USDA-3A1A1A_M/v02')%>%
  ee$Image$select('b0', 'b10', 'b30')

#Calculate weighted average
b0 <- clay$select('b0')
b0 <-  b0$multiply(1/30)


b10 <- clay$select('b10')
b10 <-  b10$multiply(9/30)

b30 <- clay$select('b30')
b30 <-  b30$multiply(20/30)


clay <- b0$add(b10)
clay <- clay$add(b30)

# Stack layers

#crs = proj$crs
clay = clay$resample('bilinear')$reproject(
  crs= crs,
  scale= res)

#soc = soc$resample('bilinear')$reproject(
#   crs= crs,
#   scale= res)

#soil <- clay$addBands(soc)
#soil<-soil$updateMask(mask)
soil<-clay$updateMask(mask)

# Extract the soil points
#Obtain points
points <- soil$sample(
  region=region,
  geometries=T,
  scale=res
)


maxNum <- points$aggregate_count('b0')
maxNum <- maxNum$getInfo()

# Export vector points
points_shp <-ee_as_sf(points
                     #,maxFeatures = maxNum
                      , via = "drive",
                     crs=crs
)
# names(points_shp)[2:3] <- c('clay', 'SOC')
# points_shp <-points_shp[,c("id", "SOC","clay" ,"geometry") ]
# 
# write_sf(points_shp, paste0('INPUTS/SOIL/SOC_clay_',ISO,'.shp'))

#points_shp <- vect(points_shp)

soc <- raster::extract(raster(SOC), points_shp, sp=T)
soil <-merge(points_shp,soc)
names(soil)[2:3] <- c('clay', 'SOC')

soil <- soil[, c("id","SOC","clay","geometry")]


write_sf(soil, paste0('INPUTS/SOIL/SOC_clay_',ISO,'.shp'),overwrite=TRUE)
