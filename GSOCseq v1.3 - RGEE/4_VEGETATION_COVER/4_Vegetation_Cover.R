#######################################################
#
# Extract vegetation cover factor from GEE
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
AOI <- 'INPUTS/AOI_POLYGON/LBN.shp'
ISO <- 'LBN'
t_points <- 'INPUTS/TARGET_POINTS/target_points_LBN.shp' 
#Vegetation cover threshold
thrsh <-2000

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
AOI <- read_sf(AOI)


#Initialize GEE ----
ee_Initialize()

#Initial setup either convert shp to gee geometry or extract from UN 2020 map----

#Convert shp to gee geometry
region <- sf_as_ee(AOI)
region <- region$geometry()
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

months <- ee$List$sequence(1, 12)

imagecol <- ee$ImageCollection("MODIS/006/MOD13A2") %>%
  ee$ImageCollection$filterDate('2015-01-01', '2020-12-31') %>%
  ee$ImageCollection$select('NDVI')%>%
  ee$ImageCollection$filterBounds(region)


#FUnction to derive NDVI threshold ratio

veg_ratio <- ee$Image()

ndvi_ratio <- function(m) {
   ndvi <-imagecol$filter(ee$Filter$calendarRange(m, m, "month")) %>%
    ee$ImageCollection$toBands()
  mask = ndvi$gt(thrsh)
  ndvi_thrsh<-ndvi$updateMask(mask)
  ndvi_thrsh_nn =ndvi_thrsh$reduce(ee$Reducer$count())
  ndvi_nn=ndvi$reduce(ee$Reducer$count())
  
  prop_cover= ndvi_thrsh_nn$divide(ndvi_nn)
  #Refactor to RothC vegetation cover factor
  prop_cover= prop_cover$multiply(-0.4)
  prop_cover= prop_cover$add(1)
  return(veg_ratio$addBands(prop_cover))
}



list <- months$map(ee_utils_pyfunc(ndvi_ratio))
veg_ratio_stack <- ee$ImageCollection(list)%>%
ee$ImageCollection$toBands()
veg_ratio_stack <- veg_ratio_stack$toFloat()
names <- veg_ratio_stack$bandNames()$getInfo()
names <- grep('count', names, value=T)   

veg_ratio_stack<-veg_ratio_stack$select(names)
veg_ratio_stack = veg_ratio_stack$resample('bilinear')$reproject(
  crs= crs,
  scale= res)

# Extract the vegetation cover points


veg_ratio_stack <-veg_ratio_stack$updateMask(mask)
#Obtain points
points <- veg_ratio_stack$sample(
  region=region,
  geometries=T,
  scale=res
)



# Export vector points
points_shp <-ee_as_sf(points
                      #,maxFeatures = maxNum
                      , via = "drive"
)




#Gap-fill missing values
lu <- read_sf(t_points)

points_shp <- st_join(lu,points_shp)
names(points_shp)[1]<- 'id'
points_shp[, 'id.y'] <- NULL



points <-  data.frame(points_shp)

cols <-names(points[,4:ncol(points)])


for (i in unique(cols)){
  points[is.na(points[,i]),i] <- 0.6
}

points_shp <-st_as_sf(points)

write_sf(points_shp, paste0('INPUTS/COV/veg_COV_',ISO,'.shp'),overwrite=TRUE)

