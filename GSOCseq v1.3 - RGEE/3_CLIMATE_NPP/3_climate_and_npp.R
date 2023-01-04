#######################################################
#
# Extract climate data from GEE
# TerraClimate
# And calculate MIAMI NPP 
# Spin up - Warm up - Forward phase
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


# Area of interest: target points created in the previous scripts 
AOI <- 'INPUTS/AOI_POLYGON/LBN.shp'
ISO <- 'LBN'

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




# SPIN UP PHASE ----
#TerraClimate Precipitation
val ='pr'
months <- ee$List$sequence(1, 12)
image1 <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('1981-01-01', '2001-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)
# Function to Calculate a monthly composite
monthly <- function(m) {
  image1$filter(ee$Filter$calendarRange(m, m, "month")) %>%
    ee$ImageCollection$mean() %>%
    ee$Image$select(val)
}
image1 <- months$map(ee_utils_pyfunc(monthly))%>%
  ee$ImageCollection$toBands()
proj = image1$projection()$getInfo()
#crs = proj$crs
pr = image1$resample('bilinear')$reproject(
  crs= crs,
  scale= res)

#TerraClimate Evapotransipiration
val ='pet'
image1 <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('1981-01-01', '2001-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)
# Function to Calculate a monthly composite
monthly <- function(m) {
  image1$filter(ee$Filter$calendarRange(m, m, "month")) %>%
    ee$ImageCollection$mean() %>%
    ee$Image$select(val)
}
image1 <- months$map(ee_utils_pyfunc(monthly))%>%
  ee$ImageCollection$toBands()

image1 <- image1$multiply(0.1)

pet = image1$resample('bilinear')$reproject(
  crs= crs,
  scale= res)


# Average temperatue
val ='tmmx'
image1 <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('1981-01-01', '2001-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)
# Function to Calculate a monthly composite
monthly <- function(m) {
  image1$filter(ee$Filter$calendarRange(m, m, "month")) %>%
    ee$ImageCollection$mean() %>%
    ee$Image$select(val)
}
image1 <- months$map(ee_utils_pyfunc(monthly))%>%
  ee$ImageCollection$toBands()

tmmx <- image1$multiply(0.1)
val ='tmmn'
image1 <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('1981-01-01', '2001-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)
# Function to Calculate a monthly composite
monthly <- function(m) {
  image1$filter(ee$Filter$calendarRange(m, m, "month")) %>%
    ee$ImageCollection$mean() %>%
    ee$Image$select(val)
}
image1 <- months$map(ee_utils_pyfunc(monthly))%>%
  ee$ImageCollection$toBands()

tmmn <- image1$multiply(0.1)

diff <- tmmx$add(tmmn)
avT = diff$divide(2)

avT = avT$resample('bilinear')$reproject(
  crs= crs,
  scale= res)

#Stack layers
SG <- avT$addBands(pr)
SG <- SG$addBands(pet)
SG<-SG$updateMask(mask)
#Obtain points
points <- SG$sample(
  region=region,
  geometries=T,
  scale=res
)



## Export vector points
points_shp <-ee_as_sf(points
                      #,maxFeatures = maxNum
                      , via = "drive" 
)
write_sf(points_shp, paste0('INPUTS/TERRA_CLIMATE/Spin_up_',ISO,'.shp'))

# FORWARD PHASE ----
#TerraClimate Precipitation

val ='pr'
months <- ee$List$sequence(1, 12)
image1 <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('2001-01-01', '2019-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)
# Function to Calculate a monthly composite
monthly <- function(m) {
  image1$filter(ee$Filter$calendarRange(m, m, "month")) %>%
    ee$ImageCollection$mean() %>%
    ee$Image$select(val)
}
image1 <- months$map(ee_utils_pyfunc(monthly))%>%
  ee$ImageCollection$toBands()
proj = image1$projection()$getInfo()
#crs = proj$crs
pr = image1$resample('bilinear')$reproject(
  crs= crs,
  scale= res)

#TerraClimate Evapotransipiration
val ='pet'
image1 <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('2001-01-01', '2019-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)
# Function to Calculate a monthly composite
monthly <- function(m) {
  image1$filter(ee$Filter$calendarRange(m, m, "month")) %>%
    ee$ImageCollection$mean() %>%
    ee$Image$select(val)
}
image1 <- months$map(ee_utils_pyfunc(monthly))%>%
  ee$ImageCollection$toBands()

image1 <- image1$multiply(0.1)

pet = image1$resample('bilinear')$reproject(
  crs= crs,
  scale= res)


# Average temperatue
val ='tmmx'
image1 <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('2001-01-01', '2019-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)
# Function to Calculate a monthly composite
monthly <- function(m) {
  image1$filter(ee$Filter$calendarRange(m, m, "month")) %>%
    ee$ImageCollection$mean() %>%
    ee$Image$select(val)
}
image1 <- months$map(ee_utils_pyfunc(monthly))%>%
  ee$ImageCollection$toBands()

tmmx <- image1$multiply(0.1)
val ='tmmn'
image1 <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('2001-01-01', '2019-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)
# Function to Calculate a monthly composite
monthly <- function(m) {
  image1$filter(ee$Filter$calendarRange(m, m, "month")) %>%
    ee$ImageCollection$mean() %>%
    ee$Image$select(val)
}
image1 <- months$map(ee_utils_pyfunc(monthly))%>%
  ee$ImageCollection$toBands()

tmmn <- image1$multiply(0.1)

diff <- tmmx$add(tmmn)
avT = diff$divide(2)

avT = avT$resample('bilinear')$reproject(
  crs= crs,
  scale= res)

#Stack layers
SG <- avT$addBands(pr)
SG <- SG$addBands(pet)
SG<-SG$updateMask(mask)
#Obtain points
points <- SG$sample(
  region=region,
  geometries=T,
  scale=res
)

## Export vector points
points_shp <-ee_as_sf(points
                      #,maxFeatures = maxNum
                      , via = "drive" 
)
write_sf(points_shp, paste0('INPUTS/TERRA_CLIMATE/Forward_up_',ISO,'.shp'))
################################################################################

# WARM UP PHASE  ----
rm(points_shp)
gc()
#TerraClimate Precipitation
val ='pr'
months <- ee$List$sequence(1, 12)
image1 <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('2001-01-01', '2019-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)%>%
  ee$ImageCollection$toBands()

pr = image1$resample('bilinear')$reproject(
  crs= crs,
  scale= res)

#TerraClimate Evapotransipiration
val ='pet'
image1 <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('2001-01-01', '2019-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)%>%
  ee$ImageCollection$toBands()

image1 <- image1$multiply(0.1)

pet = image1$resample('bilinear')$reproject(
  crs= crs,
  scale= res)


# Average temperatue
val ='tmmx'
image1 <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('2001-01-01', '2019-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)%>%
  ee$ImageCollection$toBands()

tmmx <- image1$multiply(0.1)
val ='tmmn'
image1 <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('2001-01-01', '2019-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)%>%
  ee$ImageCollection$toBands()

tmmn <- image1$multiply(0.1)

diff <- tmmx$add(tmmn)
avT = diff$divide(2)

avT = avT$resample('bilinear')$reproject(
  crs= crs,
  scale= res)


avT<-avT$updateMask(mask)
#Obtain points
points <- avT$sample(
  region=region,
  geometries=T,
  scale=res
)


## Export vector points
points_shp <-ee_as_sf(points
                      #,maxFeatures = maxNum
                      , via = "drive" 
)
write_sf(points_shp, paste0('INPUTS/TERRA_CLIMATE/avT_warmup_up_',ISO,'.shp'))

rm(points_shp)
gc()

pr<-pr$updateMask(mask)
#Obtain points
points <- pr$sample(
  region=region,
  geometries=T,
  scale=res
)


## Export vector points
points_shp <-ee_as_sf(points
                      #,maxFeatures = maxNum
                      , via = "drive" 
)
write_sf(points_shp, paste0('INPUTS/TERRA_CLIMATE/pr_warmup_up_',ISO,'.shp'))

rm(points_shp)
gc()




#Obtain points
pet<-pet$updateMask(mask)
points <- pet$sample(
  region=region,
  geometries=T,
  scale=res
)


## Export vector points
points_shp <-ee_as_sf(points
                      #,maxFeatures = maxNum
                      , via = "drive" 
)
write_sf(points_shp, paste0('INPUTS/TERRA_CLIMATE/pet_warmup_up_',ISO,'.shp'))

rm(points_shp)
gc()

# NPP from mean annual temp and total precipitation 81-2001 ----
years <- ee$List$sequence(1981, 2001)

#TerraClimate Precipitation
val ='pr'
image1 <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('1981-01-01', '2001-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)
# Function to Calculate a yearly composite
yearly <- function(y) {
  image1$filter(ee$Filter$calendarRange(y, y, "year")) %>%
    ee$ImageCollection$select(val)%>%
    ee$ImageCollection$reduce(ee$Reducer$sum())
}
image1 <- years$map(ee_utils_pyfunc(yearly))%>%
  ee$ImageCollection$toBands()


pr = image1$resample('bilinear')$reproject(
  crs= crs,
  scale= res)

#3000*(1-exp(-0.000664*dat$X10_pr_)
x <- pr$multiply(-0.000664)
x <-x$exp()
num <- ee$Image(1)
x <- num$subtract(x)
num <- ee$Number(3000)
pr_npp <- x$multiply(num)


# Average temperatue
val =c('tmmx', 'tmmn')

imagecol <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('1981-01-01', '2001-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)

subtmxtmn <- function(image) {
  return(
       image$addBands(image$select('tmmx')$subtract(image$select('tmmn'))))
}

diff <- imagecol$map(subtmxtmn)

divide2 <- function(image) {
  return(image$addBands(image$divide(2)))
}

avT <- diff$map(divide2)



# Function to Calculate a yearly composite
yearly <- function(y) {
  avT$filter(ee$Filter$calendarRange(y, y, "year")) %>%
    ee$ImageCollection$select('tmmx')%>%
    ee$ImageCollection$reduce(ee$Reducer$mean())
}
avT <- years$map(ee_utils_pyfunc(yearly))%>%
  ee$ImageCollection$toBands()

avT <- avT$multiply(0.1)

avT = avT$resample('bilinear')$reproject(
  crs= crs,
  scale= res)

#3000/(1+exp(1.315-0.119*TempStack))
x <- avT$multiply(0.119)
num <- ee$Image(1.315)
x <- num$subtract(x)
x <-x$exp()
num <- ee$Number(1)
x <- x$add(num)
num <- ee$Image(3000)

avT_npp <-num$divide(x)


#Stack layers
SG <- avT_npp$addBands(pr_npp)
SG<-SG$updateMask(mask)

#Obtain points
points <- SG$sample(
  region=region,
  geometries=T,
  scale=res
)



## Export vector points
points_shp <-ee_as_sf(points
                      #,maxFeatures = maxNum
                      , via = "drive" 
)

write_sf(points_shp, paste0('INPUTS/NPP_TERRA/NPPP_NPPT_81_01_',ISO,'.shp'))

#Calculate NPP for 20 years
#Select minimum value from pr and temp columns for each point and year

data <- as.data.frame(vect(points_shp))

for (i in 0:19){
  col1 <-grep('pr', names(data), value =T)
  #col1 <-grep(paste0('X',i), col1, value =T)
  col2 <-grep('tm', names(data), value =T)
  #col2 <-grep(paste0('X',i), col2, value =T)
  
  
  points_shp[ , paste0("yr", i)]  <- do.call(pmin, c(data[, c(col1,col2)], na.rm = TRUE))*(1/100)*0.5
  print(i)
}

data <- points_shp[,42:62]
#Calculate mean NPP over 20 years
data$mean_npp <- rowMeans(as.data.frame(vect(data)))
write_sf(data[,'mean_npp'], paste0('INPUTS/NPP_TERRA/MEAN_NPP_81_01_',ISO,'.shp'))


# MAXIMUM NPP ----
#TerraClimate Precipitation
val ='pr'
image1 <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('1981-01-01', '2001-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)
# Function to Calculate a yearly composite
yearly <- function(y) {
  image1$filter(ee$Filter$calendarRange(y, y, "year")) %>%
    ee$ImageCollection$select(val)%>%
    ee$ImageCollection$reduce(ee$Reducer$sum())
}
image1 <- years$map(ee_utils_pyfunc(yearly))%>%
  ee$ImageCollection$toBands()


pr = image1$resample('bilinear')$reproject(
  crs= crs,
  scale= res)

pr <- pr$multiply(1.05)

#3000*(1-exp(-0.000664*dat$X10_pr_)
x <- pr$multiply(-0.000664)
x <-x$exp()
num <- ee$Image(1)
x <- num$subtract(x)
num <- ee$Number(3000)
pr_npp <- x$multiply(num)


# Average temperatue
val =c('tmmx', 'tmmn')

imagecol <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('1981-01-01', '2001-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)

subtmxtmn <- function(image) {
  return(
    image$addBands(image$select('tmmx')$subtract(image$select('tmmn'))))
}

diff <- imagecol$map(subtmxtmn)

divide2 <- function(image) {
  return(image$addBands(image$divide(2)))
}

avT <- diff$map(divide2)



# Function to Calculate a yearly composite
yearly <- function(y) {
  avT$filter(ee$Filter$calendarRange(y, y, "year")) %>%
    ee$ImageCollection$select('tmmx')%>%
    ee$ImageCollection$reduce(ee$Reducer$mean())
}
avT <- years$map(ee_utils_pyfunc(yearly))%>%
  ee$ImageCollection$toBands()

avT <- avT$multiply(0.1)
avT <- avT$multiply(0.98)
avT = avT$resample('bilinear')$reproject(
  crs= crs,
  scale= res)

#3000/(1+exp(1.315-0.119*TempStack))
x <- avT$multiply(0.119)
num <- ee$Image(1.315)
x <- num$subtract(x)
x <-x$exp()
num <- ee$Number(1)
x <- x$add(num)
num <- ee$Image(3000)

avT_npp <-num$divide(x)


#Stack layers
SG <- avT_npp$addBands(pr_npp)
SG<-SG$updateMask(mask)

#Obtain points
points <- SG$sample(
  region=region,
  geometries=T,
  scale=res
)



## Export vector points
points_shp <-ee_as_sf(points
                      #,maxFeatures = maxNum
                      , via = "drive" 
)
write_sf(points_shp, paste0('INPUTS/NPP_TERRA/MAXIMUM_NPPP_NPPT_81_01_',ISO,'.shp'))

#Calculate NPP for 20 years
#Select minimum value from pr and temp columns for each point and year

data <- as.data.frame(vect(points_shp))

for (i in 0:19){
  col1 <-grep('pr', names(data), value =T)
  #col1 <-grep(paste0('X',i), col1, value =T)
  col2 <-grep('tm', names(data), value =T)
  #col2 <-grep(paste0('X',i), col2, value =T)
  
  
  points_shp[ , paste0("yr", i)]  <- do.call(pmin, c(data[, c(col1,col2)], na.rm = TRUE))*(1/100)*0.5
  print(i)
}

data <- points_shp[,42:62]
#Calculate mean NPP over 20 years
data$mean_npp <- rowMeans(as.data.frame(vect(data)))
write_sf(data[,'mean_npp'], paste0('INPUTS/NPP_TERRA/MAXIMUM_NPP_81_01_',ISO,'.shp'))


# MINIMUM NPP ----
#TerraClimate Precipitation
val ='pr'
image1 <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('1981-01-01', '2001-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)
# Function to Calculate a yearly composite
yearly <- function(y) {
  image1$filter(ee$Filter$calendarRange(y, y, "year")) %>%
    ee$ImageCollection$select(val)%>%
    ee$ImageCollection$reduce(ee$Reducer$sum())
}
image1 <- years$map(ee_utils_pyfunc(yearly))%>%
  ee$ImageCollection$toBands()


pr = image1$resample('bilinear')$reproject(
  crs= crs,
  scale= res)

pr <- pr$multiply(0.95)

#3000*(1-exp(-0.000664*dat$X10_pr_)
x <- pr$multiply(-0.000664)
x <-x$exp()
num <- ee$Image(1)
x <- num$subtract(x)
num <- ee$Number(3000)
pr_npp <- x$multiply(num)


# Average temperatue
val =c('tmmx', 'tmmn')

imagecol <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate('1981-01-01', '2001-01-01') %>%
  ee$ImageCollection$select(val)%>%
  ee$ImageCollection$filterBounds(region)

subtmxtmn <- function(image) {
  return(
    image$addBands(image$select('tmmx')$subtract(image$select('tmmn'))))
}

diff <- imagecol$map(subtmxtmn)

divide2 <- function(image) {
  return(image$addBands(image$divide(2)))
}

avT <- diff$map(divide2)



# Function to Calculate a yearly composite
yearly <- function(y) {
  avT$filter(ee$Filter$calendarRange(y, y, "year")) %>%
    ee$ImageCollection$select('tmmx')%>%
    ee$ImageCollection$reduce(ee$Reducer$mean())
}
avT <- years$map(ee_utils_pyfunc(yearly))%>%
  ee$ImageCollection$toBands()

avT <- avT$multiply(0.1)
avT <- avT$multiply(1.02)
avT = avT$resample('bilinear')$reproject(
  crs= crs,
  scale= res)

#3000/(1+exp(1.315-0.119*TempStack))
x <- avT$multiply(0.119)
num <- ee$Image(1.315)
x <- num$subtract(x)
x <-x$exp()
num <- ee$Number(1)
x <- x$add(num)
num <- ee$Image(3000)

avT_npp <-num$divide(x)


#Stack layers
SG <- avT_npp$addBands(pr_npp)
SG<-SG$updateMask(mask)

#Obtain points
points <- SG$sample(
  region=region,
  geometries=T,
  scale=res
)



## Export vector points
points_shp <-ee_as_sf(points
                      #,maxFeatures = maxNum
                      , via = "drive" 
)
write_sf(points_shp, paste0('INPUTS/NPP_TERRA/MINIMUM_NPPP_NPPT_81_01_',ISO,'.shp'))

#Calculate NPP for 20 years
#Select minimum value from pr and temp columns for each point and year

data <- as.data.frame(vect(points_shp))

for (i in 0:19){
  col1 <-grep('pr', names(data), value =T)
  #col1 <-grep(paste0('X',i), col1, value =T)
  col2 <-grep('tm', names(data), value =T)
  #col2 <-grep(paste0('X',i), col2, value =T)
  
  
  points_shp[ , paste0("yr", i)]  <- do.call(pmin, c(data[, c(col1,col2)], na.rm = TRUE))*(1/100)*0.5
  print(i)
}

data <- points_shp[,42:62]
#Calculate mean NPP over 20 years
data$mean_npp <- rowMeans(as.data.frame(vect(data)))
write_sf(data[,'mean_npp'], paste0('INPUTS/NPP_TERRA/MINIMUM_NPP_81_01_',ISO,'.shp'))


