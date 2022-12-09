############################################
#                                          #                                                                           
#                                          # 
#   GSOCseq: Script 8 Warm up stack        #
#                                          #
#                                          #  
#   Isabel Luotto                          #
#   Contact: Isabel.Luotto@fao.org         #
#                                          # 
#                                          # 
############################################

#Initial setup -----------------------------------------------------------------
rm(list=ls())
gc()

# Working directory
#wd <- 'C:/Users/luottoi/Documents/GSOCseq'
wd <- 'C:/Users/hp/Documents/FAO/GSOCseq/OCB/GSOCseq_TM/'
setwd(wd)

# 3-digit Country ISO-code
ISO <-'THA_NP'
# Load Packages 
library(terra)


path_AOI<-("INPUTS/AOI_POLYGON/NP.shp")

path_SOC<-("INPUTS/SOC_MAP/THA_NP_SOC.tif")

path_CLAY<-("INPUTS/CLAY/THA_NP_Clay_WA.tif")

path_LU<-("INPUTS/LAND_USE/THA_NP_ESA_Land_Cover_13clases_FAO_Stack.tif")

path_COV<-("INPUTS/COV/THA_NP_Cov_stack.tif")


# Set the number of years of the warm up
nWUP<-20



# Open the shapefile of the region/country
AOI<-vect(path_AOI)

#Open SOC MAP FAO
SOC_MAP_AOI<-rast(path_SOC)

# Open Clay layer

Clay_WA_AOI<-rast(path_CLAY)


# Open Vegetation Cover layer 

Cov_AOI<-rast(path_COV)
# OPen Land Use layer (ESA)

## 0	No Data
# 1 Artificial
# 2 Croplands
# 3 Grassland
# 4 Tree Covered
# 5 Shrubs Covered
# 6 Herbaceous vegetation flooded
# 7 Mangroves
# 8 Sparse Vegetation
# 9 Baresoil
# 10 Snow and Glaciers
# 11 Waterbodies
# 12 TreeCrops
# 13 Paddy fields
LU_Stack<-rast(path_LU)

# Open Land Use Stack , One Land use layer for each year (in this example we have 2016-2020, we are missing 15 layers)
# We will replicate the oldest layer times 15 (years of simulation - number of layers)
ry <- nWUP - dim(LU_Stack)[3]

LU_Stack <-c(rast(replicate(ry,LU_Stack[[1]])),LU_Stack)

# Convert LU layer  to DR layer (ESA land use , 13 classes)

#DPM/RPM (decomplosable vs resistant plant material)
#(1) Most agricultural crops and improved grassland or tree crops 1.44 
#(2) Unimproved grassland and schrub 0.67
#(3) Deciduous and tropical woodland 0.25    

DR_Stack<-(LU_Stack==2 | LU_Stack==12 | LU_Stack==13)*1.44+ (LU_Stack==4)*0.25 + (LU_Stack==3 | LU_Stack==5 | LU_Stack==6 | LU_Stack==8)*0.67


# STACK all layers
Stack_Set_AOI<-c(SOC_MAP_AOI,Clay_WA_AOI,Cov_AOI,LU_Stack,DR_Stack)

writeRaster(Stack_Set_AOI,filename=(paste0("INPUTS/STACK/",ISO,"_Stack_Set_Warm_UP.tif")),overwrite=TRUE)

