#++++++++++++++++++++++++++++++++++++#
#                                    # 
# Country Report Results Section     #
# Basic Stats                        #
#                                    #
#++++++++++++++++++++++++++++++++++++#

# Empty the environment 
rm(list = ls())

# Load libraries
library(raster)
library(sp)
library(data.table)

# User defined variables

## Working directory (wd). It should end with a "/"
#wd <- "C:/Users/hp/Documents/FAO/GSOCseq/Joint Maps/NENA/"
#wd <- "C:/Users/luottoi/Documents/GSOCseq/"
wd <- 'C:/Users/hp/Documents/FAO/GSOCseq/'


## GSOCseq output maps directory. The paste0 function will combine
## your working directory wd with the folder name that contains
## your output maps
GSOCseq_folder <- paste0(wd,"Iran/OUTPUTS/4_MAPS/corr/")


## Country of interest (specified by the 3-digit ISO code)
ISO <- "IRN"


# Set the working directory
setwd(wd)

#Load land use map ESA (the same one used for the modeling exercise)

lu <- raster("C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_Results_data/LU/ESA_Land_Cover_12clases_FAO_World_2015_1km.tif")


# Load Output Maps: Relative Sequestration Rates (SSM-BAU)
# paste0 will combine the path to the outputs maps contained in
# GSOCseq_folder and the ISO variable 
# if you get an error message, make sure that paste0 is creating a correct
# path
SSM1 <- raster(paste0(GSOCseq_folder, ISO,"_GSOCseq_RSR_SSM1_Map030_Corr.tif"))
SSM2 <- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_RSR_SSM2_Map030_Corr.tif"))
SSM3 <- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_RSR_SSM3_Map030_Corr.tif"))

RSR <- stack(SSM1,SSM2,SSM3)

lu  <- crop(lu, SSM1)
lu <- mask(lu, SSM1)
#lu <- projectRaster(lu, SSM1, method= "ngb")

#Calculate mean
stats_mean <- as.data.frame(round(zonal(RSR,lu, fun ="mean",na.rm = TRUE), 3))
stats_mean <- stats_mean[complete.cases(stats_mean[2:4]),]
#Calculate area 
a <- area(RSR)

# Correct RSR values based on pixel area to calculate totals
RSR <- RSR * a*100

#Calculate total RSR 
stats_sum <- as.data.frame(round(zonal(RSR,lu, fun ="sum",na.rm = TRUE), 3))
stats_sum[, 2:4] <- round(stats_sum[, 2:4]/1000000,3)
stats_sum <- stats_sum[complete.cases(stats_sum[2:4]),]

#Calculate area per land use
stats_area <-  as.data.frame(round(zonal(a,lu, fun ="sum",na.rm = TRUE), 0))


#Merge results
RSR_stats <- merge(stats_area,stats_sum, by= "zone", all.x= TRUE)
RSR_stats <- merge(RSR_stats,stats_mean, by= "zone", all.x= TRUE)


cns <- c("Land use","Area_km2","SSM1_tot_Mt/yr","SSM2_tot_Mt/yr","SSM3_tot_Mt/yr","SSM1_mean_t/ha","SSM2_mean_t/ha",
         "SSM3_mean_t/ha")

colnames(RSR_stats) <- cns

# relevel land uses

#RSR_stats <- rbind(units, RSR_stats)
names(RSR_stats)[1] <- 'landuse'
#   0 = 0	  No Data
#	190 = 1 Artificial
#	10 11 30 40 = 2 Croplands
#	130 = 3 Grassland
#	50 60 61 62 70 71 72 80 81 82 90 100 110 = 4 Tree Covered
#	120 121 122= 5 Shrubs Covered
#	160 180 = 6 Herbaceous vegetation flooded
#	170 = 7 Mangroves
#	150 151 152 153= 8 Sparse Vegetation
#	200 201 202 = 9 Bare soil
#	220 = 10 Snow and Glaciers
#	210 = 11 Waterbodies
#	12  = 12 Treecrops
# 20 = 13 Paddy fields(rice/ flooded crops)

labels <- data.frame(v1=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13),
                     v2= c('No Data','Artificial','Croplands',
                           'Grassland','Tree Covered','Shrubland',
                           'Herbaceous vegetation flooded','Mangroves',
                           'Sparse vegetation','Bare soil',
                           'Snow and Glaciers','Waterbodies', 'Tree crops',
                           'Paddy fields'))

#Update based on present levels
RSR_stats$landuse <- as.factor(RSR_stats$landuse)

labels <- labels[labels$v1 %in% levels(RSR_stats$landuse), ]
RSR_stats <- as.data.table(RSR_stats)
RSR_stats[, landuse := as.character(factor(landuse, labels = labels$v2))]
RSR_stats <- as.data.frame(RSR_stats)

RSR_stats$landuse <- ifelse(RSR_stats$landuse == 'Croplands'|
                              RSR_stats$landuse == 'Tree crops'|
                              RSR_stats$landuse == 'Grassland'|
                              RSR_stats$landuse == 'Paddy fields',RSR_stats$landuse,
                            'Other')

RSR_stats <- aggregate(RSR_stats[, 2:8], by=list(RSR_stats$landuse), FUN=sum)
RSR_stats <- RSR_stats[complete.cases(RSR_stats[,6:8]),]


# Load Output Maps: Absolute Sequestration Rates (Scenarios (SSM & BAU)-T0)
# paste0 will combine the path to the outputs maps contained in
# GSOCseq_folder and the ISO variable 
# if you get an error message, make sure that paste0 is creating a correct
# path
SSM1 <- raster(paste0(GSOCseq_folder, ISO,"_GSOCseq_ASR_SSM1_Map030_Corr.tif"))
SSM2 <- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_ASR_SSM2_Map030_Corr.tif"))
SSM3 <- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_ASR_SSM3_Map030_Corr.tif"))
BAU <- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_ASR_BAU_Map030_Corr.tif"))

ASR <- stack(SSM1,SSM2,SSM3, BAU)


#Calculate mean
stats_mean <- as.data.frame(round(zonal(ASR,lu, fun ="mean",na.rm = TRUE), 2))
stats_mean <- stats_mean[complete.cases(stats_mean[2:5]),]
#Calculate area 
a <- area(ASR)

# Correct ASR values based on pixel area to calculate totals
ASR <- ASR * a*100

#Calculate total ASR 
stats_sum <- as.data.frame(round(zonal(ASR,lu, fun ="sum",na.rm = TRUE), 3))
stats_sum[, 2:5] <- round(stats_sum[, 2:5]/1000000,3)


#Calculate area per land use
stats_area <-  as.data.frame(round(zonal(a,lu, fun ="sum",na.rm = TRUE), 0))


#Merge results
ASR_stats <- merge(stats_area,stats_sum, by= "zone", all.x= TRUE)
ASR_stats <- merge(ASR_stats,stats_mean, by= "zone", all.x= TRUE)


cns <- c("Land use","Area_km2","SSM1_tot_Mt/yr","SSM2_tot_Mt/yr","SSM3_tot_Mt/yr","BAU_tot_Mt/yr","SSM1_mean_t/ha","SSM2_mean_t/ha","SSM3_mean_t/ha", "BAU_mean_t/ha")
colnames(ASR_stats) <- cns

# relevel land uses
names(ASR_stats)[1] <- 'landuse'
ASR_stats$landuse <- as.factor(ASR_stats$landuse)
ASR_stats <- as.data.table(ASR_stats)
ASR_stats[, landuse := as.character(factor(landuse, labels = labels$v2))]





ASR_stats <- as.data.frame(ASR_stats)

ASR_stats$landuse <- ifelse(ASR_stats$landuse == 'Croplands'|
                              ASR_stats$landuse == 'Tree crops'|
                              ASR_stats$landuse == 'Grassland'|
                              ASR_stats$landuse == 'Paddy fields',ASR_stats$landuse,
                            'Other')
ASR_stats <- aggregate(ASR_stats[, 2:10], by=list(ASR_stats$landuse), FUN=sum)

ASR_stats <- ASR_stats[complete.cases(ASR_stats[,6:10]),]

# Keep only Croplands, Grasslands and Tree crops and 'Others
names(RSR_stats)[1] <- 'Land use'
names(ASR_stats)[1] <- 'Land use'





#Compare final output tables


RSR_stats
ASR_stats
