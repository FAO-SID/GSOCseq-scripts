#++++++++++++++++++++++++++++++++++++#
#                                    # 
# Mitigation potential of soils      #
# based on FAOSTAT total             #
# Agricultural emissions             #
# Isabel Luotto 24/05/21             #
#                                    #
#++++++++++++++++++++++++++++++++++++#

# Empty the environment 
rm(list = ls())

# Load libraries
library(FAOSTAT)
library(countrycode)
library(raster)
library(data.table)
library(sp)

# User defined variables

## Working directory
wd <- "C:/Users/luottoi/Documents/GSOCseq/Argentina"

## GSOCseq output maps directory 
GSOCseq_folder <- "C:/Users/luottoi/Documents/GSOCseq/Argentina/"


## Create a folder called FAOSTAT in your working directory
## Define a path to the FAOSTAT folder
data_folder <- paste0(wd,"/FAOSTAT/")

## Country of interest (specified by the 3-digit ISO code)
ISO <- "ARG"

# Carbon ton to CO2 conversion
CO2conv <- 3.67

# Set the working directory
setwd(wd)

# Download the total agricultural emissions form FAOSTAT

#use the link below to download the data once and download it using the
#download_faostat_bulk function
#after downloading comment lines 45-46

#all_em <- "http://fenixservices.fao.org/faostat/static/bulkdownloads/Emissions_Agriculture_Agriculture_total_E_All_Data.zip"
#download_faostat_bulk(all_em, data_folder)
data <- read_faostat_bulk(paste0(data_folder,"Emissions_Agriculture_Agriculture_total_E_All_Data.zip")) 

#Extract the most recent year
data <- data[, c("area_code","area", "item","y2018","element", "unit")]

#Convert country codes to ISO codes
data$ISO <- countrycode(data$area_code, origin = 'fao', destination = 'iso3c')
data <- data[data$ISO == ISO & data$item == "Agriculture total" |data$ISO == ISO &  data$item == "Agricultural Soils",]

#Extract CO2 equivalent

data <-data[data$element ==  "Emissions (CO2eq)",]

# Extract total agricultural emissions for country of interest
data <- na.omit(data)

#Calculate total agricultural emissions in Gigagrams
totagrem <- sum(data$y2018)

#Load Output Maps: Relative Sequestration Rates (SSM-BAU)
SSM1 <- raster(paste0(GSOCseq_folder, ISO,"_GSOCseq_RSR_SSM1_Map030.tif"))
SSM2 <- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_RSR_SSM2_Map030.tif"))
SSM3 <- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_RSR_SSM3_Map030.tif"))

#Reproject the maps to Mollweide (this will take some time and output warning messages)
SSM1 <-projectRaster(SSM1, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")
SSM2 <-projectRaster(SSM2, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")
SSM3 <-projectRaster(SSM3, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")


#Sum over every pixel to get total sequestration potential 
totssm1 <- (sum(unique(SSM1[!is.na(SSM1)]))*CO2conv*100)/1000
totssm2 <- (sum(unique(SSM2[!is.na(SSM2)]))*CO2conv*100)/1000
totssm3 <- (sum(unique(SSM3[!is.na(SSM3)]))*CO2conv*100)/1000




#Estimate mitigation potential 
SSM1 <-(totssm1/totagrem) *100
SSM2 <-(totssm2/totagrem) *100
SSM3 <-(totssm3/totagrem) *100

#Crete table 
table <- data.frame(Total_Agricultural_Emissions =c("CO2eq gigagrams/year", totagrem, totagrem, totagrem),
                    Scenario = c("/", "SSM1", "SSM2", "SSM3"),
                    C_sequestered =c("CO2eq gigagrams/year",totssm1,totssm2, totssm3),
                    Mitigation_share =c("%",SSM1,SSM2,SSM3))
table
