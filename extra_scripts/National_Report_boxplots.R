#++++++++++++++++++++++++++++++++++++#
#                                    # 
# Country Report Results Section     #
# Boxplots                           #
#                                    #
#++++++++++++++++++++++++++++++++++++#

# Empty the environment 
rm(list = ls())

# Load libraries
library(raster)
library(sp)
library(ggplot2)
library(data.table)


# User defined variables

## Working directory (wd). It should end with a "/"
wd <- "C:/Users/hp/Documents/FAO/GSOCseq/Joint Maps/NENA/"
#wd <- "C:/Users/luottoi/Documents/GSOCseq/"


## GSOCseq output maps directory. The paste0 function will combine
## your working directory wd with the folder name that contains
## your output maps
GSOCseq_folder <- paste0(wd,"SDN/")


## Country of interest (specified by the 3-digit ISO code)
ISO <- "SDN"


# Set the working directory
setwd(wd)

#Load land use map ESA (the same one used for the modeling exercise)

lu <- raster("C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_Results_data/LU/ESA_Land_Cover_12clases_FAO_World_2015_1km.tif")



#################################
#                               #
#Relative Sequestration Rates   #
#                               #
#################################

# Load Output Maps: Relative Sequestration Rates (SSM-BAU)
# paste0 will combine the path to the outputs maps contained in
# GSOCseq_folder and the ISO variable 
# if you get an error message, make sure that paste0 is creating a correct
# path
RSR1 <- raster(paste0(GSOCseq_folder, ISO,"_GSOCseq_RSR_SSM1_Map030.tif"))
RSR2<- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_RSR_SSM2_Map030.tif"))
RSR3 <- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_RSR_SSM3_Map030.tif"))
lu  <- crop(lu, RSR1)
lu <- mask(lu, RSR1)

# Combine all layers into a brick and turn into data.frame
data <- brick(RSR1,RSR2,RSR3,lu)
data <- as.data.frame(data)

#Change the column names, makes ure they follow the same order of how 
# you bricked them
cn <- c("SSM1", "SSM2", "SSM3", "Land use")
colnames(data) <-cn

#Re-level land use factor levels
data$`Land use` <-as.factor(data$`Land use`)
levels(data$`Land use`) <-c( "Croplands", "Grasslands", "Shrublands", "Tree Crops", "Croplands")

#Remove NAs and round results
data <- data[complete.cases(data),]
data[,1:3] <- round(data[,1:3],2)

#transform data frame from wide to long, use data.table for more efficient data manipulation
data <- setDT(data)

data = melt(data, id.vars = c("Land use"),
             measure.vars = c("SSM1", "SSM2", "SSM3"))
colnames(data)[2] <- "Scenario"
colnames(data)[3] <- "SR"

#################################
#                               #
#Absolute Sequestration Rates   #
#                               #
#################################

# Load Output Maps: Absolute Sequestration Rates (Scenarios - T0)
# paste0 will combine the path to the outputs maps contained in
# GSOCseq_folder and the ISO variable 
# if you get an error message, make sure that paste0 is creating a correct
# path
ASR1 <- raster(paste0(GSOCseq_folder, ISO,"_GSOCseq_ASR_SSM1_Map030.tif"))
ASR2<- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_ASR_SSM2_Map030.tif"))
ASR3 <- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_ASR_SSM3_Map030.tif"))
ASRBAU <- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_ASR_BAU_Map030.tif"))

# Combine all layers into a brick and turn into data.frame
data_a <- brick(ASR1,ASR2,ASR3,ASRBAU,lu)
data_a <- as.data.frame(data_a)

#Change the column names, makes ure they follow the same order of how 
# you bricked them
cn <- c("SSM1", "SSM2", "SSM3","BAU", "Land use")
colnames(data_a) <-cn

#Re-level land use factor levels
data_a$`Land use` <-as.factor(data_a$`Land use`)
levels(data_a$`Land use`) <-c( "Croplands", "Grasslands", "Shrublands", "Tree Crops", "Croplands")

#Remove NAs and round results
data_a <- data_a[complete.cases(data_a),]
data_a[,1:4] <- round(data_a[,1:4],2)

#wide to long, use data.table for more efficient data manipulation
data_a <- setDT(data_a)

data_a = melt(data_a, id.vars = c("Land use"),
            measure.vars = c("SSM1", "SSM2", "SSM3", "BAU"))
colnames(data_a)[2] <- "Scenario"
colnames(data_a)[3] <- "SR"


#Combine the two data frames to one
data$measure <- "RSR"
data_a$measure <- "ASR"
data <- rbind(data, data_a)

# Remove not needed objects
rm(data_a, ASR1,
           ASR2,ASR3,ASRBAU, RSR1, RSR2,RSR3)

#################
#               #
# Figure 1      #
# Boxplots       #
#               #
#################

# grouped boxplot
p <-ggplot(data, aes(x=data$`Land use`, y=SR, fill=Scenario)) + 
  stat_boxplot(geom= 'errorbar' , width = 0.3, position = 
                 position_dodge(width = 0.75) )+
  geom_boxplot(outlier.shape = NA)+
  facet_grid(~measure, labeller = labeller(measure = 
                                         c("ASR" = "Absolute Sequestration Rates",
                                           "RSR" = "Relative Sequestration Rates")))+
  ylab("[t/ha yr]") +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        legend.position="bottom")
  
p  




