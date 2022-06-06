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
RSR1 <- raster(paste0(GSOCseq_folder, ISO,"_GSOCseq_RSR_SSM1_Map030_Corr.tif"))
RSR2<- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_RSR_SSM2_Map030_Corr.tif"))
RSR3 <- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_RSR_SSM3_Map030_Corr.tif"))
lu  <- crop(lu, RSR1)
lu <- mask(lu, RSR1)

# Combine all layers into a brick and turn into data.frame
data <- brick(RSR1,RSR2,RSR3,lu)
data <- as.data.frame(data)

#Change the column names, makes ure they follow the same order of how 
# you bricked them
cn <- c("SSM1", "SSM2", "SSM3", "landuse")
colnames(data) <-cn

labels <- data.frame(v1=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13),
                     v2= c('No Data','Artificial','Croplands',
                           'Grassland','Tree Covered','Shrubland',
                           'Herbaceous vegetation flooded','Mangroves',
                           'Sparse vegetation','Bare soil',
                           'Snow and Glaciers','Waterbodies', 'Tree crops',
                           'Paddy fields'))

#Update based on present levels
data$landuse <- as.factor(data$landuse)

labels <- labels[labels$v1 %in% levels(data$landuse), ]
data <- as.data.table(data)
data[, landuse := as.character(factor(landuse, labels = labels$v2))]
data <- as.data.frame(data)
data$landuse <- ifelse(data$landuse == 'Croplands'|
                         data$landuse == 'Tree crops'|
                         data$landuse == 'Grassland'|
                         data$landuse == 'Paddy fields',data$landuse,
                         'Other')

#Remove NAs and round results
data <- data[complete.cases(data),]
data[,1:3] <- round(data[,1:3],2)

#transform data frame from wide to long, use data.table for more efficient data manipulation
data <- setDT(data)

data = melt(data, id.vars = c("landuse"),
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
ASR1 <- raster(paste0(GSOCseq_folder, ISO,"_GSOCseq_ASR_SSM1_Map030_Corr.tif"))
ASR2<- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_ASR_SSM2_Map030_Corr.tif"))
ASR3 <- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_ASR_SSM3_Map030_Corr.tif"))
ASRBAU <- raster(paste0(GSOCseq_folder,ISO, "_GSOCseq_ASR_BAU_Map030_Corr.tif"))

# Combine all layers into a brick and turn into data.frame
data_a <- brick(ASR1,ASR2,ASR3,ASRBAU,lu)
data_a <- as.data.frame(data_a)

#Change the column names, makes sure they follow the same order of how 
# you bricked them
cn <- c("SSM1", "SSM2", "SSM3","BAU", "landuse")
colnames(data_a) <-cn

#Re-level land use factor levels
data_a$landuse <-as.factor(data_a$landuse)

data_a <- as.data.table(data_a)
data_a[, landuse := as.character(factor(landuse, labels = labels$v2))]
data_a$landuse <- ifelse(data_a$landuse == 'Croplands'|
                           data_a$landuse == 'Tree crops'|
                           data_a$landuse == 'Grassland'|
                           data_a$landuse == 'Paddy fields',data_a$landuse,
                            'Other')
data <- as.data.frame(data)
#Remove NAs and round results
data_a <- data_a[complete.cases(data_a),]
data_a[,1:4] <- round(data_a[,1:4],2)

#wide to long, use data.table for more efficient data manipulation
data_a <- setDT(data_a)

data_a = melt(data_a, id.vars = c("landuse"),
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
x <- boxplot(data$SR, plot=F)
# Get min and max (like this outliers will not be displayed)
min <-  x[["stats"]][1]
max <-  x[["stats"]][5]



p <-ggplot(data, aes(x=landuse, y=SR, fill=Scenario)) + 
  stat_boxplot(geom= 'errorbar' , width = 0.3, position = 
                 position_dodge(width = 0.75) )+
  geom_boxplot(outlier.shape = NA)+
  scale_y_continuous(limits = c(min, max))+
  facet_grid(~measure, labeller = labeller(measure = 
                                         c("ASR" = "Absolute Sequestration Rates",
                                           "RSR" = "Relative Sequestration Rates")))+
  ylab("[t /ha yr]") +
  theme_bw()+
  theme(axis.title.x=element_blank(),
        legend.position="bottom")
  
p  




