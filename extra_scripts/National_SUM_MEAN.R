#++++++++++++++++++++++++++++++++++++#
#                                    # 
# Calculate total SOC stocks         #
# and Mean SOC                       #
#                                    #
# Isabel Luotto 19/01/22             #
#                                    #
#++++++++++++++++++++++++++++++++++++#

# Empty the environment 
rm(list = ls())

# Load libraries
library(raster)


# User defined variables

## Working directory (wd)
wd <- "C:/Users/hp/Documents/FAO/GSOCseq/Joint Maps/NENA/EGY"

setwd(wd)

# Define product name (in this e.g. final SOC SSM1)
product = 'final_SOC_SSM1'

#Input raster layer of interest (final SOC, t0 or RSR)
x <- raster('EGY_GSOCseq_finalSOC_SSM1_Map030.tif')

#Input corresponding uncertainty layer
un <- raster('EGY_GSOCseq_SSM_UncertaintyMap030.tif')

#The uncertainty layers are in % (we need to divide by 100)
# and multiply by the corresponding layer 
x_un <- x+ x*(un/100)


####MEAN
#Calculate mean no area correction needed
mean <-cellStats(x, "mean")
mean_un <-cellStats(x_un, "mean") -mean             


#####SOC STOCKS 
#correct pixel area and ha to km2
x <- x*area(x)*100
x_un <- x_un*area(x_un)*100
# Sum over every pixel to get total sequestration potential in Mt C
sum <- cellStats(x, "sum")/1000000 
sum_un <-cellStats(x_un, "sum")/1000000  -sum 

stats <- data.frame(product =product,mean=mean, mun=mean_un ,sum=sum,
                    sun=sum_un)

stats



