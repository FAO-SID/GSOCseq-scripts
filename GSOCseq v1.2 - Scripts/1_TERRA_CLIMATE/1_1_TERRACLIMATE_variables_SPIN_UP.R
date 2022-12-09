############################################
#                                          #                                                                           
#                                          # 
#   GSOCseq: Script 1 Spin up              #
#   Climatic variables                     #  
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


# Load Packages 
library(terra)


# Open Terra Climate downloaded from Google Earth Engine

tmp<-rast("INPUTS/TERRA_CLIMATE/AverageTemperature_1981-2001.tif")

pre_81_00<-rast("INPUTS/TERRA_CLIMATE/Precipitation_1981-2001.tif")

pet_81_00<-rast("INPUTS/TERRA_CLIMATE/PET_1981-2001.tif")


# TEMPERATURE

# Get one month temperature ( January)

tmp_Jan_1<-tmp[[1]]

dim(tmp_Jan_1)

# Create empty list

Rlist<-list()

# Average of 20 years (j)  and 12 months (i) 

######for loop starts#######
for (i in 1:12) { 

var_sum<-tmp_Jan_1*0
k<-i

for (j in 1:20) {
print(k)
var_sum<-(var_sum + tmp[[k]])

k<-k+12
}

#Calculate each month average. 

var_avg<-var_sum/20

# Save the average of each month (i)

Rlist[[i]]<-var_avg
}
#######for loop ends########

#save a stack of months averages

Temp_Stack<-rast(Rlist)

Temp_Stack<-Temp_Stack*0.1 # scale to °C
writeRaster(Temp_Stack,filename='INPUTS/TERRA_CLIMATE/Temp_Stack_81-00_TC.tif',overwrite=TRUE)

#######################################################################################

#PRECIPITATION

# Get one month Precipitation ( January)

pre_Jan_1<-pre_81_00[[1]]

dim(pre_Jan_1)

# Create empty list

Rlist<-list()

# Average of 20 years (j)  and 12 months (i) 

######for loop starts#######
for (i in 1:12) { 

var_sum<-pre_Jan_1*0
k<-i

for (j in 1:20) {
print(k)
var_sum<-(var_sum + pre_81_00[[k]])
k<-k+12
}
#Save each month average. 

var_avg<-var_sum/20

Rlist[[i]]<-var_avg
}

######for loop ends#######

#save a stack of months averages

Prec_Stack<-rast(Rlist)
writeRaster(Prec_Stack,filename='INPUTS/TERRA_CLIMATE/Prec_Stack_81-00_TC.tif',overwrite=TRUE)

########################################################################

# POTENTIAL EVAPOTRANSPIRATION 


# Get one month PET ( January)

pet_Jan_1<-pet_81_00[[1]]

dim(pet_Jan_1)

# Create empty list

Rlist<-list()

# Average of 20 years (j)  and 12 months (i) 

######for loop starts#######
for (i in 1:12) { 

var_sum<-pet_Jan_1*0
k<-i

for (j in 1:20) {
print(k)
var_sum<-(var_sum + pet_81_00[[k]])

k<-k+12

}
#Save each month average. 

var_avg<-var_sum/20


Rlist[[i]]<-var_avg
}
######for loop ends#######

#save a stack of months averages

PET_Stack<-rast(Rlist)
PET_Stack<-PET_Stack*0.1
writeRaster(PET_Stack,filename='INPUTS/TERRA_CLIMATE/PET_Stack_81-00_TC.tif',overwrite=TRUE)



