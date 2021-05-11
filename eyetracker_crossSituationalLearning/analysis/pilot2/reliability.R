#---------------------------------------#
#-------- calibration plots ------------# 
#---------------------------------------#


library(tidyverse)

rm(list = ls())

# 
input <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/rawdata/pilot2/eyetracking/")
output <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/preProcessed data/pilot2/")
destinationPath <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/")


setwd(destinationPath)

#### load eyetracker calibration data ####
df <- list.files(paste0(input)) #folder where I story my eyetracking datafiles
df <- df[grepl("calibration",df)]

head(df)

data <- do.call(bind_rows,lapply(paste0(input,df),readxl::read_xlsx))
summary(data)

length(unique(data$participant_id))

columnsIwantTokeep<- spreadsheets[c('participant_id','type','mean_centroid_x_normalised','mean_centroid_y_normalised', 'display',
                                    'mean_centroid_x_normalised', 'mean_centroid_y_normalised','face_conf','label_position','obj_position','block','list')]

rowsIwantTokeep <- c("eye_tracking")