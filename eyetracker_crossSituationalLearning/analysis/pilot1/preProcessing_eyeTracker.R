rm(list = ls())
# 
input <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/rawdata/eyetracking/")
output <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/preProcessed data/")
destinationPath <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/")

#### load eyetracker data ####
df <- list.files(paste0(input)) #folder where I story my eyetracking datafiles
df <- df[grepl("datacollection",df)]

data <- NULL
for (i in 1:length(df)){
  gsub(".xlsx$", "", df[i]) -> id
  readxl::read_xlsx(paste(input, df[i], sep = ""))-> temp
  data <- rbind(temp,data)
};
rm(temp)

summary(data)

# load Gorilla spreadsheet
spreadsheet <-readxl::read_xlsx(paste0(destinationPath,"stimuli/gorillaSpreadsheets/spreadsheet_12Apr.xlsx")) #spreadsheet that I loaded on Gorilla with the list of the trials

# the eyetracking files and the spreadsheet loaded on Gorilla are meant to be linked by the column "spreadsheet_row"
# basically the eyetracking files point to the row of the spreadsheet
# Since we would like to be able to trace back what we're presenting, then we replace in the eyetracking masterfile (data) the following columns
# with the values listed in the spreadsheet file

data$target <- gsub(".jpg$", "", spreadsheet[(data$spreadsheet_row),]$ANSWER)
data$label <- gsub(".jpg$", "", spreadsheet[(data$spreadsheet_row),]$label)
data$frequency <- gsub(".jpg$", "", spreadsheet[(data$spreadsheet_row),]$frequency)
data$learning <- gsub(".jpg$", "", spreadsheet[(data$spreadsheet_row),]$learning)
data$A <- gsub(".jpg$", "", spreadsheet[(data$spreadsheet_row),]$A)
data$B <- gsub(".jpg$", "", spreadsheet[(data$spreadsheet_row),]$B)
data$C <- gsub(".jpg$", "", spreadsheet[(data$spreadsheet_row),]$C)
data$D <- gsub(".jpg$", "", spreadsheet[(data$spreadsheet_row),]$D)
data$E <- gsub(".jpg$", "", spreadsheet[(data$spreadsheet_row),]$E)

data[sapply(data, is.character)] <-  #this converts to factor everything that has been listed "as.character"
  lapply(data[sapply(data, is.character)], as.factor)

#check that you can make sense of all columns just by looking at the summary
summary(data) #our masterfile with all the eyetracking data

rm(spreadsheet) #we don't need anymore this file

#select relevant rows from pilot
data_minimal <- 
  data %>%
  filter(type == "prediction") %>%
  select(participant_id, filename, spreadsheet_row, time_elapsed, type, 
         screen_index, x_pred_normalised, y_pred_normalised, face_conf, 
         target, label, frequency)%>%
  rename(subjID = participant_id, 
         task = filename,
         time = time_elapsed,
         trial = spreadsheet_row,
         x = x_pred_normalised,
         y = y_pred_normalised)

data_minimal <- droplevels(data_minimal)
data_minimal$subjID <- as.factor(data_minimal$subjID)

data_minimal$screen_index <- plyr::revalue(as.factor(data_minimal$screen_index), c("1" = "feature", "3" = "label"))

summary(data_minimal)

#extract zone dimensions -- we need to know where we have presented our images
# in order to do so, we extract the info about the zone areas

zones <- data[grepl("Zone|tobLabel|wugLabel", data$zone_name),] #here we extract the zone infos

# -------------------------- LABEL SCREEN ---------------------------#
# In this screen we have presented only the label 
# Tob position
orig_x_tob <- zones[zones$zone_name=="tobLabel",]$zone_x_normalised[1]
orig_y_tob <- zones[zones$zone_name=="tobLabel",]$zone_y_normalised[1]
width_tob <- zones[zones$zone_name=="tobLabel",]$zone_width_normalised[1]
height_tob <- zones[zones$zone_name=="tobLabel",]$zone_height_normalised[1]

x1_tob<-orig_x_tob 
x2_tob<-orig_x_tob + (width_tob)
y1_tob<-orig_y_tob
y2_tob<-orig_y_tob + (height_tob)

# Wug position
orig_x_wug <- zones[zones$zone_name=="wugLabel",]$zone_x_normalised[1]
orig_y_wug <- zones[zones$zone_name=="wugLabel",]$zone_y_normalised[1]
width_wug <- zones[zones$zone_name=="wugLabel" ,]$zone_width_normalised[1]
height_wug <- zones[zones$zone_name=="wugLabel",]$zone_height_normalised[1]

x1_wug<-orig_x_wug 
x2_wug<-orig_x_wug + (width_wug)
y1_wug<-orig_y_wug
y2_wug<-orig_y_wug + (height_wug)

# -------------------------- FEATURE SCREEN ---------------------------#
# This screen is where we presented the objects arranged in a grid (5 pictures)

# Upper left part of the monitor (A):
orig_x_A <- zones[zones$zone_name=="Zone2",]$zone_x_normalised[1]
orig_y_A <- zones[zones$zone_name=="Zone2",]$zone_y_normalised[1]
width_A <- zones[zones$zone_name=="Zone2",]$zone_width_normalised[1]
height_A <- zones[zones$zone_name=="Zone2",]$zone_height_normalised[1]

x1_A<-orig_x_A 
x2_A<-orig_x_A + (width_A)
y1_A<-orig_y_A
y2_A<-orig_y_A + (height_A)

# Upper right part of the monitor (B):
orig_x_B <- zones[zones$zone_name=="Zone3",]$zone_x_normalised[1]
orig_y_B <- zones[zones$zone_name=="Zone3",]$zone_y_normalised[1]
width_B <- zones[zones$zone_name=="Zone3",]$zone_width_normalised[1]
height_B <- zones[zones$zone_name=="Zone3",]$zone_height_normalised[1]

x1_B<-orig_x_B 
x2_B<-orig_x_B + (width_B)
y1_B<-orig_y_B
y2_B<-orig_y_B + (height_B)

# Center (C):
orig_x_C <- zones[zones$zone_name=="Zone1" & zones$screen_index==1,]$zone_x_normalised[1]
orig_y_C <- zones[zones$zone_name=="Zone1" & zones$screen_index==1,]$zone_y_normalised[1]
width_C <- zones[zones$zone_name=="Zone1" & zones$screen_index==1,]$zone_width_normalised[1]
height_C <- zones[zones$zone_name=="Zone1" & zones$screen_index==1,]$zone_height_normalised[1]

x1_C<-orig_x_C 
x2_C<-orig_x_C + (width_C)
y1_C<-orig_y_C
y2_C<-orig_y_C + (height_C)

# Bottom left (D):
orig_x_D <- zones[zones$zone_name=="Zone4",]$zone_x_normalised[1]
orig_y_D <- zones[zones$zone_name=="Zone4",]$zone_y_normalised[1]
width_D <- zones[zones$zone_name=="Zone4",]$zone_width_normalised[1]
height_D <- zones[zones$zone_name=="Zone4",]$zone_height_normalised[1]

x1_D<-orig_x_D 
x2_D<-orig_x_D + (width_D)
y1_D<-orig_y_D
y2_D<-orig_y_D + (height_D)

# Bottom right (E):
orig_x_E <- zones[zones$zone_name=="Zone5",]$zone_x_normalised[1]
orig_y_E <- zones[zones$zone_name=="Zone5",]$zone_y_normalised[1]
width_E <- zones[zones$zone_name=="Zone5",]$zone_width_normalised[1]
height_E <- zones[zones$zone_name=="Zone5",]$zone_height_normalised[1]

x1_E<-orig_x_E 
x2_E<-orig_x_E + (width_E)
y1_E<-orig_y_E
y2_E<-orig_y_E + (height_E)

# put all these variables in a dataframe for simplicity
ROIs <- data.frame(
  x1 = c(x1_A, x1_B, x1_C, x1_D, x1_E, x1_tob, x1_wug),
  x2 = c(x2_A, x2_B, x2_C, x2_D, x2_E, x2_tob, x2_wug),
  y1 = c(y1_A, y1_B, y1_C, y1_D, y1_E, y1_tob, y1_wug),
  y2 = c(y2_A, y2_B, y2_C, y2_D, y2_E, y2_tob, y2_wug),
  ROI = c("A","B","C","D","E","tob","wug")
)
ROIs

write.csv(data_minimal, paste0(output, "eyeTracker.csv"), row.names = F, quote = F)
write.csv(ROIs, paste0(output, "ROIs.csv"), row.names = F, quote = F)

data_minimal -> eyeTracker

for (i in 1:nrow(eyeTracker)){
  for (b in ROIs$ROI){
    print(paste0("row left: ", nrow(eyeTracker)-i,"/ROI: ",b))
    
    # define the area of the ROI
    round(ROIs[ROIs$ROI==b,]$x1, 2) -> Xmin
    round(ROIs[ROIs$ROI==b,]$x2, 2) -> Xmax
    round(ROIs[ROIs$ROI==b,]$y1, 2) -> Ymin
    round(ROIs[ROIs$ROI==b,]$y2, 2) -> Ymax
    
    # p is your point, p.x is the x coord, p.y is the y coord
    eyeTracker[i,]$x -> p.x
    eyeTracker[i,]$y -> p.y
    
    #if not within the area, give 0, otherwise 1
    eyeTracker[i,b] <- ifelse(p.x < Xmin || p.x > Xmax || p.y < Ymin || p.y > Ymax, 0,1) 
    
  }
}

summary(eyeTracker)


#save file
write.csv(eyeTracker, paste0(output,"eyeTracker_intersections.csv"), row.names = F, quote = F)

####-------------------- proportions of fixations across trials or BLOCKS by time -------------------------####
# load what we have done so far:
read.csv(paste0(output,"eyeTracker_intersections.csv"), stringsAsFactors = T) -> eyeTracker


ROI_A <- aggregate(A ~  subjID + label + frequency + screen_index + time, eyeTracker, mean) 
ROI_B <- aggregate(B ~  subjID + label + frequency + screen_index + time, eyeTracker, mean) 
ROI_C <- aggregate(C ~  subjID + label + frequency + screen_index + time, eyeTracker, mean) 
ROI_D <- aggregate(D ~  subjID + label + frequency + screen_index + time, eyeTracker, mean) 
ROI_E <- aggregate(E ~  subjID + label + frequency + screen_index + time, eyeTracker, mean) 
ROI_tob <- aggregate(tob ~  subjID + label + frequency + screen_index + time, eyeTracker, mean) 
ROI_wug <- aggregate(wug ~  subjID + label + frequency + screen_index + time, eyeTracker, mean) 

temp_AB <- merge(ROI_A,ROI_B, by=c("subjID","label","frequency","screen_index","time"))
temp_ABC <- merge(temp_AB,ROI_C, by=c("subjID","label","frequency","screen_index","time"))
temp_ABCD <- merge(temp_ABC,ROI_D, by=c("subjID","label","frequency","screen_index","time"))
temp_ABCDE <- merge(temp_ABCD,ROI_E, by=c("subjID","label","frequency","screen_index","time"))
temp_ABCDEtob <- merge(temp_ABCDE,ROI_tob, by=c("subjID","label","frequency","screen_index","time"))
temp_ABCDEwug <- merge(temp_ABCDEtob,ROI_wug, by=c("subjID","label","frequency","screen_index","time"))

prop_long <- gather(temp_ABCDEwug, ROI, prop, A:wug, factor_key=TRUE)
prop_long$ROI <- as.factor(prop_long$ROI)
summary(prop_long)

#save proportion of looks
write.csv(prop_long, paste0(input, "eyeTracker_proportions_longFormat.csv"), row.names = F, quote = F)

