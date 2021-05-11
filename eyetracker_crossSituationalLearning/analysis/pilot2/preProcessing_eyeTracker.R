library(tidyverse)

rm(list = ls())

# 
input <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/rawdata/pilot2/eyetracking/")
output <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/preProcessed data/pilot2/")
destinationPath <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/")

spreadsheet_input <- "C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/rawdata/pilot2/"
gorilla_spreadsheets <- "C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/stimuli/gorillaSpreadsheets/pilot2/"

setwd(destinationPath)

#### load eyetracker data ####
df <- list.files(paste0(input)) #folder where I story my eyetracking datafiles
df <- df[grepl("collection",df)]
df <- df[grepl("47084-15-",df)] #pilot data 2nd round (+10 ppts)

data <- do.call(bind_rows,lapply(paste0(input,df),readxl::read_xlsx))
summary(data)

length(unique(data$participant_id))

#### load learning spreadsheets ####
df <- list.files(paste0(spreadsheet_input)) #folder where I store my eyetracking datafiles
gorillaNames <-c("a26p","8mjf","ixbq","lrxf","cb7z","kkmk","qcub","radc","a1uo","hflw")
df <- df[grep(paste(gorillaNames,collapse="|"), df)]
df <- df[grep("v15", df)]

spreadsheets <- do.call(rbind,lapply(paste0(spreadsheet_input,df),read.csv, stringsAsFactor=T))

columnsIwantTokeep<- spreadsheets[c('Task.Name','Participant.Private.ID','Screen.Number','Zone.Type', 'display',
                            'Spreadsheet.Row', 'label','frequency','label_position','obj_position','block','list')]

rowsIwantTokeep <- c("eye_tracking")
displays<- c("FL - category 1","FL - category 2","FL - category 3")

spreadsheets_clean <- columnsIwantTokeep %>% 
  filter(Zone.Type == rowsIwantTokeep & display %in% displays) %>%
  rename(subjID = Participant.Private.ID, 
         task = Task.Name,
         screen = Screen.Number,
         spreadsheetRow = Spreadsheet.Row,
         labelPresented = label)

unique(spreadsheets_clean) -> spreadsheets_clean
spreadsheets_clean[sapply(spreadsheets_clean, is.character)] <- 
  lapply(spreadsheets_clean[sapply(spreadsheets_clean, is.character)], as.factor)

summary(spreadsheets_clean[,c("subjID","list","display")])

temp<-spreadsheets_clean%>%
  group_by(subjID,list)%>%
  tally()

lists_subjs <- temp[,1:2]; rm(df,temp, columnsIwantTokeep, spreadsheets_clean,displays,rowsIwantTokeep, spreadsheet_input);



#### load gorilla spreadsheets ####
df <- list.files(paste0(gorilla_spreadsheets)) #folder where I story my eyetracking datafiles
gorillaNames <-c("learning")
df <- df[grep(gorillaNames, df)]

spreadsheets <- do.call(rbind,lapply(paste0(gorilla_spreadsheets,df),read.csv, stringsAsFactor=T))

merge(spreadsheets,lists_subjs, by = "list") -> spreadsheets

length(unique(spreadsheets$subjID))
# the eyetracking files and the spreadsheet loaded on Gorilla are meant to be linked by the column "spreadsheet_row"
# basically the eyetracking files point to the row of the spreadsheet
# Since we would like to be able to trace back what we're presenting, then we replace in the eyetracking masterfile (data) the following columns
# with the values listed in the spreadsheet file


data$target <- ""; data$label <- ""; data$frequency <- ""; data$learning <- "";
data$A <- ""; data$B <- ""; data$C <- ""; data$D <- ""; data$E <- "";
data$display <- "";data$block <- 0;data$list <- 0;data$label_position <- "";
data$obj_position <- "";

for (x in unique(spreadsheets$list)){
  print(x)
  subset(spreadsheets, list == x) -> listNumber

  data[data$participant_id %in% unique(listNumber$subjID),]$target <- gsub(".jpg$", "", listNumber[(data[data$participant_id %in% unique(listNumber$subjID),]$spreadsheet_row),]$ANSWER)
  data[data$participant_id %in% unique(listNumber$subjID),]$label <- gsub(".jpg$", "", listNumber[(data[data$participant_id %in% unique(listNumber$subjID),]$spreadsheet_row),]$label)
  data[data$participant_id %in% unique(listNumber$subjID),]$frequency <- gsub(".jpg$", "", listNumber[(data[data$participant_id %in% unique(listNumber$subjID),]$spreadsheet_row),]$frequency)
  data[data$participant_id %in% unique(listNumber$subjID),]$learning <- gsub(".jpg$", "", listNumber[(data[data$participant_id %in% unique(listNumber$subjID),]$spreadsheet_row),]$learning)
  data[data$participant_id %in% unique(listNumber$subjID),]$A <- gsub(".jpg$", "", listNumber[(data[data$participant_id %in% unique(listNumber$subjID),]$spreadsheet_row),]$A)
  data[data$participant_id %in% unique(listNumber$subjID),]$B <- gsub(".jpg$", "", listNumber[(data[data$participant_id %in% unique(listNumber$subjID),]$spreadsheet_row),]$B)
  data[data$participant_id %in% unique(listNumber$subjID),]$C <- gsub(".jpg$", "", listNumber[(data[data$participant_id %in% unique(listNumber$subjID),]$spreadsheet_row),]$C)
  data[data$participant_id %in% unique(listNumber$subjID),]$D <- gsub(".jpg$", "", listNumber[(data[data$participant_id %in% unique(listNumber$subjID),]$spreadsheet_row),]$D)
  data[data$participant_id %in% unique(listNumber$subjID),]$E <- gsub(".jpg$", "", listNumber[(data[data$participant_id %in% unique(listNumber$subjID),]$spreadsheet_row),]$E)
  data[data$participant_id %in% unique(listNumber$subjID),]$block <- listNumber[(data[data$participant_id %in% unique(listNumber$subjID),]$spreadsheet_row),]$block
  data[data$participant_id %in% unique(listNumber$subjID),]$display <- gsub("FL - ", "", listNumber[(data[data$participant_id %in% unique(listNumber$subjID),]$spreadsheet_row),]$display)
  data[data$participant_id %in% unique(listNumber$subjID),]$list <- listNumber[(data[data$participant_id %in% unique(listNumber$subjID),]$spreadsheet_row),]$list
  data[data$participant_id %in% unique(listNumber$subjID),]$obj_position <- listNumber[(data[data$participant_id %in% unique(listNumber$subjID),]$spreadsheet_row),]$obj_position
  data[data$participant_id %in% unique(listNumber$subjID),]$label_position <- listNumber[(data[data$participant_id %in% unique(listNumber$subjID),]$spreadsheet_row),]$label_position
  
}

data[sapply(data, is.character)] <-  #this converts to factor everything that has been listed "as.character"
  lapply(data[sapply(data, is.character)], as.factor)

summary(data[,c("participant_id","list","label_position")])

#check that you can make sense of all columns just by looking at the summary
summary(data) #our masterfile with all the eyetracking data

rm(spreadsheets,listNumber,lists_subjs) #we don't need these files anymore

#select relevant rows from pilot
data_minimal <- 
  data %>%
  filter(type == "prediction" & display %in% c("category 1","category 2","category 3")) %>% #& !(display %in% c("category 1","category 2","category 3","finish","instructions","recalibration","calibration"))
  select(participant_id, filename, spreadsheet_row, display, face_conf, time_elapsed, type, 
         screen_index, x_pred_normalised, y_pred_normalised, 
         target, label, frequency,list,block,display,obj_position,label_position)%>%
  rename(subjID = participant_id, 
         task = filename,
         time = time_elapsed,
         trial = spreadsheet_row,
         x = x_pred_normalised,
         y = y_pred_normalised)

data_minimal <- droplevels(data_minimal)
data_minimal$subjID <- as.factor(data_minimal$subjID)

data_minimal$screen_index <- plyr::revalue(as.factor(data_minimal$screen_index), c("2" = "feature", "3" = "ISI_preLabel", "4" = "label", "5" = "ISI_postLabel"))
data_minimal$label_position <- plyr::revalue(as.factor(data_minimal$label_position), c("lLabel" = "left", "rLabel" = "right"))
data_minimal$obj_position <- plyr::revalue(as.factor(data_minimal$obj_position), c("fixed" = "control"))

summary(data_minimal)


data_minimal%>%
  group_by(list,subjID,label,label_position, screen_index, obj_position)%>%
  count()%>%
  pivot_wider(names_from = label, values_from = n)%>%
  print(n=Inf)

#extract zone dimensions -- we need to know where we have presented our images
# in order to do so, we extract the info about the zone areas

zones <- data[grepl("Zone5|Zone4|Zone3|Zone2|Zone1|controlLabel", data$zone_name),] #here we extract the zone infos

# -------------------------- LABEL SCREEN ---------------------------#
# In this screen we have presented only the label 
# LEFT POSITION
orig_x_left <- zones[zones$zone_name=="Zone1" & zones$screen_index==4,]$zone_x_normalised[1]
orig_y_left <- zones[zones$zone_name=="Zone1" & zones$screen_index==4,]$zone_y_normalised[1]
width_left <- zones[zones$zone_name=="Zone1" & zones$screen_index==4,]$zone_width_normalised[1]
height_left <- zones[zones$zone_name=="Zone1" & zones$screen_index==4,]$zone_height_normalised[1]

x1_left<-orig_x_left 
x2_left<-orig_x_left + (width_left)
y1_left<-orig_y_left
y2_left<-orig_y_left + (height_left)

#  RIGHT POSITION
orig_x_right <- zones[zones$zone_name=="controlLabel" & zones$screen_index==4,]$zone_x_normalised[1]
orig_y_right <- zones[zones$zone_name=="controlLabel" & zones$screen_index==4,]$zone_y_normalised[1]
width_right <- zones[zones$zone_name=="controlLabel" & zones$screen_index==4,]$zone_width_normalised[1]
height_right <- zones[zones$zone_name=="controlLabel" & zones$screen_index==4,]$zone_height_normalised[1]

x1_right<-orig_x_right 
x2_right<-orig_x_right + (width_right)
y1_right<-orig_y_right
y2_right<-orig_y_right + (height_right)

# -------------------------- FEATURE SCREEN ---------------------------#
# This screen is where we presented the objects arranged in a grid (5 pictures)

# Upper left part of the monitor (A):
orig_x_A <- zones[zones$zone_name=="Zone2" & zones$screen_index==2,]$zone_x_normalised[1]
orig_y_A <- zones[zones$zone_name=="Zone2" & zones$screen_index==2,]$zone_y_normalised[1]
width_A <- zones[zones$zone_name=="Zone2" & zones$screen_index==2,]$zone_width_normalised[1]
height_A <- zones[zones$zone_name=="Zone2" & zones$screen_index==2,]$zone_height_normalised[1]

x1_A<-orig_x_A 
x2_A<-orig_x_A + (width_A)
y1_A<-orig_y_A
y2_A<-orig_y_A + (height_A)

# Upper right part of the monitor (B):
orig_x_B <- zones[zones$zone_name=="Zone3" & zones$screen_index==2,]$zone_x_normalised[1]
orig_y_B <- zones[zones$zone_name=="Zone3" & zones$screen_index==2,]$zone_y_normalised[1]
width_B <- zones[zones$zone_name=="Zone3" & zones$screen_index==2,]$zone_width_normalised[1]
height_B <- zones[zones$zone_name=="Zone3" & zones$screen_index==2,]$zone_height_normalised[1]

x1_B<-orig_x_B 
x2_B<-orig_x_B + (width_B)
y1_B<-orig_y_B
y2_B<-orig_y_B + (height_B)

# Center (C):
orig_x_C <- zones[zones$zone_name=="Zone1" & zones$screen_index==2,]$zone_x_normalised[1]
orig_y_C <- zones[zones$zone_name=="Zone1" & zones$screen_index==2,]$zone_y_normalised[1]
width_C <- zones[zones$zone_name=="Zone1" & zones$screen_index==2,]$zone_width_normalised[1]
height_C <- zones[zones$zone_name=="Zone1" & zones$screen_index==2,]$zone_height_normalised[1]

x1_C<-orig_x_C 
x2_C<-orig_x_C + (width_C)
y1_C<-orig_y_C
y2_C<-orig_y_C + (height_C)

# Bottom left (D):
orig_x_D <- zones[zones$zone_name=="Zone4" & zones$screen_index==2,]$zone_x_normalised[1]
orig_y_D <- zones[zones$zone_name=="Zone4" & zones$screen_index==2,]$zone_y_normalised[1]
width_D <- zones[zones$zone_name=="Zone4" & zones$screen_index==2,]$zone_width_normalised[1]
height_D <- zones[zones$zone_name=="Zone4" & zones$screen_index==2,]$zone_height_normalised[1]

x1_D<-orig_x_D 
x2_D<-orig_x_D + (width_D)
y1_D<-orig_y_D
y2_D<-orig_y_D + (height_D)

# Bottom right (E):
orig_x_E <- zones[zones$zone_name=="Zone5" & zones$screen_index==2,]$zone_x_normalised[1]
orig_y_E <- zones[zones$zone_name=="Zone5" & zones$screen_index==2,]$zone_y_normalised[1]
width_E <- zones[zones$zone_name=="Zone5" & zones$screen_index==2,]$zone_width_normalised[1]
height_E <- zones[zones$zone_name=="Zone5" & zones$screen_index==2,]$zone_height_normalised[1]

x1_E<-orig_x_E 
x2_E<-orig_x_E + (width_E)
y1_E<-orig_y_E
y2_E<-orig_y_E + (height_E)

# put all these variables in a dataframe for simplicity
ROIs <- data.frame(
  x1 = c(x1_A, x1_B, x1_C, x1_D, x1_E, x1_left, x1_right),
  x2 = c(x2_A, x2_B, x2_C, x2_D, x2_E, x2_left, x2_right),
  y1 = c(y1_A, y1_B, y1_C, y1_D, y1_E, y1_left, y1_right),
  y2 = c(y2_A, y2_B, y2_C, y2_D, y2_E, y2_left, y2_right),
  ROI = c("A","B","C","D","E","left","right")
)
ROIs

rm(data)
write.csv(data_minimal, paste0(output, "eyeTracker_v2.csv"), row.names = F, quote = F)
write.csv(ROIs, paste0(output, "ROIs.csv"), row.names = F, quote = F)
read.csv(paste0(output, "ROIs.csv"), stringsAsFactors = T) -> ROIs

data_minimal -> eyeTracker
eyeTracker$A <- ""
eyeTracker$B <- ""
eyeTracker$C <- ""
eyeTracker$D <- ""
eyeTracker$E <- ""
eyeTracker$left <- ""
eyeTracker$right <- ""

eyeTracker %>%
  dplyr::mutate_all(as.character) %>%
  as.matrix() -> eyeTracker

system.time(
  {
    for(i in 1:nrow(eyeTracker)) {
      print(nrow(eyeTracker)-i)
      for (b in ROIs$ROI){
        round(ROIs[ROIs$ROI==b,]$x1, 2) -> Xmin
        round(ROIs[ROIs$ROI==b,]$x2, 2) -> Xmax
        round(ROIs[ROIs$ROI==b,]$y1, 2) -> Ymin
        round(ROIs[ROIs$ROI==b,]$y2, 2) -> Ymax
        
        # p is your point, p.x is the x coord, p.y is the y coord
        eyeTracker[i,"x"] -> p.x
        eyeTracker[i,"y"] -> p.y
        
        
        #if not within the area, give 0, otherwise 1
        eyeTracker[i,b] <- ifelse(p.x < Xmin || p.x > Xmax || p.y < Ymin || p.y > Ymax, 0,1) 
        
      }
    }
  }
)
  
as.data.frame(eyeTracker) %>%
  dplyr::mutate(subjID = as.factor(subjID),
                task = as.factor(task),
                type = as.factor(type),
                screen_index = as.factor(screen_index),
                target = as.factor(target),
                label = as.factor(label),
                frequency = as.factor(frequency),
                obj_position = as.factor(obj_position),
                label_position = as.factor(label_position),
                block = as.factor(block),
                list = as.factor(list),
                trial = as.character(trial) %>%
                  as.numeric(),
                x = as.character(x)%>% 
                  as.numeric(),
                y = as.character(y)%>%
                  as.numeric(),
                A = as.character(A)%>%
                  as.numeric(),
                B = as.character(B)%>%
                  as.numeric(),
                C = as.character(C)%>%
                  as.numeric(),
                D = as.character(D)%>%
                  as.numeric(),
                E = as.character(E)%>%
                  as.numeric(),
                left = as.character(left)%>%
                  as.numeric(),
                right = as.character(right)%>%
                  as.numeric(),
                time = as.character(time)%>%
                  as.numeric(),
                face_conf = as.character(face_conf)%>%
                  as.numeric())  -> eyeTracker

eyeTracker$display <- as.factor(eyeTracker$display)
summary(eyeTracker)

eyeTracker%>%
  filter(list %in% c(1,2,10,4))%>%
  group_by(list, subjID, frequency, label, obj_position, label_position)%>%
  summarise(n_distinct(label))%>%
  pivot_wider(names_from = obj_position, values_from = `n_distinct(label)`)%>%
  write.csv("check_objPosition_by_subj.csv", row.names = F, na = "")

#save file
write.csv(eyeTracker, paste0(output,"eyeTracker_intersections.csv"), row.names = F, quote = F)

####-------------------- proportions of fixations across trials or BLOCKS by time -------------------------####
# load what we have done so far:
read.csv(paste0(output,"eyeTracker_intersections.csv"), stringsAsFactors = T) -> eyeTracker

summary(eyeTracker)
eyeTracker$list <- as.factor(eyeTracker$list)
eyeTracker$subjID <- as.factor(eyeTracker$subjID)

length(unique(eyeTracker$subjID))


#### Checking for trials  with no data for key screens index ISI_preLabel, label, ISI_postlabel ####

#This table tells you the sampling rate of the participants. We should decide on a threshold though.

eyeTracker%>%
  filter(screen_index == "ISI_preLabel" & time <=500 & subjID==3690866)%>%
  group_by(subjID,trial)%>%
  count(sort = T)%>%
  print(n = Inf)

frequencyConverter <- function(avg_n, maxDuration){
  sec <- maxDuration / 1000 #transform ms to s (this prevents us to create KHz rather than Hz)
  return(avg_n /sec) 
}


eyeTracker%>%
  filter(screen_index == "ISI_preLabel" & time <=500 )%>%
  group_by(subjID,trial)%>%
  summarise(n = n())%>%
  mutate(
         avg_n_datapoints_perTrial = round(mean(n),1),
         Hz = round(frequencyConverter(avg_n_datapoints_perTrial, 500),1),
         sampling_rate_ms = round((500 / avg_n_datapoints_perTrial)),1)%>% #this is the duration of ISI_prelabel
  select(subjID,avg_n_datapoints_perTrial,Hz, sampling_rate_ms)%>%
  unique()%>%
  filter(Hz <30)
  
View(eyeTracker[eyeTracker$screen_index=="ISI_preLabel" & eyeTracker$time <=500 & eyeTracker$trial==6 & eyeTracker$subjID==3688464 ,])

hist(eyeTracker[eyeTracker$screen_index=="ISI_preLabel" & eyeTracker$time <=500 &  eyeTracker$subjID==3688464,]$time)

eyeTracker%>%
  filter(subjID==3688464)%>%
  group_by(list,subjID,label, label_position,screen_index)%>%
  count()%>%
  pivot_wider(names_from = label, values_from = n)

eyeTracker %>%
  group_by(list, subjID, block, screen_index, label, frequency, time)%>%
  mutate(A_avg = mean(A),
         B_avg = mean(B),
         C_avg = mean(C),
         D_avg = mean(D),
         E_avg = mean(E),
         left_avg = mean(left),
         right_avg = mean(right))%>%
  pivot_longer(cols = A_avg:right_avg,
               names_to = "ROI",
               values_to = "prop")%>%
  select(subjID, face_conf, time, screen_index, target, label, 
         frequency, list, block, obj_position, label_position,
         ROI, prop)-> prop_long
  
prop_long$ROI <- as.factor(prop_long$ROI)
summary(prop_long)



#save proportion of looks
write.csv(prop_long, paste0(output, "eyeTracker_proportions_byBlock_longFormat.csv"), row.names = F, quote = F)
read.csv(paste0(output, "eyeTracker_proportions_byBlock_longFormat.csv"), stringsAsFactors = T) -> prop_long
rm(prop_long);gc()

prop_long%>%
  group_by(list,subjID,screen_index,label)%>%
  count(n_distinct(trial))%>%
  pivot_wider(names_from = label, values_from = n)
  

