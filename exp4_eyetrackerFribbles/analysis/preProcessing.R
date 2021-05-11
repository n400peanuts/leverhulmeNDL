#-----------------------------------------------------------#
#---------- this script takes the raw data from Gorilla ----#
#---- it selects the columns and rows necessary for --------#
#--------------------- data analysis -----------------------#
#-----------------------------------------------------------#

rm(list=ls())

library(tidyverse)

#----------- select the experiment ------------#
expeType <- "pilot2"

#### Set current working directory ####
localDirectory <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - fribbles/")

# folder where we store the input from Gorilla as is
input <- c(paste0(localDirectory,"rawdata/",expeType, "/")) 

#folder where we save the preprocessed data after columns and rows selection
output <- c(paste0(localDirectory,"preProcessed_data/", expeType, '/')) 

#### load behavioural data ####
#--------------- load stimuli ------------------#
read.csv(paste0(localDirectory,"stimuli/stimuli.csv"))-> stimuli

#--------------- load data ------------------#
#see what's in the folder
df <- list.files(input) 
df <- df[grepl("data",df)]

# Gorilla assigns a random generated ID for each task, we need to know what is what
taskID_list1 <- data.frame(
  list = rep(1,3),
  gorillaCode = c("wcph","hpz2","jyd3"),
  task = c("learning", "2AFC", "contingency") 
)

taskID_list2 <- data.frame(
  list = rep(2,3),
  gorillaCode = c("vm7o","67pm","dlw7"),
  task = c("learning", "2AFC", "contingency") 
)

taskID_list3 <- data.frame(
  list = rep(3,3),
  gorillaCode = c("74sa","jc49","tiwi"),
  task = c("learning", "2AFC", "contingency") 
)

# c(df[grepl(taskID_list1[taskID_list1$task=="learning",]$gorillaCode, df)],
#   df[grepl(taskID_list2[taskID_list2$task=="learning",]$gorillaCode, df)],
#   df[grepl(taskID_list3[taskID_list3$task=="learning",]$gorillaCode, df)]) -> learningID

c(df[grepl(taskID_list1[taskID_list1$task=="2AFC",]$gorillaCode, df)],
  df[grepl(taskID_list2[taskID_list2$task=="2AFC",]$gorillaCode, df)],
  df[grepl(taskID_list3[taskID_list3$task=="2AFC",]$gorillaCode, df)]) -> AFCID

c(df[grepl(taskID_list1[taskID_list1$task=="contingency",]$gorillaCode, df)],
  df[grepl(taskID_list2[taskID_list2$task=="contingency",]$gorillaCode, df)],
  df[grepl(taskID_list3[taskID_list3$task=="contingency",]$gorillaCode, df)]) -> contingencyID

# load the data into our global environment
#learning <- NULL
labPic <- NULL
contingency <- NULL

if (expeType == "pilot1"){
  
  for (i in 1:length(df)){
    
    gsub("data_exp_45245-v3_task-|.csv$", "", df[i]) -> id #remove .csv
    
    if (id == taskID_list1[taskID_list1$task=="learning",]$gorillaCode){
      id <- "learning"
      assign("learning", data.frame()) #load into the environment with more intuitive names
      
    } else if (id == taskID_list1[taskID_list1$task=="2AFC",]$gorillaCode){
      id <- "labPic"
      assign("labPic", data.frame()) 
      
    } else if (id == taskID_list1[taskID_list1$task=="contingency",]$gorillaCode){
      id <- "contingency"
      assign("contingency", data.frame())
      
    }
    
    read.csv(paste0(input, df[i]),
             na.strings=c("","NA"),
             stringsAsFactors = T)-> temp
    
    assign(paste0(id), temp)
    
  } 
} else {
  for (y in 1:length(AFCID)){
    read.csv(paste(input, AFCID[y], sep = ""),na.strings=c("","NA"), stringsAsFactors = T)-> temp
    labPic <- plyr::rbind.fill(temp,labPic)
  };
  
  for (z in 1:length(contingencyID)){
    read.csv(paste(input, contingencyID[z], sep = ""), na.strings=c("","NA"), stringsAsFactors = T)-> temp
    contingency <- plyr::rbind.fill(temp,contingency)
  };
}




rm(taskID_list1,taskID_list2,taskID_list3,temp,i,df, id, contingencyID,AFCID,learningID,x,y,z)

unique(contingency$list)
unique(na.omit(contingency$Participant.Private.ID))

#### columns and rows selection ####
# in Gorilla there are a number of rows that are not necessary for our analysis, 
# therefore we're going to select only the columns and rows that we need

#### 2AFC - labPic ####

if (expeType == "pilot2"){
  columnsIwantTokeep<- labPic[c('Task.Name','Participant.Private.ID', 'display','Trial.Number','Zone.Type',
                                'Screen.Name', 'Response', 'label','frequency','Reaction.Time','list')]
  
  rowsIwantTokeep <- c("Screen 2")
  
  
  labPic <- columnsIwantTokeep %>% 
    filter(Screen.Name == rowsIwantTokeep & display == "task") %>%
    rename(subjID = Participant.Private.ID, 
           task = Task.Name,
           resp = Response, 
           rt = Reaction.Time,
           trial = Trial.Number,
           labelPresented = label)
  
  labPic$display <- NULL; labPic$Screen.Name <- NULL; labPic$Zone.Type <- NULL #we don't need these columns anymore
  rm(rowsIwantTokeep, columnsIwantTokeep)
  
} else {
  columnsIwantTokeep<- labPic[c('Task.Name','Participant.Private.ID', 'display','Trial.Number',
                                'Screen.Name', 'Response', 'label','frequency','Reaction.Time')]
  
  rowsIwantTokeep <- c("Screen 2")
  
  
  labPic <- columnsIwantTokeep %>% 
    filter(Screen.Name %in% rowsIwantTokeep &
             display %in% "task" ) %>%
    rename(subjID = Participant.Private.ID, 
           task = Task.Name,
           resp = Response, 
           rt = Reaction.Time,
           trial = Trial.Number,
           labelPresented = label)
  
  labPic$display <- NULL; labPic$Screen.Name <- NULL #we don't need these columns anymore
  rm(rowsIwantTokeep, columnsIwantTokeep)
  
}

labPic <- labPic[labPic$subjID!="3502047",]

#----------------- clean the rows from CSS and HTML metadata ---------------#
labPic <- droplevels(labPic)
labPic$labelPresented <- gsub('<p style="font-size: 700%;">', "", labPic$labelPresented)
labPic$labelPresented <- gsub('</p>', "", labPic$labelPresented); 
labPic$labelPresented <- gsub(' ', "", labPic$labelPresented); 
as.factor(labPic$labelPresented)-> labPic$labelPresented
as.factor(labPic$frequency)-> labPic$frequency

labPic$resp <- gsub('.jpg', "", labPic$resp)
as.factor(labPic$resp)-> labPic$resp



#----- map the picture to the correponding fribble --------------#
# fribble ID in stimuli contains the mapping, we're going to merge the two dataframes
# merging is possible only if the column to merge has the same name
colnames(stimuli)[1] <- 'resp'
merge(stimuli, labPic, by = c("resp"), all.y = T)-> temp

colnames(temp)[4] <- 'fribbleSelected'
colnames(temp)[9] <- 'frequency'
temp$frequency.x <-NULL #this is a duplicate

temp -> labPic; rm(temp);
labPic$resp <- as.factor(labPic$resp)
labPic$frequency <- as.factor(labPic$frequency)
labPic$fribbleSelected <- as.factor(labPic$fribbleSelected)
labPic$subjID <- as.factor(labPic$subjID)

#------------- accuracy ----------------#
ifelse(labPic$fribbleSelected == labPic$labelPresented,1,0)-> labPic$acc


aggregate(acc ~ frequency + subjID, data = labPic, mean)


# coding of the type of response



labPic$resp <- as.character(labPic$resp)
labPic$resp[is.na(labPic$resp)] <- "missing"
labPic$resp <- as.factor(labPic$resp)

labPic$type_of_resp <- c("responses") # if I made the column correctly, then we shouldn't find any row names "responses" left.
labPic[labPic$resp=="missing",]$type_of_resp <- "timedOut"

#-------------------control-----------------------------#
labPic[na.omit(labPic$labelPresented=="bim" & labPic$fribbleSelected == "bim"),]$type_of_resp <- c("match")
labPic[na.omit(labPic$labelPresented=="bim" & labPic$fribbleSelected != "bim"),]$type_of_resp <- c("errorControl")

# ------------------correct-----------------------------#
labPic[na.omit(labPic$labelPresented=="tob" & labPic$fribbleSelected == "tob"),]$type_of_resp <- c("match")
labPic[na.omit(labPic$labelPresented=="wug" & labPic$fribbleSelected == "wug"),]$type_of_resp <- c("match")
labPic[na.omit(labPic$labelPresented=="dep" & labPic$fribbleSelected == "dep"),]$type_of_resp <- c("match")

# ------------------mismatch-type1 ---------------------#
#dep
labPic[na.omit(labPic$labelPresented=="tob" & labPic$frequency=="low" & labPic$fribbleSelected == "dep"),]$type_of_resp <- c("mismatch-type1")
labPic[na.omit(labPic$labelPresented=="wug" & labPic$frequency=="high" & labPic$fribbleSelected == "dep"),]$type_of_resp <- c("mismatch-type1")
#wug
labPic[na.omit(labPic$labelPresented=="dep" & labPic$frequency=="low" & labPic$fribbleSelected == "wug"),]$type_of_resp <- c("mismatch-type1")
labPic[na.omit(labPic$labelPresented=="tob" & labPic$frequency=="high" & labPic$fribbleSelected == "wug"),]$type_of_resp <- c("mismatch-type1")
#tob
labPic[na.omit(labPic$labelPresented=="wug" & labPic$frequency=="low" & labPic$fribbleSelected == "tob"),]$type_of_resp <- c("mismatch-type1")
labPic[na.omit(labPic$labelPresented=="dep" & labPic$frequency=="high" & labPic$fribbleSelected == "tob"),]$type_of_resp <- c("mismatch-type1")

#-------------------mismatch-type2----------------------#

labPic[na.omit(labPic$labelPresented=="wug" & labPic$frequency=="high" & labPic$fribbleSelected == "tob"),]$type_of_resp <- c("mismatch-type2")
labPic[na.omit(labPic$labelPresented=="dep" & labPic$frequency=="high" & labPic$fribbleSelected == "wug"),]$type_of_resp <- c("mismatch-type2")
labPic[na.omit(labPic$labelPresented=="tob" & labPic$frequency=="high" & labPic$fribbleSelected == "dep"),]$type_of_resp <- c("mismatch-type2")

labPic[na.omit(labPic$labelPresented=="dep" & labPic$frequency=="low" & labPic$fribbleSelected == "tob"),]$type_of_resp <- c("mismatch-type2")
labPic[na.omit(labPic$labelPresented=="tob" & labPic$frequency=="low" & labPic$fribbleSelected == "wug"),]$type_of_resp <- c("mismatch-type2")
labPic[na.omit(labPic$labelPresented=="wug" & labPic$frequency=="low" & labPic$fribbleSelected == "dep"),]$type_of_resp <- c("mismatch-type2")

#----- (!) these are trials that were not supposed to be control trials, but participants nonetheless choose the control (!)
labPic[na.omit(labPic$labelPresented=="dep" & labPic$fribbleSelected == "bim" & labPic$frequency=="low"),]$type_of_resp <- c("errorControl-low")
labPic[na.omit(labPic$labelPresented=="tob" & labPic$fribbleSelected == "bim" & labPic$frequency=="low"),]$type_of_resp <- c("errorControl-low")
labPic[na.omit(labPic$labelPresented=="wug" & labPic$fribbleSelected == "bim" & labPic$frequency=="low"),]$type_of_resp <- c("errorControl-low")

labPic[na.omit(labPic$labelPresented=="dep" & labPic$fribbleSelected == "bim" & labPic$frequency=="high"),]$type_of_resp <- c("errorControl-high")
labPic[na.omit(labPic$labelPresented=="tob" & labPic$fribbleSelected == "bim" & labPic$frequency=="high"),]$type_of_resp <- c("errorControl-high")
labPic[na.omit(labPic$labelPresented=="wug" & labPic$fribbleSelected == "bim" & labPic$frequency=="high"),]$type_of_resp <- c("errorControl-high")

as.factor(labPic$type_of_resp)->labPic$type_of_resp
summary(labPic$type_of_resp) #no other response left, 

labPic$expeType <- as.factor(expeType)

write.csv(labPic, paste0(output, "labPic.csv"), quote = F, row.names = F)



#### contingency ####
if (expeType == "pilot2"){
  columnsIwantTokeep<- contingency[c('Task.Name','Participant.Private.ID', 'display','Trial.Number', 'fribbleID',
                                     'Zone.Type', 'Response', 'labelPresented','frequency','Reaction.Time','trialType','list')]
  
  rowsIwantTokeep <- c("response_slider_endValue")
  
  
  contingency <- columnsIwantTokeep %>% 
    filter(Zone.Type %in% rowsIwantTokeep) %>%
    rename(subjID = Participant.Private.ID, 
           task = Task.Name,
           resp = Response, 
           rt = Reaction.Time,
           trial = Trial.Number,
           fribblePresented = fribbleID)
  
  contingency$Zone.Type <- NULL;  #we don't need these columns anymore
  rm(rowsIwantTokeep, columnsIwantTokeep)
  
  
} else {
  columnsIwantTokeep<- contingency[c('Task.Name','Participant.Private.ID', 'display','Trial.Number', 'fribbleID',
                                     'Zone.Type', 'Response', 'labelPresented','frequency','Reaction.Time','trialType')]
  
  rowsIwantTokeep <- c("response_slider_endValue")
  
  
  contingency <- columnsIwantTokeep %>% 
    filter(Zone.Type %in% rowsIwantTokeep) %>%
    rename(subjID = Participant.Private.ID, 
           task = Task.Name,
           resp = Response, 
           rt = Reaction.Time,
           trial = Trial.Number,
           fribblePresented = fribbleID)
  
  contingency$Zone.Type <- NULL;  #we don't need these columns anymore
  rm(rowsIwantTokeep, columnsIwantTokeep)
  
}

#----------------- clean the rows from CSS and HTML metadata ---------------#
contingency <- droplevels(contingency)
contingency$labelPresented <- gsub('<p style="font-size: 500%;">', "", contingency$labelPresented)
contingency$labelPresented <- gsub('</p>', "", contingency$labelPresented); 
contingency$labelPresented <- gsub(' ', "", contingency$labelPresented); 
as.factor(contingency$labelPresented)-> contingency$labelPresented
contingency$fribblePresented <- gsub('.jpg', "", contingency$fribblePresented)
as.factor(contingency$fribblePresented)-> contingency$fribblePresented
as.factor(contingency$subjID)-> contingency$subjID
as.factor(expeType)-> contingency$expeType

write.csv(contingency, paste0(output, "contingency.csv"), quote = F, row.names = F)

aggregate(resp ~ trialType + frequency + subjID, data = contingency, mean)


#### eye tracker data ####
df <- list.files(paste0(input,"eyetracker/")) #folder where I story my eyetracking data
df <- df[grepl("collection",df)] #let's take only the experimental trials

calb <- list.files(paste0(input,"eyetracker/")) #folder where I story my eyetracking data
calb <- calb[grepl("calibration",calb)] #let's take only the experimental trials

eyeData <- NULL
for (i in 1:length(df)){
  gsub(".xlsx$", "", df[i]) -> id
  readxl::read_xlsx(paste(input, "eyetracker/", df[i], sep = ""))-> temp
  eyeData <- bind_rows(temp,eyeData)
};
rm(temp,df,i,id)

eyeData$zone_name <- as.factor(eyeData$zone_name)

calibrationData <- NULL
for (i in 1:length(calb)){
 # gsub(".xlsx$", "", calb[i]) -> id
  readxl::read_xlsx(paste(input, "eyetracker/", calb[i], sep = ""))-> temp
  calibrationData <- rbind(temp,calibrationData)
};
rm(temp,calb,i,id)

#these are our region of interest
levels(eyeData$zone_name)
summary(eyeData)
unique(eyeData$participant_id)

# the eyetracking files and the spreadsheet loaded on Gorilla are meant to be linked by the column "spreadsheet_row"
# basically the eyetracking files point to the row of the spreadsheet
# Since we would like to be able to trace back what we're presenting, then we replace in the eyetracking masterfile (data) the following columns
# with the values listed in the spreadsheet file

# first I load the spreadsheet used in Gorilla with the list of trials:
spreadsheet <-read.csv(paste0(localDirectory,"stimuli/spreadsheet Gorilla/finalSpreadsheets/pilot1/spreadsheet.csv"), stringsAsFactors = T) 

spreadsheet_list1 <-read.csv(paste0(localDirectory,"stimuli/spreadsheet Gorilla/finalSpreadsheets/learning_list1.csv"), stringsAsFactors = T) 
spreadsheet_list2 <-read.csv(paste0(localDirectory,"stimuli/spreadsheet Gorilla/finalSpreadsheets/learning_list2.csv"), stringsAsFactors = T) 
spreadsheet_list3 <-read.csv(paste0(localDirectory,"stimuli/spreadsheet Gorilla/finalSpreadsheets/learning_list3.csv"), stringsAsFactors = T) 


unique(contingency[contingency$list==1,]$subjID) -> subjlist1
unique(contingency[contingency$list==2,]$subjID) -> subjlist2
unique(contingency[contingency$list==3,]$subjID) -> subjlist3

# the column spreadsheet_row contains numbers pointing to the row in the actual spreadsheet
head(eyeData$spreadsheet_row)


#we're going to take the fribble presented for that trial and strip away the '.jpg'
if (expeType == "pilot2"){
  eyeData$target <- ""
  eyeData$list <- 0
  eyeData$position <- ""
  eyeData$labelPresented <- ""
  eyeData$frequency <- ""
  
  eyeData[eyeData$participant_id %in% subjlist1,]$target <- gsub(".jpg$", "", spreadsheet_list1[(eyeData[eyeData$participant_id %in% subjlist1,]$spreadsheet_row),]$ID)
  eyeData[eyeData$participant_id %in% subjlist1,]$list <- spreadsheet_list1[(eyeData[eyeData$participant_id %in% subjlist1,]$spreadsheet_row),]$list
  eyeData[eyeData$participant_id %in% subjlist1,]$position <- spreadsheet_list1[(eyeData[eyeData$participant_id %in% subjlist1,]$spreadsheet_row),]$ANSWER
  
  eyeData[eyeData$participant_id %in% subjlist2,]$target <- gsub(".jpg$", "", spreadsheet_list2[(eyeData[eyeData$participant_id %in% subjlist2,]$spreadsheet_row),]$ID)
  eyeData[eyeData$participant_id %in% subjlist2,]$list <- spreadsheet_list2[(eyeData[eyeData$participant_id %in% subjlist2,]$spreadsheet_row),]$list
  eyeData[eyeData$participant_id %in% subjlist2,]$position <- spreadsheet_list2[(eyeData[eyeData$participant_id %in% subjlist2,]$spreadsheet_row),]$ANSWER
  
  eyeData[eyeData$participant_id %in% subjlist3,]$target <- gsub(".jpg$", "", spreadsheet_list3[(eyeData[eyeData$participant_id %in% subjlist3,]$spreadsheet_row),]$ID)
  eyeData[eyeData$participant_id %in% subjlist3,]$list <- spreadsheet_list3[(eyeData[eyeData$participant_id %in% subjlist3,]$spreadsheet_row),]$list
  eyeData[eyeData$participant_id %in% subjlist3,]$position <- spreadsheet_list3[(eyeData[eyeData$participant_id %in% subjlist3,]$spreadsheet_row),]$ANSWER
  
  summary(as.factor(eyeData$target))
  summary(as.factor(eyeData$list))
  summary(as.factor(eyeData$position))
  
  #same for the label presented
  eyeData[eyeData$participant_id %in% subjlist1,]$labelPresented <- spreadsheet_list1[(eyeData[eyeData$participant_id %in% subjlist1,]$spreadsheet_row),]$label
  eyeData[eyeData$participant_id %in% subjlist2,]$labelPresented <- spreadsheet_list2[(eyeData[eyeData$participant_id %in% subjlist2,]$spreadsheet_row),]$label
  eyeData[eyeData$participant_id %in% subjlist3,]$labelPresented <- spreadsheet_list3[(eyeData[eyeData$participant_id %in% subjlist3,]$spreadsheet_row),]$label
  
  summary(as.factor(eyeData$labelPresented))
  
  # frequency
  eyeData[eyeData$participant_id %in% subjlist1,]$frequency <- spreadsheet_list1[(eyeData[eyeData$participant_id %in% subjlist1,]$spreadsheet_row),]$frequency
  eyeData[eyeData$participant_id %in% subjlist2,]$frequency <- spreadsheet_list2[(eyeData[eyeData$participant_id %in% subjlist2,]$spreadsheet_row),]$frequency
  eyeData[eyeData$participant_id %in% subjlist3,]$frequency <- spreadsheet_list3[(eyeData[eyeData$participant_id %in% subjlist3,]$spreadsheet_row),]$frequency
  
} else {
  eyeData$target <- ""
  eyeData$position <- ""
  eyeData$labelPresented <- ""
  eyeData$frequency <- ""
  
  eyeData$target <- gsub(".jpg$", "", spreadsheet[(eyeData$spreadsheet_row),]$ID)
  eyeData$position <- spreadsheet[(eyeData$spreadsheet_row),]$ANSWER
  
  #same for the label presented
  eyeData$labelPresented <- spreadsheet[(eyeData$spreadsheet_row),]$label
  
  # frequency
  eyeData$frequency <- spreadsheet[(eyeData$spreadsheet_row),]$frequency

  
}
summary(as.factor(eyeData$target))
summary(as.factor(eyeData$position))
summary(as.factor(eyeData$labelPresented))
summary(as.factor(eyeData$frequency))

#this converts to factor everything that has been listed "as.character"
eyeData[sapply(eyeData, is.character)] <-  
  lapply(eyeData[sapply(eyeData, is.character)], as.factor)
eyeData <- droplevels(eyeData)
#check that you can make sense of all columns just by looking at the summary
summary(eyeData) #our masterfile with all the eyetracking data

# --------------- column selection --------------#
#select relevant columns and rows 
if (expeType =="pilot2"){
  eyeData_minimal <- 
    eyeData %>%
    filter(type %in% "prediction") %>%
    select(participant_id, filename, spreadsheet_row, time_elapsed, type, 
           screen_index, x_pred_normalised, y_pred_normalised, 
           target, labelPresented, frequency, list, position) %>%
    rename(subjID = participant_id, 
           task = filename,
           time = time_elapsed,
           trial = spreadsheet_row,
           x = x_pred_normalised,
           y = y_pred_normalised)
  
  eyeData_minimal <- droplevels(eyeData_minimal)
  eyeData_minimal$subjID <- as.factor(eyeData_minimal$subjID)
  
} else {
  eyeData_minimal <- 
    eyeData %>%
    filter(type %in% "prediction") %>%
    select(participant_id, filename, spreadsheet_row, time_elapsed, type, 
           screen_index, x_pred_normalised, y_pred_normalised, 
           target, labelPresented, frequency, position) %>%
    rename(subjID = participant_id, 
           task = filename,
           time = time_elapsed,
           trial = spreadsheet_row,
           x = x_pred_normalised,
           y = y_pred_normalised)
  
  eyeData_minimal <- droplevels(eyeData_minimal)
  eyeData_minimal$subjID <- as.factor(eyeData_minimal$subjID)
  
}

summary(eyeData_minimal)
#ok now that we've got our eyetracking data, we need to know the areas of our ROI

#extract zone dimensions -- we need to know where we have presented our images
# in order to do so, we extract the info about the zone areas

zones <- eyeData[grepl("fribblezone|leftITI|centerITI|rightITI|leftLabel|rightLabel|centerLabel|buttonLeft|buttonCenter|buttonRight", 
                       eyeData$zone_name),] #here we extract the zone infos
droplevels(zones)->zones
levels(zones$zone_name)

# -------------------------- FRIBBLE SCREEN ---------------------------#
orig_x_fribble <- zones[zones$zone_name=="fribblezone",]$zone_x_normalised[1]
orig_y_fribble <- zones[zones$zone_name=="fribblezone",]$zone_y_normalised[1]
width_fribble <- zones[zones$zone_name=="fribblezone",]$zone_width_normalised[1]
height_fribble <- zones[zones$zone_name=="fribblezone",]$zone_height_normalised[1]

x1_fribble<-orig_x_fribble 
x2_fribble<-orig_x_fribble + (width_fribble)
y1_fribble<-orig_y_fribble
y2_fribble<-orig_y_fribble + (height_fribble)

# -------------------------- ITI BLANK SCREEN ---------------------------#
# center
orig_x_centerITI <- zones[zones$zone_name=="centerITI",]$zone_x_normalised[1]
orig_y_centerITI <- zones[zones$zone_name=="centerITI",]$zone_y_normalised[1]
width_centerITI <- zones[zones$zone_name=="centerITI" ,]$zone_width_normalised[1]
height_centerITI <- zones[zones$zone_name=="centerITI",]$zone_height_normalised[1]

x1_centerITI<-orig_x_centerITI 
x2_centerITI<-orig_x_centerITI + (width_centerITI)
y1_centerITI<-orig_y_centerITI
y2_centerITI<-orig_y_centerITI + (height_centerITI)

#left
orig_x_leftITI <- zones[zones$zone_name=="leftITI",]$zone_x_normalised[1]
orig_y_leftITI <- zones[zones$zone_name=="leftITI",]$zone_y_normalised[1]
width_leftITI <- zones[zones$zone_name=="leftITI" ,]$zone_width_normalised[1]
height_leftITI <- zones[zones$zone_name=="leftITI",]$zone_height_normalised[1]

x1_leftITI<-orig_x_leftITI 
x2_leftITI<-orig_x_leftITI + (width_leftITI)
y1_leftITI<-orig_y_leftITI
y2_leftITI<-orig_y_leftITI + (height_leftITI)

#right
orig_x_rightITI <- zones[zones$zone_name=="rightITI",]$zone_x_normalised[1]
orig_y_rightITI <- zones[zones$zone_name=="rightITI",]$zone_y_normalised[1]
width_rightITI <- zones[zones$zone_name=="rightITI" ,]$zone_width_normalised[1]
height_rightITI <- zones[zones$zone_name=="rightITI",]$zone_height_normalised[1]

x1_rightITI<-orig_x_rightITI 
x2_rightITI<-orig_x_rightITI + (width_rightITI)
y1_rightITI<-orig_y_rightITI
y2_rightITI<-orig_y_rightITI + (height_rightITI)

# -------------------------- LABEL SCREEN ---------------------------#
# center
orig_x_centerLabel <- zones[zones$zone_name=="centerLabel",]$zone_x_normalised[1]
orig_y_centerLabel <- zones[zones$zone_name=="centerLabel",]$zone_y_normalised[1]
width_centerLabel <- zones[zones$zone_name=="centerLabel" ,]$zone_width_normalised[1]
height_centerLabel <- zones[zones$zone_name=="centerLabel",]$zone_height_normalised[1]

x1_centerLabel<-orig_x_centerLabel 
x2_centerLabel<-orig_x_centerLabel + (width_centerLabel)
y1_centerLabel<-orig_y_centerLabel
y2_centerLabel<-orig_y_centerLabel + (height_centerLabel)

#left
orig_x_leftLabel <- zones[zones$zone_name=="leftLabel",]$zone_x_normalised[1]
orig_y_leftLabel <- zones[zones$zone_name=="leftLabel",]$zone_y_normalised[1]
width_leftLabel <- zones[zones$zone_name=="leftLabel" ,]$zone_width_normalised[1]
height_leftLabel <- zones[zones$zone_name=="leftLabel",]$zone_height_normalised[1]

x1_leftLabel<-orig_x_leftLabel 
x2_leftLabel<-orig_x_leftLabel + (width_leftLabel)
y1_leftLabel<-orig_y_leftLabel
y2_leftLabel<-orig_y_leftLabel + (height_leftLabel)

#right
orig_x_rightLabel <- zones[zones$zone_name=="rightLabel",]$zone_x_normalised[1]
orig_y_rightLabel <- zones[zones$zone_name=="rightLabel",]$zone_y_normalised[1]
width_rightLabel <- zones[zones$zone_name=="rightLabel" ,]$zone_width_normalised[1]
height_rightLabel <- zones[zones$zone_name=="rightLabel",]$zone_height_normalised[1]

x1_rightLabel<-orig_x_rightLabel 
x2_rightLabel<-orig_x_rightLabel + (width_rightLabel)
y1_rightLabel<-orig_y_rightLabel
y2_rightLabel<-orig_y_rightLabel + (height_rightLabel)


# -------------------------- BUTTON SCREEN ---------------------------#
# center
orig_x_buttonCenter <- zones[zones$zone_name=="buttonCenter",]$zone_x_normalised[1]
orig_y_buttonCenter <- zones[zones$zone_name=="buttonCenter",]$zone_y_normalised[1]
width_buttonCenter <- zones[zones$zone_name=="buttonCenter" ,]$zone_width_normalised[1]
height_buttonCenter <- zones[zones$zone_name=="buttonCenter",]$zone_height_normalised[1]

x1_buttonCenter<-orig_x_buttonCenter 
x2_buttonCenter<-orig_x_buttonCenter + (width_buttonCenter)
y1_buttonCenter<-orig_y_buttonCenter
y2_buttonCenter<-orig_y_buttonCenter + (height_buttonCenter)

#left
orig_x_buttonLeft <- zones[zones$zone_name=="buttonLeft",]$zone_x_normalised[1]
orig_y_buttonLeft <- zones[zones$zone_name=="buttonLeft",]$zone_y_normalised[1]
width_buttonLeft <- zones[zones$zone_name=="buttonLeft" ,]$zone_width_normalised[1]
height_buttonLeft <- zones[zones$zone_name=="buttonLeft",]$zone_height_normalised[1]

x1_buttonLeft<-orig_x_buttonLeft 
x2_buttonLeft<-orig_x_buttonLeft + (width_buttonLeft)
y1_buttonLeft<-orig_y_buttonLeft
y2_buttonLeft<-orig_y_buttonLeft + (height_buttonLeft)

#right
orig_x_buttonRight <- zones[zones$zone_name=="buttonRight",]$zone_x_normalised[1]
orig_y_buttonRight <- zones[zones$zone_name=="buttonRight",]$zone_y_normalised[1]
width_buttonRight <- zones[zones$zone_name=="buttonRight" ,]$zone_width_normalised[1]
height_buttonRight <- zones[zones$zone_name=="buttonRight",]$zone_height_normalised[1]

x1_buttonRight<-orig_x_buttonRight 
x2_buttonRight<-orig_x_buttonRight + (width_buttonRight)
y1_buttonRight<-orig_y_buttonRight
y2_buttonRight<-orig_y_buttonRight + (height_buttonRight)


# put all these variables in a dataframe for simplicity
ROIs <- data.frame(
  x1 = c(x1_fribble, x1_centerITI, x1_leftITI, x1_rightITI, x1_centerLabel, x1_leftLabel, x1_rightLabel, x1_buttonCenter, x1_buttonLeft, x1_buttonRight),
  x2 = c(x2_fribble, x2_centerITI, x2_leftITI, x2_rightITI, x2_centerLabel, x2_leftLabel, x2_rightLabel, x2_buttonCenter, x2_buttonLeft, x2_buttonRight),
  y1 = c(y1_fribble, y1_centerITI, y1_leftITI, y1_rightITI, y1_centerLabel, y1_leftLabel, y1_rightLabel, y1_buttonCenter, y1_buttonLeft, y1_buttonRight),
  y2 = c(y2_fribble, y2_centerITI, y2_leftITI, y2_rightITI, y2_centerLabel, y2_leftLabel, y2_rightLabel, y2_buttonCenter, y2_buttonLeft, y2_buttonRight),
  ROI = c("fribble","cITI","lITI","rITI","cLabel","lLabel","rLabel","cButton","lButton","rButton"),
  screen = c(2,3,3,3,4,4,4,5,5,5)
)
ROIs
write.csv(ROIs, paste0(output,"ROIs.csv"), row.names = F,quote=F)
write.csv(eyeData_minimal, paste0(output,"eyeTracker.csv"), row.names = F,quote=F)
