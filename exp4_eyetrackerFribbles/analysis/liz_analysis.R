setwd()
rm(list=ls())
library(plyr) # used for "round_any" fuctnion

## 
##read in and merge data from both pilots 
##
setwd("C:/Users/liz/OneDrive - Nexus365/Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - fribbles/preProcessed_data")
pilot1 = read.csv("pilot1/eyeTracker_intersections.csv")
pilot2 = read.csv("pilot2/eyeTracker_intersections.csv")
pilot1$pilotgroup = "pilot1"
pilot2$pilotgroup = "pilot2"
pilot1$list = 1 # there is no column for this in the original sheet
bothpilot = rbind(pilot1,pilot2)
str(bothpilot)

## 
##add columns that give the positions of the target and each foil type 
##
listsTable = read.csv("listsTable.csv")
bothpilot = merge(bothpilot,listsTable, by =c("list","frequency","labelPresented"))
# note: the column position codes where the target is for the foil in both pilot data sets 
bothpilot$targetLocation[bothpilot$frequency == "control"]=bothpilot$position[bothpilot$frequency == "control"]  
bothpilot$position==bothpilot$targetLocation  # note pilot 2 already contained target location in the position column, this is now redundant 
bothpilot$position=NULL
str(bothpilot)
## 
##add columns that say whether they are look at teh target and each foil type 
##
#set up empty columns
bothpilot$targetLabelLk= NA
bothpilot$mismatch1LabelLk= NA
bothpilot$mismatch2LabelLk= NA
bothpilot$rControlFoilLk= NA
bothpilot$lControlFoilLk= NA
bothpilot$cControlFoilLk= NA

table(bothpilot$mismatch1Location,bothpilot$labelPresented)

# add columns with looks to target
bothpilot$targetLabelLk[bothpilot$targetLocation=="r"] = bothpilot$rLabel[bothpilot$targetLocation=="r"]
bothpilot$targetLabelLk[bothpilot$targetLocation=="l"] = bothpilot$lLabel[bothpilot$targetLocation=="l"]
bothpilot$targetLabelLk[bothpilot$targetLocation=="c"] = bothpilot$cLabel[bothpilot$targetLocation=="c"]

# add columns with looks to mismatch type 1 (this will only match for non control trials)
f=bothpilot$mismatch1Location=="r" & bothpilot$frequency != "control"
bothpilot$mismatch1LabelLk[f] = bothpilot$rLabel[f]
f=bothpilot$mismatch1Location=="l" & bothpilot$frequency != "control"
bothpilot$mismatch1LabelLk[f] = bothpilot$lLabel[f]
f=bothpilot$mismatch1Location=="c" & bothpilot$frequency != "control"
bothpilot$mismatch1LabelLk[f] = bothpilot$cLabel[f]
# add columns with looks to mismatch type 2 (this will only match for non control trials)
f=bothpilot$mismatch2Location=="r" & bothpilot$frequency != "control"
bothpilot$mismatch2LabelLk[f] = bothpilot$rLabel[f]
f=bothpilot$mismatch2Location=="l" & bothpilot$frequency != "control"
bothpilot$mismatch2LabelLk[f] = bothpilot$lLabel[f]
f=bothpilot$mismatch2Location=="c" & bothpilot$frequency != "control"
bothpilot$mismatch2LabelLk[f] = bothpilot$cLabel[f]
# add columns with looks to foils in right position
f = bothpilot$frequency == "control" & bothpilot$targetLocation=="r"
bothpilot$lControlFoilLk[f] = bothpilot$lLabel[f]
bothpilot$cControlFoilLk[f] = bothpilot$cLabel[f]
# add columns with looks to foils in right position
f = bothpilot$frequency == "control" & bothpilot$targetLocation=="l"
bothpilot$rControlFoilLk[f] = bothpilot$rLabel[f]
bothpilot$cControlFoilLk[f] = bothpilot$cLabel[f]
# add columns with looks to foils in left position
f = bothpilot$frequency == "control" & bothpilot$targetLocation=="c"
bothpilot$rControlFoilLk[f] = bothpilot$rLabel[f]
bothpilot$lControlFoilLk[f] = bothpilot$lLabel[f]

## target coding double check
# double check coding the target in center ones
x  = subset(bothpilot, list ==1&labelPresented=="dep")
table(x$targetLocation)
table(x$targetLabelLk==x$cLabel)
x  = subset(bothpilot, list ==2&labelPresented=="tob")
table(x$targetLocation)
table(x$targetLabelLk==x$cLabel)
x  = subset(bothpilot, list ==3&labelPresented=="wug")
table(x$targetLocation)
table(x$targetLabelLk==x$cLabel)
# double check coding the target in left ones
x  = subset(bothpilot, list ==1&labelPresented=="wug")
table(x$targetLocation)
table(x$targetLabelLk==x$lLabel)
x  = subset(bothpilot, list ==2&labelPresented=="dep")
table(x$targetLocation)
table(x$targetLabelLk==x$lLabel)
x  = subset(bothpilot, list ==3&labelPresented=="tob")
table(x$targetLocation)
table(x$targetLabelLk==x$lLabel)
# double check coding the target in right ones
x  = subset(bothpilot, list ==1&labelPresented=="tob")
table(x$targetLocation)
table(x$targetLabelLk==x$rLabel)
x  = subset(bothpilot, list ==2&labelPresented=="wug")
table(x$targetLocation)
table(x$targetLabelLk==x$rLabel)
x  = subset(bothpilot, list ==3&labelPresented=="dep")
table(x$targetLocation)
table(x$targetLabelLk==x$rLabel)

## mismatch 1 coding double check
# double check coding the mm1 in center ones
x  = subset(bothpilot, list ==1&labelPresented=="wug"& frequency == "l")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$cLabel)
x  = subset(bothpilot, list ==2&labelPresented=="dep"& frequency == "l")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$cLabel)
x  = subset(bothpilot, list ==3&labelPresented=="tob"& frequency == "l")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$cLabel)
x  = subset(bothpilot, list ==1&labelPresented=="tob"& frequency == "h")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$cLabel)
x  = subset(bothpilot, list ==2&labelPresented=="wug"& frequency == "h")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$cLabel)
x  = subset(bothpilot, list ==3&labelPresented=="dep"& frequency == "h")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$cLabel)
# double check coding the mm1 in left ones
x  = subset(bothpilot, list ==1&labelPresented=="tob"& frequency == "l")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$lLabel)
x  = subset(bothpilot, list ==2&labelPresented=="wug"& frequency == "l")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$lLabel)
x  = subset(bothpilot, list ==3&labelPresented=="dep"& frequency == "l")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$lLabel)
x  = subset(bothpilot, list ==1&labelPresented=="dep"& frequency == "h")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$lLabel)
x  = subset(bothpilot, list ==2&labelPresented=="tob"& frequency == "h")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$lLabel)
x  = subset(bothpilot, list ==3&labelPresented=="wug"& frequency == "h")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$lLabel)
# double check coding the mm1 in right ones
x  = subset(bothpilot, list ==1&labelPresented=="dep"& frequency == "l")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$rLabel)
x  = subset(bothpilot, list ==2&labelPresented=="tob"& frequency == "l")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$rLabel)
x  = subset(bothpilot, list ==3&labelPresented=="wug"& frequency == "l")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$rLabel)
x  = subset(bothpilot, list ==1&labelPresented=="wug"& frequency == "h")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$rLabel)
x  = subset(bothpilot, list ==2&labelPresented=="dep"& frequency == "h")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$rLabel)
x  = subset(bothpilot, list ==3&labelPresented=="tob"& frequency == "h")
table(x$mismatch1Location)
table(x$mismatch1LabelLk==x$rLabel)

## mismatch 1 coding double check
# double check coding the mm2 in center ones
x  = subset(bothpilot, list ==1&labelPresented=="tob"& frequency == "l")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$cLabel)
x  = subset(bothpilot, list ==2&labelPresented=="wug"& frequency == "l")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$cLabel)
x  = subset(bothpilot, list ==3&labelPresented=="dep"& frequency == "l")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$cLabel)
x  = subset(bothpilot, list ==1&labelPresented=="wug"& frequency == "h")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$cLabel)
x  = subset(bothpilot, list ==2&labelPresented=="dep"& frequency == "h")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$cLabel)
x  = subset(bothpilot, list ==3&labelPresented=="tob"& frequency == "h")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$cLabel)
# double check coding the mm2 in left ones
x  = subset(bothpilot, list ==1&labelPresented=="dep"& frequency == "l")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$lLabel)
x  = subset(bothpilot, list ==2&labelPresented=="tob"& frequency == "l")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$lLabel)
x  = subset(bothpilot, list ==3&labelPresented=="wug"& frequency == "l")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$lLabel)
x  = subset(bothpilot, list ==1&labelPresented=="tob"& frequency == "h")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$lLabel)
x  = subset(bothpilot, list ==2&labelPresented=="wug"& frequency == "h")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$lLabel)
x  = subset(bothpilot, list ==3&labelPresented=="dep"& frequency == "h")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$lLabel)
# double check coding the mm1 in right ones
x  = subset(bothpilot, list ==1&labelPresented=="wug"& frequency == "l")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$rLabel)
x  = subset(bothpilot, list ==2&labelPresented=="dep"& frequency == "l")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$rLabel)
x  = subset(bothpilot, list ==3&labelPresented=="tob"& frequency == "l")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$rLabel)
x  = subset(bothpilot, list ==1&labelPresented=="dep"& frequency == "h")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$rLabel)
x  = subset(bothpilot, list ==2&labelPresented=="tob"& frequency == "h")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$rLabel)
x  = subset(bothpilot, list ==3&labelPresented=="wug"& frequency == "h")
table(x$mismatch2Location)
table(x$mismatch2LabelLk==x$rLabel)


##
## add in column for block
##
bothpilot$block = 0
bothpilot$block[bothpilot$trial >=10 & bothpilot$trial <= 39] = 1
bothpilot$block[bothpilot$trial >=60 & bothpilot$trial <= 66] = 2
bothpilot$block[bothpilot$trial >=65 & bothpilot$trial <= 90] = 3
bothpilot$block[bothpilot$trial >=91 & bothpilot$trial <= 119] = 6
bothpilot$block[bothpilot$trial >=120 & bothpilot$trial <= 165] = 5
bothpilot$block[bothpilot$trial >=166 & bothpilot$trial <= 176] = 6

##
## add in column for binned times (currently 50 ms) and columns for times relative to onset and offset of fribble
##
bin = 50
bothpilot$binTime50 = round_any(bothpilot$time, bin, f= ceiling)

bothpilot$timeRelFON = bothpilot$time
bothpilot$timeRelFON[bothpilot$screen_index==3]= bothpilot$time[bothpilot$screen_index==3]+600
bothpilot$timeRelFON[bothpilot$screen_index==6]= bothpilot$time[bothpilot$screen_index==6]+600+300
bothpilot$timeRelFON[bothpilot$screen_index==5]= bothpilot$time[bothpilot$screen_index==5]+600+300+1150
bothpilot$timeRelFOFF = bothpilot$timeRelFON -600

bothpilot$binTime50RelFON = bothpilot$binTime50
bothpilot$binTime50RelFON[bothpilot$screen_index==3]= bothpilot$binTime50[bothpilot$screen_index==3]+600
bothpilot$binTime50RelFON[bothpilot$screen_index==6]= bothpilot$binTime50[bothpilot$screen_index==6]+600+300
bothpilot$binTime50RelFON[bothpilot$screen_index==5]= bothpilot$binTime50[bothpilot$screen_index==5]+600+300+1150
bothpilot$binTime50RelFOFF = bothpilot$binTime50RelFON -600


####
bothpilot$pilotgroup = as.factor(bothpilot$pilotgroup)
library(dplyr)

bothpilot%>%
     filter(pilotgroup == "pilot1" & list==1 &  screen_index==2 & time == 0 & subjID == "3421453") %>%
     count(block,frequency)

subset = subset(bothpilot, pilotgroup == "pilot1" & list==1 &  screen_index==2 & time == 0 & subjID == "3421453")
count(subset, block,frequency,  )


bothpilot$block
list1.block1 = droplevels(subset(bothpilot, list== 1 & screen_index==2 & time == 0 & block ==1))
list2.block1 = droplevels(subset(bothpilot, list== 2 & screen_index==2 & time == 0 & block==1))
list3.block1 = droplevels(subset(bothpilot, list== 3 & screen_index==2 & time == 0 & block==1))
with(list1.block1, table(subjID, frequency))
with(list2.block1, table(subjID, frequency))
with(list3.block1, table(subjID, frequency))

list1.block2 = droplevels(subset(bothpilot, list== 1 & screen_index==2 & time == 0 & block ==2))
list2.block2 = droplevels(subset(bothpilot, list== 2 & screen_index==2 & time == 0 & block==2))
list3.block2 = droplevels(subset(bothpilot, list== 3 & screen_index==2 & time == 0 & block==2))
with(list1.block2, table(subjID, frequency))
with(list2.block2, table(subjID, frequency))
with(list3.block2, table(subjID, frequency))

list1.block3 = droplevels(subset(bothpilot, list== 1 & screen_index==2 & time == 0 & block ==3))
list2.block3 = droplevels(subset(bothpilot, list== 2 & screen_index==2 & time == 0 & block==3))
list3.block3 = droplevels(subset(bothpilot, list== 3 & screen_index==2 & time == 0 & block==3))
with(list1.block3, table(subjID, frequency))
with(list2.block3, table(subjID, frequency))
with(list3.block3, table(subjID, frequency))

list1.block6 = droplevels(subset(bothpilot, list== 1 & screen_index==2 & time == 0 & block ==6))
list2.block6 = droplevels(subset(bothpilot, list== 2 & screen_index==2 & time == 0 & block==6))
list3.block6 = droplevels(subset(bothpilot, list== 3 & screen_index==2 & time == 0 & block==6))
with(list1.block6, table(subjID, frequency))
with(list2.block6, table(subjID, frequency))
with(list3.block6, table(subjID, frequency))

list1.block5 = droplevels(subset(bothpilot, list== 1 & screen_index==2 & time == 0 & block ==5))
list2.block5 = droplevels(subset(bothpilot, list== 2 & screen_index==2 & time == 0 & block==5))
list3.block5 = droplevels(subset(bothpilot, list== 3 & screen_index==2 & time == 0 & block==5))
with(list1.block5, table(subjID, frequency))
with(list2.block5, table(subjID, frequency))
with(list3.block5, table(subjID, frequency))

list1.block6 = droplevels(subset(bothpilot, list== 1 & screen_index==2 & time == 0 & block ==6))
list2.block6 = droplevels(subset(bothpilot, list== 2 & screen_index==2 & time == 0 & block==6))
list3.block6 = droplevels(subset(bothpilot, list== 3 & screen_index==2 & time == 0 & block==6))
with(list1.block6, table(subjID, frequency))
with(list2.block6, table(subjID, frequency))
with(list3.block6, table(subjID, frequency))


table(bothpilot$trial,bothpilot$block)
str(bothpilot)
unique(subset(bothpilot,  subjID=="3430836" & frequency =="l" & screen_index ==2)$trial)

x=subset(bothpilot,  subjID=="3491839" & frequency =="l" & screen_index ==2 & trial == 111)
unique(x$binTime50)
     
bothpilot$frequency
subj1 = droplevels(subset(bothpilot,subjID == levels(bothpilot$subjID)[1]))
str(subj1)
dim(subset(subj1, frequency == "l"& time==0 & screen_index==2 & block==1))
dim(subset(subj1, frequency == "h"& time==0 & screen_index==2 & block==1))
dim(subset(subj1, frequency == "control"& time==0 & screen_index==2 & block==1))

dim(subset(subj1, frequency == "l"& time==0 & screen_index==2 & block==2))
dim(subset(subj1, frequency == "h"& time==0 & screen_index==2 & block==2))
dim(subset(subj1, frequency == "control"& time==0 & screen_index==2 & block==2))

dim(subset(subj1, frequency == "l"& time==0 & screen_index==2 & block==2))
dim(subset(subj1, frequency == "h"& time==0 & screen_index==2 & block==2))
dim(subset(subj1, frequency == "control"& time==0 & screen_index==2 & block==2))





lowMax = vector()
highMax=vector()
controlMax=vector()
i=1
subs = levels(as.factor(bothpilot$subjID))

for(  s in  subs){
  lowMax[i] = max(subset(bothpilot, subjID==s & frequency== "l"& screen_index == 6 )$time)
  highMax[i] = max(subset(bothpilot, subjID==s & frequency== "h"& screen_index == 6 )$time)
  controlMax[i]=  max(subset(bothpilot, subjID==s & frequency== "control"& screen_index == 6 )$time)
  i=i+1
}


barplot(c(mean(lowMax),mean(highMax), mean(controlMax)), xlab(c("low,"high","control""))

t.test(lowMax, highMax)

table(bothpilot$cITI, bothpilot$cLabel)
table(bothpilot$lITI, bothpilot$lLabel)
table(bothpilot$rITI, bothpilot$rLabel)

table(bothpilot$cButton,bothpilot$cLabel)
table(bothpilot$lButton,bothpilot$lLabel)
table(bothpilot$rButton,bothpilot$rLabel)

table(bothpilot$screen_index, bothpilot$cButton)
table(bothpilot$screen_index, bothpilot$cLabel)

subset(bothpilot, screen_index == 1)

screen3 = subset(bothpilot, screen_index ==3)
table(screen3$cLabel)

bothpilot$cITI= NULL
bothpilot$lITI= NULL
bothpilot$lITI= NULL

table(bothpilot$trial )


bothpilot$frequency
###

TrialsAgg = read.csv("eyetracker_bothPilots_aggByTrials.csv")
BlocksAgg = read.csv("eyetracker_bothPilots_aggByBlocks.csv")

str(BlocksAgg)
BlocksAgg$subjID = as.factor(BlocksAgg$subjID)
BlocksAgg$frequency = as.factor(BlocksAgg$frequency)
BlocksAgg$position = as.factor(BlocksAgg$position)
BlocksAgg$where = as.factor(BlocksAgg$where)

table(BlocksAgg$subjID,BlocksAgg$list )
lowMax = vector()
highMax=vector()
controlMax=vector()
i=1
subs = levels(BlocksAgg$subjID)

for(  s in  subs){
  lowMax[i] = max(subset(TrialsAgg, subjID==3396859 & frequency== "l"& screen_index == 6 )$time)
  highMax[i] = max(subset(TrialsAgg, subjID==3396859 & frequency== "h"& screen_index == 6 )$time)
  controlMax[i]=  max(subset(TrialsAgg, subjID==3396859 & frequency== "control"& screen_index == 6 )$time)
  i=i+1
}

mean(lowMax)
mean(highMax)
mean(controlMax)

subset(TrialsAgg, subjID== 3394859  & frequency== "l"& screen_index == 2 & time ==0 & where == "target")
subset(TrialsAgg, subjID==3396859 & frequency== "l"& screen_index == 6 & time ==0 & where == "target")
subset(TrialsAgg, frequency== "l", screen_index == "6")$time

table(TrialsAgg$screen_index)
