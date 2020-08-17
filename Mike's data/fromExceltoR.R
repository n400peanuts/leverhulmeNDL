# this script load the xlsx file and convert it into a tidy format for R

# load data
library(magrittr)
localGitDir <- 'C:/Users/eva_v/Documents/GitHub/leverhulmeNDL/prereg'

#sheet1
temp<-readxl::read_xlsx(paste(localGitDir, "/XS-SX-ALL.xlsx", sep = ""), col_names = F, sheet = 1, trim_ws = T)
temp<-as.data.frame(temp)

# XS - XS
subjPos <- seq(3,101,6)
accPos <- seq(5,101,6)

fmriData <- NULL
xs_xs<-NULL
for (i in 1:length(subjPos)){
  xs_xs <- data.frame(
    subjID = c(rep(temp[132, subjPos[i]],56)),
    frequency = temp[3:58,2],
    learning = c("xs"),
    testing = c("xs"),
    trialType = c(rep("control",8),rep("exp",48)),
    acc = c(temp[3:58, accPos[i]])
  )
  fmriData <- rbind(xs_xs,fmriData)
}
fmriData %<>% dplyr::mutate_if(is.character,as.factor)

# XS - SX
subjPos <- seq(3,101,6)
accPos <- seq(5,101,6)

fmriData1 <- NULL
xs_sx<-NULL
for (i in 1:length(subjPos)){
  xs_sx <- data.frame(
    subjID = c(rep(temp[132, subjPos[i]],56)),
    frequency = temp[3:58,2],
    learning = c("xs"),
    testing = c("sx"),
    trialType = c(rep("control",8),rep("exp",48)),
    acc = c(temp[74:129, accPos[i]])
  )
  fmriData1 <- rbind(xs_sx,fmriData1)
}
fmriData1 %<>% dplyr::mutate_if(is.character,as.factor)

aggregate(acc ~ subjID + trialType + frequency, data = fmriData1, mean)

rbind(fmriData, fmriData1)-> XSlearning
rm(fmriData, fmriData1, temp,i)


# sheet 3
temp<-readxl::read_xlsx(paste(localGitDir, "/XS-SX-ALL.xlsx", sep = ""), col_names = F, sheet = 3, trim_ws = T)
temp<-as.data.frame(temp)


# SX - SX
subjPos <- seq(3,101,6)
accPos <- seq(5,101,6)

fmriData <- NULL
sx_sx<-NULL
for (i in 1:length(subjPos)){
  sx_sx <- data.frame(
    subjID = c(rep(temp[132, subjPos[i]],56)),
    frequency = temp[3:58,2],
    learning = c("sx"),
    testing = c("sx"),
    trialType = c(rep("control",8),rep("exp",48)),
    acc = c(temp[3:58, accPos[i]])
  )
  fmriData <- rbind(sx_sx,fmriData)
}
fmriData %<>% dplyr::mutate_if(is.character,as.factor)

# SX - XS
subjPos <- seq(3,101,6)
accPos <- seq(5,101,6)

fmriData1 <- NULL
sx_xs<-NULL
for (i in 1:length(subjPos)){
  sx_xs <- data.frame(
    subjID = c(rep(temp[132, subjPos[i]],56)),
    frequency = temp[3:58,2],
    learning = c("sx"),
    testing = c("xs"),
    trialType = c(rep("control",8),rep("exp",48)),
    acc = c(temp[74:129, accPos[i]])
  )
  fmriData1 <- rbind(sx_xs,fmriData1)
}
fmriData1 %<>% dplyr::mutate_if(is.character,as.factor)

aggregate(acc ~ subjID + trialType + frequency, data = fmriData1[fmriData1$subjID==2,], mean)

rbind(fmriData, fmriData1)-> SXlearning
rm(fmriData, fmriData1, temp,i, xs_xs, sx_sx, sx_xs, xs_sx, subjPos, accPos)

rbind(SXlearning, XSlearning)-> fmri
write.table(fmri, paste(localGitDir, "/fmri.txt", sep = ""), col.names = T, row.names = F, quote = F)
