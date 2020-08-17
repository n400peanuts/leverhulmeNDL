library(ggplot2)

#### set local directory ####
localDir <- c("C:/Users/eva_v/Documents/University College London/Wonnacott, Elizabeth - leverhulmeNDL/")
#localDir <- c("C:/Users/liz/OneDrive - University College London/Eva_Liz_Leverhulme/leverhulmeNDL/")


#### load all the simulations ####
rbind(read.table(paste(localDir, "preReg/original_betas/40.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/original_betas/20.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/original_betas/80.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/original_betas/120.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/Half_beta3_interaction/40.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/Half_beta3_interaction/20.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/Half_beta3_interaction/80.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/Half_beta3_interaction/120.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/Half_beta2/40.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/Half_beta2/20.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/Half_beta2/80.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/Half_beta2/120.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/Half_beta2_half_beta3/40.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/Half_beta2_half_beta3/20.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/Half_beta2_half_beta3/80.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/Half_beta2_half_beta3/120.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta2_null_fullbeta3/40.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta2_null_fullbeta3/20.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta2_null_fullbeta3/80.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta2_null_fullbeta3/120.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta2_null_halfbeta3/40.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta2_null_halfbeta3/20.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta2_null_halfbeta3/80.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta2_null_halfbeta3/120.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta3_null_fullbeta2/40.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta3_null_fullbeta2/20.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta3_null_fullbeta2/80.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta3_null_fullbeta2/120.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta3_null_halfbeta2/40.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta3_null_halfbeta2/20.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta3_null_halfbeta2/80.csv", sep = ""), header = T),
      read.table(paste(localDir, "preReg/beta3_null_halfbeta2/120.csv", sep = ""), header = T))->results_collect

results_collect$CoefficientTested.fact <- as.factor(results_collect$CoefficientTested)
levels(results_collect$CoefficientTested.fact)
results_collect$BF.num <- as.numeric(results_collect$BF)

#### check for any BA values that are na ####

sum(is.na(subset(results_collect, CoefficientTested== "grandmean")$BF.num)*1 )
sum(is.na(subset(results_collect, CoefficientTested== "interaction")$BF.num)*1 )
sum(is.na(subset(results_collect, CoefficientTested== "maineffectFreq")$BF.num)*1 )
sum(is.na(subset(results_collect, CoefficientTested== "maineffectlearning")$BF.num)*1 )
sum(is.na(subset(results_collect, CoefficientTested== "LearninginLow")$BF.num)*1 )

sum(!is.finite(subset(results_collect, CoefficientTested== "grandmean")$BF.num)*1 )
sum(!is.finite(subset(results_collect, CoefficientTested== "interaction")$BF.num)*1 )
sum(!is.finite(subset(results_collect, CoefficientTested== "maineffectFreq")$BF.num)*1 )
sum(!is.finite(subset(results_collect, CoefficientTested== "maineffectlearning")$BF.num)*1 )
sum(!is.finite(subset(results_collect, CoefficientTested== "LearninginLow")$BF.num)*1 )

#### estimates of mike's original model ####
alpha <- 1.609935 # intercept
beta1 <- -1.702459 # main effect of frequency - note this is negative
beta2 <- 0.6625758 # main effect of learning
beta3 <- 1.026459 #interaction
simple_effect <- 1.175805 #low frequency condition 

#### scenario in which we have full alpha, full beta2, full beta 3 - ORIGINAL MODEL ####
## ----------------------------------------------------------------------------------##
#### ------- plot proportion of time CORRECTLY accept H1 for interaction --------------####
## ----------------------------------------------------------------------------------##

# subset to look at just the runs where original values of other fixed effects are as in original 
alphaX = alpha
beta1X = beta1 
beta2X = beta2
beta3X = beta3

table(results_collect$N_subj)

test_beta3 = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta1,3)==round(beta1X,3)& round(beta2,3)==round(beta2X,3) & round(beta3,3)==round(beta3X,3))
test_beta3= droplevels(subset(test_beta3, is.na(test_beta3$BF_cat)==FALSE)) 

table(test_beta3$N_subj, test_beta3$BF_cat)

#check whether the beta2 is really the full version -- i.e., .66
mean(as.numeric(test_beta3$model_beta2))

#check whether the data generation was able to approximate the original beta3 of the model
tapply(test_beta3$model_beta3, test_beta3$beta3, mean, na.rm=T) 

dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

beta3_means = c(beta3) 
beta3_h1sds = c(beta3, beta3/2)

sample_sizes = c(20,40,80,120)

for (beta3_mean in beta3_means) {
  for (beta3_h1sd in beta3_h1sds) {
    for (s in sample_sizes) {
      data <- subset(test_beta3, N_subj == s)
      data <-  subset(data, round(as.numeric(beta3),3) == round(beta3_mean,3))
      data <- subset(data, CoefficientTested.fact == "interaction") 
      data <-  subset(data, round(as.numeric(H1sd),3) == round(beta3_h1sd,3))
      dim(data)
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, data.frame(
        TrueEffect=beta3_mean, 
        H1sd=round(beta3_h1sd,3), 
        N_subj=s, 
        Prop_H1=prop))
    }
  }
}

print(dataSummary)

# work with factor versions of the parameters rounded to 3 decimals for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$H1sd = relevel(dataSummary$H1sd, ref = "0.5")
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))
dataSummary$TrueEffect = relevel(dataSummary$TrueEffect, ref = "1")

plottitle = "Interaction: Propotion BF>3 (100 runs) with full beta2  \n (original means is 1) "
p <- ggplot(data=dataSummary, 
            aes(x=N_subj, 
                y=Prop_H1, 
                colour=TrueEffect, 
                linetype=H1sd)) +
  ggtitle(plottitle) + 
  theme_bw() + 
  geom_line(aes(color = TrueEffect)) +
  scale_x_continuous("\nNumber of participants") +
  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p

pdf("interaction_original.pdf")
p
dev.off()


#### plot proportion of time CORRECTLY accept H1 for main effect of frequency ####
# subset to look at just the runs where original values of other fixed effects are as in original 

test_beta1 = subset(results_collect, round(results_collect$alpha,3) == round(alphaX,3) & round(results_collect$beta1,3)==round(beta1,3)& round(results_collect$beta3,3)==round(beta3,3)& round(results_collect$beta2,3)==round(beta2X,3))
test_beta1= droplevels(subset(test_beta1, is.na(test_beta1$BF_cat)==FALSE)) 

table(test_beta1$H1sd)

#check whether the beta1 is really the full version -- i.e., -1.7
mean(as.numeric(test_beta1$model_beta1))

#check whether the data generation was able to approximate the original beta2 of the model
tapply(test_beta1$model_beta2, test_beta1$beta2, mean, na.rm=T) 

#check whether the data generation was able to approximate the original beta3 of the model
tapply(test_beta1$model_beta3, test_beta1$beta3, mean, na.rm=T) 


dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

beta1_means = c(beta1)
beta1_h1sds = c(beta1, beta1/2)


for (beta1_mean in beta1_means) {
  for (beta1_h1sd in beta1_h1sds) {
    for (s in sample_sizes) {
      data <- subset(test_beta1, N_subj == s)
      data <-  subset(data, round(as.numeric(beta1),3) == round(beta1_mean,3))
      data <- subset(data, CoefficientTested.fact == "maineffectFreq") 
      data <-  subset(data, round(as.numeric(H1sd),3) == -1*round(beta1_h1sd,3))
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
 
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, 
                       data.frame(TrueEffect=beta1_mean, 
                                  H1sd=beta1_h1sd,N_subj=s, 
                                  Prop_H1=prop))
    }
  }
}

print(dataSummary)

# work with factor versions of the parameters rounded to 0.5/1 for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$H1sd = relevel(dataSummary$H1sd, ref = "-0.9")
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))
dataSummary$TrueEffect = relevel(dataSummary$TrueEffect, ref = "-1.7")
plottitle = "Main Effect Frequency: Propotion BF>3 (100 runs) main effect of frequency"
p <- ggplot(data=dataSummary,aes(x=N_subj, y=Prop_H1, colour=TrueEffect, linetype=H1sd)) + 
  ggtitle(plottitle) + 
  theme_bw() + 
  geom_line(aes(color = TrueEffect)) +
  scale_x_continuous("\nNumber of participants")  +
  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p

pdf("frequency_original.pdf")
p 
dev.off()


#### plot proportion of time CORRECTLY accept H1 for main effect of learning ####
# subset to look at just the runs where original values of other fixed effects are as in original 
alphaX = alpha
beta1X = beta1 
beta2X = beta2
beta3X = beta3

test_beta2 = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta2,3) == round(beta2X,3) & round(beta3,3)==round(beta3X,3)& round(beta1,3)==round(beta1X,3))
test_beta2= droplevels(subset(test_beta2, is.na(test_beta2$BF_cat)==FALSE)) 

#check whether the beta1 is really the full version -- i.e., .66
mean(as.numeric(test_beta2$model_beta2))

#check whether the data generation was able to approximate the original beta2 of the model
tapply(test_beta2$model_beta2, test_beta2$beta2, mean, na.rm=T) 

#check whether the data generation was able to approximate the original beta3 of the model
tapply(test_beta2$model_beta3, test_beta2$beta3, mean, na.rm=T) 

dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

beta2_means = c(beta2)
beta2_h1sds = c(beta2, beta2/2)


for (beta2_mean in beta2_means) {
  for (beta2_h1sd in beta2_h1sds) {
    for (s in sample_sizes) {
      data <- subset(test_beta2, N_subj == s)
      data <-  subset(data, round(as.numeric(beta2),3) == round(beta2_mean,3))
      data <- subset(data, CoefficientTested == "maineffectlearning") 
      data <-  subset(data, round(as.numeric(H1sd),3) == round(beta2_h1sd,3))
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, 
                           data.frame(TrueEffect=beta2_mean, 
                                      H1sd=beta2_h1sd, 
                                      N_subj=s, 
                                      Prop_H1=prop))
    }
  }
}

print(dataSummary)

# work with factor versions of the parameters rounded to 0.5/1 for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$H1sd = relevel(dataSummary$H1sd, ref = "0.3")
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))
dataSummary$TrueEffect = relevel(dataSummary$TrueEffect, ref = "0.7")
plottitle = "Main Effect Learning: Propotion BF>3 (100 runs) with half original mean and H1sd of the model (true mean .7"
p <- ggplot(data=dataSummary,
            aes(x=N_subj, 
                y=Prop_H1, 
                colour=TrueEffect, 
                linetype=H1sd)) +
     ggtitle(plottitle) + 
     theme_bw() + 
     geom_line(aes(color = TrueEffect)) +
     scale_x_continuous("\nNumber of participants")  +
     scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p


pdf("mainLearning_original.pdf")
p
dev.off()


#### plot proportion of time CORRECTLY accept H1 for  effect of learning in low frequency ####
# subset to look at just the runs where original values of other fixed effects are as in original 
alphaX = alpha
beta2X = beta2
beta3X = beta3
beta1X = beta1

test_orig = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta1,3)==round(beta1X,3))
test_orig= droplevels(subset(test_orig, is.na(test_orig$BF_cat)==FALSE)) 

dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

lfcond_means = c(simple_effect, simple_effect/2)
beta2_means = c(beta2, beta2/2)
lfcond_h1sds = c(simple_effect, simple_effect/2)

for (beta2_mean in beta2_means) {
  for (lfcond_mean in lfcond_means) {
    for (s in sample_sizes) {
      data <- subset(test_orig, N_subj == s)
      data <-  subset(data, round(as.numeric(beta2),3) == round(beta2_mean,3))
      data <- subset(data, CoefficientTested == "LearninginLow") 
      data <-  subset(data, round(as.numeric(H1sd),3) == round(lfcond_mean,3))
      
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, 
                           data.frame(TrueEffect=beta2_mean, 
                                      H1sd=lfcond_means, 
                                      N_subj=s, 
                                      Prop_H1=prop))
    }
  }
}


print(dataSummary)

# work with factor versions of the parameters rounded to 3 decimals for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$H1sd = relevel(dataSummary$H1sd, ref = "0.6")
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))
dataSummary$TrueEffect = relevel(dataSummary$TrueEffect, ref = "0.3")
plottitle = "Simple Effect Learning in lowfreq: Propotion BF>3 (100 runs) original estimate means and H1s"
p <- ggplot(data=dataSummary, 
            aes(x=N_subj, 
                y=Prop_H1, 
                colour=TrueEffect, 
                linetype=H1sd))+
  ggtitle(plottitle) + 
  theme_bw() + 
  geom_line(aes(color = TrueEffect)) +
  scale_x_continuous("\nNumber of participants") + 
  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p

pdf("LearningInLow_original.pdf")
p
dev.off()




#### scenario in which we have full alpha, beta/2, and we look at the interaction beta3 and beta3/2 ####
## ----------------------------------------------------------------------------------##
#### ------- plot proportion of time CORRECTLY accept H1 for interaction --------------####
## ----------------------------------------------------------------------------------##

alphaX = alpha
beta1X = beta1 
beta2X = beta2

table(results_collect$N_subj)

test_beta3 = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta1,3)==round(beta1X,3)& round(beta2,3)==round(beta2X,3))
test_beta3= droplevels(subset(test_beta3, is.na(test_beta3$BF_cat)==FALSE)) 

table(test_beta3$N_subj, test_beta3$BF_cat)

#check whether the beta2 is really the half-- i.e., .32
mean(as.numeric(test_beta3$model_beta2))

#check whether the data generation was able to approximate the original beta3 and beta3/2 of the model
tapply(test_beta3$model_beta3, test_beta3$beta3, mean, na.rm=T) 

dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

beta3_means = c(beta3, beta3/2) 
beta3_h1sds = c(beta3, beta3/2)

sample_sizes = c(20,40,80,120)

for (beta3_mean in beta3_means) {
  for (beta3_h1sd in beta3_h1sds) {
    for (s in sample_sizes) {
      data <- subset(test_beta3, N_subj == s)
      data <-  subset(data, round(as.numeric(beta3),3) == round(beta3_mean,3))
      data <- subset(data, CoefficientTested.fact == "interaction") 
      data <-  subset(data, round(as.numeric(H1sd),3) == round(beta3_h1sd,3))
      dim(data)
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, 
                           data.frame(TrueEffect=beta3_mean, 
                                      H1sd=beta3_h1sd, 
                                      N_subj=s, 
                                      Prop_H1=prop))
    }
  }
}

print(dataSummary)

# work with factor versions of the parameters rounded to 3 decimals for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$H1sd = relevel(dataSummary$H1sd, ref = "0.5")
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))
dataSummary$TrueEffect = relevel(dataSummary$TrueEffect, ref = "0.5")

plottitle = "Interaction: Propotion BF>3 \n (original means is 1) "
p <- ggplot(data=dataSummary, 
  aes(x=N_subj, 
      y=Prop_H1, 
      colour=TrueEffect, 
      linetype=H1sd)) +
  ggtitle(plottitle) + 
  theme_bw() + 
  geom_line(aes(color = TrueEffect)) +
  scale_x_continuous("\nNumber of participants") +
  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p

png("interaction_beta2_halfbeta3.png", res =200, width = 800)
p
dev.off()


#### plot proportion of time CORRECTLY accept H1 for main effect of frequency ####

test_beta1 = subset(results_collect, round(results_collect$alpha,3) == round(alphaX,3) & round(results_collect$beta3,3)==round(beta3,3)& round(results_collect$beta2,3)==round(beta2X/2,3))
test_beta1= droplevels(subset(test_beta1, is.na(test_beta1$BF_cat)==FALSE)) 

table(test_beta1$H1sd)

#check whether the beta1 is really the full version -- i.e., -1.7
mean(as.numeric(test_beta1$model_beta1))

#check whether the data generation was able to approximate the original beta2 of the model
tapply(test_beta1$model_beta2, test_beta1$beta2, mean, na.rm=T) 

#check whether the data generation was able to approximate the original beta3 of the model
tapply(test_beta1$model_beta3, test_beta1$beta3, mean, na.rm=T) #of course here we have also beta2/2 so won't be perfect


dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

beta1_means = c(beta1)
beta1_h1sds = c(beta1, beta1/2)


for (beta1_mean in beta1_means) {
  for (beta1_h1sd in beta1_h1sds) {
    for (s in sample_sizes) {
      data <- subset(test_beta1, N_subj == s)
      data <-  subset(data, round(as.numeric(beta1),3) == round(beta1_mean,3))
      data <- subset(data, CoefficientTested.fact == "maineffectFreq") 
      data <-  subset(data, round(as.numeric(H1sd),3) == -1*round(beta1_h1sd,3))
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, 
                           data.frame(TrueEffect=beta1_mean, 
                                      H1sd=beta1_h1sd,N_subj=s, 
                                      Prop_H1=prop))
    }
  }
}

print(dataSummary)

# work with factor versions of the parameters rounded 3 decimals for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$H1sd = relevel(dataSummary$H1sd, ref = "-0.9")
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))
dataSummary$TrueEffect = relevel(dataSummary$TrueEffect, ref = "-1.7")
plottitle = "Main Effect Frequency: Propotion BF>3 \n original means is -1.7"
p <- ggplot(data=dataSummary,aes(x=N_subj, y=Prop_H1, colour=TrueEffect, linetype=H1sd)) + 
  ggtitle(plottitle) + 
  theme_bw() + 
  geom_line(aes(color = TrueEffect)) +
  scale_x_continuous("\nNumber of participants")  +
  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p

png("frequency_halfbeta2.png", res = 150, width = 600)
p 
dev.off()


#### plot proportion of time CORRECTLY accept H1 for main effect of learning ####
# subset to look at just the runs where original values of other fixed effects are as in original 
alphaX = alpha
beta1X = beta1 
beta2X = beta2
beta3X = beta3

test_beta2 = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta3,3)==round(beta3X/2,3)& round(beta1,3)==round(beta1X,3))
test_beta2= droplevels(subset(test_beta2, is.na(test_beta2$BF_cat)==FALSE)) 

#check whether the data generation was able to approximate the original beta2 of the model
tapply(test_beta2$model_beta2, test_beta2$beta2, mean, na.rm=T) 

#check whether the data generation was able to approximate the original beta3 of the model
tapply(test_beta2$model_beta3, test_beta2$beta3, mean, na.rm=T) 

dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

beta2_means = c(beta2, beta2/2)
beta2_h1sds = c(beta2, beta2/2)


for (beta2_mean in beta2_means) {
  for (beta2_h1sd in beta2_h1sds) {
    for (s in sample_sizes) {
      data <- subset(test_beta2, N_subj == s)
      data <-  subset(data, round(as.numeric(beta2),3) == round(beta2_mean,3))
      data <- subset(data, CoefficientTested == "maineffectlearning") 
      data <-  subset(data, round(as.numeric(H1sd),3) == round(beta2_h1sd,3))
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, 
                           data.frame(TrueEffect=beta2_mean, 
                                      H1sd=beta2_h1sd, 
                                      N_subj=s, 
                                      Prop_H1=prop))
    }
  }
}

print(dataSummary)

# work with factor versions of the parameters rounded to 0.5/1 for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$H1sd = relevel(dataSummary$H1sd, ref = "0.3")
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))
dataSummary$TrueEffect = relevel(dataSummary$TrueEffect, ref = "0.7")
plottitle = "Main Effect Learning: Propotion BF>3 \n true mean is 0.7"
p <- ggplot(data=dataSummary,
            aes(x=N_subj, 
                y=Prop_H1, 
                colour=TrueEffect, 
                linetype=H1sd)) +
  ggtitle(plottitle) + 
  theme_bw() + 
  geom_line(aes(color = TrueEffect)) +
  scale_x_continuous("\nNumber of participants")  +
  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p


png("mainLearning_halfbeta2_halfbeta3.png", res = 150, width = 600)
p
dev.off()


#### plot proportion of time CORRECTLY accept H1 for  effect of learning in low frequency ####
# subset to look at just the runs where original values of other fixed effects are as in original 
alphaX = alpha
beta2X = beta2
beta3X = beta3
beta1X = beta1

test_orig = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta1,3)==round(beta1X,3) & round(beta2,3)==round(beta2X/2,3))
test_orig= droplevels(subset(test_orig, is.na(test_orig$BF_cat)==FALSE)) 

#check whether the data generation was able to approximate the original beta3 of the model
tapply(test_orig$model_beta3, test_orig$beta3, mean, na.rm=T) 

dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

lfcond_means = c(simple_effect, simple_effect/2)
beta3_means = c(beta3, beta3/2)
lfcond_h1sds = c(simple_effect, simple_effect/2)

for (beta3_mean in beta3_means) {
  for (lfcond_mean in lfcond_means) {
    for (s in sample_sizes) {
      data <- subset(test_orig, N_subj == s)
      data <-  subset(data, round(as.numeric(beta3),3) == round(beta3_mean,3))
      data <- subset(data, CoefficientTested == "LearninginLow") 
      data <-  subset(data, round(as.numeric(H1sd),3) == round(lfcond_mean,3))
      
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, 
                           data.frame(TrueEffect=beta3_mean, 
                                      H1sd=lfcond_means, 
                                      N_subj=s, 
                                      Prop_H1=prop))
    }
  }
}


print(dataSummary)

# work with factor versions of the parameters rounded to 3 decimals for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$H1sd = relevel(dataSummary$H1sd, ref = "0.6")
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))
dataSummary$TrueEffect = relevel(dataSummary$TrueEffect, ref = "0.5")
plottitle = "Simple Effect Learning in lowfreq: Propotion BF>3 \n original means is 1"
p <- ggplot(data=dataSummary, 
            aes(x=N_subj, 
                y=Prop_H1, 
                colour=TrueEffect, 
                linetype=H1sd))+
  ggtitle(plottitle) + 
  theme_bw() + 
  geom_line(aes(color = TrueEffect)) +
  scale_x_continuous("\nNumber of participants") + 
  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p

png("LearningInLow_halfbeta2.png", res = 150, width = 600)
p
dev.off()




#### scenario in which we have full alpha, beta, and we look at the interaction beta3 and beta3/2 ####
## ----------------------------------------------------------------------------------##
#### ------- plot proportion of time CORRECTLY accept H1 for interaction --------------####
## ----------------------------------------------------------------------------------##

# subset to look at just the runs where original values of other fixed effects are as in original 
alphaX = alpha
beta1X = beta1 
beta2X = beta2

table(results_collect$N_subj)

test_beta3 = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta1,3)==round(beta1X,3)& round(beta2,3)==round(beta2X,3))
test_beta3= droplevels(subset(test_beta3, is.na(test_beta3$BF_cat)==FALSE)) 

table(test_beta3$N_subj, test_beta3$BF_cat)

#check whether the beta2 is really the full version -- i.e., .66
mean(as.numeric(test_beta3$model_beta2))

#check whether the data generation was able to approximate the original beta3 and beta3/2 of the model
tapply(test_beta3$model_beta3, test_beta3$beta3, mean, na.rm=T) 

dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

beta3_means = c(beta3, beta3/2) 
beta3_h1sds = c(beta3)

sample_sizes = c(20,40,80,120)

for (beta3_mean in beta3_means) {
  for (beta3_h1sd in beta3_h1sds) {
    for (s in sample_sizes) {
      data <- subset(test_beta3, N_subj == s)
      data <-  subset(data, round(as.numeric(beta3),3) == round(beta3_mean,3))
      data <- subset(data, CoefficientTested.fact == "interaction") 
      data <-  subset(data, round(as.numeric(H1sd),3) == round(beta3_h1sd,3))
      dim(data)
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, data.frame(
        TrueEffect=beta3_mean, 
        H1sd=round(beta3_h1sd,3), 
        N_subj=s, 
        Prop_H1=prop))
    }
  }
}

print(dataSummary)

# work with factor versions of the parameters rounded to 3 decimals for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))

plottitle = "Interaction: Propotion BF>3\n with original means (1)\n or half (.5) "
p <- ggplot(data=dataSummary, 
            aes(x=N_subj, 
                y=Prop_H1, 
                colour=TrueEffect, 
                linetype=H1sd)) +
  ggtitle(plottitle) + 
  theme_bw() + 
  geom_line(aes(color = TrueEffect)) +
  scale_x_continuous("\nNumber of participants") +
  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p

png("interaction_fullbeta2_halfbeta3.png", res =200, width = 800)
p
dev.off()

#### plot proportion of time CORRECTLY accept H1 for main effect of frequency ####

test_beta1 = subset(results_collect, round(results_collect$alpha,3) == round(alphaX,3) & round(results_collect$beta3,3)==round(beta3,3)& round(results_collect$beta2,3)==round(beta2X,3))
test_beta1= droplevels(subset(test_beta1, is.na(test_beta1$BF_cat)==FALSE)) 

table(test_beta1$H1sd)

#check whether the beta1 is really the full version -- i.e., -1.7
mean(as.numeric(test_beta1$model_beta1))

#check whether the data generation was able to approximate the original beta2 of the model
tapply(test_beta1$model_beta2, test_beta1$beta2, mean, na.rm=T) 

#check whether the data generation was able to approximate the original beta3 of the model
tapply(test_beta1$model_beta3, test_beta1$beta3, mean, na.rm=T) #of course here we have also beta2/2 so won't be perfect


dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

beta1_means = c(beta1)
beta1_h1sds = c(beta1, beta1/2)


for (beta1_mean in beta1_means) {
  for (beta1_h1sd in beta1_h1sds) {
    for (s in sample_sizes) {
      data <- subset(test_beta1, N_subj == s)
      data <-  subset(data, round(as.numeric(beta1),3) == round(beta1_mean,3))
      data <- subset(data, CoefficientTested.fact == "maineffectFreq") 
      data <-  subset(data, round(as.numeric(H1sd),3) == -1*round(beta1_h1sd,3))
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, 
                           data.frame(TrueEffect=beta1_mean, 
                                      H1sd=beta1_h1sd,N_subj=s, 
                                      Prop_H1=prop))
    }
  }
}

print(dataSummary)

# work with factor versions of the parameters rounded 3 decimals for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$H1sd = relevel(dataSummary$H1sd, ref = "-0.9")
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))
dataSummary$TrueEffect = relevel(dataSummary$TrueEffect, ref = "-1.7")
plottitle = "Main Effect Frequency: Propotion BF>3 \n original means is -1.7"
p <- ggplot(data=dataSummary,aes(x=N_subj, y=Prop_H1, colour=TrueEffect, linetype=H1sd)) + 
  ggtitle(plottitle) + 
  theme_bw() + 
  geom_line(aes(color = TrueEffect)) +
  scale_x_continuous("\nNumber of participants")  +
  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p

png("frequency_halfbeta2.png", res = 150, width = 600)
p 
dev.off()


#### plot proportion of time CORRECTLY accept H1 for main effect of learning ####
# subset to look at just the runs where original values of other fixed effects are as in original 
alphaX = alpha
beta1X = beta1 
beta2X = beta2
beta3X = beta3

test_beta2 = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta3,3)==round(beta3X/2,3)& round(beta1,3)==round(beta1X,3))
test_beta2= droplevels(subset(test_beta2, is.na(test_beta2$BF_cat)==FALSE)) 

#check whether the data generation was able to approximate the original beta2 of the model
tapply(test_beta2$model_beta2, test_beta2$beta2, mean, na.rm=T) 

#check whether the data generation was able to approximate the original beta3 of the model
tapply(test_beta2$model_beta3, test_beta2$beta3, mean, na.rm=T) 

dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

beta2_means = c(beta2, beta2/2)
beta2_h1sds = c(beta2, beta2/2)


for (beta2_mean in beta2_means) {
  for (beta2_h1sd in beta2_h1sds) {
    for (s in sample_sizes) {
      data <- subset(test_beta2, N_subj == s)
      data <-  subset(data, round(as.numeric(beta2),3) == round(beta2_mean,3))
      data <- subset(data, CoefficientTested == "maineffectlearning") 
      data <-  subset(data, round(as.numeric(H1sd),3) == round(beta2_h1sd,3))
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, 
                           data.frame(TrueEffect=beta2_mean, 
                                      H1sd=beta2_h1sd, 
                                      N_subj=s, 
                                      Prop_H1=prop))
    }
  }
}

print(dataSummary)

# work with factor versions of the parameters rounded to 0.5/1 for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$H1sd = relevel(dataSummary$H1sd, ref = "0.3")
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))
dataSummary$TrueEffect = relevel(dataSummary$TrueEffect, ref = "0.7")
plottitle = "Main Effect Learning: Propotion BF>3 \n true mean is 0.7"
p <- ggplot(data=dataSummary,
            aes(x=N_subj, 
                y=Prop_H1, 
                colour=TrueEffect, 
                linetype=H1sd)) +
  ggtitle(plottitle) + 
  theme_bw() + 
  geom_line(aes(color = TrueEffect)) +
  scale_x_continuous("\nNumber of participants")  +
  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p


png("mainLearning_halfbeta2_halfbeta3.png", res = 150, width = 600)
p
dev.off()


#### plot proportion of time CORRECTLY accept H1 for  effect of learning in low frequency ####
# subset to look at just the runs where original values of other fixed effects are as in original 
alphaX = alpha
beta2X = beta2
beta3X = beta3
beta1X = beta1

test_orig = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta1,3)==round(beta1X,3) & round(beta2,3)==round(beta2X,3))
test_orig= droplevels(subset(test_orig, is.na(test_orig$BF_cat)==FALSE)) 

#check whether the data generation was able to approximate the original beta3 of the model
tapply(test_orig$model_beta3, test_orig$beta3, mean, na.rm=T) 

dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

lfcond_means = c(simple_effect, simple_effect/2)
beta3_means = c(beta3, beta3/2)
lfcond_h1sds = c(simple_effect, simple_effect/2)

for (beta3_mean in beta3_means) {
  for (lfcond_mean in lfcond_means) {
    for (s in sample_sizes) {
      data <- subset(test_orig, N_subj == s)
      data <-  subset(data, round(as.numeric(beta3),3) == round(beta3_mean,3))
      data <- subset(data, CoefficientTested == "LearninginLow") 
      data <-  subset(data, round(as.numeric(H1sd),3) == round(lfcond_mean,3))
      
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, 
                           data.frame(TrueEffect=beta3_mean, 
                                      H1sd=lfcond_means, 
                                      N_subj=s, 
                                      Prop_H1=prop))
    }
  }
}


print(dataSummary)

# work with factor versions of the parameters rounded to 3 decimals for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$H1sd = relevel(dataSummary$H1sd, ref = "0.6")
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))
dataSummary$TrueEffect = relevel(dataSummary$TrueEffect, ref = "0.5")
plottitle = "Simple Effect Learning in lowfreq: Propotion BF>3 \n original means is 1"
p <- ggplot(data=dataSummary, 
            aes(x=N_subj, 
                y=Prop_H1, 
                colour=TrueEffect, 
                linetype=H1sd))+
  ggtitle(plottitle) + 
  theme_bw() + 
  geom_line(aes(color = TrueEffect)) +
  scale_x_continuous("\nNumber of participants") + 
  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p

png("LearningInLow_halfbeta2.png", res = 150, width = 600)
p
dev.off()


#### scenario in which we have full alpha, full beta3 but NULL BETA2 ####
## ----------------------------------------------------------------------------------##
#### ------- plot proportion of time CORRECTLY accept H1 for interaction --------------####
## ----------------------------------------------------------------------------------##

alphaX = alpha
beta1X = beta1 
beta2X = 0

table(results_collect$N_subj)

test_beta3 = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta1,3)==round(beta1X,3)& round(beta2,3)==round(beta2X,3))
test_beta3= droplevels(subset(test_beta3, is.na(test_beta3$BF_cat)==FALSE)) 

table(test_beta3$N_subj, test_beta3$BF_cat)

#check whether the beta2 is really null -- i.e., 0
tapply(test_beta3$model_beta2, test_beta3$beta2, mean, na.rm=T) 

#check whether the data generation was able to approximate the original beta3 and beta3/2 of the model
tapply(test_beta3$model_beta3, test_beta3$beta3, mean, na.rm=T) 

dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

beta3_means = c(beta3, beta3/2) 
beta3_h1sds = c(beta3, beta3/2)

sample_sizes = c(20,40,80,120)

for (beta3_mean in beta3_means) {
  for (beta3_h1sd in beta3_h1sds) {
    for (s in sample_sizes) {
      data <- subset(test_beta3, N_subj == s)
      data <-  subset(data, round(as.numeric(beta3),3) == round(beta3_mean,3))
      data <- subset(data, CoefficientTested.fact == "interaction") 
      data <-  subset(data, round(as.numeric(H1sd),3) == round(beta3_h1sd,3))
      dim(data)
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, 
                           data.frame(TrueEffect=beta3_mean, 
                                      H1sd=beta3_h1sd, 
                                      N_subj=s, 
                                      Prop_H1=prop))
    }
  }
}

print(dataSummary)

# work with factor versions of the parameters rounded to 3 decimals for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$H1sd = relevel(dataSummary$H1sd, ref = "0.5")
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))
dataSummary$TrueEffect = relevel(dataSummary$TrueEffect, ref = "0.5")

plottitle = "Interaction: Propotion BF>3 \n (original means is 1) "
p <- ggplot(data=dataSummary, 
            aes(x=N_subj, 
                y=Prop_H1, 
                colour=TrueEffect, 
                linetype=H1sd)) +
  ggtitle(plottitle) + 
  theme_bw() + 
  geom_line(aes(color = TrueEffect)) +
  scale_x_continuous("\nNumber of participants") +
  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p

png("interaction_beta2NULL.png", res =200, width = 800)
p
dev.off()

#### plot proportion of time CORRECTLY accept H1 for  effect of learning in low frequency ####
# subset to look at just the runs where original values of other fixed effects are as in original 
alphaX = alpha
beta2X = 0
beta3X = beta3
beta1X = beta1

test_orig = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta1,3)==round(beta1X,3) & round(beta2,3)==round(beta2X,3))
test_orig= droplevels(subset(test_orig, is.na(test_orig$BF_cat)==FALSE)) 

#check whether the data generation was able to approximate the original beta3 of the model
tapply(test_orig$model_beta3, test_orig$beta3, mean, na.rm=T) 

dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

lfcond_means = c(simple_effect, simple_effect/2)
beta3_means = c(beta3, beta3/2)
lfcond_h1sds = c(simple_effect, simple_effect/2)

for (beta3_mean in beta3_means) {
  for (lfcond_mean in lfcond_means) {
    for (s in sample_sizes) {
      data <- subset(test_orig, N_subj == s)
      data <-  subset(data, round(as.numeric(beta3),3) == round(beta3_mean,3))
      data <- subset(data, CoefficientTested == "LearninginLow") 
      data <-  subset(data, round(as.numeric(H1sd),3) == round(lfcond_mean,3))
      
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, 
                           data.frame(TrueEffect=beta3_mean, 
                                      H1sd=lfcond_means, 
                                      N_subj=s, 
                                      Prop_H1=prop))
    }
  }
}


print(dataSummary)

# work with factor versions of the parameters rounded to 3 decimals for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$H1sd = relevel(dataSummary$H1sd, ref = "0.6")
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))
dataSummary$TrueEffect = relevel(dataSummary$TrueEffect, ref = "0.5")
plottitle = "Simple Effect Learning in lowfreq: Propotion BF>3 \n original means is 1"
p <- ggplot(data=dataSummary, 
            aes(x=N_subj, 
                y=Prop_H1, 
                colour=TrueEffect, 
                linetype=H1sd))+
  ggtitle(plottitle) + 
  theme_bw() + 
  geom_line(aes(color = TrueEffect)) +
  scale_x_continuous("\nNumber of participants") + 
  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p

png("LearningInLow_beta2NULL.png", res = 150, width = 600)
p
dev.off()

#### scenario in which we have full alpha, beta2 and beta2/half but NULL BETA3 ####
## ----------------------------------------------------------------------------------##
#### ------- plot proportion of time CORRECTLY accept H1 for interaction --------------####
## ----------------------------------------------------------------------------------##

alphaX = alpha
beta1X = beta1 
beta2X = beta2
beta3X = 0

table(results_collect$N_subj)

test_beta3 = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta1,3)==round(beta1X,3) & round(beta3,3)==round(beta3X,3) & round(beta2,3) == round(beta2X,3))
test_beta3= droplevels(subset(test_beta3, is.na(test_beta3$BF_cat)==FALSE)) 

table(test_beta3$N_subj, test_beta3$BF_cat)

#check whether the beta2 is full or half beta2 -- i.e., .33 and .66
tapply(test_beta3$model_beta2, test_beta3$beta2, mean, na.rm=T) 

#check whether the data generation was able to approximate the beta3 NULL
tapply(test_beta3$model_beta3, test_beta3$beta3, mean, na.rm=T) 

dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

beta2_means = c(beta3X) 
beta2_h1sds = c(beta3)

sample_sizes = c(20,40,80,120)

for (beta2_mean in beta2_means) {
  for (beta2_h1sd in beta2_h1sds) {
    for (s in sample_sizes) {
      data <- subset(test_beta3, N_subj == s)
      data <-  subset(data, round(as.numeric(beta3),3) == round(beta2_mean,3))
      data <- subset(data, CoefficientTested.fact == "interaction") 
      data <-  subset(data, round(as.numeric(H1sd),3) == round(beta2_h1sd,3))
      dim(data)
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category < 5) { # >5 for H1, <5 for H0
          prop = prop + 1
        }
      }
      
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, 
                           data.frame(TrueEffect=beta2_mean, 
                                      H1sd=beta2_h1sd, 
                                      N_subj=s, 
                                      Prop_H1=prop))
    }
  }
}

print(dataSummary)

# work with factor versions of the parameters rounded to 3 decimals for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$H1sd = relevel(dataSummary$H1sd, ref = "0.5")
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))
dataSummary$TrueEffect = relevel(dataSummary$TrueEffect, ref = "0.3")

plottitle = "Interaction: Num of times I accept H0 \n when true effect is 0 "
p <- ggplot(data=dataSummary, 
            aes(x=N_subj, 
                y=Prop_H1, 
                colour=TrueEffect,
                linetype=H1sd)) +
  ggtitle(plottitle) + 
  theme_bw() + 
  geom_line(aes(color = TrueEffect)) +
  scale_x_continuous("\nNumber of participants") +
  scale_y_continuous("Proportion of runs where BF < 1/3\n", limits = c(0,1)) 
p

png("interaction_beta3NULL_H0.png", res =200, width = 800, height = 600)
p
dev.off()

#### plot proportion of time CORRECTLY accept H1 for  effect of learning in low frequency ####
# subset to look at just the runs where original values of other fixed effects are as in original 
alphaX = alpha
beta2X = beta2
beta3X = 0
beta1X = beta1

test_orig = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta1,3)==round(beta1X,3) & round(beta3,3)==round(beta3X,3) )
test_orig= droplevels(subset(test_orig, is.na(test_orig$BF_cat)==FALSE)) 

#check whether the beta2 is full or half beta2 -- i.e., .33 and .66
tapply(test_beta3$model_beta2, test_beta3$beta2, mean, na.rm=T) 

#check whether the data generation was able to approximate the beta3 NULL
tapply(test_beta3$model_beta3, test_beta3$beta3, mean, na.rm=T) 

dataSummary <- data.frame(matrix(0, ncol=4, nrow=0))
names(dataSummary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

lfcond_means = c(simple_effect, simple_effect/2)
beta2_means = c(beta2, beta2/2)
lfcond_h1sds = c(simple_effect, simple_effect/2)

for (beta2_mean in beta2_means) {
  for (lfcond_mean in lfcond_means) {
    for (s in sample_sizes) {
      data <- subset(test_orig, N_subj == s)
      data <-  subset(data, round(as.numeric(beta2),3) == round(beta2_mean,3))
      data <- subset(data, CoefficientTested == "LearninginLow") 
      data <-  subset(data, round(as.numeric(H1sd),3) == round(lfcond_mean,3))
      
      prop = 0
      
      for (row in 1:nrow(data)) {
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      
      prop = prop / nrow(data)
      dataSummary <- rbind(dataSummary, 
                           data.frame(TrueEffect=beta2_mean, 
                                      H1sd=lfcond_means, 
                                      N_subj=s, 
                                      Prop_H1=prop))
    }
  }
}


print(dataSummary)

# work with factor versions of the parameters rounded to 3 decimals for printing purposes
dataSummary$H1sd = as.factor(round(dataSummary$H1sd,1))
dataSummary$H1sd = relevel(dataSummary$H1sd, ref = "0.6")
dataSummary$TrueEffect = as.factor(round(dataSummary$TrueEffect,1))
dataSummary$TrueEffect = relevel(dataSummary$TrueEffect, ref = "0.3")
plottitle = "Simple Effect Learning in lowfreq: Propotion BF>3 \n original means is 1"
p <- ggplot(data=dataSummary, 
            aes(x=N_subj, 
                y=Prop_H1, 
                colour=TrueEffect, 
                linetype=H1sd))+
  ggtitle(plottitle) + 
  theme_bw() + 
  geom_line(aes(color = TrueEffect)) +
  scale_x_continuous("\nNumber of participants") + 
  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p

png("LearningInLow_beta3NULL.png", res = 150, width = 600)
p
dev.off()

