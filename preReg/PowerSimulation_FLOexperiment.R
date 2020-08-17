rm(list = ls())
# dplyr for data manipulation
library(dplyr)
# lme4 for glmers
library(lme4)
# ggplot2 for plotting
library(ggplot2)
# MASS for generating multivariate random distributions (for subject effects)
library(MASS)
# boot for inv.logit
library(boot)
# plotrix for standard error
library(plotrix)
#emmeans to get estimates of the interactions
library(emmeans)

#### set local directory ####
localDir <- c("C:/Users/eva_v/Documents/University College London/Wonnacott, Elizabeth - leverhulmeNDL/")
#localDir <- c("C:/Users/liz/OneDrive - University College London/Eva_Liz_Leverhulme/leverhulmeNDL/")

#### load functions ####
source(paste(localDir, "tools/loadFunctionsGithub.R", sep = "")) 

urlFolder <- 'https://api.github.com/repos/n400peanuts/languagelearninglab/git/trees/master?recursive=1'
urlRaw <- 'https://raw.githubusercontent.com/n400peanuts/languagelearninglab/master/tools/'
listFunctions <- c( "inverse_log_odd.R", "myCenter.R", 
                    "lizCenter.R", "getmode.R", "lizCenter2.R", "deleteRandomRows.R")

loadFunctionsGithub(urlFolder, urlRaw, listFunctions)

generate_bin <- function(n_subj, n_obs, alpha, beta1, beta2, beta3, subj_corrs, subj_tau) {
  # make data frame where number of rows = number of subjects * number of trials per subject
  data <- data.frame(matrix(0, ncol=0, nrow = n_subj * n_obs))
  # make subject vector and add to this data frame
  data$subject <- as.factor(rep(seq(1:n_subj), each = n_obs))
  # make condition 1 vector - within subjects
  # half trials one value, half trials the other
  data$frequency <- as.factor(rep(c(0,1), each = n_obs/4)) #this is within
  # make centred version
  data$c_freq <- as.numeric(data$frequency) - mean(as.numeric(data$frequency))
  # make condition 2 vector - also within subjects
  # 1/4 trials one value, 1/4 trials the other, then repeat
  # this ensures cond1 and cond2 are not identical
  data$learning <- as.factor(rep(c(1,0), each = n_obs/2)) 
  # make centred version
  data$c_learn <- as.numeric(data$learning) - mean(as.numeric(data$learning))
  # for subject effects
  # first, we put the correlations between the random effects in a matrix
  # * if changing to simulate fewer random effects & hence fewer correlations, 
  # this will need to be adjusted
  corr_matrix <- matrix(c(1, subj_corrs[1],  
                          subj_corrs[1], 1), nrow = 2)
  # next, construct variance covariance matrix for subject effects
  # We multiply the subject effect sds (in matrix form) by the correlation matrix
  # and then again by the subject effect sds
  # so we end up with the sds squared (on the diagonal) = variance, 
  # and covariances between each pair of subject effects on the off-diagonal
  # * if changing to simulate fewer random effects, this should still work fine,
  # assuming corr_matrix has been adjusted appropriately
  subj_v_cov <- diag(subj_tau) %*% corr_matrix %*% diag(subj_tau)
  # Create the correlated subject effects, using mvrnorm to sample from multivariate normal distribution
  # means of subject intercepts and slopes are 0
  u <- mvrnorm(n = n_subj, c(0,0), subj_v_cov) #changed here c() because we have only 2
  # check the correlation - this should be fairly accurate in large samples
  # print(cor(u)) 
  # check the SDs - again, should be fairly accurate in large samples
  # print(sd(u[,1]))
  # print(sd(u[,2]))
  # finally, generate data on the basis of these parameters
  data <- data %>%
    mutate(
      # We first calculate the linear predictor eta for each row in the data frame
      # = overall intercept + subject intercept +
      eta = alpha + u[data$subject,1] +
        # cond1 value * (cond1 fixed effect + cond1 random slope) +
        data$c_freq * (beta1 + u[data$subject,2]) + 
        # cond2 value * (cond2 fixed effect + cond2 random slope) +
        data$c_learn * (beta2 ) +
        # cond1 * cond2 value * (interaction fixed effect + interaction random slope) +
        (data$c_freq * data$c_learn) * (beta3 ),
      # then transform by inverse logit to a probability (for this combo of subject/condition)
      mu = inv.logit(eta),
      # finally, generate a 0 or 1 for this row based on the probability for this subject/condition
      y = rbinom(nrow(data),1,mu))
  return(data)
}



Bf<-function(sd, obtained, dfdata = 1, likelihood = c("normal", "t"), modeloftheory= c("normal","t","cauchy", "uniform") ,lower =0, upper=1, modeoftheory = 0, scaleoftheory = 1, dftheory = 1, tail = 2)
{
  # if using normal likelihood, set dfdata to high value
  # this means t distributions used for likelihood below will approximate to normal
  if(likelihood=="normal"){
    dfdata=10^10
  }
  # likewise for if using normal theory
  if(modeloftheory=="normal"){
    dftheory = 10^10
    # if using cauchy, set df to 1
    # this means t distributions used for theory below will turn into Cauchys
    # Cauchy is t distrib with df = 1
  } else if(modeloftheory=="cauchy"){
    dftheory = 1
  }
  # set area to 0
  area <- 0
  normarea <- 0
  # if a uniform distribution is specified by theory
  if(modeloftheory=="uniform"){
    # set theta = lower bound
    theta <- lower
    # set range = difference between upper and lower bound
    range <- upper - lower
    # increment = 1/2000th of the range
    incr <- range / 2000
    # from -1000 to +1000 - A isn't used, so this is equivalent to just counting from 0 to 2000
    # this  goes through the uniform dist by increments
    for (A in -1000:1000){
      # add the increment to theta
      theta <- theta + incr
      # for a uniform distribution, the probability density is constant: 1/range
      dist_theta <- 1 / range
      # we multiply this by the density at the obtained mean in the distrib centred around current theta with observed SE
      # this distrib is either normal, t or cauchy as determined by the degrees of freedom
      # in short: if this was the true theta, how likely are the data under this?
      height <- dist_theta * dt((obtained-theta)/sd, df=dfdata)
      # we then want the area under the line
      area <- area + height * incr
      # so what this is calculating is the combo of 'how probable is this theta according to the prior'
      # and 'how likely is this mean (assuming an accurate SE) under the current theta?'
      # i.e., the marginal likelihood
    }
    # assign the overall marginal likelihood to LikelihoodTheory
    LikelihoodTheory <- area
  }else{
    # if normal theta, go through the theta distribution = your expected H1 mean distribution
    # set theta initially to mode minus 10 * the scale parameter
    theta <- modeoftheory - 10 * scaleoftheory
    # increment is 1/200th of the scale parameter
    incr <- scaleoftheory/200
    # from -2000 to +2000 - again, A isn't used, so equiv of counting from 0 to 4000
    # the resulting calculations therefore end at mean plus 10 * scale parameter
    for (A in -2000:2000){
      # add increment
      theta <- theta + incr
      # here, the theoretical height is t density at (theta - mode)/scale
      # i.e. probability of the t-statistic corresponding to this theta relative to mode
      dist_theta <- dt((theta-modeoftheory)/scaleoftheory, df=dftheory)
      # are we specifying 1 tail?
      if(identical(tail, 1)){
        # if current theta is less than the mode, set density to 0
        if (theta <= modeoftheory){
          dist_theta <- 0
        } else {
          # otherwise we multiply density by 2
          dist_theta <- dist_theta * 2
        }
      }
      # if this was the true value of theta,
      # what's the likelihood of the t-statistic corresponding to the observed mean & SE
      # in a t distribution centred on this theta?
      # multiply this by dist_theta to combine prior probability and likelihood
      height <- dist_theta * dt((obtained-theta)/sd, df = dfdata)
      # add to accumulated area
      area <- area + height * incr
      # also add to normarea
      # pretty sure normarea will always just equate to the scale of theory, in normal case
      normarea <- normarea + dist_theta*incr
    }
    LikelihoodTheory <- area/normarea
    # print(LikelihoodTheory)
  }
  # likelihood of null is simply likelihood of the observed t in t distrib centred on 0
  Likelihoodnull <- dt(obtained/sd, df = dfdata)
  # print(Likelihoodnull)
  BayesFactor <- LikelihoodTheory/Likelihoodnull
  BayesFactor
}

bf_cat <- function(bf) {
  # categorise one-tailed BFs
  if (bf > 100) {
    # extreme evidence for H1
    cat = 9
  } else if (bf > 30) {
    # very strong evidence for H1
    cat = 8
  } else if (bf > 10) {
    # strong evidence for H1
    cat = 7
  } else if (bf > 3) {
    # moderate evidence for H1
    cat = 6
  } else if (bf > 1/3) {
    # no evidence to speak of
    cat = 5
  } else if (bf > 1/10) {
    # moderate evidence for H0
    cat = 4
  } else if (bf > 1/30) {
    # strong evidence for H0
    cat = 3
  } else if (bf > 1/100) {
    # very strong evidence for H0
    cat = 2
  } else {
    # extreme evidence for H0
    cat = 1
  }
  return(cat)
}

#### Run original model ####
#load data
read.table(paste(localDir, "preReg/fmri.txt", sep = ""), header = T, stringsAsFactors = T)-> fmri

#remove bad subjs
fmri<-fmri[!(fmri$subjID==1  & fmri$learning=="sx") &
             !(fmri$subjID==14 & fmri$learning=="sx") &
             !(fmri$subjID==22 & fmri$learning=="sx") &
             !(fmri$subjID==3 & fmri$learning=="sx") &
             !(fmri$subjID==4 & fmri$learning=="xs") &
             !(fmri$subjID==7 & fmri$learning=="xs") &
             !(fmri$subjID==10 & fmri$learning=="xs")&
             !(fmri$subjID==12 & fmri$learning=="xs"),]

fmri <- fmri[(fmri$trialType!="control"),]
#get means
round(tapply(fmri$acc, list(fmri$learning,fmri$frequency), mean, na.rm=T),3)  

#center variables, we don't have NAs so I'm using lizCenter
fmri<-lizCenter(fmri, listfname = list("learning", "frequency"))

## ----------------------------------------------------------------------------------##
## --------------------------------- original model ------------------------------------##
## ----------------------------------------------------------------------------------##
fmriglmer1_V1<-glmer(acc ~  frequency.ct*learning.ct + (frequency.ct|subjID), 
                     data = fmri, 
                     family="binomial",
                     control=glmerControl(optimizer = "bobyqa"))

round(summary(fmriglmer1_V1)$coefficients,4)

# Estimate Std. Error z value Pr(>|z|)
# (Intercept)              2.4612     0.1732 14.2122   0.0000
# frequencyl              -1.7025     0.2483 -6.8555   0.0000
# learning.ct              0.1493     0.2149  0.6948   0.4872
# frequency.ct:learning.ct   1.0265     0.2615  3.9254   0.0001


## ----------------------------------------------------------------------------------##
## --------------------------------- simple effects ------------------------------------##
## ----------------------------------------------------------------------------------##


fmriglmer1_V2<-glmer(acc ~  frequency.ct+ frequency: learning.ct + (frequency.ct|subjID), 
                     data = fmri, 
                     family="binomial",
                     control=glmerControl(optimizer = "bobyqa"))

anova(fmriglmer1_V1,fmriglmer1_V2)

#npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)    
#fmriglmer1_V1    7 2265.4 2306.4 -1125.7   2251.4                        
#fmriglmer1_V2    7 2265.4 2306.4 -1125.7   2251.4     0  0  < 2.2e-16 ***

# this shows these are the same model

round(summary(fmriglmer1_V2)$coefficients,4)

# Estimate Std. Error z value Pr(>|z|)
# (Intercept)              1.6099     0.1891  8.5136   0.0000
# frequency.ct            -1.7025     0.2483 -6.8552   0.0000
# frequencyh:learning.ct   0.1493     0.2150  0.6947   0.4872
# frequencyl:learning.ct   1.1758     0.1581  7.4372   0.0000

#We get the same estimates with emmeans with the first model
fmriglmer1.emm <- emmeans(fmriglmer1_V1 , ~ frequency.ct*learning.ct )
contrast(fmriglmer1.emm, "consec",  simple = "each", combine = T)

#### get values from model used to simulate data ####
# contrast(model1.emm, "consec",  simple = "each", combine = T)
# learning.ct        frequency contrast                               estimate    SE  df z.ratio p.value
# -0.433333333333333 .         l - h                                    -2.147 0.266 Inf -8.059  <.0001 
# 0.566666666666667  .         l - h                                    -1.121 0.297 Inf -3.773  0.0006 
# .                  h         0.566666666666667 - -0.433333333333333    0.149 0.215 Inf  0.695  1.0000 
# .                  l         0.566666666666667 - -0.433333333333333    1.176 0.158 Inf  7.437  <.0001 

## ----------------------------------------------------------------------------------##
#### ----------------- store estimates of the original model --------------------------####
## ----------------------------------------------------------------------------------##

#We extract the relevant values as alpha, beta1 (first condition), beta2 (2nd condition), beta3 (interaction)

alpha = summary(fmriglmer1_V1)$coeff["(Intercept)", "Estimate"]
beta1 = summary(fmriglmer1_V1)$coeff["frequency.ct", "Estimate"]
beta2 = summary(fmriglmer1_V1)$coeff["learning.ct", "Estimate"]
beta3 = summary(fmriglmer1_V1)$coeff["frequency.ct:learning.ct", "Estimate"]
alpha
beta1 # note this is negative
beta2
beta3


# this is a table for the pre-registration
effectsTable<- matrix(c("estimate and standard error of intercept from LME model with frequency.ct * learning.ct",
                    "estimate and standard error of intercept from LME model with frequency.ct * learning.ct",
                    "estimate and standard error of intercept from LME model with frequency.ct * learning.ct",
                    "estimate and standard error of intercept from LME model with frequency.dummy : learning.ct",
                    "estimate and standard error of intercept from LME model with frequency.dummy : learning.ct",
           "main effect of frequency",
           "main effect of learning",
           "interaction frequency by learning",
           "frequency high by learning",
           "frequency low by learning",
             -round(summary(fmriglmer1_V1)$coefficients,4)[2], 
             round(summary(fmriglmer1_V1)$coefficients,4)[3],
             round(summary(fmriglmer1_V1)$coefficients,4)[4],
             round(summary(fmriglmer1_V2)$coefficients,4)[3],
             round(summary(fmriglmer1_V2)$coefficients,4)[4],
        round(summary(fmriglmer1_V1)$coefficients,4)[2,2],
        round(summary(fmriglmer1_V1)$coefficients,4)[3,2],
        round(summary(fmriglmer1_V1)$coefficients,4)[4,2],
        round(summary(fmriglmer1_V2)$coefficients,4)[3,2],
        round(summary(fmriglmer1_V2)$coefficients,4)[4,2]), ncol = 4)

colnames(effectsTable) <- c("Summary of the data from:", "Estimate of the mean for theory from:", "effects", "Std.error")
as.table(effectsTable)
## ----------------------------------------------------------------------------------##
## ------------------------------- random effects -----------------------------------##
## ----------------------------------------------------------------------------------##

#We have only one within participants variable:

VarCorr(fmriglmer1_V1)

# We create vector:   
#   subj_tau is the vector of SDs for the random effects
# [1] SD of subject intercepts
# [2] SD of subject frequency slope

subj_tau = vector(length = 2)

subj_tau[1]= attr(VarCorr(fmriglmer1_V1)$subjID, "stddev")[["(Intercept)"]] 
# [1] SD of subject intercepts

subj_tau[2]= attr(VarCorr(fmriglmer1_V1)$subjID,"stddev")[["frequency.ct"]]
# [2] SD of subject frequency slope

subj_tau

## ----------------------------------------------------------------------------------##
## ---------------------- correlations between random slopes ------------------------##
## ----------------------------------------------------------------------------------##
#we already have this info here for frequency:
subj_corrs = vector(length = 1)
subj_corrs[1] = attr(VarCorr(fmriglmer1_V1)$subjID,"corr")[2]
subj_corrs


#### generate some multilevel binary data ####

# number of subjects
n_subj = 40
# here we have 48 total trials per task per subject
n_obs = 48


data <- generate_bin(n_subj, n_obs, alpha, beta1, beta2, beta3, subj_corrs, subj_tau)


#### compute statistics from simulated data ####

model1 <- glmer(y ~ c_freq * c_learn + (c_freq|subject), family=binomial, data)
output = summary(model1)$coeff
round(output,5)

# get interaction estimate and SE
interaction_effect <- output["c_freq:c_learn", "Estimate"]
interaction_se <- output["c_freq:c_learn", "Std. Error"]

main_effect <- output["c_learn", "Estimate"]
main_se <- output["c_learn", "Std. Error"]

# obtain BF using 'true value' estimate

Bf(interaction_se, interaction_effect, #interaction
   likelihood = "normal", 
   modeloftheory = "normal", 
   modeoftheory = 0, 
   scaleoftheory = round(beta3,2), 
   tail = 1)


Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(beta2,2), 
   tail = 1)

#### Simulate data and compute BFs for different sample sizes, mean effect sizes and H1's ####
#estimates that we want to use to generate the data
means_lists = list(list(alpha, beta1, beta2/2, 0)) # beta2 null

# list(alpha,beta1,beta2/2,beta3), # beta2 half as big
# list(alpha, beta1, beta2, beta3/2)) #, # beta3 half as big
#list(alpha, beta1, 0, beta3),# beta2 null
#list(alpha,beta1,beta2,0)# beta3 null
#)

#estimates that we want to test against our simulated data
#these are coming from the original model with Mike's fmri data
#we choose to test full estimates or half of those 
#to get an alternative scenario where the effects are not as strons as Mike's
H1s_intercept = c(alpha,alpha/2) 
H1s_cond1 = c(beta1,beta1/2) *-1
H1s_cond2 = c(beta2,beta2/2) *1 
H1s_interaction = c(beta3,beta3/2) 
simple_effect = summary(fmriglmer1_V2)$coeff["frequencyl:learning.ct", "Estimate"] #this is the low frequency condition only
H1s_simple_effect = c(simple_effect,simple_effect/2)


results_collect <- data.frame(matrix(0, ncol=25, nrow=0)) #store results here

# Free parameters
n_obs=48
sample_sizes = c(120)
runs = 100


for (s in sample_sizes) {
  print(sprintf('current sample size: %s', s))
  for(i in 1: length(means_lists)) {
    print(sprintf('current data set: %s', i))
    for (run in 1:runs) {
      # for tracking if model is singular or has convergence problems
      print(run)
      is_sing = "N"
      conv_error = "N"
      
      # Generate data and model it
      data <- generate_bin(s, n_obs, means_lists[[i]][[1]], means_lists[[i]][[2]], means_lists[[i]][[3]], means_lists[[i]][[4]], subj_corrs, subj_tau)
      suppressMessages(model1 <- suppressMessages(glmer(y ~ c_freq * c_learn + (c_freq|subject), family=binomial, data, control=glmerControl(optimizer = "bobyqa"))))
      output <- summary(model1)
     
      # Get grand mean and main effects and SEs
          # 1. Check for singularity and convergence warnings
      
      if (isSingular(model1)) {
        is_sing = "Y"
      }
      conv <- output$optinfo$conv$lme4$messages
          # 2. Collect if not duplicate of singularity warning
      if (!is.null(conv) && conv != "boundary (singular) fit: see ?isSingular") {
        conv_error = "Y"
      }
          # 3. store the estimates of the model
      model_grand_mean <- output$coefficients["(Intercept)", "Estimate"]
      model_grand_mean_se <- output$coefficients["(Intercept)", "Std. Error"]
      model_freq_effect <- output$coefficients["c_freq", "Estimate"]
      model_learn_effect <- output$coefficients["c_learn", "Estimate"]
      model_freq_se <- output$coefficients["c_freq","Std. Error"]
      model_learn_se <- output$coefficients["c_learn","Std. Error"]
      model_interaction_effect <- output$coefficients["c_freq:c_learn", "Estimate"]
      model_interaction_se <- output$coefficients["c_freq:c_learn", "Std. Error"]
      
          # 4. Run a version of the model with emmeans to extract the effects for frequency low and high by learning
      model1.emm <- emmeans(model1 , ~ c_freq * c_learn )
      model1.emm.cont <-contrast(model1.emm, "consec",  simple = "each", combine = T, adjust = "bonferroni")
      summary(model1.emm.cont)->output2b
      
          # 5. get estimates of the low frequency condition
      model_low.FL_effect = output2b[4,4]
      model_low.FL_se = output2b[4,5]
      
          # 6. store here all the info
      run_info = c(s,n_obs,
                   i,
                   means_lists[[i]][[1]], means_lists[[i]][[2]], means_lists[[i]][[3]], means_lists[[i]][[4]], # alpha,beta1,beta2,beta3
                   run,#run
                   model_grand_mean, model_freq_effect, model_learn_effect, model_interaction_effect,
                   model_grand_mean_se, model_freq_se, model_learn_se, model_interaction_se,
                   model_low.FL_effect, model_low.FL_se,
                   is_sing,
                   conv_error)
      
      # Test models' estimates against Mike's original model
          # 1. Grand mean
      for (H1_intercept in H1s_intercept) {
        BF <- Bf(model_grand_mean_se, model_grand_mean, likelihood = "normal", modeloftheory = "normal", modeoftheory =0, scaleoftheory = H1_intercept, tail = 1)
        if (is.na(BF)) {BF_cat = "NA"} else {BF_cat=bf_cat(BF)}
        results_collect = rbind(results_collect, c(run_info, "grandmean", H1_intercept,BF, BF_cat,  output$coefficients["(Intercept)", "Pr(>|z|)"]  ))
      }
      
         # 2. Main frequency effect
            ## 2.1 note - I multiplied estimate by -1, the hypothesis was already *-1 above
      for (H1_cond1 in H1s_cond1) {
        BF <- Bf(model_freq_se, model_freq_effect*-1, likelihood = "normal", modeloftheory = "normal", modeoftheory =0,  scaleoftheory = H1_cond1, tail = 1)
        if (is.na(BF)) {BF_cat = "NA"} else {BF_cat=bf_cat(BF)}
        results_collect = rbind(results_collect, c(run_info, "maineffectFreq", H1_cond1,BF, BF_cat,output$coefficients["c_freq", "Pr(>|z|)"]))
      }
      
         # 3. Main effect learning
      for (H1_cond2 in H1s_cond2) {
        BF <- Bf(model_learn_se, (model_learn_effect), likelihood = "normal", modeloftheory = "normal", modeoftheory =0,  scaleoftheory = (H1_cond2), tail = 1)
        if (is.na(BF)) {BF_cat = "NA"} else {BF_cat=bf_cat(BF)}
        results_collect = rbind(results_collect, c(run_info, "maineffectlearning", H1_cond2,BF, BF_cat, output$coefficients["c_learn", "Pr(>|z|)"]))
      }
      
         # 4. Interaction frequency by learning
      for (H1_interaction in H1s_interaction) {
        BF <- Bf(model_interaction_se, model_interaction_effect, likelihood = "normal", modeloftheory = "normal", modeoftheory =0,  scaleoftheory = H1_interaction, tail = 1)
        if (is.na(BF)) {BF_cat = "NA"} else {BF_cat=bf_cat(BF)}
        results_collect = rbind(results_collect, c(run_info, "interaction", H1_interaction,BF, BF_cat, output$coefficients["c_freq:c_learn", "Pr(>|z|)"]))
      }
      
         # 5. now for simple effect of learning condition in low
      for (H1_simple_effect in H1s_simple_effect) {
        BF <- Bf(model_low.FL_se, model_low.FL_effect, likelihood = "normal", modeloftheory = "normal", modeoftheory =0,  scaleoftheory = H1_simple_effect, tail = 1)
        if (is.na(BF)) {BF_cat = "NA"} else {BF_cat=bf_cat(BF)}
        results_collect = rbind(results_collect, c(run_info, "LearninginLow", H1_simple_effect, BF, BF_cat, output2b[4,8]))
      }
      
    }
  }
}      

# rename variables
names(results_collect) <- c("N_subj","N_obs",
                            "dataParamsN",
                            "alpha", "beta1", "beta2", "beta3",
                            "run",
                            "model_grand_mean", "model_beta1", "model_beta2", "model_beta3",
                            "model_grand_mean_se", "model_beta1_se", "model_beta2_se", "model_beta3_se",
                            
                            "model_learninginLow_effect","model_learninginLow_se",
                            
                            "singular",
                            "conv_error",
                            "CoefficientTested",
                            "H1sd",
                            "BF",
                            "BF_cat",
                            "pvalue")

# write to file
write.table(results_collect, paste(localDir, "120.csv", sep = ""), row.names = FALSE, col.names = T, quote = F)

