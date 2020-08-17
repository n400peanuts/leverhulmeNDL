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

#### set local directory ####
localDir <- c("C:/Users/eva_v/Documents/GitHub/leverhulmeNDL/")

#### load functions ####
source(paste(localDir, "tools/loadFunctionsGithub.R", sep = ""))

urlFolder <- 'https://api.github.com/repos/n400peanuts/languagelearninglab/git/trees/master?recursive=1'
urlRaw <- 'https://raw.githubusercontent.com/n400peanuts/languagelearninglab/master/tools/'
listFunctions <- c( "Bf_cat.R", "inverse_log_odd.R", "myCenter.R", 
                   "lizCenter.R", "getmode.R", "lizCenter2.R", "deleteRandomRows.R")

loadFunctionsGithub(urlFolder, urlRaw, listFunctions)

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

fmri <- fmri[!(fmri$trialType=="control"),]
#get means
round(tapply(fmri$acc, list(fmri$learning,fmri$frequency), mean, na.rm=T),3)  

#center variables, we don't have NAs so I'm using lizCenter
fmri<-lizCenter(fmri, listfname = list("learning", "frequency"))

## ----------------------------------------------------------------------------------##
## --------------------------------- main effect ------------------------------------##
## ----------------------------------------------------------------------------------##
fmriglmer1_V1<-glmer(acc ~  frequency.ct*learning.ct + (frequency.ct|subjID), 
                  data = fmri, 
                  family="binomial",
                  control=glmerControl(optimizer = "bobyqa"))

round(summary(fmriglmer1_V1)$coefficients,4)

## ----------------------------------------------------------------------------------##
## --------------------------------- interaction ------------------------------------##
## ----------------------------------------------------------------------------------##


fmriglmer1_V3<-glmer(acc ~  frequency.ct + frequency.ct:learning.ct + (frequency.ct|subjID), 
                     data = fmri[fmri$trialType!='control',], 
                     family="binomial",
                     control=glmerControl(optimizer = "bobyqa"))


round(summary(fmriglmer1_V3)$coefficients,4)



#### get values from model used to simulate data ####

## ----------------------------------------------------------------------------------##
## -------------------------------- fixed effects -----------------------------------##
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
# correlations between random slopes:
#we already have this info here for frequency:
subj_corrs = vector(length = 1)
subj_corrs[1] = attr(VarCorr(fmriglmer1_V1)$subjID,"corr")[2]
subj_corrs

# number of subjects
n_subj = 40
# number of trials per subject - must be divisible by 4 where there are 2 within-subject conditions
# here we have 48 total trials per subject
n_obs = 48 + 48 #two tasks


#### generate some multilevel binary data ####

# not the original function because we have only frequency as within ppts

# arguments: n_subj = number of subjects
# n_obs = total number of trials per subject
# alpha = veridical grand mean log odds performance
# beta1 = veridical effect of cond1
# beta2 = veridical effect of cond2
# beta3 = veridical cond1 * cond2 interaction effect
# subj_corrs = list of correlations between random effects
# subj_tau = list of SDs of random effects

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
        data$c_learn * (beta2 + u[data$subject,2]) +
        # cond1 * cond2 value * (interaction fixed effect + interaction random slope) +
        (data$c_freq * data$c_learn) * (beta3 + u[data$subject,2]),
      # then transform by inverse logit to a probability (for this combo of subject/condition)
      mu = inv.logit(eta),
      # finally, generate a 0 or 1 for this row based on the probability for this subject/condition
      y = rbinom(nrow(data),1,mu))
  return(data)
}


data <- generate_bin(n_subj, n_obs, alpha, beta1, beta2, beta3, subj_corrs, subj_tau)

#### compute statistics from simulated data ####

model1 <- glmer(y ~ c_freq * c_learn + (c_freq|subject), family=binomial, data)

output = summary(model1)$coeff
round(output,5)

# get interaction estimate and SE
# get interaction estimate and SE
interaction_effect <- output["c_freq:c_learn", "Estimate"]
interaction_se <- output["c_freq:c_learn", "Std. Error"]

main_effect <- output["c_freq", "Estimate"]
main_se <- output["c_freq", "Std. Error"]

# obtain BF using 'true value' estimate

BF <- Bf(interaction_se, interaction_effect, likelihood = "normal", modeloftheory = "normal", modeoftheory = 0, scaleoftheory = round(beta3,2), tail = 1)
print(BF)

BF <- Bf(main_se, (main_effect*-1), likelihood = "normal", modeloftheory = "normal", modeoftheory = 0, 
         scaleoftheory = 
           round(beta1,2)*-1, tail = 1)
print(BF)

#### Simulate data and pompute BFs for different sample sizes, mean effect sizes and H1's ####


n_obs=8
sample_sizes = c(10,40, 60, 80, 100, 120, 140, 160)
runs = 25
means_lists = list(list(alpha,beta1,beta2,beta3),# values as in original dataset
                   list(alpha,beta1/2,beta2,beta3), # beta1 half as big
                   list(alpha, beta1, beta2, beta3/2), # beta3 half as big
                   list(alpha, 0, beta2, beta3),# beta1 null
                   list(alpha,beta1,beta2,0))# beta3 null

results_collect <- data.frame(matrix(0, ncol=31, nrow=0))
runs=25
H1s_intercept = c(alpha,alpha/2) 
H1s_cond1 = c(beta1,beta1/2) *-1
H1s_cond2 = c(beta2,beta2/2) *1 # note - this is negative because of the coding in the model, make it positive here and then when compute BF * estimate by -1 so that a negative coefficient is tested as being in the same direction as the hypotheses
H1s_interaction = c(beta3,beta3/2) 



for (s in sample_sizes) {
  #s=40
  print(sprintf('current sample size: %s', s))
  for(i in 1: length(means_lists)) {
    #i=1
    #print(sprintf('current data set: %s', i))
    for (run in 1:runs) {
      #run=1
      # for tracking if model is singular or has convergence problems
      is_sing = "N"
      conv_error = "N"
      data <- generate_bin(s, n_obs, means_lists[[i]][[1]], means_lists[[i]][[2]], means_lists[[i]][[3]], means_lists[[i]][[4]], subj_corrs, subj_tau)




      #deleteN = nrow(data)*Propmissing
      #data = deleteRandomRows(data,deleteN)
      #data = lizCenter2(data, c("c_freq", "c_learn"),"y")
      suppressMessages(model1 <- suppressMessages(glmer(y ~ c_freq * c_learn + (c_freq|subject), family=binomial, data, control=glmerControl(optimizer = "bobyqa"))))
      output <- summary(model1)
        # get grand mean and main effects and ses

# check for singularity and convergence warnings

      if (isSingular(model1)) {
        is_sing = "Y"
      }
      conv <- output$optinfo$conv$lme4$messages
      # collect if not duplicate of singularity warning
      if (!is.null(conv) && conv != "boundary (singular) fit: see ?isSingular") {
        conv_error = "Y"
      }
      model_grand_mean <- output$coefficients["(Intercept)", "Estimate"]
      model_grand_mean_se <- output$coefficients["(Intercept)", "Std. Error"]
      model_freq_effect <- output$coefficients["c_freq", "Estimate"]
      model_learn_effect <- output$coefficients["c_learn", "Estimate"]
      model_freq_se <- output$coefficients["c_freq","Std. Error"]
      model_learn_se <- output$coefficients["c_learn","Std. Error"]
      model_interaction_effect <- output$coefficients["c_freq:c_learn", "Estimate"]
      model_interaction_se <- output$coefficients["c_freq:c_learn", "Std. Error"]

      # run a version of the model where we fit an intercept for each of the fixed effects amd extract those effects

      data$c_freqFc = as.factor(data$c_freq)       # first have to create a version where these are factors
      levels(data$c_freqFc) = c("high", "low") # rename these factors
      data$c_learnFc = as.factor(data$c_learn)
      levels(data$c_learnFc) = c("FL", "LF")

       model1v1 <- suppressMessages(glmer(y ~ -1 +c_freqFc:c_learnFc+ (c_freq |subject), family=binomial, data, control=glmerControl(optimizer = "bobyqa")))
      output2<- summary(model1v1)

      # get estimate of each cell mean and se

      model_intercept_high.FL_effect =output2$coefficients["c_freqFchigh:c_learnFcFL", "Estimate"]
      model_intercept_low.FL_effect =output2$coefficients["c_freqFclow:c_learnFcFL", "Estimate"]
      model_intercept_high.LF_effect =output2$coefficients["c_freqFchigh:c_learnFcLF", "Estimate"]
      model_intercept_low.LF_effect =output2$coefficients["c_freqFclow:c_learnFcLF", "Estimate"]

      model_intercept_high.FL_se =output2$coefficients["c_freqFchigh:c_learnFcFL", "Std. Error"]
      model_intercept_low.FL_se =output2$coefficients["c_freqFclow:c_learnFcFL", "Std. Error"]
      model_intercept_high.LF_se =output2$coefficients["c_freqFchigh:c_learnFcLF","Std. Error"]
      model_intercept_low.LF_se =output2$coefficients["c_freqFclow:c_learnFcLF", "Std. Error"]

      run_info = c(s,n_obs,
                     i,
                     means_lists[[i]][[1]], means_lists[[i]][[2]], means_lists[[i]][[3]], means_lists[[i]][[4]], # alpha,beta1,beta2,beta3
                     run,#run
                     model_grand_mean, model_freq_effect, model_learn_effect, model_interaction_effect,
                     model_grand_mean_se, model_freq_se, model_learn_se, model_interaction_se,
                     model_intercept_high.FL_effect, model_intercept_high.LF_effect,
                     model_intercept_low.FL_effect,  model_intercept_low.LF_effect,
                     model_intercept_high.FL_se,     model_intercept_high.LF_se,
                     model_intercept_low.FL_se,      model_intercept_low.LF_se,
                     is_sing,
                     conv_error)


      for (H1_intercept in H1s_intercept) {
          BF <- Bf(model_grand_mean_se, model_grand_mean, likelihood = "normal", modeloftheory = "normal", modeoftheory =0, scaleoftheory = H1_intercept, tail = 1)
          if (is.na(BF)) {BF_cat = "NA"} else {BF_cat=bf_cat(BF)}
          results_collect = rbind(results_collect, c(run_info, "grandmean", H1_intercept,BF, BF_cat,  output$coefficients["(Intercept)", "Pr(>|z|)"]  ))
          }

      for (H1_cond1 in H1s_cond1) {
          BF <- Bf(model_freq_se, model_freq_effect, likelihood = "normal", modeloftheory = "normal", modeoftheory =0,  scaleoftheory = H1_cond1, tail = 1)
          if (is.na(BF)) {BF_cat = "NA"} else {BF_cat=bf_cat(BF)}
          results_collect = rbind(results_collect, c(run_info, "maineffectFreq", H1_cond1,BF, BF_cat,output$coefficients["c_freq", "Pr(>|z|)"]))
          }

## note for cond2 the model estimate is multiplied by -1 to correct for coding in the model
## removed for now as isn't key hypothesis

      #for (H1_cond2 in H1s_cond2) {
  #        BF <- Bf(model_cond1_se, (model_cond1_effect*-1), likelihood = "normal", modeloftheory = "normal", modeoftheory =0,  scaleoftheory = (H1_cond2), tail = 1)
   #       if (is.na(BF)) {BF_cat = "NA"} else {BF_cat=bf_cat(BF)}
    #      results_collect = rbind(results_collect, c(run_info, "maineffectCond2", H1_cond2,BF, BF_cat, output$coefficients["c_cond2.ct", "Pr(>|z|)"]))
     #     }


      for (H1_interaction in H1s_interaction) {
          BF <- Bf(model_interaction_se, model_interaction_effect, likelihood = "normal", modeloftheory = "normal", modeoftheory =0,  scaleoftheory = H1_interaction, tail = 1)
          if (is.na(BF)) {BF_cat = "NA"} else {BF_cat=bf_cat(BF)}
          results_collect = rbind(results_collect, c(run_info, "interaction", H1_interaction,BF, BF_cat, output$coefficients["c_freq:c_learn", "Pr(>|z|)"]))
          }
# now compute intercept for each of the four cells

      for (H1_intercept in H1s_intercept) {
          BF <- Bf(model_intercept_high.FL_se, model_intercept_high.FL_effect, likelihood = "normal", modeloftheory = "normal", modeoftheory =0,  scaleoftheory = H1_intercept, tail = 1)
          if (is.na(BF)) {BF_cat = "NA"} else {BF_cat=bf_cat(BF)}
          results_collect = rbind(results_collect, c(run_info, "mean high.FL", H1_intercept,BF, BF_cat, output2$coefficients["c_freqFchigh:c_learnFcFL", "Pr(>|z|)"]))
          }

      for (H1_intercept in H1s_intercept) {
          BF <- Bf(model_intercept_high.LF_se, model_intercept_high.LF_effect, likelihood = "normal", modeloftheory = "normal", modeoftheory =0,  scaleoftheory = H1_intercept, tail = 1)
          if (is.na(BF)) {BF_cat = "NA"} else {BF_cat=bf_cat(BF)}
          results_collect = rbind(results_collect, c(run_info, "mean high.LF", H1_intercept,BF, BF_cat,output2$coefficients["c_freqFchigh:c_learnFcLF", "Pr(>|z|)"]))
          }

      for (H1_intercept in H1s_intercept) {
          BF <- Bf(model_intercept_low.FL_se, model_intercept_low.FL_effect, likelihood = "normal", modeloftheory = "normal", modeoftheory =0,  scaleoftheory = H1_intercept, tail = 1)
          if (is.na(BF)) {BF_cat = "NA"} else {BF_cat=bf_cat(BF)}
          results_collect = rbind(results_collect, c(run_info, "mean low.FL", H1_intercept,BF, BF_cat, output2$coefficients["c_freqFclow:c_learnFcFL", "Pr(>|z|)"]))
          }

      for (H1_intercept in H1s_intercept) {
          BF <- Bf(model_intercept_low.LF_se, model_intercept_low.LF_effect, likelihood = "normal", modeloftheory = "normal", modeoftheory =0,  scaleoftheory = H1_intercept, tail = 1)
          if (is.na(BF)) {BF_cat = "NA"} else {BF_cat=bf_cat(BF)}
          results_collect = rbind(results_collect, c(run_info, "mean low.LF", H1_intercept,BF, BF_cat,output2$coefficients["c_freqFclow:c_learnFcLF", "Pr(>|z|)"]))
          }
      # add result to the data

    }
  }
}

names(results_collect) <- c("N_subj","N_obs",
                            "dataParamsN",
                            "alpha", "beta1", "beta2", "beta3",
                            "run",
                            "model_grand_mean", "model_beta1", "model_beta2", "model_beta3",
                            "model_grand_mean_se", "model_beta1_se", "model_beta2_se", "model_beta3_se",

                            "model_intercept_high.FL","model_intercept_high.LF",
                            "model_intercept_low.FL","model_intercept_high.LF",
                            "model_intercept_high.FL_se","model_intercept_high.LF_se",
                            "model_intercept_low.FL_se","model_intercept_low.LF_se",

                            "singular",
                            "conv_error",
                            "CoefficientTested",
                            "H1sd",
                            "BF",
                            "BF_cat",
                            "pvalue"

                            )

# write to file







filename = "alltest_Eva_2_F.csv"
write.csv(results_collect, filename, row.names = FALSE)


#### load all the simulations ####
df <- list.files(paste(localDir, "/preReg/", sep = "")); 

for (i in 1:length(df[1:6])){
  gsub(".csv$", "", df[i]) -> id
  assign(id, data.frame())
  read.csv(paste(localDir, "/preReg/", df[i], sep = ""),
           na.strings=c("","NA"),
           stringsAsFactors = T
           )-> temp
  assign(paste0(id), temp)
};

rbind(alltest_Eva_1_F, alltest_Eva_2_F, alltest_Eva_3_F, alltest_Eva_4_F, alltest_Eva_5_F, alltest_Eva_6_F)-> results_collect
rm(alltest_Eva_1_F, alltest_Eva_2_F, alltest_Eva_3_F, alltest_Eva_4_F, alltest_Eva_5_F, alltest_Eva_6_F)
nrow(results_collect)/560 #real number of runs

#### check for any BA values that are na ####

results_collect$CoefficientTested.fact= as.factor(results_collect$CoefficientTested)
levels(results_collect$CoefficientTested.fact)
results_collect$BF.num = as.numeric(results_collect$BF)

sum(is.na(subset(results_collect, CoefficientTested== "grandmean")$BF.num)*1 )
sum(is.na(subset(results_collect, CoefficientTested== "interaction")$BF.num)*1 )
sum(is.na(subset(results_collect, CoefficientTested== "maineffectCond1")$BF.num)*1 )
sum(is.na(subset(results_collect, CoefficientTested== "mean iconic.displ")$BF.num)*1 )
sum(is.na(subset(results_collect, CoefficientTested== "mean iconic.sit")$BF.num)*1 )
sum(is.na(subset(results_collect, CoefficientTested== "mean neutral.displ")$BF.num)*1 )
sum(is.na(subset(results_collect, CoefficientTested== "mean neutral.sit")$BF.num)*1 )

dim(subset(results_collect, CoefficientTested== "grandmean"))

#### Plots ####

## plot proportion of time CORRECTLY accept H1 for interaction

beta3_means = c(beta3, beta3/2)
beta3_h1sds = c(beta3, beta3/2)

# subset to look at just the runs where original values of other fixed effects are as in original 
alphaX = alpha
beta1X = beta1 
beta2X = beta2

test_beta3 = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta1,3)==round(beta1X,3)& round(beta2,3)==round(beta2X,3))
test_beta3= droplevels(subset(test_beta3, is.na(test_beta3$BF_cat)==FALSE)) 

summary <- data.frame(matrix(0, ncol=4, nrow=0))
names(summary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")



for (beta3_mean in beta3_means) {
  for (beta3_h1sd in beta3_h1sds) {
    for (s in sample_sizes) {
      #s = sample_sizes[1]
      #beta3_h1sd= beta3_h1sds[1]
      #beta3_mean=beta3_means[2]
      data <- subset(test_beta3, N_subj == s)
      #dim(data)
      data <-  subset(data, round(as.numeric(beta3),3) == round(beta3_mean,3))
      #dim(data)
      
      data <- subset(data, CoefficientTested == "interaction") 
      #dim(data)    
      data <-  subset(data, round(as.numeric(H1sd),3) == round(beta3_h1sd,3))
      dim(data)
      prop = 0
      
      for (row in 1:nrow(data)) {
        #print(row)
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      prop = prop / nrow(data)
      summary <- rbind(summary, data.frame(TrueEffect=beta3_mean, H1sd=beta3_h1sd, N_subj=s, Prop_H1=prop))
    }
  }
}

print(summary)

# work with factor versions of the parameters rounded to 0.4/0.8 for printing purposes
summary$H1sd = as.factor(round(summary$H1sd,1))
summary$H1sd = relevel(summary$H1sd, ref = "0.5")
summary$TrueEffect = as.factor(round(summary$TrueEffect,1))
summary$TrueEffect = relevel(summary$TrueEffect, ref = "0.5")

plottitle = "Interaction: Propotion BF>3 (100 runs) with different means and H1s \n (original means is 0.5) "
p <- ggplot(data=summary,aes(x=N_subj, y=Prop_H1, colour=TrueEffect, linetype=H1sd))
p
p <- p+ ggtitle(plottitle) +theme_bw() + geom_line(aes(color = TrueEffect))
p
p <- p+scale_x_continuous("\nNumber of participants")  
p
p <- p+  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p



#### plot proportion of time get p<.05 for interaction ####


beta3X = beta3

## p-values are exactly the same for the two H1sd so remove dupliated rows 


test_beta3pvalues = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta1,3)==round(beta1X,3)& round(beta2,3)==round(beta2X,3))
test_beta3pvalues= droplevels(subset(test_beta3pvalues, is.na(test_beta3pvalues$BF_cat)==FALSE)) 



summary <- data.frame(matrix(0, ncol=3, nrow=0))
names(summary) <- c("TrueEffect", "N_subj", "Prop_H1")

beta3_mean= beta3_means[1]
s= sample_sizes[1]

for (beta3_mean in beta3_means) {
  for (s in sample_sizes) {
    data <- subset(test_beta3pvalues, N_subj == s)
    data <-  subset(data, round(as.numeric(beta3),3) == round(beta3_mean,3))
    data <- subset(data, CoefficientTested == "interaction") 
    prop = 0
    #row=1
    for (row in 1:nrow(data)) {
      pvalue <- as.numeric(data[row, "pvalue"])
      if (pvalue < 0.05) {
        prop = prop + 1
      }
    }
    prop = prop / nrow(data)
    summary <- rbind(summary, data.frame(TrueEffect=beta3_mean, N_subj=s, Prop_H1=prop))
  }
}
table(test_beta3pvalues$H1sd)
table(test_beta3pvalues$beta3)
table(summary$H1sd)
table(summary$TrueEffect)

summary
table(test_beta3pvalues$beta3)


# work with factor versions of the parameters rounded to 0.5/1 for printing purposes
summary$TrueEffect = as.factor(round(summary$TrueEffect,1))
summary$TrueEffect = relevel(summary$TrueEffect, ref = "0.5")

plottitle = "Interaction: Proportion p<.05 (125 runs) with different means \n (original mean is 0.5) "
p <- ggplot(data=summary,aes(x=N_subj, y=Prop_H1, colour=TrueEffect))
p
p <- p+ ggtitle(plottitle) +theme_bw() + geom_line(aes(color = TrueEffect))
p
p <- p+scale_x_continuous("\nNumber of participants")  
p
p <- p+  scale_y_continuous("Proportion of runs where p < .05\n", limits = c(0,1)) 
p



#### plot proportion of time CORRECTLY accept H1 for main effect of frequency ####



# subset to look at just the runs where original values of other fixed effects are as in original 
alphaX = alpha
beta3X = beta3
beta2X = beta2


test_beta1 = subset(results_collect, round(alpha,3) == round(alphaX,3) & round(beta3,3)==round(beta3X,3)& round(beta2,3)==round(beta2X,3))
test_beta1= droplevels(subset(test_beta1, is.na(test_beta1$BF_cat)==FALSE)) 



summary <- data.frame(matrix(0, ncol=4, nrow=0))
names(summary) <- c("TrueEffect","H1sd", "N_subj", "Prop_H1")

beta1_means = c(beta1, beta1/2)
beta1_h1sds = c(beta1, beta1/2)


for (beta1_mean in beta1_means) {
  for (beta1_h1sd in beta1_h1sds) {
    for (s in sample_sizes) {
      #s = sample_sizes[1]
      #beta1_h1sd= beta1_h1sds[1]
      #beta1_mean=beta1_means[1]
      
      data <- subset(test_beta1, N_subj == s)
      data <-  subset(data, round(as.numeric(beta1),3) == round(beta1_mean,3))
      data <- subset(data, CoefficientTested == "maineffectFreq") 
      
      data <-  subset(data, round(as.numeric(H1sd),3) == round(beta1_h1sd,3))
      prop = 0
      
      for (row in 1:nrow(data)) {
        #row=1
        
        category <- data[row, "BF_cat"]
        if (category > 5) {
          prop = prop + 1
        }
      }
      prop = prop / nrow(data)
      summary <- rbind(summary, data.frame(TrueEffect=beta1_mean, H1sd=beta1_h1sd, N_subj=s, Prop_H1=prop))
    }
  }
}

print(summary)

# work with factor versions of the parameters rounded to 0.5/1 for printing purposes
summary$H1sd = as.factor(round(summary$H1sd,1))
summary$H1sd = relevel(summary$H1sd, ref = "1.1")
summary$TrueEffect = as.factor(round(summary$TrueEffect,1))
summary$TrueEffect = relevel(summary$TrueEffect, ref = "1.1")
plottitle = "Main Effect Iconicity: Propotion BF>3 (100 runs) with different means and H1s \n (original means is 1) "
p <- ggplot(data=summary,aes(x=N_subj, y=Prop_H1, colour=TrueEffect, linetype=H1sd))
p
p <- p+ ggtitle(plottitle) +theme_bw() + geom_line(aes(color = TrueEffect))
p
p <- p+scale_x_continuous("\nNumber of participants")  
p
p <- p+  scale_y_continuous("Proportion of runs where BF > 3\n", limits = c(0,1)) 
p




