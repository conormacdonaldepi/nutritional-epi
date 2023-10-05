library(survival)
library(survminer)
library(gfoRmula)
library(gtools)
library(splines2)
library(tableone)
library(splitstackshape)
library('dplyr')
library(tidyverse)

set.seed(1)
# setwd('//193.52.90.33/p_frenoy$/COLLABORATIONS/Coll_FoodSubModels_CM/ANALYSTAT')
# df = read.csv('TABLES/d03_fsm_20230822_tab.csv',sep=";")
#summary(df)

### data needed, variables must be named as follows

# df$PM <- df$processed_meat_energy*7 #Processed meat variable
# df$fatfish <- df$fatfish_energy*7 #fatty fish variable
# df$redmeat <- df$redmeat_energy*7 #red meat variable
# df$veg <- df$vegetables_energy*7 #vegetable variable
# df$alcohol <- df$alcool*7 #alcohol variable
# df$totalenergy <- df$kcal_noalcq3*7 #total energy intake (without alcohol) variable
# df$bmi <- df$imcq3 #bmi variable
# df$physact <- df$METsTotal2Q3 #physical activity variable
# df$smoke <- df$tabac_enq3 #smoking variable
# df$hypertension <- df$HTA #hypertension indicator variable
# df$age_start <- df$ageq3ve #start of followup age
# df$age_fin <- df$agefin #end of followup age

PM_intval = 350 #value we choose for our intervention on processed meat
FF_intval = 150 #value we choose for our intervention on fatty fish

#need a time variable for long-format data  
df$time_y = df$age_fin - df$age_start
df$time_m = df$time_y*6

###
# data processing step to get everything to long format

# we will stop followup at 15 years, so this is our outcome, hypertension at 15 years
df$hta_15y = 0 
df$hta_15y[df$hypertension == 1 & df$time_y<=15] = 1 #indicator for hypertension 
df$time_y[df$time_y>15] = 15 #if followup more than 15 years, set followup to 15
df$time_m[df$time_m>90] = 90 #same but for months
boot_result=data.frame() #dataframe to save the results
### bootstrap with resample the original dataset
while(nrow(boot_result) < 500){ ###change to 500 if it works
  gc()
  print(nrow(boot_result))
  index <- sample(1:nrow(df), nrow(df), replace = T)
  df_boot <- df[index,]
  df_boot = cbind(ID = 1:nrow(df_boot), df_boot)
  df_boot$ident = df_boot$ID
  
  #we create one row per participant, per month, until the end of followup for all individuals
  df_long <- df_boot %>%
    expandRows('time_m', drop = F) %>% #
    mutate(time = sequence(rle(ident)$lengths)-1)%>% #create a variable for time in months for each participant
    mutate(event = ifelse(time==as.integer(time_m)-1 & hta_15y==1, 1, 0)) #set the event indicator on the row (month) where the event happens
  df_long_10000<-head(df_long,10000)
  
  #now we get to the modelling of the discrete time hazard
  #fit the model for the outcome given all relevent covariates and flexible time indicator
  #this is the discrete hazard model Pr(Y_t=1|A, L, Y_t-1=0)
  #this will be used for modelling the interventions
  
  model_1 <- glm(formula = 'event ~ PM*poly(time,2) + fatfish*poly(time,2) + 
               age_start + bmi + smoke + alcohol + physact + redmeat + veg + totalcals', 
                 data = df_long, family = 'binomial')

  #this is the natural course, we just expand up to the full followup time for everyone
  df_long0 <- expandRows(df_boot, count=90, count.is.col=F) #make 180 rows for 180 months of followup
  df_long0$time <- rep(seq(0,89), nrow(df_boot)) #create time variable, 0-179 months
  
  #this is the intervention on processed meat, 
  #we will expand upto the full followup time, and also change the value of the exposure
  df_long1 <- df_long0
  df_long1$time <- rep(seq(0,89), nrow(df_boot))
  df_long1$PM[df_long1$PM > PM_intval] = PM_intval #intervene on PM and set the value
  df_long1$fatfish[df_long1$fatfish < FF_intval] = FF_intval #intervene on fatty fish and set the value
  #now we predict the outcome for each individual under each intervention
  df_long0$PredYA = predict(model_1, type="response", newdata = df_long0) #predict the outcome for the natural course
  df_long1$PredYB = predict(model_1, type="response", newdata = df_long1) #predict the outcome when intervening on processed meat
  
  #we take each individuals predictions, and multiply them over time for each treatment
  #first do it in the natural diet group
  surv0 <- df_long0 %>%
    group_by(ident) %>%
    mutate(surv_A = cumprod(1 - PredYA), 
           risk_A = 1 - surv_A)
  #intervention group
  surv1 <- df_long1 %>%
    group_by(ident) %>%
    mutate(surv_B = cumprod(1-PredYB),
           risk_B = 1 - surv_B)
  #now take the mean values of these predictions at each time point - this is the standardization step
  gf_surv_0 <- aggregate(surv0, by=list(surv0$time), FUN=mean)[c("time", "surv_A", "risk_A")]
  gf_surv_1 <- aggregate(surv1, by=list(surv1$time), FUN=mean)[c("time", "surv_B", "risk_B")]
  #put into one dataset
  gf <- merge(gf_surv_0, gf_surv_1, by=c("time")) %>%
    arrange(time)
  
  # #plot the survival against time
  # p <- ggplot(data = gf, aes(time))+
  #   geom_line(aes(y=surv_A, colour = 'A'))+
  #   geom_line(aes(y=surv_B, colour = 'B'))
  # p
  # 
  risk_diff = gf$risk_B - gf$risk_A #risk different for every time
  risk_diff[90] #risk difference at 180 months
  risk_ratio = gf$risk_B / gf$risk_A
  risk_ratio[90]
  result = cbind(gf_surv_0$risk_A[90], gf_surv_1$risk_B[90], risk_ratio[90])
  colnames(result) <- c("riskA", "riskB", "riskratio") 
  boot_result = rbind(boot_result, result) #this may need changed, we want to have results for the risk at 15 years, and the risk ratio
  colnames(boot_result) <- c("riskA", "riskB", "riskratio") 
  write.csv(boot_result, 'intervention_2_bootstrap.csv') # 
  
  #return(boot_result)
}

#write.csv(boot_result, 'intervention_1_bootstrap.csv') # correct if needed :) 

