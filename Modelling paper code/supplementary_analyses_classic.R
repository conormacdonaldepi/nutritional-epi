library(survival)
library(survminer)
library(gfoRmula)
library(gtools)
library(splines2)
library(tableone)
library(splitstackshape)
library('dplyr')

setwd('//193.52.90.33/p_frenoy$/COLLABORATIONS/Coll_FoodSubModels_CM/ANALYSTAT')
df = read.csv('TABLES/d03_fsm_20230822_tab.csv',sep=";")
summary(df)

### data needed, variables must be named as follows

df$PM_GRAMS <- df$processed_meat_grams*7 #Processed meat variable IN GRAMS (you can just use the unchanged variables from the FFQ)
df$alcohol <- df$alcool*7 #alcohol variable
df$totalenergy <- df$kcal_noalcq3*7 #total energy intake (without alcohol) variable
df$bmi <- df$imcq3 #bmi variable
df$physact <- df$METsTotal2Q3 #physical activity variable
df$smoke <- df$tabac_enq3 #smoking variable
df$hypertension <- df$HTA #hypertension indicator variable
df$age_start <- df$ageq3ve #start of followup age
df$age_fin <- df$agefin #end of followup age
df$E_PM <- df$E_PM*7
df$E_fatfish <- df$E_fatfish*7
df$E_eggs <- df$E_eggs*7
df$E_veg <- df$E_veg*7
df$E_leanfish <- df$E_leanfish*7
df$E_poultry <- df$E_poultry*7
df$E_redmeat <- df$E_redmeat*7
df$E_whitemeat <- df$E_whitemeat*7
df$E1 <- df$E1*7
df$E2 <- df$E2*7
df$E3 <- df$E3*7
df$E4 <- df$E4*7
df$E5 <- df$E5*7
df$E6 <- df$E6*7
df$E7 <- df$E7*7
df$E8 <- df$E8*7
df$E9 <- df$E9*7
df$E10 <- df$E10*7
df$E11 <- df$E11*7
df$E12 <- df$E12*7
df$E13 <- df$E13*7
df$E14 <- df$E14*7
df$E15 <- df$E15*7
df$E16 <- df$E16*7
df$total_energy_intake <- df$total_energy_intake*7


df$PM_GRAMS_cat = quantcut(df$PM_GRAMS, q = 4, na.rm = TRUE) #categorical variable for PM in grams


#################
# mixed unit quantile model with energy adjustment
#################
# classical version with
df$SurvObj <- with(df, Surv(age_start, age_fin, hypertension))


supp_1 <- coxph(SurvObj ~ as.factor(PM_GRAMS_cat) + bmi + smoke + physact + alcohol + totalenergy, 
                data = df)
summary(supp_1)
supp_1_sum <- summary(supp_1)$conf.int[,c(1,3:4)]
write.csv2(supp_1_sum,"RESULTS/quan_2_sum.csv")
#values to go in second row of table 1, HR, lower CI to higher CI




#################
#enery partition model
#
#################
# classical version
df$SurvObj <- with(df, Surv(age_start, age_fin, hypertension))
sub_1 <- coxph(SurvObj ~ E_PM + E_fatfish+ E_eggs + E_veg  + E_leanfish + E_poultry + 
                 E_redmeat + E_whitemeat + E1+ E2 + E3+E4+E5+E6+E7+E8+E9+E10+E11+E12+E13+E14+E15+E16+
                 bmi + smoke + physact + alcohol , 
               data = df)
summary(sub_1)
sum1 = summary(sub_1)#values for table 2
sum1_print<- sum1$conf.int[,c(1,3:4)]
write.csv2(sum1_print,"RESULTS/sum1.csv")

###coef for fatty fish
sum1_FF_100 <- 
  cbind(exp(sub_1$coefficients[2]*100),# fatfish coef
        exp(log(sum1$conf.int[2,3])*100), # fatfish lower CI
        exp(log(sum1$conf.int[2,4])*100)) # fatfish upwe CI
sum1_FF_100
write.csv2(sum1_FF_100,"RESULTS/sum1_FF_100.csv")

### coef for PM
sum1_PM_100 <-
  cbind(exp(sub_1$coefficients[1]*100), # PM coef
        exp(log(sum1$conf.int[1,3])*100), # PM lower CI
        exp(log(sum1$conf.int[1,4])*100)) # PM uper CI
sum1_PM_100
write.csv2(sum1_PM_100,"RESULTS/sum1_PM_100.csv")

###substitution
sum1_subst_100 <-
  cbind(exp(sub_1$coefficients[2]*100- sub_1$coefficients[1]*100), #subst. coef
        exp(log(sum1$conf.int[2,3])*100 - log(sum1$conf.int[1,3])*100), #lower conf. interval for substitution
        exp(log(sum1$conf.int[2,4])*100 - log(sum1$conf.int[1,4])*100)) #upper conf. interval for substitution
sum1_subst_100
write.csv2(sum1_subst_100,"RESULTS/energy_part_subst_100.csv")

#################
#leave one out model
#
#################




sub_2 <- coxph(SurvObj ~  E_fatfish + E_eggs + E_veg  + E_leanfish + E_poultry 
               + E_redmeat + E_whitemeat + E1+ E2 + E3+E4+E5+E6+E7+E8+E9+E10+E11+E12+E13+E14+E15+E16+total_energy_intake+
               bmi + smoke + physact + alcohol,
               data = df)
summary(sub_2)
sum2 = summary(sub_2)
sum2_print<- sum2$conf.int[,c(1,3:4)]
write.csv2(sum2_print,"RESULTS/sum2.csv")

### second row
sum2_FF_100 <- 
  cbind(exp(sub_2$coefficients[1]*100),# fatfish coef
        exp(log(sum2$conf.int[1,3])*100), # fatfish lower CI
        exp(log(sum2$conf.int[1,4])*100)) # fatfish upwe CI
write.csv2(sum2_FF_100,"RESULTS/leave_one_out_FF_100.csv")

