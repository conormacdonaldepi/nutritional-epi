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

df$PM <- df$processed_meat_energy*7 #Processed meat variable
df$fatfish <- df$fatfish_energy*7 #fatty fish variable
df$redmeat <- df$redmeat_energy*7 #red meat variable
df$veg <- df$vegetables_energy*7 #vegetable variable
df$alcohol <- df$alcool*7 #alcohol variable
df$totalenergy <- df$kcal_noalcq3*7 #total energy intake (without alcohol) variable
df$bmi <- df$imcq3 #bmi variable
df$physact <- df$METsTotal2Q3 #physical activity variable
df$smoke <- df$tabac_enq3 #smoking variable
df$hypertension <- df$HTA #hypertension indicator variable
df$age_start <- df$ageq3ve #start of followup age
df$age_fin <- df$agefin #end of followup age
  
df$PM_cat = quantcut(df$PM, q = 4, na.rm = TRUE) #categorical variable for PM
df$fatfish_cat = quantcut(df$fatfish, q = 4, na.rm = TRUE) #categorical variable for FF

df$time_y = df$age_fin - df$age_start #followup time for each person in years
df$time_m = df$time_y*12 #same in months


#table 1 stratified on PM, supplementary table 1
st1 <- print(CreateTableOne(vars = c('PM_cat', 'age_start', 'smoke', 'bmi', 'physact', 
                        'PM', 'fatfish', 'redmeat', 'veg', 
                        'totalenergy', 'alcohol'), data = df,
               strata = 'PM_cat'))
write.csv2(st1,"RESULTS/st1.csv")

#table 1 stratified on FF, supplementary table 2
st2 <- print(CreateTableOne(vars = c('fatfish_cat', 'age_start', 'smoke', 'bmi', 'physact', 
                        'PM', 'fatfish', 'redmeat', 'veg', 
                        'totalenergy', 'alcohol'), data = df,
               strata = 'fatfish_cat'))
write.csv2(st2,"RESULTS/st2.csv")




#################
#analyse as quantiles, part one of manuscript
#no adjustment, then energy then energy + foods
#################
# classical version
df$SurvObj <- with(df, Surv(age_start, age_fin, hypertension))
quan_1 <- coxph(SurvObj ~ as.factor(PM_cat) + bmi + smoke + physact + alcohol , 
                data = df)
summary(quan_1)
quan_1_sum <- summary(quan_1)$conf.int[,c(1,3:4)]
write.csv2(quan_1_sum,"RESULTS/quan_1_sum.csv")
#values to go in first row of table 1, HR, lower CI to higher CI


quan_2 <- coxph(SurvObj ~ as.factor(PM_cat) + bmi + smoke + physact + alcohol + totalenergy, 
                data = df)
summary(quan_2)
quan_2_sum <- summary(quan_2)$conf.int[,c(1,3:4)]
write.csv2(quan_2_sum,"RESULTS/quan_2_sum.csv")
#values to go in second row of table 1, HR, lower CI to higher CI


quan_3 <- coxph(SurvObj ~ as.factor(PM_cat) + bmi + smoke + physact + alcohol + totalenergy +
                  fatfish  + veg + redmeat, 
                data = df)
summary(quan_3)
quan_3_sum <- summary(quan_3)$conf.int[,c(1,3:4)]
write.csv2(quan_3_sum,"RESULTS/quan_3_sum.csv")

#values to go in third row of table 1, HR, lower CI to higher CI




#################
#analyse as substitutions
#no adjustment, then energy then energy + foods
#################
# classical version
df$SurvObj <- with(df, Surv(age_start, age_fin, hypertension))
sub_1 <- coxph(SurvObj ~ PM + fatfish + bmi + smoke + physact + alcohol , 
               data = df)
summary(sub_1)
sum1 = summary(sub_1)#values for table 2
sum1_print<- sum1$conf.int[,c(1,3:4)]
write.csv2(sum1_print,"RESULTS/sum1.csv")

### first row
sum1_FF_100 <- 
  cbind(exp(sub_1$coefficients[2]*100),# fatfish coef
        exp(log(sum1$conf.int[2,3])*100), # fatfish lower CI
        exp(log(sum1$conf.int[2,4])*100)) # fatfish upwe CI
sum1_FF_100
write.csv2(sum1_FF_100,"RESULTS/sum1_FF_100.csv")

sum1_PM_100 <-
  cbind(exp(sub_1$coefficients[1]*100), # PM coef
        exp(log(sum1$conf.int[1,3])*100), # PM lower CI
        exp(log(sum1$conf.int[1,4])*100)) # PM uper CI
sum1_PM_100
write.csv2(sum1_PM_100,"RESULTS/sum1_PM_100.csv")

sum1_subst_100 <-
  cbind(exp(sub_1$coefficients[2]*100- sub_1$coefficients[1]*100), #subst. coef
        exp(log(sum1$conf.int[2,3])*100 - log(sum1$conf.int[1,3])*100), #lower conf. interval for substitution
        exp(log(sum1$conf.int[2,4])*100 - log(sum1$conf.int[1,4])*100)) #upper conf. interval for substitution
sum1_subst_100
write.csv2(sum1_subst_100,"RESULTS/sum1_subst_100.csv")


sub_2 <- coxph(SurvObj ~ PM + fatfish + bmi + smoke + physact + alcohol + totalenergy, 
               data = df)
summary(sub_2)
sum2 = summary(sub_2)
sum2_print<- sum2$conf.int[,c(1,3:4)]
write.csv2(sum2_print,"RESULTS/sum2.csv")

### second row
sum2_FF_100 <- 
  cbind(exp(sub_2$coefficients[2]*100),# fatfish coef
        exp(log(sum2$conf.int[2,3])*100), # fatfish lower CI
        exp(log(sum2$conf.int[2,4])*100)) # fatfish upwe CI
write.csv2(sum2_FF_100,"RESULTS/sum2_FF_100.csv")

sum2_PM_100 <- 
  cbind(exp(sub_2$coefficients[1]*100), # PM coef
        exp(log(sum2$conf.int[1,3])*100), # PM lower CI
        exp(log(sum2$conf.int[1,4])*100)) # PM uper CI
write.csv2(sum2_PM_100,"RESULTS/sum2_PM_100.csv")

sum2_subst_100 <-
  cbind(exp(sub_2$coefficients[2]*100- sub_2$coefficients[1]*100), #subst. coef
        exp(log(sum2$conf.int[2,3])*100 - log(sum2$conf.int[1,3])*100), #lower conf. interval for substitution
        exp(log(sum2$conf.int[2,4])*100 - log(sum2$conf.int[1,4])*100)) #upper conf. interval for substitution
write.csv2(sum2_subst_100,"RESULTS/sum2_subst_100.csv")



sub_3 <- coxph(SurvObj ~ PM + fatfish + bmi + smoke + physact + alcohol+ totalenergy + 
                 veg + redmeat, 
               data = df)
summary(sub_3)
sum3 = summary(sub_3)
sum3_print<- sum3$conf.int[,c(1,3:4)]
write.csv2(sum3_print,"RESULTS/sum3.csv")

### third row
sum3_FF_100 <- 
  cbind(exp(sub_3$coefficients[2]*100),# fatfish coef
        exp(log(sum3$conf.int[2,3])*100), # fatfish lower CI
        exp(log(sum3$conf.int[2,4])*100)) # fatfish upwe CI
write.csv2(sum3_FF_100,"RESULTS/sum3_FF_100.csv")

sum3_PM_100 <- 
  cbind(exp(sub_3$coefficients[1]*100), # PM coef
        exp(log(sum3$conf.int[1,3])*100), # PM lower CI
        exp(log(sum3$conf.int[1,4])*100)) # PM uper CI
write.csv2(sum3_PM_100,"RESULTS/sum3_PM_100.csv")

sum3_subst_100 <-
  cbind(exp(sub_3$coefficients[2]*100- sub_3$coefficients[1]*100), #subst. coef
        exp(log(sum3$conf.int[2,3])*100 - log(sum3$conf.int[1,3])*100), #lower conf. interval for substitution
        exp(log(sum3$conf.int[2,4])*100 - log(sum3$conf.int[1,4])*100)) #upper conf. interval for substitution
write.csv2(sum3_subst_100,"RESULTS/sum3_subst_100.csv")

