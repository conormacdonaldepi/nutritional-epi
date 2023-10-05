### scripts to create survival curves from survival data
library(ggplot2)
library(tidyverse)
library(ggpubr)
setwd("~/Projects/Nutri Epi/Substitution models/Analysis/for pauline/results")

intervention_1_data <- read_csv2("intervention_1_data.csv")
intervention_2_data <- read_csv2("intervention_2_data.csv")
intervention_3_data <- read_csv2("intervention_3_data.csv")
intervention_4_data <- read_csv2("intervention_4_data.csv")
intervention_5_data <- read_csv2("intervention_5_data.csv")

intervention_1_data$RR1 = intervention_1_data$risk_B/intervention_1_data$risk_A
intervention_1_data$RD1 = (intervention_1_data$risk_B-intervention_1_data$risk_A)*100
intervention_2_data$RR1 = intervention_2_data$risk_B/intervention_2_data$risk_A
intervention_2_data$RD1 = (intervention_2_data$risk_B-intervention_2_data$risk_A)*100
intervention_3_data$RR1 = intervention_3_data$risk_B/intervention_3_data$risk_A
intervention_3_data$RD1 = (intervention_3_data$risk_B-intervention_3_data$risk_A)*100
intervention_4_data$RR1 = intervention_4_data$risk_B/intervention_4_data$risk_A
intervention_4_data$RD1 = (intervention_4_data$risk_B-intervention_4_data$risk_A)*100
intervention_5_data$RR1 = intervention_5_data$risk_B/intervention_5_data$risk_A
intervention_5_data$RD1 = (intervention_5_data$risk_B-intervention_5_data$risk_A)*100



p1 <-  ggplot(intervention_1_data, aes(time*2)) + 
  geom_line(aes(y = surv_A, linetype = 'Natural diet')) +
  geom_line(aes(y = surv_B, linetype = 'Intervention'))+
  labs(linetype = "Treatment") +
  xlab("Time (months)") + ylab("Survival probability")+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01), 
    limits = c(0.7, 1))

p2 <-   ggplot(intervention_2_data, aes(time*2)) + 
  geom_line(aes(y = surv_A, linetype = 'Natural diet')) +
  geom_line(aes(y = surv_B, linetype = 'Intervention'))+
  labs(linetype = "Treatment") +
  xlab("Time (months)") + ylab("Survival probability")+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01), 
    limits = c(0.7, 1))


p3 <-   ggplot(intervention_3_data, aes(time*2)) + 
  geom_line(aes(y = surv_A, linetype = 'Natural diet')) +
  geom_line(aes(y = surv_B, linetype = 'Intervention'))+
  labs(linetype = "Treatment") +
  xlab("Time (months)") + ylab("Survival probability")+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01), 
    limits = c(0.7, 1))


p4 <-   ggplot(intervention_4_data, aes(time*2)) + 
  geom_line(aes(y = surv_A, linetype = 'Natural diet')) +
  geom_line(aes(y = surv_B, linetype = 'Intervention'))+
  labs(linetype = "Treatment") +
  xlab("Time (months)") + ylab("Survival probability")+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01), 
    limits = c(0.7, 1))


p5 <-   ggplot(intervention_5_data, aes(time*2)) + 
  geom_line(aes(y = surv_A, linetype = 'Natural diet')) +
  geom_line(aes(y = surv_B, linetype = 'Intervention'))+
  labs(linetype = "Treatment") +
  xlab("Time (months)") + ylab("Survival probability")+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01), 
    limits = c(0.7, 1))


p6 <-   ggplot(intervention_1_data, aes(time*2)) + 
  geom_line(aes(y = RR1, linetype = 'risk ratio')) +
  geom_line(aes(y = RD1, linetype = 'risk difference'))+
  labs(linetype = "Treatment") +
  xlab("Time (months)") + ylab("Effect estimate")+
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01))


ggarrange(p1,
          p2 + rremove("y.text") + rremove("y.title"),
          p3, 
          labels = c("A) Intervention 1", "B) Intervention 2", "C) Intervention 3"),
          ncol = 2, nrow = 2, hjust = -1.1,
          common.legend = TRUE, legend="bottom")
