### scripts to create survival curves from survival data
library(ggplot2)
library(tidyverse)
library(ggpubr)
setwd("~/Projects/Nutri Epi/Substitution models/Analysis/for pauline/results")

output_results <- function(boot_results){
  y0_lower = quantile(boot_results$riskA, 0.05)
  y0_upper = quantile(boot_results$riskA, 0.95)
  y1_lower = quantile(boot_results$riskB, 0.05)
  y1_upper = quantile(boot_results$riskB, 0.95)
  RD_lower = quantile(boot_results$riskdiff, 0.05)
  RD_upper = quantile(boot_results$riskdiff, 0.95)
  RR_lower = quantile(boot_results$riskratio, 0.05)
  RR_upper = quantile(boot_results$riskratio, 0.95)
  
  output = rbind(y0_lower, y0_upper, y1_lower, y1_upper, RD_lower, RD_upper,
                 RR_lower, RR_upper)
  return(output)
}

intervention_1_boot <- read_csv2("intervention_1_bootstrap.csv")
intervention_1_boot$riskdiff = intervention_1_boot$riskB - intervention_1_boot$riskA
output_results(intervention_1_boot)


intervention_2_boot <- read_csv2("intervention_2_bootstrap.csv")
intervention_2_boot$riskdiff = intervention_2_boot$riskB - intervention_2_boot$riskA
output_results(intervention_2_boot)

intervention_3_boot <- read_csv2("intervention_3_bootstrap.csv")
intervention_3_boot$riskdiff = intervention_3_boot$riskB - intervention_3_boot$riskA
output_results(intervention_3_boot)

intervention_4_boot <- read_csv2("intervention_4_bootstrap.csv")
intervention_4_boot$riskdiff = intervention_4_boot$riskB - intervention_4_boot$riskA
output_results(intervention_4_boot)

intervention_5_boot <- read_csv2("intervention_5_bootstrap.csv")
intervention_5_boot$riskdiff = intervention_5_boot$riskB - intervention_5_boot$riskA
output_results(intervention_5_boot)