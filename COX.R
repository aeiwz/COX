library(tidyverse)
library(finalfit)

library(readr)
HSTi <- read_csv("HSTi_all_cause_death_2 2.csv")


library(survival)

library(dplyr)
library(forcats)
HSTi <- HSTi %>%
  mutate(
    # Label and recode other variables
    age = ff_label(age, "Age (years)"),
    HSTi_ng_L = ff_label(HSTi_ng_L, "HSTi (ng/L)"),
    sex = factor(sex) %>% 
      fct_recode("Male" = "1", 
                 "Female" = "0") %>% 
      ff_label("Sex"),
    
    # Add HSTi_level column
    HSTi_level = case_when(
      HSTi_ng_L < 2 ~ 1,  # Low
      HSTi_ng_L >= 2 & HSTi_ng_L < 4 ~ 2,  # Medium
      HSTi_ng_L >= 4 ~ 3  # High
    ) %>% ff_label("HSTi Level")
  )





survival_object <- HSTi %$% 
  Surv(time, status)

# Explore:
head(survival_object) # + marks censoring, in this case "Alive"


# Expressing time in years
survival_object <- HSTi %$% 
  Surv(time/365, status)

head(survival_object)


# Overall survival in whole cohort
my_survfit <- survfit(survival_object ~ 1, data = HSTi)
my_survfit # 205 patients, 71 events


summary(my_survfit, times = c(0, 1, 2, 3, 4, 5))


dependent_os <- "Surv(time/365, status)"
explanatory  <- c("HSTi_level")

HSTi %>% 
  surv_plot(dependent_os, explanatory, pval = TRUE)



library(survival)
coxph(Surv(time, status_os) ~ age + sex + thickness + ulcer, data = melanoma) %>% 
  summary()