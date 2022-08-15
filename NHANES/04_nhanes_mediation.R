#
rm(list = ls())
library(mediation)
library(bigreadr)
library(dplyr)
library(randomForest)
library(survey)
library(svrepmisc)

# 
setwd("~/Infection_SES/NHANES/01_raw/")
merge_nhanes_df <- fread2("merge_nhanes_df_all.txt")

base_varx <- c("Infect",  
               "Age", "Sex", "Race", "Cycle",
               "retain", "retain_SES",
               "SDMVPSU", "SDMVSTRA",
               "WTINT", "WTMEC", "WTDRD1")
exposure <- "SES4"

sum_list <- list()
medi_list <- list()
for(a in c("lifescore",
           "Nosmoke", "Activity", "Diet", "Noalcohol", "Sleep", "Nodrug", 
           "Hypertension", "Diabetes", "Mental")){
  
  if (a == "Diet") {
    nhanes_designx <- svydesign(data = merge_nhanes_df[,c(base_varx, exposure, a)], 
                                id = ~SDMVPSU, 
                                strata = ~SDMVSTRA, 
                                weights = ~WTDRD1, 
                                nest=TRUE)
    nhcx <- subset(nhanes_designx, retain == 1 & retain_SES == 1 & 
                     !is.na(Infect) & WTDRD1 > 0)
  } else {
    nhanes_designx <- svydesign(data = merge_nhanes_df[,c(base_varx, exposure, a)], 
                                id = ~SDMVPSU, 
                                strata = ~SDMVSTRA, 
                                weights = ~WTMEC, 
                                nest=TRUE)
    nhcx <- subset(nhanes_designx, retain == 1 & retain_SES == 1 & !is.na(Infect))
  }

  med_fomula <- paste0(a, " ~ ",exposure," + Age + Sex + Race + Cycle")
  out_fomula <- paste0("Infect ~ ",exposure," + ",a," + Age + Sex + + Race + Cycle")
  # X-M model
  if (a == "lifescore") {
    med.fit <- (svyglm(eval(parse(text = med_fomula)), design=nhcx, 
                       na.action = na.omit))
  } else {
    med.fit <- (svyglm(eval(parse(text = med_fomula)), design=nhcx, 
                       family=binomial, na.action = na.omit))
  }
  
  # X-M-Y model
  out.fit <- (svyglm(eval(parse(text = out_fomula)), design=nhcx, 
                     family=binomial, na.action = na.omit))
  sum_x <- c(exp(coef(out.fit)[2]),
             exp(confint(out.fit)[2,]),
             coef(summary(out.fit))[2,4])
  orx <- paste0(sum_x[1] %>% round(4), 
                "(",
                sum_x[2]%>% round(4),
                ",",
                sum_x[3]%>% round(4),
                ")")
  sum_list[[a]] <- c(a, orx)
  
  # mediation model
  med.out <- mediate(med.fit, out.fit, treat = exposure, mediator = a,
                     sims = 1000, robustSE = T,
                     boot = F, boot.ci.type = "perc")
  #
  medi_list[[a]] <- (c(med.out$tau.coef, med.out$tau.ci, med.out$tau.p,
                       med.out$d.avg, med.out$d.avg.ci, med.out$d.avg.p,
                       med.out$z.avg, med.out$z.avg.ci, med.out$z.avg.p,
                       med.out$n.avg, med.out$n.avg.ci, med.out$n.avg.p))
}
Reduce(rbind, sum_list)
Reduce(rbind, medi_list)

