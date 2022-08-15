#
rm(list = ls())
library(mediation)
library(bigreadr)
library(dplyr)
library(randomForest)

# 
options(digits = 8)
# "" 05_2010/
type <- "05_2010/"
setwd("~/Infection_SES/")
merge_df <- fread2(paste0(type,"merge_df.txt"))

# merge_df <- subset(merge_df, merge_df$SES_rev %in% c(1,3))
# merge_df$SES_rev[merge_df$SES_rev == 3] <- 2

expo <- "Townsend"
base_data <- merge_df[,c("Infect", 
                         "Age", "Sex", "EUR_eth", "center")]
base_data$exposure <- merge_df[[expo]]

sum_list <- list()
medi_list <- list()
for(a in c("lifescore1",
           "Nosmoke", "Activity", "Diet", "Noalcohol", "Sleep" ,"Nocannabis",
           "Air_p_score1", "Air_p_score2",
           "pm25", "pm2510", "pm10", "no2","nox",
           "logi_dist2road", "log_traff_intens", "noise",
           "CVD",
           "CAD", "AF", "stroke", "Hypert",
           "diab",
           "ment",
           "anx", "dp", "bp",
           "cancer")){
  # a <- "Air_p_score1"
  # X-M model
  if (a %in% c("pm25", "pm2510", "pm10", "no2","nox",
               "logi_dist2road", "log_traff_intens", "noise",
               "lifescore1", "Air_p_score1", "Air_p_score2")) {
    base_data$mediator <- merge_df[[a]]
    med.fit <- lm(mediator ~ exposure + Age + Sex + EUR_eth + center, 
                  data = base_data)
  } else {
    base_data$mediator <- merge_df[[a]] %>% 
      as.numeric()
    med.fit <- glm(mediator ~ exposure + Age + Sex + EUR_eth + center, 
                   data = base_data, family = binomial("logit"))
  }
  
  # X-M-Y model
  out.fit <- glm(Infect ~  exposure + mediator + Age + Sex + EUR_eth + center,
                 data = base_data, family = binomial("logit"))
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
  med.out <- mediate(med.fit, out.fit, treat = "exposure", mediator = "mediator",
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

