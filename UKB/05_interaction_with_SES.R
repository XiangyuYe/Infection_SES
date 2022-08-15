#################
rm(list = ls())
gc()
library(bigreadr)
library(dplyr)

setwd("~/Infection_SES/")

sum_list2 <- list()
# 01_Respiratory/ 02_Digestive/ 03_Blood_Sexually/ 
for (type in c("","01_Respiratory/", "02_Digestive/", "03_Blood_Sexually/","05_2010/")) {
  if (type %in% c("", "05_2010/")) {
    df_path <- paste0(type,"merge_df.txt")
  } else {
    df_path <- paste0(type,"merge_df_match.txt")
  }
  merge_df <- fread2(df_path)

  sum_list <- list()
  # pm25 pm2510  pm10   no2   nox    noise
  for (envx in c("lifescore1",
                 "Nosmoke", "Activity", "Diet", "Noalcohol", "Sleep" ,"Nocannabis",
                 "Air_p_score1", "Air_p_score2",
                 "pm25", "pm2510", "pm10", "no2","nox",
                 "logi_dist2road", "log_traff_intens", "noise",
                 "CVD",
                 "CAD", "AF", "stroke", "Hypert",
                 "diab",
                 "ment", 
                 "anx", "dp", "bp",
                 "cancer")) {
    # envx <- "Air_p_score1"
    ses <- "SES_rev"
    if (type == "") {
      fomux <- paste0("Infect ~ ",ses," * ",envx, "+ Age + Sex + EUR_eth + center + Townsend")
    } else {
      fomux <- paste0("Infect ~ SES_rev * ",envx)
    }
    
    glmx <- glm(formula = fomux, data = merge_df, family = binomial(link = "logit"))
    sum_x <- data.frame(exp(coef(glmx)),
                        exp(confint(glmx)))
    orx <- paste0(sum_x[,1] %>% round(4), 
                  "(",
                  sum_x[,2]%>% round(4),
                  ",",
                  sum_x[,3]%>% round(4),
                  ")")
    sum_list[[envx]] <- data.frame(Var = envx,
                                   OR = orx,
                                   P = coef(summary(glmx))[,4])
  }
  sum_list2[[type]] <- Reduce(rbind, sum_list)
}

a <- sum_list2[[2]]
a <- a[grep(":", rownames(a)),]
a
