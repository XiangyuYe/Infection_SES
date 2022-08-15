rm(list = ls())
gc()
library(bigreadr)
library(dplyr)

setwd("~/Infection_SES/")

### subgroup for whole cohort
# "" 05_2010/
type <- "05_2010/"
merge_df <- fread2(paste0(type,"merge_df.txt"))

# lifescore in factor format
merge_df$lifescore_f <- factor(merge_df$lifescore1)

sum_list <- list()
for (envx in c("lifescore_f","lifescore1",
               "Nosmoke", "Activity", "Diet", "Noalcohol", "Sleep" ,"Nocannabis",
               "Air_p_score1", "Air_p_score2",
               "pm25", "pm2510", "pm10", "no2","nox",
               "logi_dist2road", "log_traff_intens", "noise",
               "CVD","CAD", "AF", "stroke", "Hypert",
               "diab",
               "ment", "anx", "dp", "bp",
               "cancer")) {
  
  sum_list_x <- lapply(c(1:3),function(x){
    fomux <- paste0("Infect ~ ",envx," + Sex + Age + EUR_eth + center")
    glmx <- glm(formula = fomux, data = merge_df[merge_df$SES == x,])
    sum_x <- data.frame(exp(coef(glmx)),
                        exp(confint(glmx)),
                        coef(summary(glmx))[,4],
                        envx,
                        x)
    colnames(sum_x) <- c("OR", "low_CI", "high_CI", "P",
                         "Var", "SES")
    return(sum_x[grep(envx, rownames(sum_x)),])
  })
  
  sum_list[[envx]] <- Reduce(rbind, sum_list_x)
}

group_mat <- Reduce(rbind,sum_list)
save(group_mat, file = paste0(type,"group_mat.RData"))

# SES*lifescore 
merge_df$SES_life <- paste0(merge_df$SES,"_", merge_df$lifescore1)
merge_df$SES_life[grep("NA",merge_df$SES_life)] <- NA
merge_df$SES_life <- factor(merge_df$SES_life)


fomu_sl <- paste0("Infect ~ SES_life + Sex + Age + EUR_eth + center")
glm_sl <- glm(formula = fomu_sl, data = merge_df)
sum_sl <- data.frame(exp(coef(glm_sl)),
                    exp(confint(glm_sl)),
                    coef(summary(glm_sl))[,4])

colnames(sum_sl) <- c("OR", "low_CI", "high_CI", "P")
sum_sl <- sum_sl[grep("SES_life", rownames(sum_sl)),]
sum_sl <- rbind(c(1,1,1,0),sum_sl)
sum_sl$SES <- rep(c(1:3), each = 3)
sum_sl$Lifestyle.scores <- rep(c(1:3), times = 3)
save(sum_sl, file = paste0(type,"sum_sl.RData"))

################
### subgroup for each infectious 
rm(list = ls())
gc()
library(bigreadr)
library(dplyr)

setwd("~/Infection_SES/")

for (type in c("01_Respiratory/", "02_Digestive/",
               "03_Blood_Sexually/")) {
  
  merge_df <- fread2(paste0(type,"merge_df_match.txt"))
  
  # lifescore in factor format
  merge_df$lifescore_f <- factor(merge_df$lifescore1)
  
  sum_list <- list()
  for (envx in c("lifescore_f","lifescore1",
                 "Nosmoke", "Activity", "Diet", "Noalcohol", "Sleep" ,"Nocannabis",
                 "Air_p_score1", "Air_p_score2",
                 "pm25", "pm2510", "pm10", "no2","nox",
                 "logi_dist2road", "log_traff_intens", "noise",
                 "CVD","CAD", "AF", "stroke", "Hypert",
                 "diab",
                 "ment", "anx", "dp", "bp",
                 "cancer")) {
    sum_list_x <- lapply(c(1:3),function(x){
      fomux <- paste0("Infect ~ ",envx," + Sex + Age + EUR_eth + center")
      glmx <- glm(formula = fomux, data = merge_df[merge_df$SES == x,])
      sum_x <- data.frame(exp(coef(glmx)),
                          exp(confint(glmx)),
                          coef(summary(glmx))[,4],
                          envx,
                          x)
      colnames(sum_x) <- c("OR", "low_CI", "high_CI", "P",
                           "Var", "SES")
      return(sum_x[grep(envx, rownames(sum_x)),])
    })
    
    sum_list[[envx]] <- Reduce(rbind, sum_list_x)
  }
  
  group_mat <- Reduce(rbind,sum_list)
  save(group_mat, file = paste0(type,"group_mat.RData"))
  
  # SES*lifescore
  merge_df$SES_life <- paste0(merge_df$SES,"_", merge_df$lifescore1)
  merge_df$SES_life[grep("NA",merge_df$SES_life)] <- NA
  merge_df$SES_life <- factor(merge_df$SES_life)


  fomu_sl <- paste0("Infect ~ SES_life + Sex + Age + EUR_eth + center")
  glm_sl <- glm(formula = fomu_sl, data = merge_df)
  sum_sl <- data.frame(exp(coef(glm_sl)),
                       exp(confint(glm_sl)),
                       coef(summary(glm_sl))[,4])

  colnames(sum_sl) <- c("OR", "low_CI", "high_CI", "P")
  sum_sl <- sum_sl[grep("SES_life", rownames(sum_sl)),]
  sum_sl <- rbind(c(1,1,1,0),sum_sl)
  sum_sl$SES <- rep(c(1:3), each = 3)
  sum_sl$Lifestyle.scores <- rep(c(1:3), times = 3)
  save(sum_sl, file = paste0(type,"sum_sl.RData"))
  
}




