rm(list = ls())
gc()
library(bigreadr)
library(dplyr)

type <- "05_2010/"
setwd("~/Infection_SES/")
life_factor_dff <- fread2("life_factor_dff.txt")

load(paste0(type,"ind_2010.RData"))
life_factor_dff_2010 <- subset(life_factor_dff, life_factor_dff$eid %in% ind_2010[[1]])
life_factor_dff_2010$Infect <- ifelse(life_factor_dff_2010$eid %in% ind_2010[[2]], 1, 0)

######
pollutant_df <- fread2("pollutant_df.txt")
pollutant_dff_2010 <- pollutant_df[match(life_factor_dff_2010$eid,pollutant_df$eid),]
#####
hisodis_df <- fread2("hisodis.txt")
hisodis_dff_2010 <- hisodis_df[match(life_factor_dff_2010$eid,hisodis_df$eid),]

merge_df <- Reduce(cbind,list(life_factor_dff_2010[,-which(colnames(life_factor_dff_2010) %in% c("lifescore2", "lifescore3", "Income2", "Education2"))], 
                              pollutant_dff_2010[,-1],
                              hisodis_dff_2010[,-1]))
#####
merge_df_all <- fread2("merge_df.txt")
merge_df$Air_p_score1 <- merge_df_all$Air_p_score1[match(merge_df$eid, merge_df_all$eid)]
merge_df$Air_p_score2 <- merge_df_all$Air_p_score2[match(merge_df$eid, merge_df_all$eid)]

merge_df$SES_rev <- 4-merge_df$SES
merge_df$Income_rev <- 6-merge_df$Income

fwrite2(merge_df, file = paste0(type,"merge_df.txt"), sep = "\t")

source("./code/00_Description.R")
type_list <- c("qualitative", "non-normal", "qualitative",
               "normal", "normal", rep("qualitative",13),
               rep("non-normal",8), "normal","normal",
               rep("qualitative", 11), "non-normal", "non-normal",
               "qualitative","qualitative")
a <- Descrp(merge_df[,c(3:19,22:47)], type_list,4)


infect_sub <- subset(merge_df, merge_df$Infect == 1)
noninfect_sub <- subset(merge_df, merge_df$Infect == 0)
b <- Descrp(infect_sub[,c(3:19,22:47)], type_list,4)
c <- Descrp(noninfect_sub[,c(3:19,22:47)], type_list,4)

sum_list <- list()
for (xx in c(42,34,43)) {
  matx <- merge_df[,c(22,xx,3,5,6,21)]
  glmx <- glm(Infect~., data = matx, family = binomial(link = "logit")) 
  sum_x <- c(exp(coef(glmx)[2]),
             exp(confint(glmx)[2,]),
             coef(summary(glmx))[2,4])
  valuex <- colnames(merge_df)[[xx]]
  orx <- paste0(sum_x[1] %>% round(4), 
                "(",
                sum_x[2]%>% round(4),
                ",",
                sum_x[3]%>% round(4),
                ")")
  px <- sum_x[4]
  sum_list[[xx]] <- c(valuex, orx, as.character(px))
}
Reduce(rbind,sum_list)
