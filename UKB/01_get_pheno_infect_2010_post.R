rm(list = ls())
gc()
library(bigreadr)
library(dplyr)
library(lubridate)
library(stringr)

type <- "05_2010/"
setwd("~/Infection_SES/")

## 1. is enrolled in 2010
load("~/Datasets/ukb/pheno/sqc.RData")
ICD_all_mat <- fread2("~/Datasets/ukb/ukb47503.csv", 
                      select = c("eid", "53-0.0"))

enroll_year <- str_split(ICD_all_mat[["53-0.0"]], "-", simplify = T)[,1]
is_2010 <- ICD_all_mat[which(enroll_year == 2010),"eid"]


## 2. subset participants enrolled 2010
merge_df_all <- fread2("merge_df.txt")
merge_df_2010 <- subset(merge_df_all, merge_df_all$eid %in% is_2010)
fwrite2(merge_df_2010, file = paste0(type,"merge_df.txt"), sep = "\t")

#####
type = "05_2010/"
merge_df <- fread2(paste0(type, "merge_df.txt"))
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
for (xx in c(4,7,8,
             46,47,11,12,
             19,13:18,
             44,45,23:28,31,32,
             42,33:36,
             37,
             43,38:40,
             41)) {
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
  sum_list[[valuex]] <- c(valuex, orx, as.character(px))
}
Reduce(rbind,sum_list)


