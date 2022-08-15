rm(list = ls())
gc()
library(ggplot2)
library(RColorBrewer)
library(scales)
library(viridis)
library(dplyr)
# c("","01_Respiratory/", "02_Digestive/",
#   "03_Blood_Sexually/", "05_2010/")
for (type in c("","01_Respiratory/", "02_Digestive/",
               "03_Blood_Sexually/", "05_2010/")) {
  print(type)
  
  setwd(paste0("~/Infection_SES/", type))
  load("group_mat.RData")
  group_mat$CIs <- paste0(group_mat[,"OR"] %>% round(4), 
                          "(",
                          group_mat[,"low_CI"]%>% round(4),
                          ",",
                          group_mat[,"high_CI"]%>% round(4),
                          ")")
  print(group_mat[,c("CIs","P","SES")])
  load("sum_sl.RData")
  sum_sl$CIs <- paste0(sum_sl[,"OR"] %>% round(4), 
                       "(",
                       sum_sl[,"low_CI"]%>% round(4),
                       ",",
                       sum_sl[,"high_CI"]%>% round(4),
                       ")")
  print(sum_sl[,c("CIs","P","SES")])
}

