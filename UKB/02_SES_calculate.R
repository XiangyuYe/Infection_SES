rm(list = ls())
gc()
library(bigreadr)
library(dplyr)
library(poLCA)

setwd("~/Infection_SES/")
life_factor_df <- fread2("life_factor_df_test.txt")
life_factor_dff <- subset(life_factor_df, life_factor_df$QC & 
                            rowSums(is.na(life_factor_df[,1:13]))==0)

##
SES_LCA_list <- list()
for (n_class in 1:10) {
  SES_LCA_list[[n_class]] <- poLCA(cbind(Income1, Education1, Employment)~1, 
                                   data = life_factor_dff, 
                                   nclass = n_class, maxiter = 10000, graphs = FALSE, 
                                   tol = 1e-6, na.rm = TRUE, probs.start = NULL, nrep = 1, 
                                   verbose = TRUE, calc.se = TRUE)
}

save(SES_LCA_list, file = "SES_LCA_list.RData")

########
load("SES_LCA_list.RData")
source("code/00_LCA_out.R")
#
LCA_out(SES_LCA_list,3)
LCA_out(SES_LCA_list,4)
#
life_factor_dff$SES <- ifelse(SES_LCA_list[[3]]$predclass == 2, 1,
                              ifelse(SES_LCA_list[[3]]$predclass == 1, 2, 3))

life_factor_dff <- life_factor_dff[,c(1:8,25,9,11,13:22,23,24,10,12)]

fwrite2(life_factor_dff, file = "life_factor_dff.txt")
