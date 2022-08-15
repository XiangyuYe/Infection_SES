library(foreign)
library(bigreadr)
library(tidyverse)

setwd("~/Infection_SES/NHANES/01_raw/")

cov_list <- list()
demog_list <- fread2("NHANES_Demographics_Data_f.txt")

for (cyclex in c("1999-2000", "2001-2002", "2003-2004",
                 "2005-2006", "2007-2008", "2009-2010", 
                 "2011-2012", "2013-2014", "2015-2016", 
                 "2017-2018")) {
  
  demog_file <- demog_list[which(demog_list$Years == cyclex), "Files"]
  demog_df <- fread2(paste0("Demographics/", cyclex, "_", demog_file, ".txt"), fill=TRUE)
  
  if (cyclex %in% c("1999-2000", "2001-2002")) {
    weight_col <-  c("WTINT4YR", "WTMEC4YR")
  } else {
    weight_col <-  c("WTINT2YR", "WTMEC2YR")
    
  }
  
  cov_df <- demog_df[,c("SEQN", "RIDRETH1", "RIAGENDR", "RIDAGEYR", 
                        weight_col, "SDMVPSU", "SDMVSTRA")]
  colnames(cov_df) <- c("SEQN", "Race", "Sex", "Age", 
                        "WTINT", "WTMEC", "SDMVPSU", "SDMVSTRA")
  cov_df$Race <- ifelse(is.na(cov_df$Race), NA, 
                        ifelse(cov_df$Race %in% c(2, 3), 0,
                               ifelse(cov_df$Race %in% c(1, 4), 1, NA)))
  cov_df$Cycle <- cyclex
  
  SEQN_retain <- demog_df$SEQN[!demog_df$RIDEXPRG %in% c(1,3) &
                                 demog_df$RIDAGEYR >= 20]
  cov_df$retain <- 0
  cov_df$retain[match(SEQN_retain, cov_df$SEQN)] <- 1
  cov_list[[cyclex]] <- cov_df
}

cov_df_all <- Reduce(rbind, cov_list)

fwrite2(cov_df_all, file = paste0( "cov_df_all.txt"), sep = "\t")
save(cov_list, file = paste0( "cov_list.RData"))

