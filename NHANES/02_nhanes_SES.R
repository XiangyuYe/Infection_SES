library(foreign)
library(bigreadr)
library(tidyverse)

setwd("~/Infection_SES/NHANES/01_raw/")
demog_list <- fread2("NHANES_Demographics_Data_f.txt")
ques_file <- fread2("NHANES_Questionnaire_Data_f.txt")

# cyclex <- "2005-2006"

SES_list <- list()
for (cyclex in c("1999-2000", "2001-2002", "2003-2004",
                 "2005-2006", "2007-2008", "2009-2010", 
                 "2011-2012", "2013-2014", "2015-2016", 
                 "2017-2018")) {
  
  demog_file <- demog_list[which(demog_list$Years == cyclex), "Files"]
  demog_df <- fread2(paste0("Demographics/", cyclex, "_", demog_file, ".txt"), fill=TRUE)
  
  ## 1. Family income 
  income <- rep(0, nrow(demog_df))
  income[which(demog_df$INDFMPIR < 4 & demog_df$INDFMPIR > 1)] <- 1
  income[which(demog_df$INDFMPIR <= 1 & demog_df$INDFMPIR >= 0)] <- 2
  income[which(is.na(demog_df$INDFMPIR) | demog_df$INDFMPIR < 0)] <- -99
  
  income_df <- data.frame(SEQN = demog_df$SEQN,
                          Income = income)
  print(paste0("MSG: Family income for ", cyclex, " is ok!"))
  
  ## 2. Education
  edu <- rep(0, nrow(demog_df))
  edu[which(demog_df$DMDEDUC2 == 3)] <- 1
  edu[which(demog_df$DMDEDUC2 %in% c(1, 2))] <- 2
  edu[which(is.na(demog_df$DMDEDUC2) | 
              demog_df$DMDEDUC2 > 5 | 
              demog_df$DMDEDUC2 < 0)] <- -99
  
  education_df <- data.frame(SEQN = demog_df$SEQN,
                             Education = edu)
  print(paste0("MSG: Education for ", cyclex, " is ok!"))
  
  ## 3. Employment
  occup_file <- ques_file[which(ques_file$Years == cyclex &
                                  ques_file$`Data.File.Name` == "Occupation"),
                          "Files"]
  occup_df <- fread2(paste0("Questionnaire/", cyclex, "_", occup_file, ".txt"), fill=TRUE)
  
  occup_col <- ifelse(cyclex == "1999-2000", "OCQ150", "OCD150")
  
  occup_df[is.na(occup_df)] <- -99
  unemploy <- rep(0, nrow(occup_df))
  unemploy[which(occup_df[[occup_col]] == 3 |
                   (occup_df[[occup_col]] == 4 & !occup_df$OCQ380 %in% c(2,3)))] <- 1
  unemploy[which(is.na(occup_df[[occup_col]]) | 
                   occup_df[[occup_col]] < 0 | 
                   occup_df[[occup_col]] > 5)] <- -99
  
  unemploy_df <- data.frame(SEQN = occup_df$SEQN,
                            Unemployment = unemploy)
  print(paste0("MSG: Employment for ", cyclex, " is ok!"))
  
  ## 4. Health Insurance
  heal_insur_file <- ques_file[which(ques_file$Years == cyclex &
                                       ques_file$`Data.File.Name` == "Health Insurance"),
                               "Files"]
  
  heal_insur_df <- fread2(paste0("Questionnaire/", cyclex, "_", heal_insur_file, ".txt"), fill=TRUE)
  
  if (cyclex %in% c("1999-2000", "2001-2002", "2003-2004")) {
    heal_insur_df[is.na(heal_insur_df)] <- -99
    
    non_heal_insur <- rep(1, nrow(heal_insur_df))
    non_heal_insur[which(heal_insur_df$HID030A == 1 |
                           heal_insur_df$HID030C == 1 | 
                           heal_insur_df$HID030E == 1)] <- 0
    non_heal_insur[which(heal_insur_df$HID010 == 2)] <- 2
    non_heal_insur[which(is.na(heal_insur_df$HID010) | 
                           heal_insur_df$HID010 > 2)] <- -99
  } else {
    heal_insur_df[is.na(heal_insur_df)] <- -99
    
    non_heal_insur <- rep(1, nrow(heal_insur_df))
    non_heal_insur[which(heal_insur_df$HIQ031A == 14 |
                           heal_insur_df$HIQ031C == 16 | 
                           heal_insur_df$HIQ031J == 23)] <- 0
    non_heal_insur[which(heal_insur_df$HIQ011 == 2)] <- 2
    non_heal_insur[which(is.na(heal_insur_df$HIQ011) | 
                           heal_insur_df$HIQ011 > 2)] <- -99
  }
  
  non_heal_insur_df <- data.frame(SEQN = heal_insur_df$SEQN,
                                  Non_health_insurance = non_heal_insur)
  print(paste0("MSG: Health Insurance for ", cyclex, " is ok!"))
  
  ## SES df
  SEQN_retain <- demog_df$SEQN[!demog_df$RIDEXPRG %in% c(1,3) &
                                 demog_df$RIDAGEYR >= 20]
  SES_SEQN <- Reduce(intersect, list(income_df$SEQN,
                                     education_df$SEQN,
                                     unemploy_df$SEQN,
                                     non_heal_insur_df$SEQN,
                                     SEQN_retain))
  SES_df <- data.frame(SEQN = SES_SEQN,
                       Income = income_df$Income[match(SES_SEQN, income_df$SEQN)],
                       Education = education_df$Education[match(SES_SEQN, education_df$SEQN)],
                       Unemployment = unemploy_df$Unemployment[match(SES_SEQN, unemploy_df$SEQN)],
                       Non_health_insurance = non_heal_insur_df$Non_health_insurance[match(SES_SEQN, non_heal_insur_df$SEQN)])
  
  SES_list[[cyclex]] <- SES_df[rowSums(SES_df == -99) == 0,]
}

SES_df_all <- Reduce(rbind, SES_list)
save(SES_list, file = "SES_list.RData")
fwrite2(SES_df_all, file = "SES_df_all.txt", sep = "\t")

##########
library(poLCA)
library(bigreadr)
SES_df_all <- fread2("SES_df_all.txt")
for (xx in 2:ncol(SES_df_all)) {
  SES_df_all[,xx] <- factor(SES_df_all[,xx])
}

SES_LCA_list <- list()
for (n_class in 1:6) {
  SES_LCA_list[[n_class]] <- poLCA(cbind(Income, Education, Unemployment, Non_health_insurance)~1, 
                                   data = SES_df_all, 
                                   nclass = n_class, maxiter = 10000, graphs = FALSE, 
                                   tol = 1e-6, na.rm = TRUE, probs.start = NULL, nrep = 1, 
                                   verbose = TRUE, calc.se = TRUE)
}

save(SES_LCA_list, file = "SES_LCA_list.RData")
