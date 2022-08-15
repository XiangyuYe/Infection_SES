library(foreign)
library(bigreadr)
library(tidyverse)

setwd("~/Infection_SES/NHANES/01_raw/")
hisofdis_files <- fread2("hisofdis.txt")

hisofdis_list <- list()
for (cyclex in c("1999-2000", "2001-2002", "2003-2004")) {
  ## 1. Hypertension
  hypert_file <- hisofdis_files[which(hisofdis_files$cycle == cyclex &
                                        hisofdis_files$description == "Blood Pressure & Cholesterol"),
                                "file"]
  hypert_df <- fread2(paste0("Questionnaire/", cyclex, "_", hypert_file, ".txt"), fill=TRUE)
  
  hypert_df[is.na(hypert_df)] <- -99
  
  hypert <- rep(-99, nrow(hypert_df))
  hypert[hypert_df$BPQ020 == 1] <- 1
  hypert[hypert_df$BPQ020 == 2] <- 0
  
  Hypertension_df <- data.frame(SEQN = hypert_df$SEQN,
                                Hypertension = hypert)
  print(paste0("MSG: Hypertension for ", cyclex, " is ok!"))
  
  ## 2. Diabetes
  diab_file <- hisofdis_files[which(hisofdis_files$cycle == cyclex &
                                      hisofdis_files$description == "Diabetes"),
                              "file"]
  diab_df <- fread2(paste0("Questionnaire/", cyclex, "_", diab_file, ".txt"), fill=TRUE)
  
  diab_df[is.na(diab_df)] <- -99
  
  diab <- rep(-99, nrow(diab_df))
  diab[diab_df$DIQ010 == 1] <- 1
  diab[diab_df$DIQ010 == 2] <- 0
  
  Diabetes_df <- data.frame(SEQN = diab_df$SEQN,
                            Diabetes = diab)
  print(paste0("MSG: Diabetes for ", cyclex, " is ok!"))
  
  ## 3. Mental health
  # 3.1 Depression
  dp_file <- hisofdis_files[which(hisofdis_files$cycle == cyclex &
                                    hisofdis_files$description == "Mental Health - Depression"),
                            "file"]
  dp_df <- fread2(paste0("Questionnaire/", cyclex, "_", dp_file, ".txt"), fill=TRUE)
  
  dp_df[is.na(dp_df)] <- -99
  
  dp <- rep(-99, nrow(dp_df))
  dp[dp_df$CIDDSCOR == 1] <- 1
  dp[dp_df$CIDDSCOR == 5] <- 0
  
  Depression_df <- data.frame(SEQN = dp_df$SEQN,
                              Depression = dp)
  print(paste0("MSG: Depression for ", cyclex, " is ok!"))
  
  # 3.2 Generalized Anxiety Disorder
  gad_file <- hisofdis_files[which(hisofdis_files$cycle == cyclex &
                                     hisofdis_files$description == "Mental Health - Generalized Anxiety Disorder"),
                             "file"]
  gad_df <- fread2(paste0("Questionnaire/", cyclex, "_", gad_file, ".txt"), fill=TRUE)
  
  gad_df[is.na(gad_df)] <- -99
  
  gad <- rep(-99, nrow(gad_df))
  gad[gad_df$CIDGSCOR == 1] <- 1
  gad[gad_df$CIDGSCOR == 5] <- 0
  
  Anxiety_df <- data.frame(SEQN = gad_df$SEQN,
                           Anxiety = gad)
  print(paste0("MSG: Anxiety for ", cyclex, " is ok!"))
  
  # 3.3 Panic Disorder
  pd_file <- hisofdis_files[which(hisofdis_files$cycle == cyclex &
                                    hisofdis_files$description == "Mental Health - Panic Disorder"),
                            "file"]
  pd_df <- fread2(paste0("Questionnaire/", cyclex, "_", pd_file, ".txt"), fill=TRUE)
  
  pd_df[is.na(pd_df)] <- -99
  
  pd <- rep(-99, nrow(pd_df))
  pd[pd_df$CIDPSCOR == 1] <- 1
  pd[pd_df$CIDPSCOR == 5] <- 0
  
  Panic_df <- data.frame(SEQN = pd_df$SEQN,
                         Panic = pd)
  print(paste0("MSG: Panic for ", cyclex, " is ok!"))
  
  ##
  hisofdis_SEQN <- unique(c(Hypertension_df$SEQN,
                            Diabetes_df$SEQN,
                            Depression_df$SEQN, Anxiety_df$SEQN, Panic_df$SEQN))
  
  hisofdis_df <- data.frame(SEQN = hisofdis_SEQN,
                            Hypertension = -99,
                            Diabetes = -99,
                            Mental = 0,
                            Depression = -99,
                            Anxiety = -99,
                            Panic = -99)
  hisofdis_df$Hypertension[match(Hypertension_df$SEQN, hisofdis_SEQN)] <- Hypertension_df[, 2]
  hisofdis_df$Diabetes[match(Diabetes_df$SEQN, hisofdis_SEQN)] <- Diabetes_df[, 2]
  
  hisofdis_df$Mental <- 0
  hisofdis_df$Depression[match(Depression_df$SEQN, hisofdis_SEQN)] <- Depression_df[, 2]
  hisofdis_df$Anxiety[match(Anxiety_df$SEQN, hisofdis_SEQN)] <- Anxiety_df[, 2]
  hisofdis_df$Panic[match(Panic_df$SEQN, hisofdis_SEQN)] <- Panic_df[, 2]

  hisofdis_df$Mental[hisofdis_df$Panic == 1 | 
                       hisofdis_df$Depression == 1 | 
                       hisofdis_df$Anxiety == 1] <- 1
  hisofdis_df$Mental[hisofdis_df$Panic == -99 & 
                       hisofdis_df$Depression == -99 & 
                       hisofdis_df$Anxiety == -99] <- -99
  
  hisofdis_list[[cyclex]] <- hisofdis_df
  
}

#
hisofdis_df_all <- Reduce(rbind, hisofdis_list)
save(hisofdis_list, file = "disofhis_list.RData")
fwrite2(hisofdis_df_all, file = "hisofdis_df_all.txt", sep = "\t")


