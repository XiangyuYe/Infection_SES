rm(list = ls())
gc()
library(bigreadr)
library(dplyr)
library(lubridate)
library(stringr)


## ICD code
## ICD code
## the secondary has no influence on outcomes
ICD10_code <- c(paste0("41202-0.", 0:74),    # Diagnoses - main ICD10
                # paste0("41204-0.", 0:183),   # Diagnoses - secondary ICD10
                paste0("41270-0.",0:222))    # Diagnoses - ICD10
ICD10_date <- c(paste0("41262-0.", 0:74),
                paste0("41280-0.",0:222))

ICD9_code <-c(paste0("41203-0.", 0:27),      # Diagnoses - main ICD9
              # paste0("41205-0.", 0:29),      # Diagnoses - secondary ICD9
              paste0("41271-0.", 0:46))      # Diagnoses - ICD9
ICD9_date <- c(paste0("41263-0.", 0:27),
               paste0("41281-0.",0:46))

load("~/Datasets/ukb/pheno/sqc.RData")
ICD_all_mat <- fread2("~/Datasets/ukb/ukb47503.csv", 
                      select = c("eid", "53-0.0",
                                 ICD10_code, ICD10_date,
                                 ICD9_code, ICD9_date))

diag_date_mat <- ICD_all_mat[,c("eid",
                                ICD10_date,
                                ICD9_date)]



enroll_date <- ICD_all_mat[["53-0.0"]]
diag_pre_post_mat <- sapply(diag_date_mat[,-1], function(x){
  interv <- ifelse(is.na(x), NA,
                   interval(enroll_date[!is.na(x)], 
                            x[!is.na(x)]) %>%
                     time_length(unit = "year"))
  # with pre to be F and post to be T
  return(interv >= 0)
})


for (type in c("", "01_Respiratory","02_Digestive", "03_Blood_Sexually", "04_Bacterial")) {
  
  # type <- ""
  # infectious diseases in ICD10
  data_path <- paste0("~/Infection_SES/", type,"/")
  
  infect10_code <- fread2(paste0(data_path,"ICD10_infect.txt"))[,"Value"]
  ICD10_mat <- ICD_all_mat[,ICD10_code]
  infect10_mat <- lapply(1:ncol(ICD10_mat), 
                         function(x){
                           is_infect <- ICD10_mat[,x] %in% infect10_code
                           return(is_infect)
                         }) %>% Reduce(cbind, .)
  
  # infectious diseases in ICD9
  infect9_code <- fread2(paste0(data_path,"ICD9_infect.txt"))[,"Value"]
  ICD9_mat <- ICD_all_mat[,ICD9_code]
  infect9_mat <- lapply(1:ncol(ICD9_mat), 
                        function(x){
                          is_infect <- ICD9_mat[,x] %in% infect9_code
                          return(is_infect)
                        }) %>% Reduce(cbind, .)
  
  diag_mat <- cbind(infect10_mat, infect9_mat)
  infect_ind <- ICD_all_mat[which(rowSums(diag_mat * diag_pre_post_mat, na.rm = T)>0),"eid"]
  
  print(length(infect_ind))
  save(infect_ind, file  = paste0(data_path,"infect_ind_post.RData"))
  ##
  if (type == "") {
    uninfect_ind <- setdiff(ICD_all_mat$eid, infect_ind)
    save(uninfect_ind, file  = paste0(data_path,"uninfect_ind_post.RData"))
    infect_ind_pre <- ICD_all_mat[which(rowSums(diag_mat * !diag_pre_post_mat, na.rm = T)>0),"eid"]
    save(infect_ind_pre, file  = paste0(data_path,"infect_ind_pre.RData"))
  }
}


