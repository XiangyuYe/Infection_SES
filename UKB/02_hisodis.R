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
                   interval(enroll_date, 
                            x) %>%
                     time_length(unit = "year"))
  # with pre to be T and post to be F
  return(interv < 0)
})


ICD9_mat <- ICD_all_mat[,ICD9_code]
ICD10_mat <- ICD_all_mat[,ICD10_code]
data_path <- paste0("~/Infection_SES/")

cadio_ind <- list()
for (type in c("CAD","AF", "stroke", "Hypert", "Diabetes")) {
  
  # cadio diseases in ICD10
  cadio10_code <- fread2(paste0(data_path,"ICD10_",type,".txt"))[,"Value"]
  cadio10_mat <- lapply(1:ncol(ICD10_mat), 
                         function(x){
                           is_cadio <- ICD10_mat[,x] %in% cadio10_code
                           return(is_cadio)
                         }) %>% Reduce(cbind, .)
  
  # cadio diseases in ICD9
  cadio9_code <- fread2(paste0(data_path,"ICD10_",type,".txt"))[,"Value"]
  cadio9_mat <- lapply(1:ncol(ICD9_mat), 
                        function(x){
                          is_cadio <- ICD9_mat[,x] %in% cadio9_code
                          return(is_cadio)
                        }) %>% Reduce(cbind, .)
  
  diag_cadio_mat <- cbind(cadio10_mat, cadio9_mat)
  cadio_ind[[type]] <- ICD_all_mat[which(rowSums(diag_cadio_mat * diag_pre_post_mat, na.rm = T)>0),"eid"]
  
  print(length(cadio_ind[[type]]))
  ##
}
save(cadio_ind, file  = paste0(data_path,"cadio_ind.RData"))


##### mental health #####
self_rep_code <- c(paste0("20002-0.",0:33))
bp_code <- c("20126-0.0")
ment_diag_code <- c(paste0("20544-0.",1:16))

ment_mat <- fread2("/c/home/Datasets/ukb/ukb47503.csv", 
                   select = c("eid", 
                              self_rep_code, bp_code, ment_diag_code))

# anxiety
anx_ind <- c(lapply(ment_mat[,self_rep_code], function(x){which(x %in% c("1287"))}),
             lapply(ment_mat[,ment_diag_code], function(x){which(x %in% c("15"))})) %>%
  unlist() %>% unique()

# depression
dp_ind <- c(lapply(ment_mat[,self_rep_code], function(x){which(x %in% c("1286"))}),
            lapply(ment_mat[,bp_code], function(x){which(x %in% c("3","4","5"))}),
            lapply(ment_mat[,ment_diag_code], function(x){which(x %in% c("11"))})) %>%
  unlist() %>% unique()

# bp
bp_ind <- c(lapply(ment_mat[,self_rep_code], function(x){which(x %in% c("1291"))}),
            lapply(ment_mat[,bp_code], function(x){which(x %in% c("1","2"))}),
            lapply(ment_mat[,ment_diag_code], function(x){which(x %in% c("10"))})) %>%
  unlist() %>% unique()

ment_ind <- list(anx = ment_mat[anx_ind,"eid"],
                dp = ment_mat[dp_ind,"eid"],
                bp = ment_mat[bp_ind,"eid"])

save(ment_ind, file = "~/Infection_SES/ment_ind.RData")


###############
cancer_diag_code <- c("40005-0.0", 
                      "40006-0.0")
cancer_diag_mat <- fread2("~/Datasets/ukb/ukb47503.csv", 
                          select = c("eid", "53-0.0", 
                                     cancer_diag_code))
cancer_diag_mat[cancer_diag_mat == ""] <- NA

enroll_date <- cancer_diag_mat[["53-0.0"]]
diag_date <- cancer_diag_mat[["40005-0.0"]]
cancer_diag_mat$interv <- ifelse(is.na(diag_date), 
                                   NA,
                                   interval(enroll_date, 
                                            diag_date) %>% 
                                   time_length(unit = "year"))
  # with pre to be T and post to be F
cancer_ind <- cancer_diag_mat[!is.na(cancer_diag_mat[["40006-0.0"]]) &
                                cancer_diag_mat$interv <= 0,"eid"]
save(cancer_ind, file = "~/Infection_SES/cancer_ind.RData")

###############
setwd("~/Infection_SES/")
load(paste0("cadio_ind.RData"))
load(paste0("ment_ind.RData"))
load(paste0("cancer_ind.RData"))

hisodis <- data.frame(eid = ICD_all_mat$eid,
                      CAD = NA,
                      AF = NA,
                      stroke = NA, 
                      Hypert = NA,
                      diab = NA, 
                      anx = NA,
                      dp = NA,
                      bp = NA,
                      cancer = NA)
hisodis[,-1] <- sapply(c(cadio_ind, ment_ind, list(cancer_ind)),
                       function(x){
                         ifelse(hisodis$eid %in% x, 1, 0)
                       })
hisodis$CVD <- ifelse(rowSums(hisodis[,2:5], na.rm = T) > 0,
                      1, 0)
hisodis$ment <- ifelse(rowSums(hisodis[,7:9], na.rm = T) > 0,
                       1, 0)
fwrite2(hisodis, file = "hisodis_pre.txt", sep = "\t")
