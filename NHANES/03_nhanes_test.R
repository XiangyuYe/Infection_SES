library(foreign)
library(bigreadr)
library(tidyverse)

setwd("~/Infection_SES/NHANES/01_raw/")

cov_df_all <- fread2("cov_df_all.txt")
SES_df_all <- fread2("SES_df_all.txt")
infect_df_all <- fread2("infect_df_all.txt")
lifestyle_df_all <- fread2("lifestyle_df_all.txt")
hisofdis_df_all <- fread2("hisofdis_df_all.txt")

merge_nhanes_df <- merge(cov_df_all, SES_df_all, by = "SEQN",
                         all.x = T)
merge_nhanes_df <- merge(merge_nhanes_df, infect_df_all, by = "SEQN",
                         all.x = T)
merge_nhanes_df <- merge(merge_nhanes_df, lifestyle_df_all, by = "SEQN",
                         all.x = T)
merge_nhanes_df <- merge(merge_nhanes_df, hisofdis_df_all, by = "SEQN",
                         all.x = T)

merge_nhanes_df[merge_nhanes_df == -99] <- NA
merge_nhanes_df$WTINT <- ifelse(merge_nhanes_df$Cycle %in% c("1999-2000", "2001-2002"),
                                merge_nhanes_df$WTINT/10*2,
                                merge_nhanes_df$WTINT/10*1)
merge_nhanes_df$WTMEC <- ifelse(merge_nhanes_df$Cycle %in% c("1999-2000", "2001-2002"),
                                merge_nhanes_df$WTMEC/10*2,
                                merge_nhanes_df$WTMEC/10*1)
merge_nhanes_df$WTDRD1 <- ifelse(merge_nhanes_df$Cycle %in% c("1999-2000", "2001-2002"),
                                 merge_nhanes_df$WTDRD1/10*2,
                                 merge_nhanes_df$WTDRD1/10*1)

fwrite2(merge_nhanes_df, file = paste0( "merge_nhanes_df_all.txt"), sep = "\t")

#################
# [1] "SEQN"                 "Race"                 "Sex"                 
# [4] "Age"                  "WTINT"                "WTMEC"               
# [7] "SDMVPSU"              "SDMVSTRA"             "Cycle"               
# [10] "retain"               "Income"               "Education"           
# [13] "Education2"           "Unemployment"         "Non_health_insurance"
# [16] "retain_SES"           "SES3"                 "SES4"                
# [19] "Infect"               "lifescore"            "Nosmoke"             
# [22] "Activity"             "Diet"                 "Noalcohol"           
# [25] "Sleep"                "Nodrug"               "retain_lifestyle"    
# [28] "Hypertension"         "Diabetes"             "Mental"              
# [31] "Depression"           "Anxiety"              "Panic"   
library(survey)
library(bigreadr)
library(tidyverse)
NHANES_all <- svydesign(data=One, id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC4YR, nest=TRUE)                    
NHANES <- subset(NHANES_all, inAnalysis==1)



svymean(~Depression, NHANES)