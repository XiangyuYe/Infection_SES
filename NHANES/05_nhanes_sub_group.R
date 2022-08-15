library(survey)
library(bigreadr)
library(dplyr)
library(haven)
library(jtools)
library(svrepmisc)

setwd("~/Infection_SES/NHANES/01_raw/")
merge_nhanes_df <- fread2("merge_nhanes_df_all.txt")
merge_nhanes_df$lifescoref <- factor(merge_nhanes_df$lifescore)

base_vars <- c("SEQN", "Race", "Sex", 
               "Age", "SES4", "Infect", "Cycle", 
               "retain", "retain_SES",
               "SDMVPSU", "SDMVSTRA",
               "WTINT", "WTMEC", "WTDRD1")
int_vars <- c("Activity", "Sleep", "Hypertension", "Diabetes")
mec_vars <- c("lifescoref", "Nosmoke", "Noalcohol", "Nodrug", 
              "Mental", "Depression", "Anxiety", "Panic" )
diet_vars <- c("Diet")

describe_list <- list(int_vars, mec_vars, diet_vars)
wt_list <- c("~WTINT", "~WTMEC", "~WTDRD1")

names(describe_list) <- names(wt_list) <- c("INT", "MEC", "DIET")

#####
sum_list <- list()
for (nvar_list in c("INT", "MEC")) {
  var_listx <- describe_list[[nvar_list]]
  wtx <- wt_list[nvar_list]
  #
  nhanes_design <- svydesign(data = merge_nhanes_df[,c(base_vars, var_listx)], 
                             id = ~SDMVPSU, 
                             strata = ~SDMVSTRA, 
                             weights = eval(parse(text = wtx)), 
                             nest=TRUE)
  nhc <- subset(nhanes_design, retain == 1 & retain_SES == 1)
  
  for (varx in var_listx) {
    sum_list_x <- lapply(c(1:3),function(x){
      glm_infect_fomula <- paste0("Infect ~ ",varx," + Sex + Age + Race + Cycle")
      nhc_sesx <- subset(nhc, SES4 == x)
      glm_infect <- (svyglm(eval(parse(text = glm_infect_fomula)), design = nhc_sesx, 
                            family = quasibinomial, na.action = na.omit))
      sum_infect <- data.frame(exp(coef(glm_infect)),
                               exp(confint(glm_infect)),
                               coef(summary(glm_infect))[,4],
                               varx,
                               x)
      colnames(sum_infect) <- c("OR", "low_CI", "high_CI", "P",
                                "Var", "SES")
      return(sum_infect[grep(varx, rownames(sum_infect)),])
    })
    
    sum_list[[varx]] <- Reduce(rbind, sum_list_x)
  }
}

#
merge_nhanes_df$WTDRD1[is.na(merge_nhanes_df$WTDRD1)] <- 0
nhanes_design <- svydesign(data = merge_nhanes_df[,c(base_vars, diet_vars)], 
                           id = ~SDMVPSU, 
                           strata = ~SDMVSTRA, 
                           weights = ~WTDRD1, 
                           nest=TRUE)
nhc <- subset(nhanes_design, retain == 1 & retain_SES == 1 & WTDRD1 > 0)

varx <- "Diet"
sum_list_x <- lapply(c(1:3),function(x){
    glm_infect_fomula <- paste0("Infect ~ ",varx," + Sex + Age + Race + Cycle")
    nhc_sesx <- subset(nhc, SES4 == x)
    glm_infect <- (svyglm(eval(parse(text = glm_infect_fomula)), design = nhc_sesx, 
                          family = quasibinomial, na.action = na.omit))
    sum_infect <- data.frame(exp(coef(glm_infect)),
                             exp(confint(glm_infect)),
                             coef(summary(glm_infect))[,4],
                             varx,
                             x)
    colnames(sum_infect) <- c("OR", "low_CI", "high_CI", "P",
                              "Var", "SES")
    return(sum_infect[grep(varx, rownames(sum_infect)),])
  })
  
sum_list[[varx]] <- Reduce(rbind, sum_list_x)

group_mat <- Reduce(rbind,sum_list)
save(group_mat, file = paste0("group_mat.RData"))

# SES*lifescore 
merge_nhanes_df$SES_rev <- 4 - merge_nhanes_df$SES4
merge_nhanes_df$SES_life <- paste0(merge_nhanes_df$SES_rev,"_", merge_nhanes_df$lifescore)
merge_nhanes_df$SES_life[grep("NA",merge_nhanes_df$SES_life)] <- NA
merge_nhanes_df$SES_life <- factor(merge_nhanes_df$SES_life)

nhanes_design <- svydesign(data = merge_nhanes_df, 
                           id = ~SDMVPSU, 
                           strata = ~SDMVSTRA, 
                           weights = ~WTMEC, 
                           nest=TRUE)
nhc <- subset(nhanes_design, retain == 1 & retain_SES == 1)

fomu_sl <- paste0("Infect ~ SES_life + Sex + Age + Race + Cycle")
glm_sl <- (svyglm(eval(parse(text = fomu_sl)), design = nhc, 
                  family = quasibinomial, na.action = na.omit))
sum_sl <- data.frame(exp(coef(glm_sl)),
                     exp(confint(glm_sl)),
                     coef(summary(glm_sl))[,4])

colnames(sum_sl) <- c("OR", "low_CI", "high_CI", "P")
sum_sl <- sum_sl[grep("SES_life", rownames(sum_sl)),]
sum_sl <- rbind(c(1,1,1,0),sum_sl)
sum_sl$SES <- rep(c(1:3), each = 3)
sum_sl$Lifestyle.scores <- rep(c(1:3), times = 3)
save(sum_sl, file = "sum_sl.RData")


