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
merge_nhanes_df$WTDRD1[is.na(merge_nhanes_df$WTDRD1)] <- 0
fwrite2(merge_nhanes_df, file = paste0( "merge_nhanes_df_all.txt"), sep = "\t")

################
library(survey)
library(bigreadr)
library(tidyverse)
library(haven)
library(jtools)
library(svrepmisc)

setwd("~/Infection_SES/NHANES/01_raw/")
merge_nhanes_df <- fread2("merge_nhanes_df_all.txt")
merge_nhanes_dff <- merge_nhanes_df[which(merge_nhanes_df$retain == 1 & 
                                            merge_nhanes_df$retain_SES == 1),]
  
base_vars <- c("SEQN", "Race", "Sex", 
               "Age", "SES4", "Infect", "Cycle", 
               "retain", "retain_SES",
               "SDMVPSU", "SDMVSTRA",
               "WTINT", "WTMEC", "WTDRD1")

### 1. description on base variables
nhanes_design <- svydesign(data = merge_nhanes_df[,base_vars], 
                           id = ~SDMVPSU, 
                           strata = ~SDMVSTRA, 
                           weights = ~WTINT, 
                           nest=TRUE)
nhc <- subset(nhanes_design, retain == 1 & retain_SES == 1)

## 1.1 continuous variable in normal distribution
# calculating means
mean_vec <- vector()
sd_vec <- vector()
p_vec <- vector()
mean_vec[1] <- svymean(~Age, design = nhc, 
                       na.rm = TRUE)[1] %>% round(4)
# calculating standard deviations
sd_vec[1] <- svysd(~Age, design = nhc, 
                   na.rm = TRUE)[1] %>% round(4)
# by SES
mean_vec[2:4] <- svyby(formula = ~Age, by = ~SES4, design = nhc, 
                  svymean, na.rm = TRUE)[,2] %>% round(4)

for (sesx in 1:3) {
  nhc_sesx <- subset(nhc, SES4 == sesx)
  sd_vec[sesx+1] <- svysd(formula = ~Age, design = nhc_sesx, 
                          na.rm = TRUE)[1] %>% round(4)
}

lm1 <- (svyglm(SES4~Age, design = nhc, na.action = na.omit))
coef(summary(lm1))[2,4]

# by infection
mean_vec[5:6] <- svyby(formula = ~Age, by = ~Infect, design = nhc, 
                       svymean, na.rm = TRUE)[,2] %>% round(4)
for (infectx in 0:1) {
  nhc_infectx <- subset(nhc, Infect == infectx)
  sd_vec[infectx+5] <- svysd(formula = ~Age, design = nhc_infectx, 
        na.rm = TRUE)[1] %>% round(4)
}

paste0(mean_vec, "¡À", sd_vec)

## 1.2 continuous variable in non-normal distribution
# svyquantile(~*, design = nhc, na.rm = TRUE, c(.25,.5,.75),ci=TRUE)


## 1.3 descriptive stats for categorical variables
base_out_df <- data.frame(All = NA,
                          SES1 = NA,
                          SES2 = NA,
                          SES3 = NA,
                          P1 = NA,
                          Infect0 = NA,
                          Infect1 = NA,
                          OR2 = NA,
                          P2 = NA,
                          Var = NA)
for (varx in c("Race", "Sex", "SES4", "Infect")) {
  fomula0 <- paste0("~", varx)
  frq <- table(merge_nhanes_dff[[varx]])
  prop <- svytable(eval(parse(text = fomula0)), design = nhc) %>% 
    prop.table %>% round(4)
  frq_desp0 <- paste0(frq, "(",prop, ")")
  #

  if (varx != "SES4") {
    #
    fomula_ses <- paste0("~", varx, "+SES4")
    frq <- table(merge_nhanes_dff[[varx]], merge_nhanes_dff$SES4)
    prop <- svytable(eval(parse(text = fomula_ses)), design = nhc) %>% 
      prop.table(.,2) %>% round(4)
    frq_desp_ses <- paste0(frq, "(",prop, ")") %>%
      matrix(., nrow = 3, byrow = T)
    #
    lm_ses_fomula <- paste0("SES4~", varx)
    lm_ses <- (svyglm(eval(parse(text = lm_ses_fomula)), design=nhc, na.action = na.omit))
    p_ses <- coef(summary(lm_ses))[2,4]
    
  } else {
    frq_desp_ses <- matrix(-99, nrow = 3, ncol = length(table(merge_nhanes_dff[[varx]])))
    p_ses <- -99
  }
  
  #
  if (varx != "Infect") {
    fomula_infect <- paste0("~", varx, "+Infect")
    frq <- table(merge_nhanes_dff[[varx]], merge_nhanes_dff$Infect)
    prop <- svytable(eval(parse(text = fomula_infect)), design = nhc) %>% 
      prop.table(.,2) %>% round(4)
    frq_desp_infect <- paste0(frq, "(",prop, ")") %>%
      matrix(., nrow = 2, byrow = T)
  } else {
    frq_desp_infect <- matrix(-99, nrow = 2, ncol = length(table(merge_nhanes_dff[[varx]])))
  }

  if (!varx %in% c("Infect", "Race", "Age", "Sex")) {
    #
    glm_infect_fomula <- paste0("Infect~", varx, " + Age + Race + Sex + Cycle")
    glm_infect <- (svyglm(eval(parse(text = glm_infect_fomula)), design=nhc, 
                          family=quasibinomial, na.action = na.omit))
    sum_infect <- c(exp(coef(glm_infect)[2]),
                    exp(confint(glm_infect)[2,]),
                    coef(summary(glm_infect))[2,4])
    or_infect <- paste0(sum_infect[1] %>% round(4), 
                  "(",
                  sum_infect[2]%>% round(4),
                  ",",
                  sum_infect[3]%>% round(4),
                  ")")
    p_infect <- sum_infect[4]
    
  } else {
    or_infect = -99
    p_infect <- -99
  }
  
  outx <- rbind(frq_desp0, 
                frq_desp_ses, p_ses, 
                frq_desp_infect, or_infect, p_infect,
                varx) %>% t
  colnames(outx) <- colnames(base_out_df)
  base_out_df <- rbind(base_out_df, 
                       outx)
}

####################
int_vars <- c("Income", "Education2", "Unemployment", "Non_health_insurance",
              "Activity", "Sleep", "Hypertension", "Diabetes")
mec_vars <- c("lifescore", "Nosmoke", "Noalcohol", "Nodrug", 
              "Mental", "Depression", "Anxiety", "Panic" )
diet_vars <- c("Diet")

describe_list <- list(int_vars, mec_vars, diet_vars)
wt_list <- c("~WTINT", "~WTMEC", "~WTDRD1")

names(describe_list) <- names(wt_list) <- c("INT", "MEC", "DIET")

var_out_list <- list()
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
  
  #
  var_out_df <- data.frame(All = NA,
                           SES1 = NA,
                           SES2 = NA,
                           SES3 = NA,
                           P1 = NA,
                           Infect0 = NA,
                           Infect1 = NA,
                           OR2 = NA,
                           P2 = NA,
                           OR3 = NA,
                           P3 = NA,
                           Var = NA)
  for (varx in var_listx) {
    fomula0 <- paste0("~", varx)
    frq <- table(merge_nhanes_dff[[varx]])
    prop <- svytable(eval(parse(text = fomula0)), design = nhc) %>% 
      prop.table %>% round(4)
    frq_desp0 <- paste0(frq, "(",prop, ")")
    #
    
    if (varx != "SES4") {
      #
      fomula_ses <- paste0("~", varx, "+SES4")
      frq <- table(merge_nhanes_dff[[varx]], merge_nhanes_dff$SES4)
      prop <- svytable(eval(parse(text = fomula_ses)), design = nhc) %>% 
        prop.table(.,2) %>% round(4)
      frq_desp_ses <- paste0(frq, "(",prop, ")") %>%
        matrix(., nrow = 3, byrow = T)
      #
      lm_ses_fomula <- paste0("SES4~", varx)
      lm_ses <- (svyglm(eval(parse(text = lm_ses_fomula)), design=nhc, na.action = na.omit))
      p_ses <- coef(summary(lm_ses))[2,4]
      
    } else {
      frq_desp_ses <- matrix(-99, nrow = 3, ncol = length(table(merge_nhanes_dff[[varx]])))
      p_ses <- -99
    }
    
    
    if (!varx %in% c("Infect", "Race", "Age", "Sex")) {
      #
      fomula_infect <- paste0("~", varx, "+Infect")
      frq <- table(merge_nhanes_dff[[varx]], merge_nhanes_dff$Infect)
      prop <- svytable(eval(parse(text = fomula_infect)), design = nhc) %>% 
        prop.table(.,2) %>% round(4)
      frq_desp_infect <- paste0(frq, "(",prop, ")") %>%
        matrix(., nrow = 2, byrow = T)
      #
      glm_infect_fomula <- paste0("Infect~", varx, " + Age + Race + Sex + Cycle")
      glm_infect <- (svyglm(eval(parse(text = glm_infect_fomula)), design=nhc, 
                            family=quasibinomial, na.action = na.omit))
      sum_infect <- c(exp(coef(glm_infect)[2]),
                      exp(confint(glm_infect)[2,]),
                      coef(summary(glm_infect))[2,4])
      or_infect <- paste0(sum_infect[1] %>% round(4), 
                          "(",
                          sum_infect[2]%>% round(4),
                          ",",
                          sum_infect[3]%>% round(4),
                          ")")
      p_infect <- sum_infect[4]
      #
      glm_infect_fomula2 <- paste0("Infect~", varx, "*SES4 + Age + Race + Sex + Cycle")
      glm_infect2 <- (svyglm(eval(parse(text = glm_infect_fomula2)), design=nhc, 
                            family=quasibinomial, na.action = na.omit))
      n_inter <- grep(":", names(coef(glm_infect2)))
      sum_infect2 <- c(exp(coef(glm_infect2)[n_inter]),
                      exp(confint(glm_infect2)[n_inter,]),
                      coef(summary(glm_infect2))[n_inter,4])
      or_infect2 <- paste0(sum_infect2[1] %>% round(4), 
                          "(",
                          sum_infect2[2]%>% round(4),
                          ",",
                          sum_infect2[3]%>% round(4),
                          ")")
      p_infect2 <- sum_infect2[4]
    } else {
      frq_desp_infect <- matrix(-99, nrow = 2, ncol = length(table(merge_nhanes_dff[[varx]])))
      or_infect = -99
      p_infect <- -99
      or_infect2 = -99
      p_infect2 <- -99
    }
    
    outx <- rbind(frq_desp0, 
                  frq_desp_ses, p_ses, 
                  frq_desp_infect, or_infect, p_infect,
                  or_infect2, p_infect2,
                  varx) %>% t
    colnames(outx) <- colnames(var_out_df)
    var_out_df <- rbind(var_out_df, 
                        outx)
  }
  var_out_list[[nvar_list]] <- var_out_df
}


merge_nhanes_dff_diet <- subset(merge_nhanes_dff, !is.na(merge_nhanes_dff$WTDRD1))
nhanes_design <- svydesign(data = merge_nhanes_dff_diet[,c(base_vars, diet_vars)], 
                           id = ~SDMVPSU, 
                           strata = ~SDMVSTRA, 
                           weights = ~WTDRD1, 
                           nest=TRUE)
nhc <- subset(nhanes_design, retain == 1 & retain_SES == 1)

varx <- "Diet"
fomula0 <- paste0("~", varx)
frq <- table(merge_nhanes_dff_diet[[varx]])
prop <- svytable(eval(parse(text = fomula0)), design = nhc) %>% 
  prop.table %>% round(4)
frq_desp0 <- paste0(frq, "(",prop, ")")
#

fomula_ses <- paste0("~", varx, "+SES4")
frq <- table(merge_nhanes_dff[[varx]], merge_nhanes_dff$SES4)
prop <- svytable(eval(parse(text = fomula_ses)), design = nhc) %>% 
  prop.table(.,2) %>% round(4)
frq_desp_ses <- paste0(frq, "(",prop, ")") %>%
  matrix(., nrow = 3, byrow = T)
#
lm_ses_fomula <- paste0("SES4~", varx)
lm_ses <- (svyglm(eval(parse(text = lm_ses_fomula)), design=nhc, na.action = na.omit))
p_ses <- coef(summary(lm_ses))[2,4]

#
fomula_infect <- paste0("~", varx, "+Infect")
frq <- table(merge_nhanes_dff[[varx]], merge_nhanes_dff$Infect)
prop <- svytable(eval(parse(text = fomula_infect)), design = nhc) %>% 
  prop.table(.,2) %>% round(4)
frq_desp_infect <- paste0(frq, "(",prop, ")") %>%
  matrix(., nrow = 2, byrow = T)
#
glm_infect_fomula <- paste0("Infect~", varx, " + Age + Race + Sex + Cycle")
glm_infect <- (svyglm(eval(parse(text = glm_infect_fomula)), design=nhc, 
                      family=quasibinomial, na.action = na.omit))
sum_infect <- c(exp(coef(glm_infect)[2]),
                exp(confint(glm_infect)[2,]),
                coef(summary(glm_infect))[2,4])
or_infect <- paste0(sum_infect[1] %>% round(4), 
                    "(",
                    sum_infect[2]%>% round(4),
                    ",",
                    sum_infect[3]%>% round(4),
                    ")")
p_infect <- sum_infect[4]
#
glm_infect_fomula2 <- paste0("Infect~", varx, "*SES4 + Age + Race + Sex + Cycle")
glm_infect2 <- (svyglm(eval(parse(text = glm_infect_fomula2)), design=nhc, 
                       family=quasibinomial, na.action = na.omit))
n_inter <- grep(":", names(coef(glm_infect2)))
sum_infect2 <- c(exp(coef(glm_infect2)[n_inter]),
                 exp(confint(glm_infect2)[n_inter,]),
                 coef(summary(glm_infect2))[n_inter,4])
or_infect2 <- paste0(sum_infect2[1] %>% round(4), 
                     "(",
                     sum_infect2[2]%>% round(4),
                     ",",
                     sum_infect2[3]%>% round(4),
                     ")")
p_infect2 <- sum_infect2[4]
print(rbind(frq_desp0, 
            frq_desp_ses, p_ses, 
            frq_desp_infect, or_infect, p_infect,
            or_infect2, p_infect2,
            varx) %>% t)

