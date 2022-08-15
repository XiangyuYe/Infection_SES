rm(list = ls())
gc()
library(bigreadr)
library(dplyr)

# 01_Respiratory/ 02_Digestive/ 03_Blood_Sexually/ 04_Bacterial/
type <- ""
setwd("~/Infection_SES/")
life_factor_dff <- fread2("life_factor_dff.txt")
#### use all control as control
load(paste0("uninfect_ind.RData"))
load(paste0(type,"infect_ind_post.RData"))
life_factor_dff$Infect <- NA
life_factor_dff$Infect[life_factor_dff$eid %in% infect_ind] <- 1
life_factor_dff$Infect[life_factor_dff$eid %in% uninfect_ind] <- 0
life_factor_dff <- subset(life_factor_dff, !is.na(life_factor_dff$Infect))

#### use all else as control
# load(paste0(type,"infect_ind.RData"))
# life_factor_dff$Infect <- ifelse(life_factor_dff$eid %in% infect_ind, 1, 0)

######
pollutant_df <- fread2("pollutant_df.txt")
pollutant_dff <- pollutant_df[match(life_factor_dff$eid,pollutant_df$eid),]
#####
hisodis_df <- fread2("hisodis_pre.txt")
hisodis_dff <- hisodis_df[match(life_factor_dff$eid,hisodis_df$eid),]


merge_df <- Reduce(cbind,list(life_factor_dff[,-which(colnames(life_factor_dff) %in% c("lifescore2", "lifescore3", "Income2", "Education2"))], 
                              pollutant_dff[,-1],
                              hisodis_dff[,-1]))

#######################
if (type == "") {
  beta_air <- vector()
  for (xx in c(23:27)) {
    matx <- merge_df[,c(22,xx,3,5,6,21)]
    glmx <- glm(Infect~., data = matx, family = binomial(link = "logit")) 
    beta_air <- c(beta_air,coef(glmx)[2])
  }
  beta_air_weighted <- beta_air/sum(beta_air)*5
  merge_df$Air_p_score1 <- (as.matrix(merge_df[,c(23:27)]) %*% beta_air_weighted) %>% as.numeric()
  
  ######################
  beta_air <- vector()
  for (xx in c(23:28,31,32)) {
    matx <- merge_df[,c(22,xx,3,5,6,21)]
    glmx <- glm(Infect~., data = matx, family = binomial(link = "logit")) 
    beta_air <- c(beta_air,coef(glmx)[2])
  }
  beta_air_weighted <- beta_air/sum(beta_air)*8
  merge_df$Air_p_score2 <- (as.matrix(merge_df[,c(23:28,31,32)]) %*% beta_air_weighted) %>% as.numeric()
  
} else {
  merge_df_all <- fread2("merge_df.txt")
  merge_df$Air_p_score1 <- merge_df_all$Air_p_score1[match(merge_df$eid, merge_df_all$eid)]
  merge_df$Air_p_score2 <- merge_df_all$Air_p_score2[match(merge_df$eid, merge_df_all$eid)]
}
#####
merge_df$SES_rev <- 4-merge_df$SES
merge_df$Income_rev <- 6-merge_df$Income1
fwrite2(merge_df, file = paste0(type,"merge_df.txt"), sep = "\t")

source("./code/00_Description.R")
type_list <- c("qualitative", "non-normal", "qualitative",
               "normal", "normal", rep("qualitative",13),
               rep("non-normal",8), "normal","normal",
               rep("qualitative", 11), "non-normal", "non-normal",
               "qualitative","qualitative")
a <- Descrp(merge_df[,c(3:19,22:47)], type_list,4)
a[c(74,56,58,60,62,64,76,66,68,70,72),]

infect_sub <- subset(merge_df, merge_df$Infect == 1)
noninfect_sub <- subset(merge_df, merge_df$Infect == 0)
b <- Descrp(infect_sub[,c(3:19,22:47)], type_list,4)
c <- Descrp(noninfect_sub[,c(3:19,22:47)], type_list,4)
b[c(74,56,58,60,62,64,76,66,68,70,72)-1,]
c[c(74,56,58,60,62,64,76,66,68,70,72)-1,]

SES_sub1 <- subset(merge_df, merge_df$SES == 1)
SES_sub2 <- subset(merge_df, merge_df$SES == 2)
SES_sub3 <- subset(merge_df, merge_df$SES == 3)

d <- Descrp(SES_sub1[,c(3:19,22:47)], type_list,4)
e <- Descrp(SES_sub2[,c(3:19,22:47)], type_list,4)
f <- Descrp(SES_sub3[,c(3:19,22:47)], type_list,4)


sum_list <- list()
  for (xx in c(4,7,8,
               46,47,11,12,
               19,13:18,
               44,45,23:28,31,32,
               42,33:36,
               37,
               43,38:40,
               41)) {
  matx <- merge_df[,c(22,xx,3,5,6,21)]
  glmx <- glm(Infect~., data = matx, family = binomial(link = "logit")) 
  sum_x <- c(exp(coef(glmx)[2]),
             exp(confint(glmx)[2,]),
             coef(summary(glmx))[2,4])
  valuex <- colnames(merge_df)[[xx]]
  orx <- paste0(sum_x[1] %>% round(4), 
                "(",
                sum_x[2]%>% round(4),
                ",",
                sum_x[3]%>% round(4),
                ")")
  px <- sum_x[4]
  sum_list[[valuex]] <- c(valuex, orx, as.character(px))
}
Reduce(rbind,sum_list)

sum_list <- list()
for (xx in c(4,7,8,
             46,47,11,12,
             19,13:18,
             44,45,23:28,31,32,
             42,33:36,
             37,
             43,38:40,
             41)) {
  matx <- merge_df[,c(9,xx)]
  lmx <- lm(SES~., data = matx) 
  valuex <- colnames(merge_df)[[xx]]
  px <- coef(summary(lmx))[2,4]
  sum_list[[valuex]] <- c(valuex, as.character(px))
}
Reduce(rbind,sum_list)

#######################
require(ppcor)
merge_df$Education1 <- 8 - merge_df$Education1

cor_df <- matrix(NA, 
                 nrow = 38,
                 ncol = 38) %>% as.data.frame()
dimnames(cor_df) <- list(colnames(merge_df)[c(4,7:19,22:45)],
                         colnames(merge_df)[c(4,7:19,22:45)])
pval_df <- cor_df
#
for (i in c(4,7:19,22:45)) {
  ix <- colnames(merge_df)[[i]]
  print(paste0("Compare in ", ix, "."))
  for (j in c(4,7:19,22:45)) {
    jx <- colnames(merge_df)[[j]]
    print(jx)
    if (i == j) {
      cor_df[ix,jx] <- 1
      pval_df[ix,jx] <- 0
    } else {
      matx <- merge_df[,c(i,j,3,5,6,21)]
      matx <- matx[rowSums(is.na(matx)) == 0,]
      pcorx <- ppcor::pcor(matx, method = "spearman")
      
      cor_df[ix,jx] <- pcorx$estimate[1,2]
      pval_df[ix,jx] <- pcorx$p.value[1,2]
    }
  }
}

write.table(cor_df, file = "cor_df.txt",col.names = T,row.names = T, sep = "\t",quote = F)
write.table(pval_df, file = "pval_df.txt",col.names = T,row.names = T, sep = "\t",quote = F)

