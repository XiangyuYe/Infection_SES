rm(list = ls())
gc()
library(MatchIt)
library(bigreadr)
library(dplyr) 

setwd("~/Infection_SES/")

# 01_Respiratory/ 02_Digestive/ 03_Blood_Sexually/ 04_Bacterial/ 05_2010/
type <- "03_Blood_Sexually/"
redo_match <- "F"

# load("infect_ind.RData")
merge_df <- fread2(paste0(type,"merge_df.txt"))
if (redo_match) {
  formula_match <- Infect ~ Age + Sex + EUR_eth + center
  match_out = matchit (formula=formula_match, 
                       data = merge_df,
                       distance = "glm", link = "logit", 
                       method ="nearest", ratio = 4)
  summary(match_out)
  # plot(match_out, type = "jitter")
  # plot(match_out, type = "hist")
  # output
  match_data <- match.data(match_out)
  fwrite2(match_data, file = paste0(type,"merge_df_match.txt"))
} else if (file.exists(paste0(type,"merge_df_match.txt"))) {
  merge_df_match_old <- fread2(paste0(type,"merge_df_match.txt"))
  match_eid <- merge_df_match_old$eid
  match_data <- cbind(merge_df[match(match_eid, merge_df$eid),],
                          merge_df_match_old[,c("distance", "weights", "subclass")])

  fwrite2(match_data, file = paste0(type,"merge_df_match.txt"))
}


######
rm(list = ls())
gc()
library(MatchIt)
library(bigreadr)
library(dplyr) 

setwd("~/Infection_SES/")

# # 01_Respiratory/ 02_Digestive/ 03_Blood_Sexually/ 04_Bacterial/ 05_2010/
# for (type in c("01_Respiratory/", "02_Digestive/", "03_Blood_Sexually/", "04_Bacterial/", "05_2010/")) {
# 
type <- "03_Blood_Sexually/"
# merge_df_match <- fread2(paste0(type,"merge_df_match.txt"))
# merge_df_match$SES_rev <- 4-merge_df_match$SES
# merge_df_match$Income_rev <- 6-merge_df_match$Income1
# fwrite2(merge_df_match, file = paste0(type,"merge_df_match.txt"))
# 
# }
merge_df_match <- fread2(paste0(type,"merge_df_match.txt"))

source("./code/00_Description.R")
merge_df_match <- fread2(paste0(type,"merge_df_match.txt"))
type_list <- c("qualitative", "non-normal", "qualitative",
               "normal", "normal", rep("qualitative",13),
               rep("non-normal",8), "normal","normal",
               rep("qualitative", 11), "non-normal", "non-normal",
               "qualitative","qualitative")
a <- Descrp(merge_df_match[,c(3:19,22:47)], type_list,4)
### 
infect_sub <- subset(merge_df_match, merge_df_match$Infect == 1)
noninfect_sub <- subset(merge_df_match, merge_df_match$Infect == 0)
b <- Descrp(infect_sub[,c(3:19,22:47)], type_list,4)
c <- Descrp(noninfect_sub[,c(3:19,22:47)], type_list,4)

a[c(74,56,58,60,62,64,76,66,68,70,72),]
b[c(74,56,58,60,62,64,76,66,68,70,72)-1,]
c[c(74,56,58,60,62,64,76,66,68,70,72)-1,]
# sapply(which(type_list == "qualitative"), function(a){
#   p <- chisq.test(x=merge_df_match[,a+2],
#                   y=merge_df_match$Infect)$`p.value`
#   return(paste0(colnames(merge_df_match)[a+2], ": ",p))
# })%>% data.frame()
# 
# sapply(which(type_list == "non-normal"), function(a){
#   value <- merge_df_match[,a+2]
#   group <- merge_df_match$Infect
#   p <- wilcox.test(value~group)$`p.value`
#   return(paste0(colnames(merge_df_match)[a+2], ": ",p))
# })%>% data.frame()
# 
# sapply(which(type_list == "normal"), function(a){
#   value <- merge_df_match[,a+2]
#   group <- merge_df_match$Infect
#   p <- t.test(value~group)$`p.value`
#   return(paste0(colnames(merge_df_match)[a+2], ": ",p))
# })%>% data.frame()

#####################
# cov_mat <- merge_df[,c(3:6, 8:11)]
sum_list <- list()
for (xx in c(4,7,8,
             # 46,47,11,12,
             # 19,13:18,
             # 44,45,23:28,31,32,
             42,33:36,
             37,
             43,38:40,
             41)) {
  matx <- merge_df_match[,c(22,xx,3,5,6,21)]
  glmx <- glm(Infect~., data = matx, family = binomial(link = "logit")) 
  sum_x <- c(exp(coef(glmx)[2]),
             exp(confint(glmx)[2,]),
             coef(summary(glmx))[2,4])
  valuex <- colnames(merge_df_match)[[xx]]
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


