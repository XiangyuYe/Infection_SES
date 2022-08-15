library(survey)
library(bigreadr)
library(dplyr)
library(svrepmisc)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(viridis)

setwd("~/Infection_SES/NHANES/01_raw/")
load("group_mat.RData")
load("sum_sl.RData")
### description
group_mat$CIs <- paste0(group_mat[,"OR"] %>% round(4), 
                        "(",
                        group_mat[,"low_CI"]%>% round(4),
                        ",",
                        group_mat[,"high_CI"]%>% round(4),
                        ")")
print(group_mat[,c("CIs","P","SES")])

sum_sl$CIs <- paste0(sum_sl[,"OR"] %>% round(4), 
                     "(",
                     sum_sl[,"low_CI"]%>% round(4),
                     ",",
                     sum_sl[,"high_CI"]%>% round(4),
                     ")")
print(sum_sl[,c("CIs","P","SES")])

### plot life score 1
group_mat_life <- subset(group_mat,group_mat$Var == "lifescoref")
group_mat_life$SES <- factor(4-group_mat_life$SES, levels = c(3,2,1),
                             labels = c("High", "Medium", "Low"))

group_mat_life$Var <- rep(c("2-3","4-6"), times = 3)
ref_life <- data.frame(OR = 1,
                       low_CI = 1,
                       high_CI = 1,
                       P = 0,
                       Var = "0-1",
                       SES = levels(group_mat_life$SES))
group_mat_life <- rbind(group_mat_life,ref_life)

color_use <- viridis(3)

p1 <- ggplot(group_mat_life, aes(x = SES,y = OR, color = Var)) + 
  geom_pointrange(aes(ymin=low_CI, ymax=high_CI),
                  stat='identity',position = position_dodge(0.4),
                  size = 0.8)+
  geom_hline(yintercept = 1,color="grey",lty=5,lwd=1)+
  scale_color_manual(values = color_use, name="Lifestyle scores")+
  guides(shape = guide_legend(override.aes = list(size = 0.6)))+
  xlab("")+
  ggtitle("b")+
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(size = 15,face = "bold",hjust = -0.05))

#### plot life score 2
sum_sl$SES <- factor(sum_sl$SES, levels = c(1,2,3),
                     labels = c("Low", "Medium", "High"))
sum_sl$Lifestyle.scores <- factor(sum_sl$Lifestyle.scores, 
                                  levels = c(1,2,3),
                                  labels = c("0-1", "2-3","4-6"))

# aes(x=interaction(SES, Lifestyle.scores,sep = '\n'),y=n,group=gloss)

color_use <- viridis(9)
p2 <- ggplot(sum_sl, aes(x = interaction(Lifestyle.scores,SES,sep = '\n'),
                         y = OR,
                         color = interaction(Lifestyle.scores,SES,sep = '\n'))) + 
  geom_pointrange(aes(ymin=low_CI, ymax=high_CI),
                  size = 0.8)+
  geom_hline(yintercept = 1,color="grey",lty=5,lwd=1)+
  scale_color_manual(values = color_use)+
  xlab("")+
  ggtitle("a")+
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.position = "none",
        plot.title = element_text(size = 15,face = "bold",hjust = -0.05))

require(patchwork)
tiff("Group3.tiff", height = 8,width = 12, 
     units = "in", res = 600, compression = "lzw")
print(p2/p1)
dev.off()


#####
load("group_mat.RData")
int_vars <- c("Activity", "Sleep", "Hypertension", "Diabetes")
mec_vars <- c("Nosmoke", "Noalcohol", "Nodrug", 
              "Mental", "Depression", "Anxiety", "Panic" )

group_mat_f <- subset(group_mat,group_mat$Var %in% c("Nosmoke", "Activity", "Diet", "Noalcohol", "Sleep", "Nodrug", 
                                                     "Hypertension", "Diabetes", "Mental"))
group_mat_f$SES <- factor(group_mat_f$SES, levels = c(1,2,3),
                            labels = c("High", "Medium", "Low"))
group_mat_f$Var <- factor(group_mat_f$Var, 
                            levels = c("Nosmoke", "Activity", "Diet", "Noalcohol", "Sleep", "Nodrug", 
                                       "Hypertension", "Diabetes", "Mental"))
color_use <- brewer.pal(3, "Dark2")

tiff("Group1.tiff", height = 5.5,width = 12, 
     units = "in", res = 600, compression = "lzw")
pp <- ggplot(group_mat_f, aes(x = Var,y = OR, color = SES)) + 
  geom_hline(yintercept = 1,color="grey",lty=5,lwd=1)+
  geom_pointrange(aes(ymin=low_CI, ymax=high_CI),
                  stat='identity',position = position_dodge(0.4),
                  size = 0.8)+
  
  scale_color_manual(values = color_use)+
  guides(shape = guide_legend(override.aes = list(size = 0.6)))+
  xlab("")+
  scale_x_discrete(labels = c("No current smoking", 
                              "Regular physical activity", 
                              "Healthy diet", 
                              "No alcohol consumption", 
                              "Healthy sleep pattern",
                              "No drug use",
                              "CVD",
                              "Diabetes",
                              "Mental"
  ))+
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.95, hjust = 0.95),
        axis.title = element_text(size = 15, face = "bold"),
        legend.title = element_text(face = "bold"))
print(pp)
dev.off()

###########################
group_mat1 <- read.table("clipboard", sep = "\t", header = T)
group_mat1$SES <- factor(group_mat1$SES, levels = c(1,2,3),
                         labels = c("High", "Medium", "Low"))

p1 <- ggplot(group_mat1, aes(x = interaction(SES,Var,sep = '\n'),
                             y = OR,
                             color = interaction(SES,Var,sep = '\n'))) + 
  geom_pointrange(aes(ymin=low_CI, ymax=high_CI),
                  size = 0.8)+
  geom_hline(yintercept = 1,color="grey",lty=5,lwd=1)+
  scale_color_manual(values = c(brewer.pal(5, "PuRd")[3:5],
                                brewer.pal(5, "PuBu")[3:5]))+
  xlab("")+
  ggtitle("a")+
  # scale_x_discrete(labels = c("PM2.5","PM2.5-10","PM10","NO2","NOx",
  #                             "Trafc.intensity","Distance.to.road","Noise"))+
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.position = "none",
        plot.title = element_text(size = 15,face = "bold",hjust = -0.05))

group_mat2 <- read.table("clipboard", sep = "\t", header = T)
group_mat2$SES <- factor(group_mat2$SES, levels = c(1,2,3),
                         labels = c("High", "Medium", "Low"))
group_mat2$Var <- factor(group_mat2$Var, 
                         levels = c("White", "Non-white"))

p2 <- ggplot(group_mat2, aes(x = interaction(SES,Var,sep = '\n'),
                             y = OR,
                             color = interaction(SES,Var,sep = '\n'))) + 
  geom_pointrange(aes(ymin=low_CI, ymax=high_CI),
                  size = 0.8)+
  geom_hline(yintercept = 1,color="grey",lty=5,lwd=1)+
  scale_color_manual(values = c(brewer.pal(6, "RdPu")[3:5],
                                brewer.pal(6, "BuGn")[3:5],
                                brewer.pal(6, "GnBu")[3:5]))+
  xlab("")+
  ggtitle("b")+
  # scale_x_discrete(labels = c("PM2.5","PM2.5-10","PM10","NO2","NOx",
  #                             "Trafc.intensity","Distance.to.road","Noise"))+
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.position = "none",
        plot.title = element_text(size = 15,face = "bold",hjust = -0.05))

require(patchwork)
tiff("Group_sub.tiff", height = 8,width = 10, 
     units = "in", res = 600, compression = "lzw")
print(p1/p2)
dev.off()