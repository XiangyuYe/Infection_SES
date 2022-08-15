library(tidyverse)

setwd("~/Infection_SES/NHANES/")
# 01-02
FPED_0102 <- fread2("01_raw/MyPyrEquivDB_v1/data/equiv0102/equiv0102.txt",
                    fill = T, header = F)
col_1_2 <- data.frame(substr(FPED_0102$V1, 1, 8),
                      substr(FPED_0102$V1, 9, 9))
FPED_0102 <- cbind(col_1_2, FPED_0102[,-1])

# 94-00
FPED_9400 <- fread2("01_raw/MyPyrEquivDB_v1/data/equiv9400/equiv9400.txt",
                    fill = T, header = F)
col_1_2 <- data.frame(substr(FPED_9400$V1, 1, 8),
                      substr(FPED_9400$V1, 9, 9))
FPED_9400 <- cbind(col_1_2, FPED_9400[,-1])

#
col_FPED_9402 <- fread2("01_raw/MyPyrEquivDB_v1/data/formats/col_equiv.txt")

colnames(FPED_0102) <- colnames(FPED_9400) <- col_FPED_9402$Name

fwrite2(FPED_0102, file = "01_raw/FPED/FPED_2001-2002.txt", sep = "\t")
fwrite2(FPED_9400, file = "01_raw/FPED/FPED_1994-2000.txt", sep = "\t")

###
library(readxl)
setwd("F:/R_data/Infection_SES/NHANES/")

#
for (cyclex in c("0304", "0506", "0708", "0910",
                 "1112", "1314", "1516", "1718")) {
  FPED_cyclex <- read_xls(paste0("01_raw/FPED_",cyclex,".xls"),
                          sheet = NULL,
                          col_names = TRUE)
  colnames(FPED_cyclex) <- str_split(colnames(FPED_cyclex), " ", simplify = T)[,1]
  cyclexx <- paste0("20", substr(cyclex, 1, 2),
                    "-",
                    "20", substr(cyclex, 3, 4))
  
  
  fwrite2(FPED_cyclex, file = paste0("01_raw/FPED/FPED_",cyclexx,".txt"), 
          sep = "\t")
}


