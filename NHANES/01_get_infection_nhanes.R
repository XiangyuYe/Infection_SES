library(bigreadr)
library(foreign)
library(stringr)

nhanes_path <- "~/Infection_SES/NHANES/"
source("~/Infection_SES/code/trans_code_nhanes.R")
setwd(nhanes_path)

infect_filelist <- fread2("01_raw/infection.txt")
all_cycle <- c("1999-2000", "2001-2002", "2003-2004", "2005-2006",
               "2007-2008", "2009-2010", "2011-2012", "2013-2014", 
               "2015-2016", "2017-2018")

infect_cycle_list <- list()
for (cyclex in all_cycle) {
  # cyclex <- all_cycle[1]
  targ_filen <- infect_filelist[infect_filelist$cycle == cyclex,"file"]
  
  # load indect list
  infect_list <- list()
  for (fn in targ_filen) {
    targ_file <- paste0("01_raw/Laboratory/", 
                        cyclex, "_", fn, ".txt")
    infect_list[[fn]] <- fread2(targ_file)
  }
  names(infect_list) <- infect_filelist[infect_filelist$cycle == cyclex, "var_code"]
  
  # re-code HCV
  if (all(c("HBC1DV","HC2V") %in% names(infect_list))) {
    HBC1DV <- infect_list[["HBC1DV"]]
    HBC1DV$LBXHCR <- NA
    HBC1DV$LBXHCR[match(infect_list[["HC2V"]]$SEQN,
                        HBC1DV$SEQN)] <- infect_list[["HC2V"]]$SSHCVRNA
    infect_list[["HBCDV"]] <- HBC1DV[,c("SEQN", "LBXHBC", "LBDHBG", 
                                       "LBDHCV", "LBXHCR", 
                                       "LBDHD")]
  }
  
  # trans code
  infect_list_trans <- lapply(1:length(infect_list), function(x){
    trans_code(infect_list[[x]], 
               names(infect_list)[x])
  })
  names(infect_list_trans) <- names(infect_list)
  infect_cycle_list[[cyclex]] <- infect_list_trans
}

infect_list <- 
  lapply(infect_cycle_list, function(x){
    infect_dfx <- Reduce(rbind, x)
    infect_dfx <- infect_dfx[infect_dfx$SEQN != "Uncoded" &
                               !is.na(infect_dfx$infect) &
                               infect_dfx$infect != 99,]
    aggregate(infect_dfx$infect, by = list(SEQN = infect_dfx$SEQN), max)
  })
infect_df_all <- Reduce(rbind, infect_list)
colnames(infect_df_all)[2] <- "Infect"
infect_df_all <- infect_df_all[which(!is.na(infect_df_all$Infect) & 
                                       infect_df_all$Infect != -99),]

fwrite2(infect_df_all, file = paste0(nhanes_path, "01_raw/infect_df_all.txt"), sep = "\t")
save(infect_cycle_list, file = paste0(nhanes_path, "01_raw/infect_cycle_list.RData"))

