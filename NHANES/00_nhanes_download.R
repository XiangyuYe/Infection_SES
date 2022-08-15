library(bigreadr)
library(foreign)
library(stringr)

##
project_path <- "~/Infection_SES/"
setwd(project_path)
options(timeout = max(3000, getOption("timeout")))

for (dominx in c("Demographics", "Dietary", 
                 "Examination",
                 "Laboratory", "Questionnaire")) {
  nhanes_file <- read.table(paste0("./NHANES/01_raw/NHANES_",dominx,"_Data.txt"), 
                           sep = "\t", header = T)
  rm_idx1 <-  which(nhanes_file$Data.File == "RDC Only")
  rm_idx2 <-  grep("ZIP",nhanes_file$Data.File)
  rm_idx3 <-  grep("FTP",nhanes_file$Data.File)
  rm_idx4 <-  grep("GB]",nhanes_file$Data.File)
  rm_idx <- unique(c(rm_idx1, rm_idx2, rm_idx3, rm_idx4))
  
  if (length(rm_idx) > 0) {
    nhanes_file_f <- nhanes_file[-rm_idx,]
  } else {
    nhanes_file_f <- nhanes_file
  }

  nhanes_file_f <- nhanes_file_f[,]
  nhanes_file_f$Files <- paste0(str_split(nhanes_file_f$Doc.File, " ", simplify = T)[,1])
  nhanes_file_f$link <- paste0("https://wwwn.cdc.gov/nchs/nhanes/", nhanes_file_f$Years, "/", 
                              nhanes_file_f$Files, ".XPT")
  
  write.table(nhanes_file_f, file = paste0("./NHANES/01_raw/NHANES_",dominx,"_Data_f.txt"),
              col.names = T,row.names = F, quote = F,sep = "\t")
  
  for (nn in 1:nrow(nhanes_file_f)) {
    nhanes_out <- paste0(nhanes_file_f$Years[nn], "_", nhanes_file_f$Files[nn])
    linkx <- nhanes_file_f$link[nn]
    result <- tryCatch({
      download.file(linkx,
                    tf <- paste0("./NHANES/01_raw/",dominx, "/",nhanes_out, ".XPT"),
                    meth  = "wget",
                    mode="wb")
    }, error = function(e) {
      print("ERROR")
    }, finally = {
      print("OK")
    })
    if (result == "ERROR") {
      nhanes_df <- data.frame(paste0("./NHANES/01_raw/",dominx, "/",nhanes_out, ".XPT"))
    } else {
      nhanes_df <- foreign::read.xport(tf)
    }
    fwrite2(nhanes_df, file = paste0("./NHANES/01_raw/",dominx, "/",nhanes_out, ".txt"), sep = "\t")
  }
}





