
trans_code <- function(code_df = NULL,
                       code_var = NULL){
  coded_df <- "Uncoded"
  # Chlamydia_Gonorrhea
  if (code_var == "Chlamydia_Gonorrhea") {
    if (!all(c("SEQN", "URXUCL") %in% colnames(code_df))) {
      return(NA)
    } else {
      code_df[is.na(code_df)] <- "-99"
      if (ncol(code_df) == 3) {
        infect_idx <- which(code_df[,2] == 1 | code_df[,3] == 1)
        uninfect_idx <- which(code_df[,2] == 2 & code_df[,3] == 2)
      } else {
        infect_idx <- which(code_df[,2] == 1)
        uninfect_idx <- which(code_df[,2] == 2)
      }

      coded_df <- data.frame(SEQN = code_df$SEQN,
                             infect = -99)
      coded_df[infect_idx,2] <- 1
      coded_df[uninfect_idx,2] <- 0
    }
  }
  
  # # HAV
  # if (code_var == "HAV") {
  #   if (!all(colnames(code_df) == c("SEQN", "LBXHA"))) {
  #     return(NA)
  #   } else {
  #     infect_idx[is.na(infect_idx)] <- "-99"
  #     infect_idx <- which(code_df[,2] == 1)
  #     uninfect_idx <- which(code_df[,2] == 2)
  #     coded_df <- data.frame(SEQN = code_df$SEQN,
  #                            infect = -99)
  #     coded_df[infect_idx,2] <- 1
  #     coded_df[uninfect_idx,2] <- 0
  #   }
  # }
  
  # HBCDV
  if (code_var == "HBCDV") {
    if (!all(colnames(code_df) == c("SEQN", "LBXHBC", "LBDHBG", 
                                    "LBDHCV", "LBXHCR", 
                                    "LBDHD"))) {
      return(NA)
    } else {
      code_df[is.na(code_df)] <- "-99"
      infect_idx <- which(code_df[,3] == 1 | 
                            (code_df[,4] == 1 & code_df[,5] == 1) |
                            code_df[,6] == 1)
      uninfect_idx <- which(code_df[,3] == 2 & 
                            (code_df[,4] == 2 | code_df[,5] == 2) &
                            code_df[,6] == 2)
      coded_df <- data.frame(SEQN = code_df$SEQN,
                             infect = -99)
      coded_df[infect_idx,2] <- 1
      coded_df[uninfect_idx,2] <- 0
    }
  }
  # HBDV
  if (code_var == "HBDV") {
    if (!all(colnames(code_df) == c("SEQN", "LBXHBC", "LBDHBG", 
                                    "LBDHD"))) {
      return(NA)
    } else {
      coded_df[is.na(code_df)] <- "-99"
      infect_idx <- which(code_df[,3] == 1 | 
                            code_df[,4] == 1)
      uninfect_idx <- which(code_df[,3] == 2 & 
                              code_df[,4] == 2)
      coded_df <- data.frame(SEQN = code_df$SEQN,
                             infect = -99)
      coded_df[infect_idx,2] <- 1
      coded_df[uninfect_idx,2] <- 0
    }
  }
  
  # HCV
  if (code_var == "HCV1") {
    if (!all(colnames(code_df) == c("SEQN",
                                    "LBDHCV", "LBXHCR", "LBXHCG"))) {
      return(NA)
    } else {
      coded_df[is.na(code_df)] <- "-99"
      infect_idx <- which(code_df[,2] == 1 & code_df[,3] == 1)
      uninfect_idx <- which(code_df[,2] == 2 | code_df[,3] == 2)
      coded_df <- data.frame(SEQN = code_df$SEQN,
                             infect = -99)
      coded_df[infect_idx,2] <- 1
      coded_df[uninfect_idx,2] <- 0
    }
  }
  
  # HCV
  if (code_var == "HCV2") {
    if (!all(c("SEQN", "LBXHCR", "LBXHCG") %in% colnames(code_df))) {
      return(NA)
    } else {
      coded_df[is.na(code_df)] <- "-99"
      infect_idx <- which(code_df[,"LBXHCR"] == 1)
      uninfect_idx <- which(code_df[,"LBXHCR"] %in% c(2,3))
      coded_df <- data.frame(SEQN = code_df$SEQN,
                             infect = -99)
      coded_df[infect_idx,2] <- 1
      coded_df[uninfect_idx,2] <- 0
    }
  }
  
  # HEV
  if (code_var == "HEV") {
    if (!all(colnames(code_df) == c("SEQN", "LBDHEG", "LBDHEM"))) {
      return(NA)
    } else {
      coded_df[is.na(code_df)] <- "-99"
      infect_idx <- which(code_df[,2] == 1 & code_df[,3] == 1)
      uninfect_idx <- which(code_df[,2] == 2 | code_df[,3] == 2)
      coded_df <- data.frame(SEQN = code_df$SEQN,
                             infect = -99)
      coded_df[infect_idx,2] <- 1
      coded_df[uninfect_idx,2] <- 0
    }
  }
  # HIV
  if (code_var == "HIV") {
    if (!all(c("SEQN", "LBDHI") %in% colnames(code_df))) {
      return(NA)
    } else {
      coded_df[is.na(code_df)] <- "-99"
      infect_idx <- which(code_df[,2] == 1)
      uninfect_idx <- which(code_df[,2] == 2)
      coded_df <- data.frame(SEQN = code_df$SEQN,
                             infect = -99)
      coded_df[infect_idx,2] <- 1
      coded_df[uninfect_idx,2] <- 0
    }
  }
  
  # HIV2
  if (code_var == "HIV2") {
    if (!all(c("SEQN", "LBXHIVC", "LBXHIV1", "LBXHIV2", "LBXHNAT") == colnames(code_df))) {
      return(NA)
    } else {
      coded_df[is.na(code_df)] <- "-99"
      infect_idx <- which(code_df[,2] == 1 & (code_df[,3] == 1 | code_df[,4] == 1))
      uninfect_idx <- which(code_df[,2] == 2 & code_df[,3] != 1 & code_df[,4] != 1)
      coded_df <- data.frame(SEQN = code_df$SEQN,
                             infect = -99)
      coded_df[infect_idx,2] <- 1
      coded_df[uninfect_idx,2] <- 0
    }
  }
  
  # HSV2
  if (code_var == "HSV2") {
    if (!all(c("SEQN", "LBXHE2") %in% colnames(code_df))) {
      return(NA)
    } else {
      coded_df[is.na(code_df)] <- "-99"
      infect_idx <- which(code_df[,"LBXHE2"] == 1)
      uninfect_idx <- which(code_df[,"LBXHE2"] == 2)
      coded_df <- data.frame(SEQN = code_df$SEQN,
                             infect = -99)
      coded_df[infect_idx,2] <- 1
      coded_df[uninfect_idx,2] <- 0
    }
  }
  
  # HPV Vaginal/Penile Swab
  if (code_var %in% c("HPV_Swab1", "HPV_Swab2")) {
    if (!all(c("SEQN", "LBDRPCR") %in% colnames(code_df))) {
      return(NA)
    } else {
      coded_df[is.na(code_df)] <- "-99"
      infect_idx <- which(code_df[,"LBDRPCR"] == 1)
      uninfect_idx <- which(code_df[,"LBDRPCR"] == 2)
      coded_df <- data.frame(SEQN = code_df$SEQN,
                             infect = -99)
      coded_df[infect_idx,2] <- 1
      coded_df[uninfect_idx,2] <- 0
    }
  }
  
  # HPV Oral Rinse
  if (code_var == "HPV_OR") {
    if (!all(c("SEQN", "ORXHPV") %in% colnames(code_df))) {
      return(NA)
    } else {
      coded_df[is.na(code_df)] <- "-99"
      infect_idx <- which(code_df[,"ORXHPV"] == 1)
      uninfect_idx <- which(code_df[,"ORXHPV"] == 2)
      coded_df <- data.frame(SEQN = code_df$SEQN,
                             infect = -99)
      coded_df[infect_idx,2] <- 1
      coded_df[uninfect_idx,2] <- 0
    }
  }
  
  # syphilis (ref NHANES Codebook)
  if (code_var == "syphilis") {
    if (!all(colnames(code_df) == c("SEQN", "LBXSY1", "LBDSY3", "LBDSY4"))) {
      return(NA)
    } else {
      coded_df[is.na(code_df)] <- "-99"
      infect_idx <- which(
        ((code_df[,2] %in% c(1, 3)) & 
           (code_df[,3] >= 8)) | # Recent positive syphilis infection
          ((code_df[,2] %in% c(1, 3)) & 
             code_df[,3] >= 0 & code_df[,3] < 8 & 
             (code_df[,4] == 1)) # Remote positive syphilis infection
      )
      uninfect_idx <- which(code_df[,2] == 2 | 
                              ((code_df[,2] %in% c(1, 3)) & 
                                 (code_df[,3] >= 0 & code_df[,3] < 8) & 
                                 (code_df[,4] == 2)))
      coded_df <- data.frame(SEQN = code_df$SEQN,
                             infect = -99)
      coded_df[infect_idx,2] <- 1
      coded_df[uninfect_idx,2] <- 0
    }
  }
  
  # TB
  if (code_var == "TB") {
    if (!all(c("SEQN", "LBXTBIN") %in% colnames(code_df))) {
      return(NA)
    } else {
      coded_df[is.na(code_df)] <- "-99"
      infect_idx <- which(code_df[,"LBXTBIN"] == 1)
      uninfect_idx <- which(code_df[,"LBXTBIN"] == 2)
      coded_df <- data.frame(SEQN = code_df$SEQN,
                             infect = -99)
      coded_df[infect_idx,2] <- 1
      coded_df[uninfect_idx,2] <- 0
    }
  }
  
  return(coded_df)
}
