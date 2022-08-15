## 1. calculate
calcu.score.2015 <- function(KCAL = NULL,
                             VTOTALLEG = NULL,
                             VDRKGRLEG = NULL,
                             F_TOTAL = NULL,
                             FWHOLEFRT = NULL,
                             G_WHOLE = NULL,
                             D_TOTAL = NULL,
                             PFALLPROTLEG = NULL,
                             PFSEAPLANTLEG = NULL,
                             MONOPOLY = NULL,
                             SFAT = NULL,
                             SODI = NULL,
                             G_REFINED = NULL,
                             ADD_SUGARS = NULL) {
  # 
  if (is.na(KCAL)){
    
    HEI2015_TOTAL_SCORE <- NA
    return(HEI2015_TOTAL_SCORE)
    
  } else if (KCAL == 0) {
    
    HEI2015_TOTAL_SCORE <- 0
    return(HEI2015_TOTAL_SCORE)
    
  } else {
    
    # 1.1 HEI2015C1_TOTALVEG
    if (VTOTALLEG == 0) {
      HEI2015C1_TOTALVEG <- 0
    } else {
      VEGDEN <- VTOTALLEG/(KCAL/1000)
      HEI2015C1_TOTALVEG <- 5*(VEGDEN/1.1)
      if (HEI2015C1_TOTALVEG > 5) {
        HEI2015C1_TOTALVEG <- 5
      }
    }
    
    # 1.2 HEI2015C2_GREEN_AND_BEAN
    if (VDRKGRLEG == 0) {
      HEI2015C2_GREEN_AND_BEAN <- 0
    } else {
      GRBNDEN <- VDRKGRLEG/(KCAL/1000)
      HEI2015C2_GREEN_AND_BEAN <- 5*(GRBNDEN/0.2)
      if (HEI2015C2_GREEN_AND_BEAN > 5) {
        HEI2015C2_GREEN_AND_BEAN <- 5
      }
    }
    
    # 1.3 HEI2015C3_TOTALFRUIT
    if (F_TOTAL == 0) {
      HEI2015C3_TOTALFRUIT <- 0
    } else {
      FRTDEN <- F_TOTAL/(KCAL/1000)
      HEI2015C3_TOTALFRUIT <- 5*(FRTDEN/0.8)
      if (HEI2015C3_TOTALFRUIT > 5) {
        HEI2015C3_TOTALFRUIT <- 5
      }
    }
    
    # 1.4 HEI2015C4_WHOLEFRUIT
    if (FWHOLEFRT == 0) {
      HEI2015C4_WHOLEFRUIT <- 0
    } else {
      WHFRDEN <- FWHOLEFRT/(KCAL/1000)
      HEI2015C4_WHOLEFRUIT <- 5*(WHFRDEN/0.4)
      if (HEI2015C4_WHOLEFRUIT > 5) {
        HEI2015C4_WHOLEFRUIT <- 5
      }
    }
    
    # 1.5 HEI2015C5_WHOLEGRAIN
    if (G_WHOLE == 0) {
      HEI2015C5_WHOLEGRAIN <- 0
    } else {
      WGRNDEN <- G_WHOLE/(KCAL/1000)
      HEI2015C5_WHOLEGRAIN <- 5*(WGRNDEN/1.5)
      if (HEI2015C5_WHOLEGRAIN > 5) {
        HEI2015C5_WHOLEGRAIN <- 5
      }
    }
    
    # 1.6 HEI2015C6_TOTALDAIRY
    if (D_TOTAL == 0) {
      HEI2015C6_TOTALDAIRY <- 0
    } else {
      DAIRYDEN <- D_TOTAL/(KCAL/1000)
      HEI2015C6_TOTALDAIRY <- 5*(DAIRYDEN/1.3)
      if (HEI2015C6_TOTALDAIRY > 5) {
        HEI2015C6_TOTALDAIRY <- 5
      }
    }
    
    # 1.7 HEI2015C7_TOTPROT
    if (PFALLPROTLEG == 0) {
      HEI2015C7_TOTPROT <- 0
    } else {
      PROTDEN <- PFALLPROTLEG/(KCAL/1000)
      HEI2015C7_TOTPROT <- 5*(PROTDEN/2.5)
      if (HEI2015C7_TOTPROT > 5) {
        HEI2015C7_TOTPROT <- 5
      }
    }
    
    # 1.8 HEI2015C8_SEAPLANT_PROT
    if (PFSEAPLANTLEG == 0) {
      HEI2015C8_SEAPLANT_PROT <- 0
    } else {
      SEAPLDEN <- PFSEAPLANTLEG/(KCAL/1000)
      HEI2015C8_SEAPLANT_PROT <- 5*(SEAPLDEN/0.8)
      if (HEI2015C8_SEAPLANT_PROT > 5) {
        HEI2015C8_SEAPLANT_PROT <- 5
      }
    }
    
    # 1.9 HEI2015C9_FATTYACID
    if (SFAT > 0) {
      FARATIO = MONOPOLY/SFAT
      FARMIN <- 1.2
      FARMAX <- 2.5
    } 
    if (SFAT == 0 & MONOPOLY == 0) {
      HEI2015C9_FATTYACID <- 0
    } else if (SFAT == 0 & MONOPOLY > 0) {
      HEI2015C9_FATTYACID <- 10
    } else if (FARATIO >= FARMAX) {
      HEI2015C9_FATTYACID <- 10
    } else if (FARATIO <= FARMIN) {
      HEI2015C9_FATTYACID <- 0
    } else {
      HEI2015C9_FATTYACID <- 10*((FARATIO-FARMIN) / (FARMAX-FARMIN))
    }
    
    # 1.10 HEI2015C10_SODIUM
    SODDEN <- SODI/KCAL
    SODMIN <- 1.1
    SODMAX <- 2.0
    if (SODDEN <= SODMIN) {
      HEI2015C10_SODIUM <- 10
    } else if (SODDEN >= SODMAX) {
      HEI2015C10_SODIUM <- 0
    } else {
      HEI2015C10_SODIUM <- 10 - (10*(SODDEN-SODMIN) / (SODMAX-SODMIN))
    }
    
    # 1.11 HEI2015C11_REFINEDGRAIN
    RGDEN <- G_REFINED/KCAL
    RGMIN=1.8
    RGMAX=4.3
    if (RGDEN <= RGMIN) {
      HEI2015C11_REFINEDGRAIN <- 10
    } else if (RGDEN >= RGMAX) {
      HEI2015C11_REFINEDGRAIN <- 0
    } else {
      HEI2015C11_REFINEDGRAIN <- 10 - (10*(RGDEN-RGMIN) / (RGMAX-RGMIN))
    }
    
    # 1.12 HEI2015C12_SFAT
    SFAT_PERC <- 100*(SFAT*9/KCAL)
    SFATMIN=8
    SFATMAX=16
    if (SFAT_PERC >= SFATMAX) {
      HEI2015C12_SFAT <- 0
    } else if (SFAT_PERC <= SFATMIN) {
      HEI2015C12_SFAT <- 10
    } else {
      HEI2015C12_SFAT <- 10 - (10*(SFAT_PERC-SFATMIN) / (SFATMAX-SFATMIN))
    }
    
    # 1.13 HEI2015C13_ADDSUG
    ADDSUG_PERC <- 100*(ADD_SUGARS*16/KCAL)
    ADDSUGMIN=6.5
    ADDSUGMAX=26
    if (ADDSUG_PERC >= ADDSUGMAX) {
      HEI2015C13_ADDSUG <- 0
    } else if (ADDSUG_PERC <= ADDSUGMIN) {
      HEI2015C13_ADDSUG <- 10
    } else {
      HEI2015C13_ADDSUG <- 10 - (10*(ADDSUG_PERC-ADDSUGMIN) / (ADDSUGMAX-ADDSUGMIN))
    }
    HEI2015_TOTAL_SCORE <- HEI2015C1_TOTALVEG + HEI2015C2_GREEN_AND_BEAN + 
      HEI2015C3_TOTALFRUIT + HEI2015C4_WHOLEFRUIT + HEI2015C5_WHOLEGRAIN + 
      HEI2015C6_TOTALDAIRY + HEI2015C7_TOTPROT + HEI2015C8_SEAPLANT_PROT + 
      HEI2015C9_FATTYACID + HEI2015C10_SODIUM + HEI2015C11_REFINEDGRAIN + 
      HEI2015C12_SFAT + HEI2015C13_ADDSUG
    
    return(HEI2015_TOTAL_SCORE)
    
  }
}
