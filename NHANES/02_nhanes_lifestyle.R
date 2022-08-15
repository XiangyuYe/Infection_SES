library(foreign)
library(bigreadr)
library(tidyverse)

setwd("~/Infection_SES/NHANES/01_raw/")
life_files <- fread2("life_files.txt")

cyclex <- "2001-2002"

lifestyle_list <- list()
for (cyclex in c("1999-2000", "2001-2002", "2003-2004",
                 "2005-2006", "2007-2008", "2009-2010", 
                 "2011-2012", "2013-2014", "2015-2016", 
                 "2017-2018")) {
  ## 1. smoke
  smk_file <- life_files[which(life_files$cycle == cyclex &
                                 life_files$description == "Smoking - Cigarette Use"),
                         "file"]
  smk_df <- fread2(paste0("Questionnaire/", cyclex, "_", smk_file, ".txt"), fill=TRUE)
  smk_df[is.na(smk_df)] <- -99
  
  nosmoke <- rep(-99, nrow(smk_df))
  nosmoke[smk_df$SMQ020 == 1] <- 0
  nosmoke[smk_df$SMQ020 == 2 | 
            (smk_df$SMQ050Q >= 30 & smk_df$SMQ050U == 4)] <- 1
  
  nosmoke_df <- data.frame(SEQN = smk_df$SEQN,
                           Nosmoke = nosmoke)
  print(paste0("MSG: NoSomke for ", cyclex, " is ok!"))
  
  ## 2. activity
  act_file <- life_files[which(life_files$cycle == cyclex &
                                 life_files$description == "Physical Activity"),
                         "file"]
  act_df <- fread2(paste0("Questionnaire/", cyclex, "_", act_file, ".txt"), fill=TRUE)
  act_df[is.na(act_df)] <- -99
  if (cyclex %in% c("1999-2000","2001-2002","2003-2004","2005-2006")) {
    act_df <- aggregate(act_df$PADMETS, list(SEQN = act_df$SEQN), sum)
    act <- ifelse(act_df$x >= quantile(act_df$x, 0.3), 1, 
                  ifelse(act_df$x < quantile(act_df$x, 0.3), 0, -99))
  } else {
    act_frq_vigor_ind <- act_df$PAQ650 == 1  & act_df$PAQ655 >= 1 & act_df$PAQ655 <= 7
    act_frq_moder_ind <- act_df$PAQ665 == 1  & act_df$PAQ670 >= 5 & act_df$PAQ670 <= 7
    act_frq_ind <- act_frq_vigor_ind & act_frq_moder_ind
    
    act_time_vigor_ind <- act_df$PAQ650 == 1  & act_df$PAQ660 >= 75/7 & act_df$PAQ660 <= 999
    act_time_moder_ind <- act_df$PAQ665 == 1  & act_df$PAQ675 >= 150/7 & act_df$PAQ675 <= 999
    
    act_ind <- act_frq_ind | act_time_vigor_ind | act_time_moder_ind
    act <- rep(0, nrow(act_df))
    act[act_ind] <- 1
    act[!act_df$PAQ650 %in% c(1,2) | !act_df$PAQ665 %in% c(1,2)] <- -99
  }
  activity_df <- data.frame(SEQN = act_df$SEQN,
                            Activity = act)
  print(paste0("MSG: Activity for ", cyclex, " is ok!"))
  
  ## 3. healthy diet
  ## 3.1 dietary data
  dietary_file <- life_files[which(life_files$cycle == cyclex &
                                     life_files$description == "Dietary Interview - Individual Foods"),
                             "file"]
  dietary_df <- fread2(paste0("Dietary/", cyclex, "_", dietary_file, ".txt"), fill=TRUE)
  colnames(dietary_df) <- gsub("DR1I", "", colnames(dietary_df))
  colnames(dietary_df) <- gsub("DRXI", "", colnames(dietary_df))
  colnames(dietary_df) <- gsub("DRDI", "", colnames(dietary_df))
  
  #
  keep_col_dietary <- c("KCAL", "SFAT", "SODI", "MFAT", "PFAT")
  dietary_df_agg <- aggregate(dietary_df[,keep_col_dietary], 
                              by = list(SEQN = dietary_df$SEQN), 
                              sum)
  
  ## 3.2fped data
  fped_df <- fread2(paste0("FPED/FPED_", cyclex, ".txt"), fill=TRUE)
  names_col_fped <- c("G_WHOLE",	   # Whole Grains
                     "G_REFINED",	   # Refined Grains
                     "V_TOTAL",	     # Total Vegetables
                     "V_DRKGR",	     # Dark Green
                     "F_TOTAL",	     # Total Fruit
                     "F_CITMLB",	   # Citrus, Melons and Berries
                     "F_OTHER",	     # Other Fruit
                     "D_TOTAL",	     # Total Dairy
                     "PF_MPS_TOTAL", # Total Meat, Poultry, and Seafood
                     "PF_SEAFD_HI",	 # Seafood High n-3
                     "PF_SEAFD_LOW", # Seafood Low n-3
                     "PF_EGGS",	     # Eggs
                     "PF_SOY",	     # Soybean Products
                     "PF_NUTSDS",	   # Nuts and Seeds
                     "V_LEGUMES",	   # Legumes as Vegetable
                     "PF_LEGUMES",	 # Legumes as Protein
                     "SOLID_FATS",	 # Solid Fats
                     "ADD_SUGARS"	   # Added Sugar
  )
  # extract columns from fped_df and format
  if (cyclex %in% c("1999-2000","2001-2002","2003-2004")) {
    # changes in column names
    keep_col_fped <- c("G_WHL",	      # Number of whole grain ounce equivalents
                       "G_NWHL",	    # Number of non-whole grain ounce equivalents
                       "V_TOTAL",	    # Total number of vegetable cup equivalents, excl legumes
                       "V_DRKGR",	    # Number of dark-green vegetable cup equivalents
                       "F_TOTAL",	    # Total number of fruit cup equivalents
                       "F_CITMLB",    # Number of citrus, melon, berry cup equivalents
                       "F_OTHER",	    # Number of other fruit cup equivalents
                       "D_TOTAL",	    # Total number of milk group (milk, yogurt & cheese) cup equivalents
                       "M_MPF",	      # Oz cooked lean meat from meat, poultry, fish
                       "M_FISH_HI",	  # Oz cooked lean meat from fish, other seafood high in Omega-3
                       "M_FISH_LO",	  # Oz cooked lean meat from fish, other seafood low in Omega-3
                       "M_EGG",	      # Oz equivalents of lean meat from eggs
                       "M_SOY",	      # Oz equivalents of lean meat from soy product
                       "M_NUTSD",	    # Oz equivalents of lean meat from nuts and seeds
                       "LEGUMES",	    # Number of cooked dry beans and peas cup equivalents
                       "DISCFAT_SOL",	# Grams of discretionary Solid fat
                       "ADD_SUG"	    #Teaspoon equivalents of added sugars
                       
    )
    
    # calculate fped for each person
    dietary_dff <- dietary_df[which(dietary_df$FDCD %in% fped_df$FOODCODE),]
    fped_df_match <- fped_df[match(dietary_dff$FDCD,fped_df$FOODCODE),keep_col_fped]
    fped_df_match <- fped_df_match*dietary_df$GRMS/100
    fped_df_agg <- aggregate(fped_df_match, 
                             by = list(SEQN = dietary_dff$SEQN), sum)
    colnames(fped_df_agg)[-1] <- names_col_fped[-15] # except 
    fped_df_agg$V_LEGUMES <- 0
    
  } else {
    # no change in column names
    keep_col_fped <- names_col_fped
    # calculate fped for each person
    dietary_dff <- dietary_df[which(dietary_df$FDCD %in% fped_df$FOODCODE),]
    fped_df_match <- fped_df[match(dietary_dff$FDCD,fped_df$FOODCODE),keep_col_fped]
    fped_df_match <- fped_df_match*dietary_df$GRMS/100
    fped_df_agg <- aggregate(fped_df_match, 
                             by = list(SEQN = dietary_dff$SEQN), sum)
  }
  
  ## 3.3 demog data
  demog_list <- fread2("NHANES_Demographics_Data_f.txt")
  demog_file <- demog_list[which(demog_list$Years == cyclex), "Files"]
  demog_df <- fread2(paste0("Demographics/", cyclex, "_", demog_file, ".txt"), fill=TRUE)
  keep_col_demog <- c("SEQN", "RIDAGEYR", "RIAGENDR", "SDDSRVYR", "SDMVPSU", "SDMVSTRA")
  demog_df_f <- demog_df[,keep_col_demog]
  
  ## 3.4 merge data
  merge_dietary <- merge(demog_df_f, fped_df_agg, by = "SEQN", all.x = T)
  merge_dietary <- merge(merge_dietary, dietary_df_agg, by = "SEQN", all.x = T)
  
  # Creates additional required variables: 
  # FWHOLEFRT, MONOPOLY, VTOTALLEG, VDRKGRLEG, PFALLPROTLEG and PFSEAPLANTLEG
  merge_dietary$FWHOLEFRT <- merge_dietary$F_CITMLB + merge_dietary$F_OTHER
  merge_dietary$MONOPOLY <- merge_dietary$MFAT + merge_dietary$PFAT
  merge_dietary$VTOTALLEG <- merge_dietary$V_TOTAL + merge_dietary$V_LEGUMES
  merge_dietary$VDRKGRLEG <- merge_dietary$V_DRKGR + merge_dietary$V_LEGUMES
  merge_dietary$PFALLPROTLEG <- merge_dietary$PF_MPS_TOTAL + merge_dietary$PF_EGGS + 
    merge_dietary$PF_NUTSDS + merge_dietary$PF_SOY + merge_dietary$PF_LEGUMES 
  merge_dietary$PFSEAPLANTLEG <- merge_dietary$PF_SEAFD_HI + merge_dietary$PF_SEAFD_LOW + 
    merge_dietary$PF_NUTSDS + merge_dietary$PF_SOY + merge_dietary$PF_LEGUMES
  
  merge_dietary <- merge_dietary[rowSums(is.na(merge_dietary)) == 0,]
  
  
  ## 3.5 calculate HEI 
  source("~/Infection_SES/code/calcu_score_2015.R")
  
  merge_dietary$HEI2015_TOTAL_SCORE <- 
    sapply(1:nrow(merge_dietary), function(x){
      samplex <- merge_dietary[x,]
      hei_2015 <- with(
        samplex,
        calcu.score.2015(KCAL = KCAL,
                         VTOTALLEG = VTOTALLEG,
                         VDRKGRLEG = VDRKGRLEG,
                         F_TOTAL = F_TOTAL,
                         FWHOLEFRT = FWHOLEFRT,
                         G_WHOLE = G_WHOLE,
                         D_TOTAL = D_TOTAL,
                         PFALLPROTLEG = PFALLPROTLEG,
                         PFSEAPLANTLEG = PFSEAPLANTLEG,
                         MONOPOLY = MONOPOLY,
                         SFAT = SFAT,
                         SODI = SODI,
                         G_REFINED = G_REFINED,
                         ADD_SUGARS = ADD_SUGARS))
      
      return(hei_2015)
    })
  
  wtdr_col <- ifelse(cyclex %in% c("1999-2000", "2001-2002"), "WTDR4YR", "WTDRD1")
  dietary_df_uniseqn <- dietary_df[!duplicated(dietary_df$SEQN),]
  diet_df <- data.frame(SEQN = merge_dietary$SEQN,
                        Diet = ifelse(is.na(merge_dietary$HEI2015_TOTAL_SCORE), NA,
                                      ifelse(merge_dietary$HEI2015_TOTAL_SCORE >= 
                                               quantile(merge_dietary$HEI2015_TOTAL_SCORE, 0.8, na.rm = T), 1, 0)),
                        WTDRD1 = dietary_df_uniseqn[[wtdr_col]][match(merge_dietary$SEQN, dietary_df_uniseqn$SEQN)])
  
  print(paste0("MSG: Dietary for ", cyclex, " is ok!"))
  
  ## 4. alcohol
  alch_file <- life_files[which(life_files$cycle == cyclex &
                                  life_files$description == "Alcohol Use"),
                          "file"]
  alch_df <- fread2(paste0("Questionnaire/", cyclex, "_", alch_file, ".txt"), fill=TRUE)
  alch_df[is.na(alch_df)] <- -99
  
  if (cyclex == "2017-2018") {
    noalcohol <- rep(-99, nrow(alch_df))
    noalcohol[alch_df$ALQ111 == 2] <- 1
    noalcohol[alch_df$ALQ111 == 1] <- 0
  } else {
    col_drink_1yr <- ifelse(cyclex == "1999-2000", "ALQ100",
                            ifelse(cyclex == "2001-2002", "ALD100",
                                   "ALQ101"))
    
    noalcohol <- rep(-99, nrow(alch_df))
    noalcohol[alch_df$ALQ110 == 2] <- 1
    noalcohol[alch_df[[col_drink_1yr]] == 1 | alch_df$ALQ110 == 1] <- 0
  }
  
  noalcohol_df <- data.frame(SEQN = alch_df$SEQN,
                             Noalcohol = noalcohol)
  print(paste0("MSG: Dietary for ", cyclex, " is ok!"))
  
  ## 5. sleep behavior
  if (cyclex %in% c("1999-2000","2001-2002","2003-2004")) {
    print(paste0("MSG: No sleep data for ", cyclex, "!"))
    Sleep_df <- data.frame(SEQN = demog_df$SEQN,
                           Sleep = NA)
  } else {
    slp_file <- life_files[which(life_files$cycle == cyclex &
                                   life_files$description == "Sleep Disorders"),
                           "file"]
    slp_df <- fread2(paste0("Questionnaire/", cyclex, "_", slp_file, ".txt"), fill=TRUE)
    slp_df[is.na(slp_df)] <- -99
    
    slp <- rep(0, nrow(slp_df))
    slp[which(slp_df$SLD010H <= 8 & slp_df$SLD010H >= 7 &
                slp_df$SLQ050 == 2 &
                slp_df$SLQ060 == 2)] <- 1
    slp[which(slp_df$SLD010H >= 24 | slp_df$SLD010H < 0 |
                (!slp_df$SLQ050 %in% c(1,2) &
                   !slp_df$SLQ060 %in% c(1,2)))] <- -99
    Sleep_df <- data.frame(SEQN = slp_df$SEQN,
                           Sleep = slp)
    
    print(paste0("MSG: Sleep for ", cyclex, " is ok!"))
  }
  
  
  ## 6. drug use
  du_file <- life_files[which(life_files$cycle == cyclex &
                                life_files$description == "Drug Use"),
                        "file"]
  du_df <- fread2(paste0("Questionnaire/", cyclex, "_", du_file, ".txt"), fill=TRUE)
  du_df[is.na(du_df)] <- -99
  
  if (cyclex %in% c("1999-2000","2001-2002","2003-2004")) {
    nodu <- rep(1, nrow(du_df))
    nodu[which(du_df$DUQ100 == 1)] <- 0
    nodu[which(!du_df$DUQ100 %in% c(1,2))] <- -99
    
    Nodrug_df <- data.frame(SEQN = du_df$SEQN,
                            Nodrug = nodu)
  } else {
    nodu <- rep(1, nrow(du_df))
    nodu[which(du_df$DUQ100 == 1 | du_df$DUQ240 == 1)] <- 0
    nodu[which(!du_df$DUQ100 %in% c(1,2) & !du_df$DUQ240 %in% c(1,2))] <- -99
    
    Nodrug_df <- data.frame(SEQN = du_df$SEQN,
                            Nodrug = nodu)
  }
  
  print(paste0("MSG: Nodrug for ", cyclex, " is ok!"))

  
  ## 7. Healthy lifestyle
  # only retain non-pregnant adults (>=20)
  SEQN_list <- list(nosmoke_df$SEQN,
                    activity_df$SEQN,
                    diet_df$SEQN,
                    noalcohol_df$SEQN,
                    Sleep_df$SEQN,
                    Nodrug_df$SEQN)
  
  lifestyle_SEQN_inter <- Reduce(intersect, SEQN_list)
  lifestyle_SEQN_all <- unlist(SEQN_list) %>% unique
  

  lifestyle_df <- data.frame(SEQN = lifestyle_SEQN_all,
                             WTDRD1 = NA,
                             lifescore = NA,
                             Nosmoke = NA,
                             Activity = NA,
                             Diet = NA,
                             Noalcohol = NA,
                             Sleep = NA,
                             Nodrug = NA)

  lifestyle_df$Nosmoke[match(nosmoke_df$SEQN, lifestyle_df$SEQN)] <- nosmoke_df$Nosmoke
  lifestyle_df$Activity[match(activity_df$SEQN, lifestyle_df$SEQN)] <- activity_df$Activity
  lifestyle_df$WTDRD1[match(diet_df$SEQN, lifestyle_df$SEQN)] <- diet_df$WTDRD1
  lifestyle_df$Diet[match(diet_df$SEQN, lifestyle_df$SEQN)] <- diet_df$Diet
  lifestyle_df$Noalcohol[match(noalcohol_df$SEQN, lifestyle_df$SEQN)] <- noalcohol_df$Noalcohol
  lifestyle_df$Sleep[match(Sleep_df$SEQN, lifestyle_df$SEQN)] <- Sleep_df$Sleep
  lifestyle_df$Nodrug[match(Nodrug_df$SEQN, lifestyle_df$SEQN)] <- Nodrug_df$Nodrug
  
  lifestyle_df[lifestyle_df == -99] <- NA
  
  lf_df <- lifestyle_df[,-c(1:3)]
  lifestyle_df$lifescore <- rep(NA, nrow(lf_df))
  lifestyle_df$lifescore[rowSums(lf_df, na.rm = T) >= 4] <- 3 
  lifestyle_df$lifescore[rowSums(lf_df, na.rm = T) >= 2 &
                           (rowSums(lf_df, na.rm = T) + rowSums(is.na(lf_df))) < 4 ] <- 2 
  lifestyle_df$lifescore[(rowSums(lf_df, na.rm = T) + rowSums(is.na(lf_df))) < 2 ] <- 1 
  
  ## return outcome
  SEQN_retain <- demog_df$SEQN[!demog_df$RIDEXPRG %in% c(1,3) &
                                 demog_df$RIDAGEYR >= 20]
  lifestyle_df$retain_lifestyle <- 0
  lifestyle_df$retain_lifestyle[lifestyle_df$SEQN %in% intersect(SEQN_retain, lifestyle_SEQN_inter)] <- 1
  
  lifestyle_list[[cyclex]] <- lifestyle_df
  
}

lifestyle_df_all <- Reduce(rbind, lifestyle_list)
save(lifestyle_list, file = "lifestyle_list.RData")
fwrite2(lifestyle_df_all, file = "lifestyle_df_all.txt", sep = "\t")
