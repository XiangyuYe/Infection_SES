library(bigreadr)
library(dplyr)
library(stringr)

pheno_str <- "~/Datasets/ukb/ukb47503.csv"

# habit and baseline
## raw data
habit_code <- c("189-0.0",          ## 02 Townsend deprivation index
                "738-0.0",          ## 03 Income
                paste0("6138-0.", 0:5),  ## 04-09 Education
                paste0("6142-0.", 0:6),  ## 10-16 Employment status
                "20116-0.0",        ## 17 Smoking status
                "2897-0.0",         ## 18 Age stopped smoking
                "884-0.0",          ## 19 Number of days/week of moderate physical activity 10+ minutes
                "894-0.0",          ## 20 Duration of moderate activity
                "904-0.0",          ## 21 Number of days/week of vigorous physical activity 10+ minutes
                "914-0.0",          ## 22 Duration of vigorous activity
                "1309-0.0",         ## 23 Fresh fruit intake
                "1319-0.0",         ## 24 Dried fruit intake
                "1289-0.0",         ## 25 Cooked vegetable intake
                "1299-0.0",         ## 26 Salad / raw vegetable intake
                "1329-0.0",         ## 27 Oily fish intake
                "1339-0.0",         ## 28 Non-oily fish intake
                "1349-0.0",         ## 29 Processed meat intake
                "1369-0.0",         ## 30 Beef intake
                "1379-0.0",         ## 31 Lamb/mutton intake
                "1389-0.0",         ## 32 Pork intake
                "1438-0.0",         ## 33 Bread intake
                "1448-0.0",         ## 34 Bread type
                "1458-0.0",         ## 35 Cereal intake
                "1468-0.0",         ## 36 Cereal type
                "20117-0.0",        ## 37 Alcohol 
                "1180-0.0",         ## 38 Morning/evening person (chronotype)
                "1160-0.0",         ## 39 Sleep duration
                "1200-0.0",         ## 40 Sleeplessness/insomnia
                "1210-0.0",         ## 41 Snoring
                "1220-0.0",         ## 42 Daytime dozing/sleeping (narcolepsy)
                "20453-0.0"         ## 43 Ever taken cannabis
                
)
baseline_code <- c("21001-0.0",    ## 44 BMI
                   "22001-0.0",    ## 45 Genetic sex
                   "21003-0.0",    ## 46 age when attended assessment centre
                   "31-0.0",       ## 47 Sex
                   "53-0.0",
                   "53-1.0",
                   "53-2.0",
                   "53-3.0",        ## 48-51 Date of attending assessment centre
                   paste0("21000-0.0"), ## 52 Ethnic_background
                   "54-0.0"         ## 53 UK Biobank assessment centre
                   )
habit_baseline_df <- fread2(pheno_str, select = c("eid", habit_code, baseline_code))

################## data QC ##################
## consistent gender
cnd_i <- habit_baseline_df[,45] == habit_baseline_df[,47]&
  !is.na(habit_baseline_df[,45])

## valid eid
cnd_ii <- habit_baseline_df$eid > 0&
  !is.na(habit_baseline_df$eid)

qc_cnd <- cnd_i & cnd_ii

################## eth ##################
eth_EUR <- c(1, 1001, 1002, 1003) # White
EUR_cnd <- habit_baseline_df[["21000-0.0"]] %in% eth_EUR

eth_AFR <- c(4, 4001, 4002, 4003) # Black or Black British
eth_ASA <- c(5, 2003, 3004) # ASA defined by Sir Yang (Do perform better in PRS for EAS)
AFR_cnd <- habit_baseline_df[["21000-0.0"]] %in% eth_AFR
ASA_cnd <- habit_baseline_df[["21000-0.0"]] %in% eth_ASA

eth_list <- list(EUR_eid = habit_baseline_df$eid[EUR_cnd],
                 AFR_eid = habit_baseline_df$eid[AFR_cnd],
                 ASA_eid = habit_baseline_df$eid[ASA_cnd])
save(eth_list, file = "~/Infection_SES/eth_list.RData")
################## BMI group ##################
BMI_grp = ifelse(is.na(habit_baseline_df[["21001-0.0"]]), NA,
                 ifelse(habit_baseline_df[["21001-0.0"]] > 18.5 & 
                          habit_baseline_df[["21001-0.0"]]  < 30, 1, 
                        ifelse(habit_baseline_df[["21001-0.0"]] <= 18.5, 0, 
                               ifelse(habit_baseline_df[["21001-0.0"]] >= 30, 2, NA))))

##############################################
# Income, Education and Employment groupings #
# start from 1 due to LCA analysis needs     #
##############################################
################## Income group ##################
Income1 <- ifelse(is.na(habit_baseline_df[["738-0.0"]]), NA,
                 ifelse(habit_baseline_df[["738-0.0"]] > 0,
                        habit_baseline_df[["738-0.0"]], NA))

Income2 <- ifelse(is.na(Income1), NA,
                  ifelse(Income1 == 1, 1,
                         ifelse(Income1 %in% c(2, 3),2 ,3)))
################## education ##################
# values increase from 6138-0.0 to 6138-0.5
# take 6138-0.0 as the highest education status
# 1-2: College or above
# 3-6: High school or equivalent
# -7: Less than high school (33853828)
edu_cnd1 <- ifelse(is.na(habit_baseline_df[["6138-0.0"]]), NA,
                  ifelse(habit_baseline_df[["6138-0.0"]] == -3, NA,
                         ifelse(habit_baseline_df[["6138-0.0"]] == -7, 7, 
                                habit_baseline_df[["6138-0.0"]])))

edu_cnd2 <- ifelse(is.na(habit_baseline_df[["6138-0.0"]]), NA,
                  ifelse(habit_baseline_df[["6138-0.0"]] %in% c(1:2), 3,
                         ifelse(habit_baseline_df[["6138-0.0"]] > 2, 2, 
                                ifelse(habit_baseline_df[["6138-0.0"]] == -7, 1, NA))))

################## Employment ##################
# values increase from 6142-0.0 to 6142-0.6 
# take 6142-0.0 as the highest education status
# Employed Status including those in paid employment or self-employed, retired, 
# doing unpaid or voluntary work, or being full or part time students)
emp_cnd <- ifelse(is.na(habit_baseline_df[["6142-0.0"]]), NA,
                  ifelse(habit_baseline_df[["6142-0.0"]] %in% c(1, 2, 6, 7), 2,
                         ifelse(habit_baseline_df[["6142-0.0"]] %in% c(3:5, -7), 1, NA)))

################## no current smoke ##################
# never smoker
nosmoke_cnd <- ifelse(is.na(habit_baseline_df[["20116-0.0"]]), NA,
                      ifelse(habit_baseline_df[["20116-0.0"]] == 0, 1,
                             ifelse(habit_baseline_df[["20116-0.0"]] > 0, 0, NA)))

# Former smokers who had quit smoking more than 30 years
nosmoke_year <- habit_baseline_df[["21003-0.0"]] - habit_baseline_df[["2897-0.0"]]

# no current smoker
nosmoke_cnd[which(nosmoke_year > 30)] <- 1

################## activity ##################
# frequency: moderate at least 5 days a week and vigorous once a week
num_moderate <- ifelse(habit_baseline_df[["884-0.0"]] < 0, NA, 
                       habit_baseline_df[["884-0.0"]])
num_vigorous <- ifelse(habit_baseline_df[["904-0.0"]] < 0, NA, 
                       habit_baseline_df[["904-0.0"]])
num_activity <- num_moderate > 5 & num_vigorous > 1

# ¡Ý 150 minutes moderate activity per week
time_moderate <- ifelse(habit_baseline_df[["894-0.0"]] > 150, T, F)
# ¡Ý 75 minutes vigorous activity per week 
time_vigorous <- ifelse(habit_baseline_df[["914-0.0"]] > 75, T, F)

# activity index
act_mat <- cbind(num_activity, time_moderate, time_vigorous)

# activity_cnd <- num_activity | time_moderate | time_vigorous
activity_cnd <- rep(0, nrow(act_mat))
activity_cnd[rowSums(act_mat, na.rm = T) > 0] <- 1
activity_cnd[rowSums(is.na(act_mat)) == 3] <- NA


################## healthy diet ##################
diet_numcol <- c(23:36)
diet_df <- apply(habit_baseline_df[, diet_numcol], 2, 
                 function(a) {
                   a[a == -10] <- 0
                   a[a<0] <- NA
                   return(a)
                 }) %>% as.data.frame()

### fruit: per serving = fresh fruit- 1 piece OR dried fruit- 5 pieces
fruit_mat <- cbind(diet_df[["1309-0.0"]], 
                   diet_df[["1319-0.0"]]/5) 

# Fruits: ¡Ý 4 servings/day
fruit_cnd <- rowSums(fruit_mat) >= 4
fruit_cnd[rowSums(fruit_mat,na.rm = T) >= 4] <- T

### vegetable: per serving = cooked/raw vegetables- 3 heaped tablespoons
veg_mat <- cbind(diet_df[["1289-0.0"]]/3, 
                 diet_df[["1299-0.0"]]/3)

# Vegetables: ¡Ý 4 servings/day
veg_cnd <- rowSums(veg_mat) >= 4
veg_cnd[rowSums(veg_mat,na.rm = T) >= 4] <- T

### fish: Fish: ¡Ý2 times/week
fish_mat <- cbind(diet_df[["1329-0.0"]], 
                  diet_df[["1339-0.0"]]) 
# 0	Never
# 1	Less than once a week (take as 0.5)
# 2	Once a week
# 3	2-4 times a week
# 4	5-6 times a week
# 5	Once or more daily

# Oily fish intake ¡Ý2 times/week OR Non-oily fish intake ¡Ý2 times/week
fish_cnd <- fish_mat[,1] >= 3 | fish_mat[,2] >= 3
# include 2 + 2 (Once a week)
fish_cnd[fish_mat[,1] ==2 & fish_mat[,2] == 2] <- T

### processed red meat: ¡Ü 1 times/week
pmeat_cnd <- diet_df[["1349-0.0"]] <= 2

### non-processed red meat: ¡Ü 1.5 times/week
npmeat_mat <- cbind(diet_df[["1369-0.0"]], 
                    diet_df[["1379-0.0"]], 
                    diet_df[["1389-0.0"]])
# 1 + 2 OR 1 * 3
npmeat_cnd <- rowSums(npmeat_mat, na.rm = T) <= 3
# exclude 3 + 0 + 0 (¡Ý2 times/week)
npmeat_cnd[npmeat_mat[,1] >=3 | npmeat_mat[,2] >=3 | npmeat_mat[,3] >=3] <- F

### Whole grains: ¡Ý 3 servings/day
grains_mat <- matrix(0, nrow(habit_baseline_df), 2)

# bran/oat/muesli cereal- 1 bowl/day 
bread_idx <- which(habit_baseline_df[["1448-0.0"]] == 3)
grains_mat[bread_idx, 1] <- habit_baseline_df[["1438-0.0"]][bread_idx]

# wholemeal/wholegrain bread- 1 slice/day
cereal_idx <- which(habit_baseline_df[["1468-0.0"]] %in% c(1, 2, 3))
grains_mat[cereal_idx, 2] <- habit_baseline_df[["1458-0.0"]][cereal_idx]

#
grains_cnd <- rowSums(grains_mat, na.rm = T) >= 3

### Healthy diet: At least 4 of the 6 food groups
diet_mat <- data.frame(fruit_cnd, veg_cnd, fish_cnd, 
                       pmeat_cnd, npmeat_cnd, grains_cnd)

diet_cnd <- rowSums(diet_mat) >= 4

# Can satisfy even when missing = F
diet_cnd[rowSums(diet_mat,na.rm = T) >= 4] <- T

# Never satisfy even when missing = T
diet_cnd[rowSums(is.na(diet_mat)) + rowSums(diet_mat,na.rm = T) < 4] <- F


################## alcohol ##################
# never alcohol use
noalcohol_cnd <- ifelse(is.na(habit_baseline_df[["20117-0.0"]]), NA, 
                        ifelse(habit_baseline_df[["20117-0.0"]] == 0, 1,
                               ifelse(habit_baseline_df[["20117-0.0"]] > 0, 0, NA)))

################## sleep behavior ##################
# "1180-0.0",         ## 38 Morning/evening person (chronotype)
# "1160-0.0",         ## 39 Sleep duration
# "1200-0.0",         ## 40 Sleeplessness/insomnia
# "1210-0.0",         ## 41 Snoring
# "1220-0.0",         ## 42 Daytime dozing/sleeping (narcolepsy)
sleep_numcol <- c(38:42)
sleep_df <- apply(habit_baseline_df[, sleep_numcol], 2, 
                  function(a) {
                    a[a<0] <- NA
                    return(a)
                  }) %>% as.data.frame()

## define from 31848595
# chronotype (morning preference)
# 1	Definitely a 'morning' person
# 2	More a 'morning' than 'evening' person
chrono_cnd <- ifelse(is.na(sleep_df[["1180-0.0"]]), NA,
                     ifelse(sleep_df[["1180-0.0"]] < 3, 1, 0) )

# Sleep duration 7-8h
duration_cnd <- ifelse(is.na(sleep_df[["1160-0.0"]]), NA,
                       ifelse(sleep_df[["1160-0.0"]] <= 8 &
                                sleep_df[["1160-0.0"]] >= 7, 1, 0) )

# 1: never or rarely insomnia
insomnia_cnd <- ifelse(is.na(sleep_df[["1200-0.0"]]),NA,
                       ifelse(sleep_df[["1200-0.0"]] == 1, 1, 0))

# 2: No complaints of snoring
snoring_cnd <- ifelse(is.na(sleep_df[["1210-0.0"]]),NA,
                      ifelse(sleep_df[["1210-0.0"]] == 2, 1, 0))

# no frequently narcolepsy
# 0	Never/rarely
# 1	Sometimes
narcolepsy_cnd <- ifelse(is.na(sleep_df[["1220-0.0"]]),NA,
                         ifelse(sleep_df[["1220-0.0"]] < 2, 1, 0))

# sleep mat
sleep_mat <- data.frame(chrono_cnd, duration_cnd, insomnia_cnd,
                        snoring_cnd, narcolepsy_cnd)

sleep_cnd <- rowSums(sleep_mat) >= 4

# Can satisfy even when missing = F
sleep_cnd[rowSums(sleep_mat,na.rm = T) >= 4] <- T

# Never satisfy even when missing = T
sleep_cnd[rowSums(is.na(sleep_mat)) + rowSums(sleep_mat,na.rm = T) < 4] <- F


################## never use cannabis ##################
cannabis_cnd <- ifelse(is.na(habit_baseline_df[["20453-0.0"]]), NA, 
                       ifelse(habit_baseline_df[["20453-0.0"]] == 0, 1,
                              ifelse(habit_baseline_df[["20453-0.0"]] > 0, 0, NA)))

################## life factors mat format ##################
lf_df <- data.frame(Nosmoke = nosmoke_cnd, 
                    Activity = activity_cnd, 
                    Diet = diet_cnd, 
                    Noalcohol = noalcohol_cnd,
                    Sleep = sleep_cnd,
                    Nocannabis = cannabis_cnd)

lifescore1 <- rep(NA, nrow(lf_df))
lifescore1[rowSums(lf_df, na.rm = T) >= 4] <- 3 
lifescore1[rowSums(lf_df[,c(1:5)], na.rm = T) == 3] <- 3
lifescore1[rowSums(lf_df, na.rm = T) >= 2 &
             (rowSums(lf_df, na.rm = T) + rowSums(is.na(lf_df))) < 4 ] <- 2 
lifescore1[(rowSums(lf_df, na.rm = T) + rowSums(is.na(lf_df))) < 2 | 
             rowSums(lf_df, na.rm = T) == 0] <- 1 

lifescore2 <- rep(2, nrow(lf_df))
lifescore2[rowSums(lf_df, na.rm = T) >= 4] <- 3 
lifescore2[rowSums(lf_df[,c(1:5)], na.rm = T) >= 3] <- 3 
lifescore2[rowSums(lf_df == 0, na.rm = T) >= 4] <- 1

lifescore3 <- rep(2, nrow(lf_df))
lifescore3[rowSums(lf_df, na.rm = T) >= 4] <- 3 
lifescore3[rowSums(lf_df[,c(1:5)], na.rm = T) >= 3] <- 3 
lifescore3[rowSums(lf_df == 0, na.rm = T) >= 5] <- 1

#
life_factor_df <- data.frame(eid = habit_baseline_df[["eid"]],
                             QC = qc_cnd,
                             EUR_eth = EUR_cnd,
                             Townsend = habit_baseline_df[["189-0.0"]],
                             Sex = habit_baseline_df[["22001-0.0"]],
                             Age = habit_baseline_df[["21003-0.0"]],
                             BMI = habit_baseline_df[["21001-0.0"]],
                             BMI_grp = BMI_grp, 
                             Income1 = Income1,
                             Income2 = Income2,
                             Education1 = edu_cnd1,
                             Education2 = edu_cnd2,
                             Employment = emp_cnd,
                             Nosmoke = nosmoke_cnd, 
                             Activity = activity_cnd, 
                             Diet = diet_cnd, 
                             Noalcohol = noalcohol_cnd,
                             Sleep = sleep_cnd,
                             Nocannabis = cannabis_cnd,
                             lifescore1 = lifescore1,
                             lifescore2 = lifescore2,
                             lifescore3 = lifescore2,
                             data_in = str_split(habit_baseline_df[["53-0.0"]], "-",simplify = T)[,1],
                             center = habit_baseline_df[["54-0.0"]])

fwrite2(life_factor_df, file = "~/Infection_SES/life_factor_df.txt", sep = "\t")
