library(bigreadr)
library(dplyr)
# library(lubridate)

pheno_str <- "~/Datasets/ukb/ukb47503.csv"

# pollutant data
## raw data
airpollutant_code <- c("24003-0.0",     ## 2010 Nitrogen dioxide        
                       "24004-0.0",     ## 2010 Nitrogen oxides         
                       "24005-0.0",     ## 2010 pm10    
                       "24006-0.0",     ## 2010 pm2.5      
                       "24008-0.0"      ## 2010 pm2.5-10
)
noisepollutant_code <- c("24020-0.0",   ## 2010 Average daytime sound level of noise pollutant       
                         "24021-0.0",   ## 2010 Average evening sound level of noise pollutant      
                         "24022-0.0"    ## 2010 Average night-time sound level of noise pollutant
)
traffic_code <- c("24011-0.0",          ## Traffic intensity on the nearest major road
                  "24012-0.0"           ## Inverse distance to the nearest major road
                  )

pollutant_df <- fread2(pheno_str, 
                       select = c("eid", airpollutant_code, noisepollutant_code, traffic_code))
noisepollutant <- cbind(pollutant_df[["24020-0.0"]], 
                        pollutant_df[["24021-0.0"]]+5,
                        pollutant_df[["24022-0.0"]]+10)
noisepollutant_weight <- rowMeans(noisepollutant, na.rm = T)

pollutant_df2 <- data.frame(eid = pollutant_df[["eid"]],
                            pm25 = pollutant_df[["24006-0.0"]], 
                            pm2510 = pollutant_df[["24008-0.0"]], 
                            pm10 = pollutant_df[["24005-0.0"]], 
                            no2 = pollutant_df[["24003-0.0"]],
                            nox = pollutant_df[["24004-0.0"]], 
                            noise = noisepollutant_weight,
                            traff_intens = pollutant_df[["24011-0.0"]],
                            dist2road = 1/pollutant_df[["24012-0.0"]],
                            log_traff_intens = log(pollutant_df[["24011-0.0"]]),
                            logi_dist2road = -log(pollutant_df[["24012-0.0"]])
)

fwrite2(pollutant_df2, file = "~/Infection_SES/pollutant_df.txt", sep = "\t")
