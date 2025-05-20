#### Historical Run-Timing Curves ####
## Jonah Bacon
## jonah.bacon@alaska.gov
## 16 May 2025

library(tidyverse)

rm(list = ls())

CurrentYear <- 2024                                                             ## current in-season year 
StreamLife <- 17.5                                                              ## estimated life in stream of fish
ObsEff <- 1.0                                                                   ## observer efficiency

# Historical Run-Timing Curves ----

DIST.STOCKS <- read.csv("input/DISTRICT_STOCKS.csv")

STOCKS.IND.df <- read.csv("input/STOCKS_IND.csv") 
STOCKS.AGG.df <- read.csv("input/STOCKS_AGG.csv")

HIST.AY.RT.df <- read.csv("input/RUN_TIMING.csv") %>% 
  filter(Year >=1976) %>%                                                       ## only include years after 1976
  mutate(Year = as.integer(Year), ## converts character to integer
         Date = as.Date(Date), ## converts character to date
         JulianDay = as.POSIXlt(Date)$yday, ## converts date to Julian Date integer
         MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## create a MMDD date without year
         OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
         PinkOddEven = if_else(SPECIES == "Pink Salmon" & Year %% 2 == 0,"Even",NA),
         PinkOddEven = if_else(SPECIES == "Pink Salmon" & Year %% 2 != 0,"Odd",PinkOddEven)) %>% 
  full_join(DIST.STOCKS,
            by = join_by(SPECIES == Full_Species,STOCK == Stock)) %>% 
  filter(!is.na(Date) & SPECIES %in% c("Chum Salmon","Sockeye Salmon","Pink Salmon"))

HIST.10Y.RT.df <- HIST.AY.RT.df %>% 
  filter(Year >= (CurrentYear-10))

HIST.20Y.RT.df <- HIST.AY.RT.df %>% 
  filter(Year >= (CurrentYear-20))

JDAY.MODEL.df <- data.frame(JulianDay = c(min(HIST.AY.RT.df$JulianDay):max(HIST.AY.RT.df$JulianDay)))

## Model Run Timing Curves - All Years - Individual Stocks -------------------------------------

i = 1 ; j = 1                                                                               
rm("RTC.IND.AY.df") ; for (i in c(1:nrow(STOCKS.IND.df))){
  HIST.AUC.df <- HIST.AY.RT.df %>% 
    filter(SPECIES == STOCKS.IND.df$SPECIES[i] & STOCK == STOCKS.IND.df$STOCK[i])
  
  if(STOCKS.IND.df$SPECIES[i] == "Pink Salmon"){
    for (j in 1:2) {
      
      model = glm(PercentEscape ~ JulianDay, family=binomial, data = filter(HIST.AUC.df, PinkOddEven == unique(HIST.AUC.df$PinkOddEven)[j]))       ## fits glm logistic model to data
      ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
      
      model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
        mutate(Species = STOCKS.IND.df$SPECIES[i],
               PinkOddEven = unique(HIST.AUC.df$PinkOddEven)[j],
               Stock = STOCKS.IND.df$STOCK[i],
               District = STOCKS.IND.df$DISTRICT[i],
               MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## expresses date in MMDD format
               OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
               PercentEscape = round(predict(model, JDAY.MODEL.df, type = "response"),3)) %>%   ## uses model to predict run % based on date         
        bind_cols(setNames(as_tibble(predict(model, JDAY.MODEL.df, se.fit = TRUE)[1:2]), c('fit_link', 'se_link'))) %>% 
        mutate(fit_resp = ilink(fit_link), 
               se_upr = round(ilink(fit_link + (1.65 * se_link)),3),
               se_lwr = round(ilink(fit_link - (1.65 * se_link)),3),
               LowerSEG = ceiling(PercentEscape*STOCKS.IND.df$LOWER[i]),
               UpperSEG = ceiling(PercentEscape*STOCKS.IND.df$UPPER[i]),
               LowerSEG_se_upr = ceiling(se_upr*STOCKS.IND.df$LOWER[i]),
               LowerSEG_se_lwr = ceiling(se_lwr*STOCKS.IND.df$LOWER[i]),
               UpperSEG_se_upr = ceiling(se_upr*STOCKS.IND.df$UPPER[i]),
               UpperSEG_se_lwr = ceiling(se_lwr*STOCKS.IND.df$UPPER[i]) )
      
      if(exists('RTC.IND.10Y.df') && is.data.frame(get('RTC.IND.10Y.df'))){   ## checks to see if output dataframe exists
        RTC.IND.AY.df <- rbind(RTC.IND.AY.df,model.preds)                 ## if it does, it appends new data
      }
      else {                                                                      ## if it doesn't creates new dataframe
        RTC.IND.AY.df <- model.preds
      }
    }
  }
  else {                                                                      
    
    model = glm(PercentEscape ~ JulianDay, family=binomial, data = HIST.AUC.df)       ## fits glm logistic model to data
    ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
    
    model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
      mutate(Species = STOCKS.IND.df$SPECIES[i],
             PinkOddEven = NA,
             Stock = STOCKS.IND.df$STOCK[i],
             District = STOCKS.IND.df$DISTRICT[i],
             MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## expresses date in MMDD format
             OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
             PercentEscape = round(predict(model, JDAY.MODEL.df, type = "response"),3)) %>%   ## uses model to predict run % based on date         
      bind_cols(setNames(as_tibble(predict(model, JDAY.MODEL.df, se.fit = TRUE)[1:2]), c('fit_link', 'se_link'))) %>% 
      mutate(fit_resp = ilink(fit_link), 
             se_upr = round(ilink(fit_link + (1.65 * se_link)),3),
             se_lwr = round(ilink(fit_link - (1.65 * se_link)),3),
             LowerSEG = ceiling(PercentEscape*STOCKS.IND.df$LOWER[i]),
             UpperSEG = ceiling(PercentEscape*STOCKS.IND.df$UPPER[i]),
             LowerSEG_se_upr = ceiling(se_upr*STOCKS.IND.df$LOWER[i]),
             LowerSEG_se_lwr = ceiling(se_lwr*STOCKS.IND.df$LOWER[i]),
             UpperSEG_se_upr = ceiling(se_upr*STOCKS.IND.df$UPPER[i]),
             UpperSEG_se_lwr = ceiling(se_lwr*STOCKS.IND.df$UPPER[i]) )
    
    if(exists('RTC.IND.AY.df') && is.data.frame(get('RTC.IND.AY.df'))){   ## checks to see if output dataframe exists
      RTC.IND.AY.df <- rbind(RTC.IND.AY.df,model.preds)                 ## if it does, it appends new data
    }
    else {                                                                      ## if it doesn't creates new dataframe
      RTC.IND.AY.df <- model.preds
    }
  }
}

RTC.IND.AY.df$YEAR.RANGE <- "All"
RTC.IND.AY.df$TYPE.STOCK <- "Individual"


## Model Run Timing Curves - Last 10 Years - Individual Stocks ---------------------------------

i = 1 ; j = 1                                                                              
rm("RTC.IND.10Y.df") ; for (i in c(1:nrow(STOCKS.IND.df))){
  HIST.AUC.df <- HIST.10Y.RT.df %>% 
    filter(SPECIES == STOCKS.IND.df$SPECIES[i] & STOCK == STOCKS.IND.df$STOCK[i])
  
  if(STOCKS.IND.df$SPECIES[i] == "Pink Salmon"){
    for (j in 1:2) {
      
      model = glm(PercentEscape ~ JulianDay, family=binomial, data = filter(HIST.AUC.df, PinkOddEven == unique(HIST.AUC.df$PinkOddEven)[j]))       ## fits glm logistic model to data
      ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
      
      model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
        mutate(Species = STOCKS.IND.df$SPECIES[i],
               PinkOddEven = unique(HIST.AUC.df$PinkOddEven)[j],
               Stock = STOCKS.IND.df$STOCK[i],
               District = STOCKS.IND.df$DISTRICT[i],
               MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## expresses date in MMDD format
               OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
               PercentEscape = round(predict(model, JDAY.MODEL.df, type = "response"),3)) %>%   ## uses model to predict run % based on date         
        bind_cols(setNames(as_tibble(predict(model, JDAY.MODEL.df, se.fit = TRUE)[1:2]), c('fit_link', 'se_link'))) %>% 
        mutate(fit_resp = ilink(fit_link), 
               se_upr = round(ilink(fit_link + (1.65 * se_link)),3),
               se_lwr = round(ilink(fit_link - (1.65 * se_link)),3),
               LowerSEG = ceiling(PercentEscape*STOCKS.IND.df$LOWER[i]),
               UpperSEG = ceiling(PercentEscape*STOCKS.IND.df$UPPER[i]),
               LowerSEG_se_upr = ceiling(se_upr*STOCKS.IND.df$LOWER[i]),
               LowerSEG_se_lwr = ceiling(se_lwr*STOCKS.IND.df$LOWER[i]),
               UpperSEG_se_upr = ceiling(se_upr*STOCKS.IND.df$UPPER[i]),
               UpperSEG_se_lwr = ceiling(se_lwr*STOCKS.IND.df$UPPER[i]) )
      
      if(exists('RTC.IND.10Y.df') && is.data.frame(get('RTC.IND.10Y.df'))){   ## checks to see if output dataframe exists
        RTC.IND.10Y.df <- rbind(RTC.IND.10Y.df,model.preds)                 ## if it does, it appends new data
      }
      else {                                                                      ## if it doesn't creates new dataframe
        RTC.IND.10Y.df <- model.preds
      }
    }
  }
  else {                                                                      
    
    model = glm(PercentEscape ~ JulianDay, family=binomial, data = HIST.AUC.df)       ## fits glm logistic model to data
    ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
    
    model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
      mutate(Species = STOCKS.IND.df$SPECIES[i],
             PinkOddEven = NA,
             Stock = STOCKS.IND.df$STOCK[i],
             District = STOCKS.IND.df$DISTRICT[i],
             MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## expresses date in MMDD format
             OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
             PercentEscape = round(predict(model, JDAY.MODEL.df, type = "response"),3)) %>%   ## uses model to predict run % based on date         
      bind_cols(setNames(as_tibble(predict(model, JDAY.MODEL.df, se.fit = TRUE)[1:2]), c('fit_link', 'se_link'))) %>% 
      mutate(fit_resp = ilink(fit_link), 
             se_upr = round(ilink(fit_link + (1.65 * se_link)),3),
             se_lwr = round(ilink(fit_link - (1.65 * se_link)),3),
             LowerSEG = ceiling(PercentEscape*STOCKS.IND.df$LOWER[i]),
             UpperSEG = ceiling(PercentEscape*STOCKS.IND.df$UPPER[i]),
             LowerSEG_se_upr = ceiling(se_upr*STOCKS.IND.df$LOWER[i]),
             LowerSEG_se_lwr = ceiling(se_lwr*STOCKS.IND.df$LOWER[i]),
             UpperSEG_se_upr = ceiling(se_upr*STOCKS.IND.df$UPPER[i]),
             UpperSEG_se_lwr = ceiling(se_lwr*STOCKS.IND.df$UPPER[i]) )
    
    if(exists('RTC.IND.10Y.df') && is.data.frame(get('RTC.IND.10Y.df'))){   ## checks to see if output dataframe exists
      RTC.IND.10Y.df <- rbind(RTC.IND.10Y.df,model.preds)                 ## if it does, it appends new data
    }
    else {                                                                      ## if it doesn't creates new dataframe
      RTC.IND.10Y.df <- model.preds
    }
  }
}

RTC.IND.10Y.df$YEAR.RANGE <- "Ten"
RTC.IND.10Y.df$TYPE.STOCK <- "Individual"


## Model Run Timing Curves - Last 20 Years - Individual Stocks ---------------------------------

i = 1 ; j = 1                                                        
rm("RTC.IND.20Y.df") ; for (i in c(1:nrow(STOCKS.IND.df))){
  HIST.AUC.df <- HIST.20Y.RT.df %>% 
    filter(SPECIES == STOCKS.IND.df$SPECIES[i] & STOCK == STOCKS.IND.df$STOCK[i])
  
  if(STOCKS.IND.df$SPECIES[i] == "Pink Salmon"){
    for (j in 1:2) {
      
      model = glm(PercentEscape ~ JulianDay, family=binomial, data = filter(HIST.AUC.df, PinkOddEven == unique(HIST.AUC.df$PinkOddEven)[j]))       ## fits glm logistic model to data
      ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
      
      model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
        mutate(Species = STOCKS.IND.df$SPECIES[i],
               PinkOddEven = unique(HIST.AUC.df$PinkOddEven)[j],
               Stock = STOCKS.IND.df$STOCK[i],
               District = STOCKS.IND.df$DISTRICT[i],
               MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## expresses date in MMDD format
               OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
               PercentEscape = round(predict(model, JDAY.MODEL.df, type = "response"),3)) %>%   ## uses model to predict run % based on date         
        bind_cols(setNames(as_tibble(predict(model, JDAY.MODEL.df, se.fit = TRUE)[1:2]), c('fit_link', 'se_link'))) %>% 
        mutate(fit_resp = ilink(fit_link), 
               se_upr = round(ilink(fit_link + (1.65 * se_link)),3),
               se_lwr = round(ilink(fit_link - (1.65 * se_link)),3),
               LowerSEG = ceiling(PercentEscape*STOCKS.IND.df$LOWER[i]),
               UpperSEG = ceiling(PercentEscape*STOCKS.IND.df$UPPER[i]),
               LowerSEG_se_upr = ceiling(se_upr*STOCKS.IND.df$LOWER[i]),
               LowerSEG_se_lwr = ceiling(se_lwr*STOCKS.IND.df$LOWER[i]),
               UpperSEG_se_upr = ceiling(se_upr*STOCKS.IND.df$UPPER[i]),
               UpperSEG_se_lwr = ceiling(se_lwr*STOCKS.IND.df$UPPER[i]) )
      
      if(exists('RTC.IND.20Y.df') && is.data.frame(get('RTC.IND.20Y.df'))){   ## checks to see if output dataframe exists
        RTC.IND.20Y.df <- rbind(RTC.IND.20Y.df,model.preds)                 ## if it does, it appends new data
      }
      else {                                                                      ## if it doesn't creates new dataframe
        RTC.IND.20Y.df <- model.preds
      }
    }
  }
  else {                                                                      
    
    model = glm(PercentEscape ~ JulianDay, family=binomial, data = HIST.AUC.df)       ## fits glm logistic model to data
    ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
    
    model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
      mutate(Species = STOCKS.IND.df$SPECIES[i],
             PinkOddEven = NA,
             Stock = STOCKS.IND.df$STOCK[i],
             District = STOCKS.IND.df$DISTRICT[i],
             MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## expresses date in MMDD format
             OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
             PercentEscape = round(predict(model, JDAY.MODEL.df, type = "response"),3)) %>%   ## uses model to predict run % based on date         
      bind_cols(setNames(as_tibble(predict(model, JDAY.MODEL.df, se.fit = TRUE)[1:2]), c('fit_link', 'se_link'))) %>% 
      mutate(fit_resp = ilink(fit_link), 
             se_upr = round(ilink(fit_link + (1.65 * se_link)),3),
             se_lwr = round(ilink(fit_link - (1.65 * se_link)),3),
             LowerSEG = ceiling(PercentEscape*STOCKS.IND.df$LOWER[i]),
             UpperSEG = ceiling(PercentEscape*STOCKS.IND.df$UPPER[i]),
             LowerSEG_se_upr = ceiling(se_upr*STOCKS.IND.df$LOWER[i]),
             LowerSEG_se_lwr = ceiling(se_lwr*STOCKS.IND.df$LOWER[i]),
             UpperSEG_se_upr = ceiling(se_upr*STOCKS.IND.df$UPPER[i]),
             UpperSEG_se_lwr = ceiling(se_lwr*STOCKS.IND.df$UPPER[i]) )
    
    if(exists('RTC.IND.20Y.df') && is.data.frame(get('RTC.IND.20Y.df'))){   ## checks to see if output dataframe exists
      RTC.IND.20Y.df <- rbind(RTC.IND.20Y.df,model.preds)                 ## if it does, it appends new data
    }
    else {                                                                      ## if it doesn't creates new dataframe
      RTC.IND.20Y.df <- model.preds
    }
  }
}

RTC.IND.20Y.df$YEAR.RANGE <- "Twenty"
RTC.IND.20Y.df$TYPE.STOCK <- "Individual"


## Model Run Timing Curves - All Years - Aggregate Stocks -------------------------------------

i = 1 ; j = 1                                                                               
rm("RTC.AGG.AY.df") ; for (i in c(1:nrow(STOCKS.AGG.df))){
  HIST.AUC.df <- HIST.AY.RT.df %>% 
    filter(SPECIES == STOCKS.AGG.df$SPECIES[i] & District == STOCKS.AGG.df$DISTRICT[i])
  
  if(STOCKS.AGG.df$SPECIES[i] == "Pink Salmon"){
    for (j in 1:2) {
      
      model = glm(PercentEscape ~ JulianDay, family=binomial, data = filter(HIST.AUC.df, PinkOddEven == unique(HIST.AUC.df$PinkOddEven)[j]))       ## fits glm logistic model to data
      ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
      
      model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
        mutate(Species = STOCKS.AGG.df$SPECIES[i],
               PinkOddEven = unique(HIST.AUC.df$PinkOddEven)[j],
               Stock = STOCKS.AGG.df$STOCK[i],
               District = STOCKS.AGG.df$DISTRICT[i],
               MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## expresses date in MMDD format
               OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
               PercentEscape = round(predict(model, JDAY.MODEL.df, type = "response"),3)) %>%   ## uses model to predict run % based on date         
        bind_cols(setNames(as_tibble(predict(model, JDAY.MODEL.df, se.fit = TRUE)[1:2]), c('fit_link', 'se_link'))) %>% 
        mutate(fit_resp = ilink(fit_link), 
               se_upr = round(ilink(fit_link + (1.65 * se_link)),3),
               se_lwr = round(ilink(fit_link - (1.65 * se_link)),3),
               LowerSEG = ceiling(PercentEscape*STOCKS.AGG.df$LOWER[i]),
               UpperSEG = ceiling(PercentEscape*STOCKS.AGG.df$UPPER[i]),
               LowerSEG_se_upr = ceiling(se_upr*STOCKS.AGG.df$LOWER[i]),
               LowerSEG_se_lwr = ceiling(se_lwr*STOCKS.AGG.df$LOWER[i]),
               UpperSEG_se_upr = ceiling(se_upr*STOCKS.AGG.df$UPPER[i]),
               UpperSEG_se_lwr = ceiling(se_lwr*STOCKS.AGG.df$UPPER[i]) )
      
      if(exists('RTC.AGG.10Y.df') && is.data.frame(get('RTC.AGG.10Y.df'))){   ## checks to see if output dataframe exists
        RTC.AGG.AY.df <- rbind(RTC.AGG.AY.df,model.preds)                 ## if it does, it appends new data
      }
      else {                                                                      ## if it doesn't creates new dataframe
        RTC.AGG.AY.df <- model.preds
      }
    }
  }
  else {                                                                      
    
    model = glm(PercentEscape ~ JulianDay, family=binomial, data = HIST.AUC.df)       ## fits glm logistic model to data
    ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
    
    model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
      mutate(Species = STOCKS.AGG.df$SPECIES[i],
             PinkOddEven = NA,
             Stock = STOCKS.AGG.df$STOCK[i],
             District = STOCKS.AGG.df$DISTRICT[i],
             MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## expresses date in MMDD format
             OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
             PercentEscape = round(predict(model, JDAY.MODEL.df, type = "response"),3)) %>%   ## uses model to predict run % based on date         
      bind_cols(setNames(as_tibble(predict(model, JDAY.MODEL.df, se.fit = TRUE)[1:2]), c('fit_link', 'se_link'))) %>% 
      mutate(fit_resp = ilink(fit_link), 
             se_upr = round(ilink(fit_link + (1.65 * se_link)),3),
             se_lwr = round(ilink(fit_link - (1.65 * se_link)),3),
             LowerSEG = ceiling(PercentEscape*STOCKS.AGG.df$LOWER[i]),
             UpperSEG = ceiling(PercentEscape*STOCKS.AGG.df$UPPER[i]),
             LowerSEG_se_upr = ceiling(se_upr*STOCKS.AGG.df$LOWER[i]),
             LowerSEG_se_lwr = ceiling(se_lwr*STOCKS.AGG.df$LOWER[i]),
             UpperSEG_se_upr = ceiling(se_upr*STOCKS.AGG.df$UPPER[i]),
             UpperSEG_se_lwr = ceiling(se_lwr*STOCKS.AGG.df$UPPER[i]) )
    
    if(exists('RTC.AGG.AY.df') && is.data.frame(get('RTC.AGG.AY.df'))){   ## checks to see if output dataframe exists
      RTC.AGG.AY.df <- rbind(RTC.AGG.AY.df,model.preds)                 ## if it does, it appends new data
    }
    else {                                                                      ## if it doesn't creates new dataframe
      RTC.AGG.AY.df <- model.preds
    }
  }
}

RTC.AGG.AY.df$YEAR.RANGE <- "All"
RTC.AGG.AY.df$TYPE.STOCK <- "Aggregate"


## Model Run Timing Curves - Last 10 Years - Aggregate Stocks ---------------------------------

i = 1 ; j = 1                                                                              
rm("RTC.AGG.10Y.df") ; for (i in c(1:nrow(STOCKS.AGG.df))){
  HIST.AUC.df <- HIST.10Y.RT.df %>% 
    filter(SPECIES == STOCKS.AGG.df$SPECIES[i] & District == STOCKS.AGG.df$DISTRICT[i])
  
  if(STOCKS.AGG.df$SPECIES[i] == "Pink Salmon"){
    for (j in 1:2) {
      
      model = glm(PercentEscape ~ JulianDay, family=binomial, data = filter(HIST.AUC.df, PinkOddEven == unique(HIST.AUC.df$PinkOddEven)[j]))       ## fits glm logistic model to data
      ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
      
      model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
        mutate(Species = STOCKS.AGG.df$SPECIES[i],
               PinkOddEven = unique(HIST.AUC.df$PinkOddEven)[j],
               Stock = STOCKS.AGG.df$STOCK[i],
               District = STOCKS.AGG.df$DISTRICT[i],
               MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## expresses date in MMDD format
               OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
               PercentEscape = round(predict(model, JDAY.MODEL.df, type = "response"),3)) %>%   ## uses model to predict run % based on date         
        bind_cols(setNames(as_tibble(predict(model, JDAY.MODEL.df, se.fit = TRUE)[1:2]), c('fit_link', 'se_link'))) %>% 
        mutate(fit_resp = ilink(fit_link), 
               se_upr = round(ilink(fit_link + (1.65 * se_link)),3),
               se_lwr = round(ilink(fit_link - (1.65 * se_link)),3),
               LowerSEG = ceiling(PercentEscape*STOCKS.AGG.df$LOWER[i]),
               UpperSEG = ceiling(PercentEscape*STOCKS.AGG.df$UPPER[i]),
               LowerSEG_se_upr = ceiling(se_upr*STOCKS.AGG.df$LOWER[i]),
               LowerSEG_se_lwr = ceiling(se_lwr*STOCKS.AGG.df$LOWER[i]),
               UpperSEG_se_upr = ceiling(se_upr*STOCKS.AGG.df$UPPER[i]),
               UpperSEG_se_lwr = ceiling(se_lwr*STOCKS.AGG.df$UPPER[i]) )
      
      if(exists('RTC.AGG.10Y.df') && is.data.frame(get('RTC.AGG.10Y.df'))){   ## checks to see if output dataframe exists
        RTC.AGG.10Y.df <- rbind(RTC.AGG.10Y.df,model.preds)                 ## if it does, it appends new data
      }
      else {                                                                      ## if it doesn't creates new dataframe
        RTC.AGG.10Y.df <- model.preds
      }
    }
  }
  else {                                                                      
    
    model = glm(PercentEscape ~ JulianDay, family=binomial, data = HIST.AUC.df)       ## fits glm logistic model to data
    ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
    
    model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
      mutate(Species = STOCKS.AGG.df$SPECIES[i],
             PinkOddEven = NA,
             Stock = STOCKS.AGG.df$STOCK[i],
             District = STOCKS.AGG.df$DISTRICT[i],
             MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## expresses date in MMDD format
             OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
             PercentEscape = round(predict(model, JDAY.MODEL.df, type = "response"),3)) %>%   ## uses model to predict run % based on date         
      bind_cols(setNames(as_tibble(predict(model, JDAY.MODEL.df, se.fit = TRUE)[1:2]), c('fit_link', 'se_link'))) %>% 
      mutate(fit_resp = ilink(fit_link), 
             se_upr = round(ilink(fit_link + (1.65 * se_link)),3),
             se_lwr = round(ilink(fit_link - (1.65 * se_link)),3),
             LowerSEG = ceiling(PercentEscape*STOCKS.AGG.df$LOWER[i]),
             UpperSEG = ceiling(PercentEscape*STOCKS.AGG.df$UPPER[i]),
             LowerSEG_se_upr = ceiling(se_upr*STOCKS.AGG.df$LOWER[i]),
             LowerSEG_se_lwr = ceiling(se_lwr*STOCKS.AGG.df$LOWER[i]),
             UpperSEG_se_upr = ceiling(se_upr*STOCKS.AGG.df$UPPER[i]),
             UpperSEG_se_lwr = ceiling(se_lwr*STOCKS.AGG.df$UPPER[i]) )
    
    if(exists('RTC.AGG.10Y.df') && is.data.frame(get('RTC.AGG.10Y.df'))){   ## checks to see if output dataframe exists
      RTC.AGG.10Y.df <- rbind(RTC.AGG.10Y.df,model.preds)                 ## if it does, it appends new data
    }
    else {                                                                      ## if it doesn't creates new dataframe
      RTC.AGG.10Y.df <- model.preds
    }
  }
}

RTC.AGG.10Y.df$YEAR.RANGE <- "Ten"
RTC.AGG.10Y.df$TYPE.STOCK <- "Aggregate"


## Model Run Timing Curves - Last 20 Years - Aggregate Stocks ---------------------------------

i = 1 ; j = 1                                                        
rm("RTC.AGG.20Y.df") ; for (i in c(1:nrow(STOCKS.AGG.df))){
  HIST.AUC.df <- HIST.20Y.RT.df %>% 
    filter(SPECIES == STOCKS.AGG.df$SPECIES[i] & District == STOCKS.AGG.df$DISTRICT[i])
  
  if(STOCKS.AGG.df$SPECIES[i] == "Pink Salmon"){
    for (j in 1:2) {
      
      model = glm(PercentEscape ~ JulianDay, family=binomial, data = filter(HIST.AUC.df, PinkOddEven == unique(HIST.AUC.df$PinkOddEven)[j]))       ## fits glm logistic model to data
      ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
      
      model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
        mutate(Species = STOCKS.AGG.df$SPECIES[i],
               PinkOddEven = unique(HIST.AUC.df$PinkOddEven)[j],
               Stock = STOCKS.AGG.df$STOCK[i],
               District = STOCKS.AGG.df$DISTRICT[i],
               MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## expresses date in MMDD format
               OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
               PercentEscape = round(predict(model, JDAY.MODEL.df, type = "response"),3)) %>%   ## uses model to predict run % based on date         
        bind_cols(setNames(as_tibble(predict(model, JDAY.MODEL.df, se.fit = TRUE)[1:2]), c('fit_link', 'se_link'))) %>% 
        mutate(fit_resp = ilink(fit_link), 
               se_upr = round(ilink(fit_link + (1.65 * se_link)),3),
               se_lwr = round(ilink(fit_link - (1.65 * se_link)),3),
               LowerSEG = ceiling(PercentEscape*STOCKS.AGG.df$LOWER[i]),
               UpperSEG = ceiling(PercentEscape*STOCKS.AGG.df$UPPER[i]),
               LowerSEG_se_upr = ceiling(se_upr*STOCKS.AGG.df$LOWER[i]),
               LowerSEG_se_lwr = ceiling(se_lwr*STOCKS.AGG.df$LOWER[i]),
               UpperSEG_se_upr = ceiling(se_upr*STOCKS.AGG.df$UPPER[i]),
               UpperSEG_se_lwr = ceiling(se_lwr*STOCKS.AGG.df$UPPER[i]) )
      
      if(exists('RTC.AGG.20Y.df') && is.data.frame(get('RTC.AGG.20Y.df'))){   ## checks to see if output dataframe exists
        RTC.AGG.20Y.df <- rbind(RTC.AGG.20Y.df,model.preds)                 ## if it does, it appends new data
      }
      else {                                                                      ## if it doesn't creates new dataframe
        RTC.AGG.20Y.df <- model.preds
      }
    }
  }
  else {                                                                      
    
    model = glm(PercentEscape ~ JulianDay, family=binomial, data = HIST.AUC.df)       ## fits glm logistic model to data
    ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
    
    model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
      mutate(Species = STOCKS.AGG.df$SPECIES[i],
             PinkOddEven = NA,
             Stock = STOCKS.AGG.df$STOCK[i],
             District = STOCKS.AGG.df$DISTRICT[i],
             MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## expresses date in MMDD format
             OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
             PercentEscape = round(predict(model, JDAY.MODEL.df, type = "response"),3)) %>%   ## uses model to predict run % based on date         
      bind_cols(setNames(as_tibble(predict(model, JDAY.MODEL.df, se.fit = TRUE)[1:2]), c('fit_link', 'se_link'))) %>% 
      mutate(fit_resp = ilink(fit_link), 
             se_upr = round(ilink(fit_link + (1.65 * se_link)),3),
             se_lwr = round(ilink(fit_link - (1.65 * se_link)),3),
             LowerSEG = ceiling(PercentEscape*STOCKS.AGG.df$LOWER[i]),
             UpperSEG = ceiling(PercentEscape*STOCKS.AGG.df$UPPER[i]),
             LowerSEG_se_upr = ceiling(se_upr*STOCKS.AGG.df$LOWER[i]),
             LowerSEG_se_lwr = ceiling(se_lwr*STOCKS.AGG.df$LOWER[i]),
             UpperSEG_se_upr = ceiling(se_upr*STOCKS.AGG.df$UPPER[i]),
             UpperSEG_se_lwr = ceiling(se_lwr*STOCKS.AGG.df$UPPER[i]) )
    
    if(exists('RTC.AGG.20Y.df') && is.data.frame(get('RTC.AGG.20Y.df'))){   ## checks to see if output dataframe exists
      RTC.AGG.20Y.df <- rbind(RTC.AGG.20Y.df,model.preds)                 ## if it does, it appends new data
    }
    else {                                                                      ## if it doesn't creates new dataframe
      RTC.AGG.20Y.df <- model.preds
    }
  }
}

RTC.AGG.20Y.df$YEAR.RANGE <- "Twenty"
RTC.AGG.20Y.df$TYPE.STOCK <- "Aggregate"

## Create RTC dataframes ----------------------------------------------------

RTC.IND.df <- rbind(RTC.IND.AY.df, RTC.IND.10Y.df, RTC.IND.20Y.df)
RTC.AGG.df <- rbind(RTC.AGG.AY.df, RTC.AGG.10Y.df, RTC.AGG.20Y.df)

# write.csv(RTC.IND.df, "output/Historical_Ind.RTC.csv", row.names=FALSE)        ## writes output dataframe to csv file
# write.csv(RTC.AGG.df, "output/Historical_AGG.RTC.csv", row.names=FALSE)        ## writes output dataframe to csv file

save(list = c("RTC.IND.df","RTC.AGG.df","HIST.AY.RT.df"), file = "data/HistoricRTC.RData")

