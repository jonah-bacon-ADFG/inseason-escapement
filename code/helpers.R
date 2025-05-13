
# getwd() # Adjust as needed to "inseason-escapement" directory

library(tidyverse)
library(scales)

rm(list = ls())

CurrentYear <- 2023                                                             ## current in-season year 
StreamLife <- 17.5                                                              ## estimated life in stream of fish
ObsEff <- 1.0                                                                   ## observer efficiency

# Historical Run-Timing Curves ----

DIST.STOCKS <- read.csv("input/DISTRICT_STOCKS.csv")

STOCKS.IND.df <- read.csv("input/STOCKS.csv") 
STOCKS.AGG.df <- read.csv("input/AGG_STOCKS.csv")

HIST.AY.RT.df <- read.csv("input/InputRUN_TIMING.csv") %>% 
  filter(Year >=1976) %>%                                                       ## only include years after 1976
  mutate(Year = as.integer(Year), ## converts character to integer
         Date = as.Date(Date), ## converts character to date
         JulianDay = as.POSIXlt(Date)$yday, ## converts date to Julian Date integer
         MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## create a MMDD date without year
         OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01"))) %>% 
  left_join(DIST.STOCKS,
            by = join_by(SPECIES,STOCK))

HIST.10Y.RT.df <- HIST.AY.RT.df %>% 
  filter(Year >= (CurrentYear-10))

HIST.20Y.RT.df <- HIST.AY.RT.df %>% 
  filter(Year >= (CurrentYear-20))

JDAY.MODEL.df <- data.frame(JulianDay = c(min(HIST.AY.RT.df$JulianDay):max(HIST.AY.RT.df$JulianDay)))

## Model Run Timing Curves - All Years - Individual Stocks -------------------------------------


## Make sure there is no object named "RTC.IND.AY.df" in your R Studio environment before running following for-loop:
i = 1                                                                                ## for loop runs the AUC calculations for
for (i in c(1:nrow(STOCKS.IND.df))){                                            ## each group
  HIST.AUC.df <- HIST.AY.RT.df %>% 
    filter(SPECIES == STOCKS.IND.df$SPECIES[i] & STOCK == STOCKS.IND.df$STOCK[i])
  
  model = glm(PercentEscape ~ JulianDay, family=binomial, data = HIST.AUC.df)       ## fits glm logistic model to data
  ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
  
  model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
    mutate(Species = STOCKS.IND.df$SPECIES[i],
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

RTC.IND.AY.df$YEAR.RANGE <- "All"
RTC.IND.AY.df$TYPE.STOCK <- "Individual"


## Model Run Timing Curves - Last 10 Years - Individual Stocks ---------------------------------


## Make sure there is no object named "RTC.IND.10Y.df" in your R Studio environment before running following for-loop:
i = 1                                                                                ## for loop runs the AUC calculations for
for (i in c(1:nrow(STOCKS.IND.df))){                                            ## each group
  HIST.AUC.df <- HIST.10Y.RT.df %>% 
    filter(SPECIES == STOCKS.IND.df$SPECIES[i] & STOCK == STOCKS.IND.df$STOCK[i])
  
  model = glm(PercentEscape ~ JulianDay, family=binomial, data = HIST.AUC.df)       ## fits glm logistic model to data
  ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
  
  model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
    mutate(Species = STOCKS.IND.df$SPECIES[i],
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

RTC.IND.10Y.df$YEAR.RANGE <- "Ten"
RTC.IND.10Y.df$TYPE.STOCK <- "Individual"


## Model Run Timing Curves - Last 20 Years - Individual Stocks ---------------------------------


## Make sure there is no object named "RTC.IND.20Y.df" in your R Studio environment before running following for-loop:
i = 1                                                                                ## for loop runs the AUC calculations for
for (i in c(1:nrow(STOCKS.IND.df))){                                            ## each group
  HIST.AUC.df <- HIST.20Y.RT.df %>% 
    filter(SPECIES == STOCKS.IND.df$SPECIES[i] & STOCK == STOCKS.IND.df$STOCK[i])
  
  model = glm(PercentEscape ~ JulianDay, family=binomial, data = HIST.AUC.df)       ## fits glm logistic model to data
  ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
  
  model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
    mutate(Species = STOCKS.IND.df$SPECIES[i],
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

RTC.IND.20Y.df$YEAR.RANGE <- "Twenty"
RTC.IND.20Y.df$TYPE.STOCK <- "Individual"


## Model Run Timing Curves - All Years - Aggregate Stocks -------------------------------------


## Make sure there is no object named "RTC.AGG.AY.df" in your R Studio environment before running following for-loop:
i = 1                                                                                ## for loop runs the AUC calculations for
for (i in c(1:nrow(STOCKS.AGG.df))){                                            ## each group
  HIST.AUC.df <- HIST.AY.RT.df %>%
    filter(SPECIES == STOCKS.AGG.df$SPECIES[i] & DISTRICT == STOCKS.AGG.df$DISTRICT[i])
  
  model = glm(PercentEscape ~ JulianDay, family=binomial, data = HIST.AUC.df)       ## fits glm logistic model to data
  ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
  
  model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
    mutate(Species = STOCKS.AGG.df$SPECIES[i],
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

RTC.AGG.AY.df$YEAR.RANGE <- "All"
RTC.AGG.AY.df$TYPE.STOCK <- "Aggregate"


## Model Run Timing Curves - Last 10 Years - Aggregate Stocks ---------------------------------


## Make sure there is no object named "RTC.AGG.10Y.df" in your R Studio environment before running following for-loop:
i = 1                                                                                ## for loop runs the AUC calculations for
for (i in c(1:nrow(STOCKS.AGG.df))){                                            ## each group
  HIST.AUC.df <- HIST.10Y.RT.df %>%
    filter(SPECIES == STOCKS.AGG.df$SPECIES[i] & DISTRICT == STOCKS.AGG.df$DISTRICT[i])
  
  model = glm(PercentEscape ~ JulianDay, family=binomial, data = HIST.AUC.df)       ## fits glm logistic model to data
  ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
  
  model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
    mutate(Species = STOCKS.AGG.df$SPECIES[i],
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

RTC.AGG.10Y.df$YEAR.RANGE <- "Last 10 years"
RTC.AGG.10Y.df$TYPE.STOCK <- "Aggregate"


## Model Run Timing Curves - Last 20 Years - Aggregate Stocks ---------------------------------


## Make sure there is no object named "RTC.AGG.20Y.df" in your R Studio environment before running following for-loop:
i = 1                                                                                ## for loop runs the AUC calculations for
for (i in c(1:nrow(STOCKS.AGG.df))){                                            ## each group
  HIST.AUC.df <- HIST.20Y.RT.df %>%
    filter(SPECIES == STOCKS.AGG.df$SPECIES[i] & DISTRICT == STOCKS.AGG.df$DISTRICT[i])
  
  model = glm(PercentEscape ~ JulianDay, family=binomial, data = HIST.AUC.df)       ## fits glm logistic model to data
  ilink <- family(model)$linkinv                                                ## calculates se and CI for fitted curve
  
  model.preds <- JDAY.MODEL.df %>% ## creates row from min to max date of data
    mutate(Species = STOCKS.AGG.df$SPECIES[i],
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

RTC.AGG.20Y.df$YEAR.RANGE <- "Last 20 years"
RTC.AGG.20Y.df$TYPE.STOCK <- "Aggregate"


## Create RTC dataframes ----------------------------------------------------

RTC.IND.df <- rbind(RTC.IND.AY.df, RTC.IND.10Y.df, RTC.IND.20Y.df)
RTC.AGG.df <- rbind(RTC.AGG.AY.df, RTC.AGG.10Y.df, RTC.AGG.20Y.df)

# write.csv(RTC.IND.df, "output/Historical_Ind.RTC.csv", row.names=FALSE)        ## writes output dataframe to csv file
# write.csv(RTC.AGG.df, "output/Historical_AGG.RTC.csv", row.names=FALSE)        ## writes output dataframe to csv file


# Current Year In-Season Escapement Calculations --------------------------

## GENERAL NOTE TO-DO: Need to filter out "Bay Counts" from current year data ##
CY.RT.df <- read.csv("input/CURRENTYEAR.csv") %>% 
  mutate(Year = as.integer(Year), ## converts character to integer
         Date = as.Date(Date), ## converts character to date
         JulianDay = as.POSIXlt(Date)$yday, ## converts date to Julian Date integer
         MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## create a MMDD date without year
         OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")))

CY.STOCKS.df<- CY.RT.df %>% 
  group_by(Year,SPECIES,SURVEY_TYPE,STOCK) %>%                                  ## Groups dataframe by categories
  summarize(TotalCount = sum(COUNT))                                            ## Sums all survey counts across the year

DIST.STOCKS <- read.csv("input/DISTRICT_STOCKS.csv")

## Make sure there is no object named "OUTPUTAUCCURRENTYEAR.dat" in your R Studio environment before running following for-loop:
## nrow counts how many unique groups
i=1                                                                             ## for loop runs the AUC calculations for
for (i in c(1:nrow(CY.STOCKS.df))){                                             ## each group
  AUC.CY.df <- CY.RT.df %>% 
    filter(Year == CY.STOCKS.df$Year[i] &
             SPECIES == CY.STOCKS.df$SPECIES[i] &
             STOCK == CY.STOCKS.df$STOCK[i] &
             SURVEY_TYPE == CY.STOCKS.df$SURVEY_TYPE[i])
  
  if (AUC.CY.df$COUNT[1] != 0) {                                                ## creates count of zero if first survey count is nonzero
    FirstRow <- head(AUC.CY.df,1) %>% 
      mutate(JulianDay = JulianDay - StreamLife,
             Date = Date - StreamLife,
             COUNT = 0)
    AUC.CY.df <- rbind(FirstRow, AUC.CY.df)
  }
  
  if (AUC.CY.df$COUNT[nrow(AUC.CY.df)] != 0) {                                  ## creates count of zero if last survey count is nonzero
    LastRow <- tail(AUC.CY.df,1) %>% 
      mutate(JulianDay = JulianDay + StreamLife,
             Date = Date + StreamLife,
             COUNT = 0)
    AUC.CY.df <- rbind(AUC.CY.df, LastRow)
  }
  
  
  for (j in c(1:nrow(AUC.CY.df))){                                              ## Calculates columns Fishdays, SumFishDays, EscInd, and SumEscape
    if (j>1){
      AUC.CY.df$Days[j]     <-  AUC.CY.df$JulianDay[j]-AUC.CY.df$JulianDay[j-1]
      AUC.CY.df$FishDays[j]    <- (AUC.CY.df$COUNT[j]+
                                     AUC.CY.df$COUNT[j-1])*(AUC.CY.df$Days[j])/2
      
      AUC.CY.df$SumFishDays[j] <- AUC.CY.df$FishDays[j] + AUC.CY.df$SumFishDays[j-1]
      AUC.CY.df$EscInd[j]      <- AUC.CY.df$FishDays[j]/(StreamLife*ObsEff)
      AUC.CY.df$SumEscape[j]   <- AUC.CY.df$EscInd[j] + AUC.CY.df$SumEscape[j-1]
    }
    else{
      AUC.CY.df$Days[j] <- 0
      AUC.CY.df$FishDays[j] <-0
      AUC.CY.df$SumFishDays[j] <-0
      AUC.CY.df$EscInd <- 0
      AUC.CY.df$SumEscape[j] <-0
    }
    
  }
  
  for (k in c(1:nrow(AUC.CY.df))){                                              ## Calculates PercentEscape Column.
    AUC.CY.df$PercentEscape[k] <- AUC.CY.df$SumEscape[k] / AUC.CY.df$SumEscape[nrow(AUC.CY.df)]
  }
  
  if(exists('OUTPUTAUCCURRENTYEAR.dat') && is.data.frame(get('OUTPUTAUCCURRENTYEAR.dat'))){                           ##checks to see if output dataframe exists
    OUTPUTAUCCURRENTYEAR.dat <- rbind(OUTPUTAUCCURRENTYEAR.dat,AUC.CY.df)       ##if it does, it appends new data
  }
  else {                                                                        ## if it doesn't creates new dataframe
    OUTPUTAUCCURRENTYEAR.dat <- AUC.CY.df
  }
  
}

## Escapement for Individual-Stream Management Objectives ------------------

IND.CY.AUC.df <- OUTPUTAUCCURRENTYEAR.dat %>% 
  filter(!(SPECIES %in% c("Chinook Salmon","Coho Salmon")))

IND.CY.AUC.table <- OUTPUTAUCCURRENTYEAR.dat %>% 
  filter(!(SPECIES %in% c("Chinook Salmon","Coho Salmon"))) %>% 
  rename(Stock = STOCK,
         SurveyType = SURVEY_TYPE,
         Count = COUNT) %>% 
  mutate(FishDays = number(round(FishDays,0),big.mark = ","),
         SumFishDays = number(round(SumFishDays,0),big.mark = ","),
         EscInd = number(round(EscInd,0),big.mark = ","),
         SumEscape = number(round(SumEscape,0),big.mark = ",")) %>% 
  select(-c(Year,MMDD,OriginDate,OFFICIAL_ESCAPEMENT,PercentEscape))


## Escapement for District Aggregate SEGs ----------------------------------


AGG.CY.AUC.df <- OUTPUTAUCCURRENTYEAR.dat %>% 
  filter(!(SPECIES %in% c("Chinook Salmon","Coho Salmon"))) %>% 
  left_join(DIST.STOCKS,
            by = join_by(SPECIES,STOCK)) %>% 
  # mutate(DISTRICT = ifelse(DISTRICT == "Kamishak","Kamishak District",DISTRICT),
  #        DISTRICT = ifelse(DISTRICT == "Southern","Southern District",DISTRICT),
  #        DISTRICT = ifelse(DISTRICT == "Outer","Outer District",DISTRICT)) %>% 
  filter(!is.na(DISTRICT))

i=1
j=1
for (i in 1:length(unique(AGG.CY.AUC.df$DISTRICT))) {
  
  for (j in 1:length(unique(AGG.CY.AUC.df$SPECIES))) {
    
    temp.df <- filter(AGG.CY.AUC.df, DISTRICT == unique(AGG.CY.AUC.df$DISTRICT)[i] & SPECIES == unique(AGG.CY.AUC.df$SPECIES)[j]) %>% 
      arrange(Date)
    
    for (k in 1:nrow(temp.df)) {
      
      if (k>1){
        
        temp.df$DistSumEscape[k] = temp.df$DistSumEscape[k-1] + temp.df$EscInd[k]
        
      }
      
      else{
        
        temp.df$DistSumEscape[k] = 0
        
      }
    }
    
    if(exists('temp.df2') && is.data.frame(get('temp.df2'))){                           ##checks to see if output dataframe exists
      temp.df2 <- rbind(temp.df2,temp.df)       ##if it does, it appends new data
    }
    else {                                                                        ## if it doesn't creates new dataframe
      temp.df2 <- temp.df
    }
    
  }
}

AGG.CY.AUC.df <- temp.df2 %>% 
  group_by(SPECIES,DISTRICT,JulianDay) %>% 
  summarise(DistSumEscape = max(DistSumEscape))

AGG.CY.AUC.surv.df <- temp.df2 %>% 
  select(SPECIES,DISTRICT,STOCK,JulianDay,COUNT)

AGG.CY.AUC.table <- temp.df2 %>% 
  rename(Stock = STOCK,
         SurveyType = SURVEY_TYPE,
         Count = COUNT,
         StreamSumEscape = SumEscape) %>% 
  mutate(FishDays = number(round(FishDays,0),big.mark = ","),
         SumFishDays = number(round(SumFishDays,0),big.mark = ","),
         EscInd = number(round(EscInd,0),big.mark = ","),
         StreamSumEscape = number(round(StreamSumEscape,0),big.mark = ","),
         DistSumEscape = number(round(DistSumEscape,0),big.mark = ",")) %>% 
  select(-c(Year,MMDD,OriginDate,OFFICIAL_ESCAPEMENT,PercentEscape))

# write.csv(temp.df2, paste0("output/",CurrentYear,".AGG.IN-SEASON.ESCAPEMENT.csv"), row.names=FALSE)  ## writes output dataframe to csv file
# write.csv(IND.CY.AUC.df, paste0("output/",CurrentYear,".IND.IN-SEASON.ESCAPEMENT.csv"), row.names=FALSE)  ## writes output dataframe to csv file

