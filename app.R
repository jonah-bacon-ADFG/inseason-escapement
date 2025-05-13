
# getwd() # Adjust as needed to "inseason-escapement" directory

library(tidyverse)
library(scales)
library(shiny)
library(shinythemes)
library(waiter)
library(DT)
library(gridExtra)
library(rsconnect)

rm(list = ls())

CurrentYear <- 2023                                                             ## current in-season year 
StreamLife <- 17.5                                                              ## estimated life in stream of fish
ObsEff <- 1.0                                                                   ## observer efficiency

# Historical Run-Timing Curves ----

DIST.STOCKS <- read.csv("data/DISTRICT_STOCKS.csv")

STOCKS.IND.df <- read.csv("data/STOCKS.csv") 
STOCKS.AGG.df <- read.csv("data/AGG_STOCKS.csv")

HIST.AY.RT.df <- read.csv("data/InputRUN_TIMING.csv") %>% 
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
CY.RT.df <- read.csv("data/CURRENTYEAR.csv") %>% 
  mutate(Year = as.integer(Year), ## converts character to integer
         Date = as.Date(Date), ## converts character to date
         JulianDay = as.POSIXlt(Date)$yday, ## converts date to Julian Date integer
         MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## create a MMDD date without year
         OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")))

CY.STOCKS.df<- CY.RT.df %>% 
  group_by(Year,SPECIES,SURVEY_TYPE,STOCK) %>%                                  ## Groups dataframe by categories
  summarize(TotalCount = sum(COUNT))                                            ## Sums all survey counts across the year

DIST.STOCKS <- read.csv("data/DISTRICT_STOCKS.csv")

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


ui <- fluidPage(
  useWaiter(), 
  waiterPreloader(), #optional loading screen that only shows briefly
  theme = shinytheme("sandstone"),
  navbarPage(paste(CurrentYear,"In-Season Run Timing Curve Visualization"), 
             tabPanel("Charts",
                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          # tags$head(tags$style(HTML("hr {border-top: 1px solid #b3b3b3;}"))),
                          h2("Chart Options"),
                          hr(),
                          conditionalPanel(condition = "input.mainpanels_id == 'District-Wide Aggregate SEG Goals'",
                                           selectInput(inputId = "speciesInput1",
                                                       label = h3("Species"),
                                                       choices = list("Pink Salmon" = "Pink Salmon",
                                                                      "Chum Salmon" = "Chum Salmon")),
                                           selectInput(inputId = "districtInput1",
                                                       label = h3("District"),
                                                       c("")),
                                           selectInput(inputId = "yearInput1",
                                                       label = h3("Year Range"),
                                                       c(""))),
                          conditionalPanel(condition = "input.mainpanels_id == 'Individual-Stock Management Objective Goals'",
                                           selectInput(inputId = "speciesInput2",
                                                       label = h3("Species"),
                                                       choices = list("Pink Salmon" = "Pink Salmon",
                                                                      "Chum Salmon" = "Chum Salmon",
                                                                      "Sockeye Salmon" = "Sockeye Salmon")),
                                           selectInput(inputId = "stockInput2",
                                                       label = h3("Stock"),
                                                       c("")),
                                           selectInput(inputId = "yearInput2",
                                                       label = h3("Year Range"),
                                                       c("")))),
                        mainPanel(
                          width = 10,
                          tabsetPanel(
                            tabPanel(title = "District-Wide Aggregate SEG Goals", 
                                     plotOutput("agg.seg.escape", height = "700px", hover = hoverOpts(id ="plot_hover")),
                                     verbatimTextOutput("hover_info"),
                                     DTOutput("agg.seg.escape.table")),
                            tabPanel(title = "Individual-Stock Management Objective Goals", 
                                     plotOutput("manage.obj.escape", height = "700px", hover = hoverOpts(id ="plot_hover")),
                                     verbatimTextOutput("hover_info"),
                                     DTOutput("manage.obj.escape.table")),
                            id="mainpanels_id"
                          )
                        )
                      )
             ),
             tabPanel("About", 
                      h3("About this application:"),
                      h4("This application is designed to take in-season escapement data and compare them to historical
                        run-timing curves to assess how a given stock's escapement is progressing within the current year.
                        A variety of methods were employed to estimate escapement across different streams and stocks: aerial 
                        surveys, ground (stream walking) surveys, and weir (physical or video) counts."),
                      h4("Aerial and ground survey counts are used to calculate a final escapement index for each stream based on 
                         the area-under-the-curve method (pink and chum salmon) or the peak survey count (sockeye salmon)."),
                      h4("Historical run-timing curves are calculated using a glm binomial model. The percent of escapement is modeled
                        as a function of the Julian day-of-year. Three run-timing models are created. The first model includes 
                        all escapement data collected from 1976 to the year prior to the current year, the second includes only the
                        previous 10 years of escapement data, and the third includes the previous 20 years of escapement data."),
                      h4("Current year escapement is compared to differing types of escapement goals: District-wide aggregate 
                      sustainbable escapement goals (SEG's) and individual-stock management objectives."),
                      br(),
                      p("If you have questions or suggestions, please contact Jonah Bacon,
                             jonah.bacon@alaska.gov"),
                      br(),
                      p("Application version 1.1")
             )
             
  ))


theme_set(theme_bw(base_size = 20)+
            theme(panel.grid.major.y = element_line(color = "gray80",linewidth = 0.5,linetype = 2), 
                  panel.grid.minor.y = element_line(color = "gray80",linewidth = 0.5,linetype = 2),
                  panel.grid.major.x = element_blank(), 
                  panel.grid.minor.x = element_blank(),
                  panel.border = element_blank(),
                  axis.line = element_line(color = "black"),
                  axis.text = element_text(color = "black"),
                  axis.title = element_text(color = "black", size = 20, face = "bold"),
                  axis.text.x = element_text(angle = 270,vjust=-0.1)))

server <- function(input, output, session) {
  
  ## Dynamic drop-down lists - SEG's ----
  
  observeEvent(input$speciesInput1,
               {
                 a <- input$speciesInput1
                 updateSelectInput(session,
                                   inputId = "districtInput1",
                                   choices = unique(RTC.AGG.df[(RTC.AGG.df$Species == input$speciesInput1 & RTC.AGG.df$TYPE.STOCK == "Aggregate"), ]$District)
                 )
               }
  )
  observeEvent(input$districtInput1,
               {
                 b <- input$districtInput1
                 updateSelectInput(session, "yearInput1",
                                   choices = unique(RTC.AGG.df[(RTC.AGG.df$Species == input$speciesInput1 & RTC.AGG.df$District == input$districtInput1), ]$YEAR.RANGE)
                 )
               }
  )
  
  ## Make data reactive - SEG's ----
  
  aggregate_data <- reactive({
    
    RTC.AGG.df %>% filter(Species == input$speciesInput1 & District == input$districtInput1 & YEAR.RANGE == input$yearInput1)
    
  })
  
  aggregate.CY_data <- reactive({
    
    AGG.CY.AUC.df %>%
      filter(SPECIES == input$speciesInput1 & DISTRICT == input$districtInput1)
    
  })
  
  aggregate.CY.surv_data <- reactive({
    
    AGG.CY.AUC.surv.df %>% 
      filter(SPECIES == input$speciesInput1 & DISTRICT == input$districtInput1)
    
  })
  
  aggregate.CY_table <- reactive({
    
    AGG.CY.AUC.table %>% 
      filter(SPECIES == input$speciesInput1 & DISTRICT == input$districtInput1) %>% 
      select(-c(SPECIES,DISTRICT))
    
  })
  
  ## Output plots - SEG's ----
  
  output$agg.seg.escape <- renderPlot({
    
    p1 <- ggplot(aggregate_data(), aes(x = OriginDate)) +
      
      geom_line(aes(y = LowerSEG), color ="steelblue", linewidth = 0.7) +
      geom_ribbon(aes(ymin = LowerSEG_se_lwr, ymax = LowerSEG_se_upr), fill = "steelblue", alpha = 0.3) +
      
      geom_line(aes(y = UpperSEG), color ="steelblue", linewidth = 0.7) +
      geom_ribbon(aes(ymin = UpperSEG_se_lwr, ymax = UpperSEG_se_upr), fill = "steelblue", alpha = 0.3) +
      
      geom_ribbon(aes(ymin = LowerSEG_se_upr, ymax = UpperSEG_se_lwr), fill = "steelblue", alpha = 0.1) +
      
      geom_line(data = aggregate.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = DistSumEscape),
                color = "red", lty = 2, linewidth = 1, alpha = 0.5) +
      geom_point(data = aggregate.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = DistSumEscape),
                 shape = 25, fill = "red", color = "black", size = 3) +
      
      scale_x_date(breaks = "1 week", date_labels = "%b %d") +
      xlab("Date") +
      ylab("Cumulative Escapement")
    
    
    p2 <- ggplot(data = aggregate.CY.surv_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y=COUNT, color = STOCK, fill = STOCK)) + ## AUC plot
      
      geom_line(linewidth = 1, alpha = 0.5) +
      geom_point(shape = 25, color = "black", size = 3) +
      
      scale_x_date(breaks = "1 week", date_labels = "%b %d", limits = c(as.Date(121, origin = as.Date("0000-01-01")),as.Date(325, origin = as.Date("0000-01-01")))) +
      xlab("Date") +
      ylab("Survey Count") +
      
      theme(legend.position = "inside",
            legend.position.inside = c(0.1,0.9))
    
    grid.arrange(p1, p2, ncol = 1, nrow = 2)
    
  })
  
  output$agg.seg.escape.table <- renderDataTable(aggregate.CY_table())
  
  ## Dynamic drop-down lists - ISMO's ----
  
  observeEvent(input$speciesInput2,
               {
                 x <- input$speciesInput2
                 updateSelectInput(session, 
                                   inputId = "stockInput2",
                                   choices = unique(RTC.IND.df[(RTC.IND.df$Species == input$speciesInput2 & RTC.IND.df$TYPE.STOCK == "Individual"), ]$Stock)
                 )
               }
  )
  observeEvent(input$stockInput2,
               {
                 y <- input$stockInput2
                 updateSelectInput(session, "yearInput2",
                                   choices = unique(RTC.IND.df[(RTC.IND.df$Species == input$speciesInput2 & RTC.IND.df$Stock == input$stockInput2), ]$YEAR.RANGE)
                 )
               }
  )
  
  ## Make data reactive - ISMO's ----
  
  ind.stock_data <- reactive({
    
    RTC.IND.df %>% filter(Species == input$speciesInput2 & Stock == input$stockInput2 & YEAR.RANGE == input$yearInput2)
    
  })
  
  ind.stock.CY_data <- reactive({
    
    IND.CY.AUC.df %>% 
      filter(SPECIES == input$speciesInput2 & STOCK == input$stockInput2)
    
  })
  
  ind.stock.CY_table <- reactive({
    
    IND.CY.AUC.table %>% 
      filter(SPECIES == input$speciesInput2 & Stock == input$stockInput2) %>% 
      select(-c(SPECIES,Stock))
    
  })
  
  ## Output plots - ISMO's ----
  
  output$manage.obj.escape <- renderPlot({
    
    p1 <- ggplot(ind.stock_data(), aes(x = OriginDate)) +
      
      geom_line(aes(y = LowerSEG), color ="steelblue", linewidth = 0.7) +
      geom_ribbon(aes(ymin = LowerSEG_se_lwr, ymax = LowerSEG_se_upr), fill = "steelblue", alpha = 0.3) +
      
      geom_line(aes(y = UpperSEG), color ="steelblue", linewidth = 0.7) +
      geom_ribbon(aes(ymin = UpperSEG_se_lwr, ymax = UpperSEG_se_upr), fill = "steelblue", alpha = 0.3) +
      
      geom_ribbon(aes(ymin = LowerSEG_se_upr, ymax = UpperSEG_se_lwr), fill = "steelblue", alpha = 0.1) +
      
      geom_line(data = ind.stock.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = SumEscape), 
                color = "red", lty = 2, linewidth = 1, alpha = 0.5) +
      geom_point(data = ind.stock.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = SumEscape), 
                 shape = 25, fill = "red", color = "black", size = 3) +
      
      scale_x_date(breaks = "1 week", date_labels = "%b %d") +
      xlab("Date") +
      ylab("Cumulative Escapement")
    
    
    p2 <- ggplot(data = ind.stock.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y=COUNT)) + ## AUC plot
      
      geom_line(color = "red", linewidth = 1, alpha = 0.5) + 
      geom_point(shape = 25, fill = "red", color = "black", size = 3) +
      
      scale_x_date(breaks = "1 week", date_labels = "%b %d", limits = c(as.Date(121, origin = as.Date("0000-01-01")),as.Date(325, origin = as.Date("0000-01-01")))) +
      xlab("Date") +
      ylab("Survey Count")
    
    grid.arrange(p1, p2, ncol = 1, nrow = 2)
    
  })
  
  output$manage.obj.escape.table <- renderDataTable(ind.stock.CY_table())
  
  ## WORK IN PROGRESS: Hover text around cursor on plot ----
  
  # displayed_text <- reactive({
  #   req(input$plot_hover)
  #   hover <- input$plot_hover
  #   dist <- sqrt((hover$y - filtered_data2()$SumEscape)^2)
  # 
  #   if(min(dist) < 100) {
  #     filtered_data2()[filtered_data2()$SumEscape == which.min(dist),]
  #     filtered_data2()$SumEscape[which.min(dist)]
  #   } else {
  #     NULL
  #   }
  # })
  # 
  # output$hover_info <- renderPrint({
  #   req(displayed_text())
  # 
  #   cat("Cumulative escapement\n")
  #   displayed_text()
  # })
  
}

shinyApp(ui,server)

# runApp()

# deployApp("code",
#           appName = "inseason_escapement_monitoring_app",
#           appTitle = "In-Season LCI Escapement Monitoring App",
#           appVisibility = "public"
# )

# deployApp("shiny-app-code",
#           appName = "inseason_escapement_monitoring_app",
#           appTitle = "In-Season LCI Escapement Monitoring App",
#           appVisibility = "public"
# )