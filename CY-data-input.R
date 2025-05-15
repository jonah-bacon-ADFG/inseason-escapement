
library(tidyverse)
library(xlsx)
library(fs)


CurrentYear <- 2024                                                             ## current in-season year 
StreamLife <- 17.5                                                              ## estimated life in stream of fish
ObsEff <- 1.0                                                                   ## observer efficiency

DIST.STOCKS <- read.csv("data/DISTRICT_STOCKS.csv")

# Upload current in-season data ----

## Ground Surveys ----

ground_dir <- paste0("O:/DCF/SALMON/ESCAPEMENT/",CurrentYear,"/GROUND/2_EDITED")

ground.df <- map_dfr(
  dir_ls(ground_dir, glob = "*.xlsx") |> set_names(),
  \(f) read_xlsx(f),
  .id = "File_Name") %>% 
  select(-c(Rowid,DeviceTime,SatelliteTime,Latitude,Longitude)) %>% 
  unite("Comments", c(Comments,comments), na.rm = T) %>% 
  unite("Edits", c(Edits,edits), na.rm = T) %>% 
  unite("Condition", c("Stream_Condition","Steam Condition","Stream_Conditions","Stream Condition","stream_condition","Stream Conditions"), na.rm = T) %>% 
  separate_wider_delim(File_Name, delim = "/", names = c(NA,NA,NA,NA,NA,NA,NA,"Survey")) %>% 
  filter(!grepl("TEST",Survey)) %>% 
  separate_wider_delim(Survey, delim = "_", names = c("Stock","Date",NA)) %>% 
  mutate(Comments = na_if(Comments,""),
         Edits = na_if(Edits,""),
         Condition = na_if(Condition,"")) %>% 
  filter(Species %in% c("Pink","Sockeye","Chum","Chinook","Coho")) %>% 
  group_by(Stock,Date,Condition,Species) %>% 
  summarise(SurveyCount = sum(Count)) %>% 
  group_by(Stock,Species,Date) %>% 
  mutate(Condition = max(as.integer(Condition)),
         Date = as.Date(ymd(Date)),
         JulianDay = as.POSIXlt(Date)$yday, ## converts date to Julian Date integer
         MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## create a MMDD date without year
         OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
         Survey = "Ground") %>% 
  ungroup() %>% 
  left_join(select(DIST.STOCKS,-Species),
            by = join_by(Stock == Code),
            relationship = "many-to-many") %>% 
  select(-Stock) %>% 
  rename(Stock = Stock.y)

## Aerial Surveys ----

aerial_dir <- paste0("O:/DCF/SALMON/ESCAPEMENT/",CurrentYear,"/AERIAL/4_GIS/9_SUMMARIES/EXCEL")

aerial.df <- map_dfr(
  dir_ls(aerial_dir, glob = "*.xlsx") |> set_names(),
  \(f) read_xlsx(f),
  .id = "File_Name") %>% 
  # select(-c(Rowid,DeviceTime,SatelliteTime,Latitude,Longitude)) %>% 
  unite("Count", c(SUM_COUNT,SUM_SURVEY), na.rm = T) %>% 
  unite("Condition", c(MEAN_OVERA,MEAN_QUALI), na.rm = T) %>% 
  filter(LIVE == "Live" | is.na(LIVE)) %>% 
  filter(SECT_CODE != 1) %>% 
  separate_wider_delim(File_Name, delim = "/", names = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,"Survey")) %>% 
  separate_wider_delim(Survey, delim = "_", names = c(NA,"Date",NA,NA)) %>% 
  mutate(Species = ifelse(SPECIES == "sockeye salmon","Sockeye",SPECIES),
         Stock = ifelse(STOCK == "Aialik","Aialik Lake",STOCK),
         Count = as.integer(Count),
         Condition = as.integer(Condition)) %>% 
  group_by(Date,Stock,Species,Condition) %>% 
  summarise(SurveyCount = sum(Count)) %>% 
  mutate(Date = as.Date(ymd(Date)),
         JulianDay = as.POSIXlt(Date)$yday, ## converts date to Julian Date integer
         MMDD = format(as.Date(JulianDay, origin = as.Date("0000-01-01")), "%m-%d"), ## create a MMDD date without year
         OriginDate = as.Date(JulianDay, origin = as.Date("0000-01-01")),
         Survey = "Aerial") %>% 
  left_join(select(DIST.STOCKS,-c(Species,Code)),
            by = join_by(Stock),
            relationship = "many-to-many")


all.survey.df <- full_join(aerial.df,ground.df,
                           by = join_by(Date, Stock, District, Species, Condition, SurveyCount,
                                        JulianDay, MMDD, OriginDate, Survey)) %>% 
  filter(!(Species %in% c("Chinook","Coho"))) %>% 
  mutate(Species = ifelse(Species == "Pink","Pink Salmon",Species),
         Species = ifelse(Species == "Chum","Chum Salmon",Species),
         Species = ifelse(Species == "Sockeye","Sockeye Salmon",Species)) 
         # Stock = ifelse(Stock %in% DIST.STOCKS$Code,DIST.STOCKS$Stock[DIST.STOCKS$Code],Stock)) %>% 

escapement.df <- all.survey.df %>% 
  group_by(Species,Stock,District,Survey) %>%                                  ## Groups dataframe by categories
  summarize(EscapementCount = sum(SurveyCount))                                            ## Sums all survey counts across the year

## Make sure there is no object named "CY.AUC.df" in your R Studio environment before running following for-loop:
## nrow counts how many unique groups

rm(list = "CY.AUC.df")

i=1                                                                             ## for loop runs the AUC calculations for
for (i in c(1:nrow(escapement.df))){                                             ## each group
  temp.CY.AUC.df <- all.survey.df %>% 
    filter(  Species == escapement.df$Species[i] &
             Stock == escapement.df$Stock[i] &
             Survey == escapement.df$Survey[i])
  
  if (temp.CY.AUC.df$SurveyCount[1] != 0) {                                                ## creates count of zero if first survey count is nonzero
    FirstRow <- head(temp.CY.AUC.df,1) %>% 
      mutate(JulianDay = JulianDay - StreamLife,
             Date = Date - StreamLife,
             Condition = NA,
             SurveyCount = 0)
    temp.CY.AUC.df <- rbind(FirstRow, temp.CY.AUC.df)
  }
  
  if (temp.CY.AUC.df$SurveyCount[nrow(temp.CY.AUC.df)] != 0) {                                  ## creates count of zero if last survey count is nonzero
    LastRow <- tail(temp.CY.AUC.df,1) %>%
      mutate(JulianDay = JulianDay + StreamLife,
             Date = Date + StreamLife,
             Condition = NA,
             SurveyCount = 0)
    temp.CY.AUC.df <- rbind(temp.CY.AUC.df, LastRow)
  }
  
  
  for (j in c(1:nrow(temp.CY.AUC.df))){                                              ## Calculates columns Fishdays, SumFishDays, EscInd, and SumEscape
    if (j>1){
      temp.CY.AUC.df$Days[j]     <-  temp.CY.AUC.df$JulianDay[j]-temp.CY.AUC.df$JulianDay[j-1]
      temp.CY.AUC.df$FishDays[j]    <- (temp.CY.AUC.df$SurveyCount[j]+
                                     temp.CY.AUC.df$SurveyCount[j-1])*(temp.CY.AUC.df$Days[j])/2
      
      temp.CY.AUC.df$SumFishDays[j] <- temp.CY.AUC.df$FishDays[j] + temp.CY.AUC.df$SumFishDays[j-1]
      temp.CY.AUC.df$EscInd[j]      <- temp.CY.AUC.df$FishDays[j]/(StreamLife*ObsEff)
      temp.CY.AUC.df$SumEscape[j]   <- temp.CY.AUC.df$EscInd[j] + temp.CY.AUC.df$SumEscape[j-1]
    }
    else{
      temp.CY.AUC.df$Days[j] <- 0
      temp.CY.AUC.df$FishDays[j] <-0
      temp.CY.AUC.df$SumFishDays[j] <-0
      temp.CY.AUC.df$EscInd <- 0
      temp.CY.AUC.df$SumEscape[j] <-0
    }
    
  }
  
  # for (k in c(1:nrow(temp.CY.AUC.df))){                                              ## Calculates PercentEscape Column.
  #   temp.CY.AUC.df$PercentEscape[k] <- temp.CY.AUC.df$SumEscape[k] / temp.CY.AUC.df$SumEscape[nrow(temp.CY.AUC.df)]
  # }
  
  if(exists('CY.AUC.df') && is.data.frame(get('CY.AUC.df'))){                           ##checks to see if output dataframe exists
    CY.AUC.df <- rbind(CY.AUC.df,temp.CY.AUC.df)       ##if it does, it appends new data
  }
  else {                                                                        ## if it doesn't creates new dataframe
    CY.AUC.df <- temp.CY.AUC.df
  }
  
}


## Escapement for Individual-Stream Management Objectives ------------------

IND.CY.AUC.table <- CY.AUC.df %>% 
  mutate(FishDays = number(round(FishDays,0),big.mark = ","),
         SumFishDays = number(round(SumFishDays,0),big.mark = ","),
         EscInd = number(round(EscInd,0),big.mark = ","),
         SumEscape = number(round(SumEscape,0),big.mark = ",")) %>% 
  select(-c(MMDD,OriginDate))


## Escapement for District Aggregate SEGs ----------------------------------


AGG.CY.AUC.df <- CY.AUC.df %>% 
  left_join(DIST.STOCKS,
            by = join_by(Species == SPECIES,
                         Stock == STOCK)) %>% 
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