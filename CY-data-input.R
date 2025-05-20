
library(tidyverse)
library(readxl)
library(fs)
library(scales)

rm(list = ls())

CurrentYear <- 2024                                                             ## current in-season year 
StreamLife <- 17.5                                                              ## estimated life in stream of fish
ObsEff <- 1.0                                                                   ## observer efficiency

DIST.STOCKS <- read.csv("input/DISTRICT_STOCKS.csv") %>% 
  select(-Full_Species)

# Upload current in-season data ----

## Ground Surveys ----

ground_dir <- paste0("O:/DCF/SALMON/ESCAPEMENT/",CurrentYear,"/GROUND/3_FINAL")
ground_files <- dir_ls(ground_dir, glob = "*.xlsx")[!grepl("~",dir_ls(ground_dir, glob = "*.xlsx"))]

i=1
rm("ground.raw.df") ; for (i in 1:length(ground_files)) {
  file_path <- ground_files[i]
  file_sheet <- excel_sheets(file_path)[grepl("\\FINAL",excel_sheets(file_path))]
  survey <- read_xlsx(path = file_path, sheet = file_sheet)
  survey$Survey = file_sheet
  
  if(exists('ground.raw.df') && is.data.frame(get('ground.raw.df'))){                           ##checks to see if output dataframe exists
    ground.raw.df <- rbind(ground.raw.df,survey)       ##if it does, it appends new data
  }
  else {                                                                        ## if it doesn't creates new dataframe
    ground.raw.df <- survey
  }
  
}

length(unique(ground.raw.df$Survey))

ground.df <- ground.raw.df %>% 
  select(-c(Rowid,DeviceTime,SatelliteTime,Latitude,Longitude,Comments,Edits)) %>% 
  separate_wider_delim(Survey, delim = "_", names = c("Code","Date",NA)) %>% 
  # separate_wider_delim(Survey, delim = "_", names = c("Stock","Date",NA)) %>% 
  filter(Species %in% c("Pink","Sockeye","Chum","Chinook","Coho")) %>% 
  group_by(Code,Date,Stream_Condition,Species) %>% 
  summarise(SurveyCount = sum(Count)) %>% 
  mutate(Date = as.Date(ymd(Date)),
         JulianDay = as.POSIXlt(Date)$yday, ## converts date to Julian Date integer
         SurveyType = "Ground") %>% 
  ungroup() %>% 
  left_join(DIST.STOCKS,
            by = join_by(Species,Code)) %>% 
  select(-Code)

## Aerial Surveys ----

aerial_dir <- paste0("O:/DCF/SALMON/ESCAPEMENT/",CurrentYear,"/AERIAL/4_GIS/9_SUMMARIES/EXCEL")

aerial.df <- map_dfr(
  dir_ls(aerial_dir, glob = "*.xlsx") |> set_names(),
  \(f) read_xlsx(f),
  .id = "File_Name") %>% 
  # select(-c(Rowid,DeviceTime,SatelliteTime,Latitude,Longitude)) %>% 
  unite("Count", c(SUM_COUNT,SUM_SURVEY), na.rm = T) %>% 
  unite("Stream_Condition", c(MEAN_OVERA,MEAN_QUALI), na.rm = T) %>% 
  filter(LIVE == "Live" | is.na(LIVE)) %>% 
  filter(SECT_CODE != 1) %>% 
  separate_wider_delim(File_Name, delim = "/", names = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,"Survey")) %>% 
  separate_wider_delim(Survey, delim = "_", names = c(NA,"Date",NA,NA)) %>% 
  mutate(Species = ifelse(SPECIES == "sockeye salmon","Sockeye",SPECIES),
         Stock = ifelse(STOCK == "Aialik","Aialik Lake",STOCK),
         Count = as.integer(Count),
         Stream_Condition = as.integer(Stream_Condition)) %>% 
  group_by(Date,Stock,Species,Stream_Condition) %>% 
  summarise(SurveyCount = sum(Count)) %>% 
  mutate(Date = as.Date(ymd(Date)),
         JulianDay = as.POSIXlt(Date)$yday, ## converts date to Julian Date integer
         SurveyType = "Aerial") %>% 
  left_join(select(DIST.STOCKS,-c(Code)),
            by = join_by(Species,Stock),
            relationship = "many-to-many")


all.survey.df <- full_join(aerial.df,ground.df,
                           by = join_by(Date, Stock, District, Species, Stream_Condition, SurveyCount,
                                        JulianDay, SurveyType)) %>% 
  filter(!(Species %in% c("Chinook","Coho"))) %>% 
  mutate(Species = ifelse(Species == "Pink","Pink Salmon",Species),
         Species = ifelse(Species == "Chum","Chum Salmon",Species),
         Species = ifelse(Species == "Sockeye","Sockeye Salmon",Species)) 

escapement.df <- all.survey.df %>% 
  group_by(Species,Stock,District,SurveyType) %>%                                  ## Groups dataframe by categories
  summarize(EscapementCount = sum(SurveyCount))                                            ## Sums all survey counts across the year

## Make sure there is no object named "CY.AUC.df" in your R Studio environment before running following for-loop:
## nrow counts how many unique groups
rm(list = "CY.AUC.df"); for (i in c(1:nrow(escapement.df))){                              ## each group
  temp.CY.AUC.df <- all.survey.df %>% 
    filter(  Species == escapement.df$Species[i] &
             Stock == escapement.df$Stock[i] &
             SurveyType == escapement.df$SurveyType[i])
  
  if (temp.CY.AUC.df$SurveyCount[1] != 0) {                                                ## creates count of zero if first survey count is nonzero
    FirstRow <- head(temp.CY.AUC.df,1) %>% 
      mutate(JulianDay = JulianDay - StreamLife,
             Date = Date - StreamLife,
             Stream_Condition = NA,
             SurveyCount = 0)
    temp.CY.AUC.df <- rbind(FirstRow, temp.CY.AUC.df)
  }
  
  # if (temp.CY.AUC.df$SurveyCount[nrow(temp.CY.AUC.df)] != 0) {                                  ## creates count of zero if last survey count is nonzero
  #   LastRow <- tail(temp.CY.AUC.df,1) %>%
  #     mutate(JulianDay = JulianDay + StreamLife,
  #            Date = Date + StreamLife,
  #            Stream_Condition = NA,
  #            SurveyCount = 0)
  #   temp.CY.AUC.df <- rbind(temp.CY.AUC.df, LastRow)
  # }
  
  
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

  if(exists('CY.AUC.df') && is.data.frame(get('CY.AUC.df'))){                           ##checks to see if output dataframe exists
    CY.AUC.df <- rbind(CY.AUC.df,temp.CY.AUC.df)       ##if it does, it appends new data
  }
  else {                                                                        ## if it doesn't creates new dataframe
    CY.AUC.df <- temp.CY.AUC.df
  }
  
}


## Escapement for Individual-Stream Management Objectives ------------------

IND.CY.AUC.df <- CY.AUC.df %>% 
  mutate(Stream_Condition = as.character(Stream_Condition)) %>% 
  replace_na(list(Stream_Condition = "NA"))

IND.CY.AUC.table <- CY.AUC.df %>% 
  mutate(FishDays = number(round(FishDays,0),big.mark = ","),
         SumFishDays = number(round(SumFishDays,0),big.mark = ","),
         EscInd = number(round(EscInd,0),big.mark = ","),
         SumEscape = number(round(SumEscape,0),big.mark = ","),
         Stream_Condition = as.character(Stream_Condition)) %>% 
         replace_na(list(Stream_Condition = "NA"))

## Escapement for District Aggregate SEGs ----------------------------------

AGG.CY.AUC.df <- CY.AUC.df %>% 
  filter(!is.na(District))

rm(list = "temp.df2") ; for (i in 1:length(unique(AGG.CY.AUC.df$District))) {
  
  for (j in 1:length(unique(AGG.CY.AUC.df$Species))) {
    
    temp.df <- filter(AGG.CY.AUC.df, District == unique(AGG.CY.AUC.df$District)[i] & Species == unique(AGG.CY.AUC.df$Species)[j]) %>% 
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
  group_by(Species,District,JulianDay) %>% 
  summarise(DistSumEscape = max(DistSumEscape))

AGG.CY.AUC.surv.df <- temp.df2 %>% 
  mutate(Stream_Condition = as.character(Stream_Condition)) %>% 
  replace_na(list(Stream_Condition = "NA")) %>%
  select(Species,District,Stock,JulianDay,SurveyType,Stream_Condition,SurveyCount)

AGG.CY.AUC.table <- temp.df2 %>% 
  rename(StreamSumEscape = SumEscape) %>% 
  mutate(FishDays = number(round(FishDays,0),big.mark = ","),
         SumFishDays = number(round(SumFishDays,0),big.mark = ","),
         EscInd = number(round(EscInd,0),big.mark = ","),
         StreamSumEscape = number(round(StreamSumEscape,0),big.mark = ","),
         DistSumEscape = number(round(DistSumEscape,0),big.mark = ","),
         Stream_Condition = as.character(Stream_Condition)) %>% 
         replace_na(list(Stream_Condition = "NA")) %>% 
  select(c(Date,Species,Stock,District,SurveyType,Stream_Condition,SurveyCount,JulianDay,Days,FishDays,SumFishDays,EscInd,StreamSumEscape,DistSumEscape))


# Save updated CY data ----------------------------------------------------

save(list = c("AGG.CY.AUC.df","AGG.CY.AUC.surv.df","AGG.CY.AUC.table","IND.CY.AUC.df","IND.CY.AUC.table"), 
     file = paste0("data/Inseason_",CurrentYear,"_AUC.RData"))

