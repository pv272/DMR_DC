

#These are functions used for the DC experiment that have been modified in comparison to DMRfunctions_20220201 in Ratop

#Closest weight 
#I changed the inner join for a left join 
add_closestweight<-function(Data, animalid = AnimalID, date = Date, Weight = tblWeights){
  Data %>% 
    rename(AnimalID = {{animalid}}) %>% #in case of  a different variable name in data
    left_join(.,Weight) %>%
    mutate(DayDiff = abs(round(difftime(WeightDate,{{date}},units="days")))) %>%
    select(-WeightType, -WeightDate) %>% 
    # GROUP BY ANIMALID, DATE
    group_by(AnimalID,{{date}}) %>% 
    filter(DayDiff == min(DayDiff)) %>%
    ungroup() %>%
    rename(ClosestWeight = Weight) %>% 
    #UNGROUP
    #GROUP BY ANIMALID,DATE,DAYDIFF
    group_by(AnimalID,{{date}},DayDiff) %>% 
    mutate(ClosestWeight=mean(ClosestWeight)) %>% 
    distinct() %>% 
    ungroup() %>%
    #UNGROUP
    rename(ClosestWeightDayDiff=DayDiff) %>% 
    relocate(ClosestWeightDayDiff,.before=ClosestWeight)
}


#closest weight after 
#Was already a left join 
add_closestweight_before<-function(Data, animalid = AnimalID, date = Date, Weight = tblWeights){
  Data %>% 
    rename(AnimalID = {{animalid}}) %>% #in case of  a different variable name in data
    left_join(.,Weight, by=c("AnimalID" = "AnimalID")) %>%
    mutate(DayDiff = round(difftime(WeightDate,{{date}},units="days"))) %>%
    select(-WeightType, -WeightDate) %>% 
    #RETAIN WEIGHT BEFORE (if there is no weight the row will disappear)
    filter(DayDiff <= 0) %>% 
    #GROUP BY ANIMALID, DATE
    group_by(AnimalID,{{date}}) %>% 
    filter(DayDiff == max(DayDiff)) %>%
    rename(ClosestWeight_Before = Weight) %>%
    ungroup() %>%
    #UNGROUP
    #GROUP BY ANIMALID,DATE,DAYDIFF
    group_by(AnimalID,{{date}},DayDiff) %>% 
    mutate(ClosestWeight_Before = mean(ClosestWeight_Before)) %>% 
    distinct() %>% 
    ungroup() %>%
    #UNGROUP
    #RENAME
    rename(ClosestWeightDayDiff_Before = DayDiff) %>% 
    #REOCATE
    relocate(ClosestWeightDayDiff_Before,.before=ClosestWeight_Before)
}


#closest weight after 
#Was already a left join 
add_closestweight_after<-function(Data, animalid = AnimalID, date = Date, Weight = tblWeights){
  Data %>% 
    rename(AnimalID = {{animalid}}) %>% #in case of  a different variable name in data
    left_join(.,Weight, by=c("AnimalID" = "AnimalID")) %>%
    mutate(DayDiff = round(difftime(WeightDate,{{date}},units="days"))) %>%
    select(-WeightType, -WeightDate) %>% 
    #RETAIN WEIGHT BEFORE (if there is no weight the row will disappear)
    filter(DayDiff >= 0) %>% 
    #GROUP BY ANIMALID, DATE
    group_by(AnimalID,{{date}}) %>% 
    filter(DayDiff == min(DayDiff)) %>%
    rename(ClosestWeight_After = Weight) %>%
    ungroup() %>%
    #UNGROUP
    #GROUP BY ANIMALID,DATE,DAYDIFF
    group_by(AnimalID,{{date}},DayDiff) %>% 
    mutate(ClosestWeight_After = mean(ClosestWeight_After)) %>% 
    distinct() %>% 
    ungroup() %>%
    #UNGROUP
    #RENAME
    rename(ClosestWeightDayDiff_After = DayDiff) %>% 
    #REOCATE
    relocate(ClosestWeightDayDiff_After,.before=ClosestWeight_After)
}

