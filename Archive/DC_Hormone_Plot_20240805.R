



# packages ----------------------------------------------------------------

library(tidyverse)




# General -----------------------------------------------------------------

#vline for breeders removal 

#vline for last in group parturition 

#vline for isolation 

#vline for pairing

#vline for parturition

#I would like to have plot on a landscape format

#I would like to add Elo score, identify when inn times there may have been a reversal in the direction of submissive calls


#Create a graph on a single group


# All data ----------------------------------------------------------------


#Group info 
#The Sampling_Cat of Tusker must be changed to slow_Post as the evictor had produced litter after giving birth
DC_Info_Group <- read.csv("DC_Info_Group_20240710.csv") %>% 
  mutate(IsolationDate = ymd(IsolationDate),
         PairingDate = ymd(PairingDate),
         EvictionDate = ymd(EvictionDate))
names(DC_Info_Group)


#Individual info 
DC_Info_F <- read.csv("DC_Info_F_20240710.csv") %>% 
  #REFORMAT DATE
  mutate(KingRemovalDate = ymd(KingRemovalDate),
         OriginalGroup_LastParturitionDate = ymd(OriginalGroup_LastParturitionDate),
         IsolationDate = ymd(IsolationDate),
         PairingDate = ymd(PairingDate),
         EvictionDate = ymd(EvictionDate)) %>% 
  #ADD SAMPLING CAT 
  left_join(., DC_Info_Group %>% 
              distinct(ExperimentalGroup,
                       Sampling_Cat)) %>% 
  relocate(Sampling_Cat) %>% 
  #ARRANGE 
  arrange(Sampling_Cat,
          ExperimentalGroup,
          EvictionStatus)
names(DC_Info_F)


#Urine info 
#check what info is there
DC_Info_Urine_F <- read.csv("DC_Info_Urine_F_20240710.csv") %>% 
  rename(SampleID = UrineNumber)
View(DC_Info_Urine_F )


#Hormone concentration 
#write csv
DC_Conc_All <-read.csv("DC_Conc_All_20240805.csv")


#Hormone Info to plot 
# I will use non-conservative value
# Remove sample that have not been analysed
Hormones_ToPlot <- DC_Info_Urine_F %>% 
  inner_join(., DC_Conc_All) %>%
  
  #ONLY RETAIN NON-CONS VALUES FOR PLOTTING
  filter(ConcType == "NC") %>% 
  
  #SELECT COLUMNS 
  select(Sampling_Cat,
         ExperimentalGroup,
         AnimalID,
         BS_Original,
         EvictionStatus,
         UrineIsol_DayDiff,
         UrinePairing_DayDiff,
         UrineEviction_DayDiff,
         UrineParturition_DayDiff,
         Hormone,
         QTcat,
         Conc)

View(Hormones_ToPlot)


View(DC_Conc_All)
  


names(Hormones_ToPlot)

View(Hormones)



# King Removal ------------------------------------------------------------

#Relative to isolation 
KingRemov_Plot <- DC_Info_F %>% 
  mutate(KingRemoval = KingRemovalDate - IsolationDate) %>% 
  distinct(ExperimentalGroup,
           KingRemoval)


#Relative to pairing 
KingRemov_Plot <- DC_Info_F %>% 
  mutate(KingRemoval = KingRemovalDate - PairingDate) %>% 
  distinct(ExperimentalGroup,
           KingRemoval)


# Pre-Isolation Parturition -----------------------------------------------


#Relative to Isolation 
PreIsolPartPlot <- DC_Info_F %>% 
  mutate(LastParturition = OriginalGroup_LastParturitionDate - IsolationDate) %>% 
  select(ExperimentalGroup,
         LastParturition)
View(PreIsolPartPlot)


#Relative to Pairing
PreIsolPartPlot <- DC_Info_F %>% 
  mutate(LastParturition = OriginalGroup_LastParturitionDate - PairingDate) %>% 
  select(ExperimentalGroup,
         LastParturition)
View(PreIsolPartPlot)


# Isolation ---------------------------------------------------------------

#Relative to Pairing
IsolationPlot <- DC_Info_Group %>% 
  #ADD PAIRING ISOLATION DATE
  mutate(Isolation = as.numeric(IsolationDate - PairingDate)) %>% 
  select(ExperimentalGroup,
         Isolation)
View(IsolationPlot)



# Pairing -----------------------------------------------------------------


#Relative to Isolation
PairingPlot <- DC_Info_Group %>% 
  #ADD PAIRING ISOLATION DATE
  mutate(Pairing = as.numeric(PairingDate - IsolationDate)) %>% 
  select(ExperimentalGroup,
         Pairing)
View(IsolationPlot)



# Eviction ----------------------------------------------------------------

#Relative to Pairing
EvictionPlot <-  DC_Info_Group %>% 
  #ADD PAIRING ISOLATION DATE
  mutate(Eviction = as.numeric(EvictionDate - IsolationDate)) %>% 
  select(ExperimentalGroup,
         Eviction)
View(EvictionPlot)


#Relative to Pairing
EvictionPlot <-  DC_Info_Group %>% 
  #ADD PAIRING ISOLATION DATE
  mutate(Eviction = as.numeric(EvictionDate - PairingDate)) %>% 
  select(ExperimentalGroup,
         Eviction)
View(EvictionPlot)




# PP parturition ----------------------------------------------------------


#Post pairing parturition
PPPart_plot <- DC_Info_F %>% 
  
  #RETAIN PARTURITION AFTER PAIRING
  filter(ParturitionDate > PairingDate) %>%
  
  #DISTINCT
  distinct( Sampling_Cat,
            Treatment,
            ExperimentalGroup,
            EvictionStatus,
            BS_Original,
           AnimalID,
           PairingDate) %>% 
  left_join(., Litter_Info, by=c("AnimalID" = "AnimalID")) %>% 
  
  #MMANUALLY CORRECT TUSKER
  mutate(Sampling_Cat = case_when(ExperimentalGroup == "Tusker" ~ "Slow_Post",
                                 TRUE ~ Sampling_Cat)) %>% 
  
  #SELECTIVE RETAIN
  filter(AnimalID != "DRF004",
         Sampling_Cat %in% c("Rapid_Conc", "Rapid_PreFirst") & min(ParturitionDate) | 
           Sampling_Cat %in% c("Slow_Conc", "Slow_PreFirst") & EvictionStatus == "Evictor" & min(ParturitionDate) | 
           Sampling_Cat %in% c("Slow_Conc", "Slow_PreFirst") & EvictionStatus == "Evictee"| 
           Sampling_Cat %in% c("Slow_PreFirst", "Slow_Conc") & EvictionStatus == "Evictor" & min(ParturitionDate) |
           Sampling_Cat == "Slow_None" |
           Sampling_Cat == "Slow_Post" | 
           Sampling_Cat == "None")
  
  
  #ARRANGE
  arrange(Treatment, 
          ExperimentalGroup,
          ParturitionDate)
View(PPPart_plot)


#Check where I would have an equivalent in data prep
View(PPPart_plot %>% 
       group_by(ExperimentalGroup) %>% 
       mutate(BrCount = sum(n_distinct(AnimalID))) %>% 
       ungroup() %>% 
       filter(BrCount > 1))


names(Litter_Info)

View(DC_Info_F)



# Set colour --------------------------------------------------------------

#Set colour for quantification category 
Col_QTcat <- setNames(c('red', 'orange', 'green'), c("ND","D","Q"))



#Set colour for eviction status 
Col_EvStatus <- setNames(c('red', '#40E0D0', 'green'), c("Evictee","InGroup","Evictor"))


#Set point shape for eviction status
#All animals that stay in group get a round shaped point
Sh_EvStatus <- setNames(c(15,16,16), c("Evictee","InGroup","Evictor"))


#Set line type for eviction status
#All animals that stay in group get a solid line, animal that leave a dashed line  
L_EvStatus <- setNames(c("dashed", "solid", "solid"), c("Evictee","InGroup","Evictor"))



#Set colour for BS_Original
#Queens get a triangle, helper gets a point 
Col_BS <- setNames(c('red', 'green'), c("Breeder","Helper"))

#Set shape for BS_Original
#Queens get a triangle, helper gets a point 
Sh_BS <- setNames(c(17, 16), c("Breeder","Helper"))

#Set line type for BS_Original
#Helper get a joined line, breeder a two dashed line 
L_BS <- setNames(c('dashed', 'solid'), c("Breeder","Helper"))



# Til pairing -------------------------------------------------------------
#Evictor / Evictee => Colour 
#Breeder / Helper =>  Point shape 
#I want loop over Sampling_Cat, then Treatment

#We only remove the king in 10/12 groups from the breeding status 
#Some helpers also came from the same group 
#what was the breeding status of the group where breeding status were not removed? 


#I want show king removal and last parturition for all groups from the breeder treatment 


#Data set
TilPairing_ToPlot <- Hormones_ToPlot %>% 
  filter(UrinePairing_DayDiff <= 0)
names(TilPairing_ToPlot)


#King removal
#Relative to isolation
#If present, this is going to set the earliest ppoint in the graph
KingRemov_Plot <- DC_Info_F %>% 
  mutate(KingRemoval = as.numeric(KingRemovalDate - IsolationDate)) %>% 
  distinct(ExperimentalGroup,
           KingRemoval) %>% 
  filter(ExperimentalGroup == "Corona") %>% 
  pull(KingRemoval)


#Last group parturition 
#Relative to isolation 
#This is going to set the earliest point in the graph if there was no king removal
#Relative to Isolation 
PreIsolPart_Plot <- DC_Info_F %>% 
  mutate(LastParturition = OriginalGroup_LastParturitionDate - IsolationDate) %>% 
  distinct(ExperimentalGroup,
         LastParturition) %>% 
  filter(ExperimentalGroup == "Corona") %>% 
  pull(LastParturition)

View(PreIsolPart_Plot)


#Pairing




#PLOT DATA 
p <-  ggplot(TilPairing_ToPlot %>% 
               filter(ExperimentalGroup == "Corona",
                      Hormone == "T"), aes(x=UrineIsol_DayDiff, y = Conc, group = AnimalID, color = EvictionStatus, linetype = BS_Original)) +
  
  #ADD LINE
  geom_line(size = 1) +
  
  #ADD POINTS 
  geom_point(size = 3) +
  
  #SET COLOURS FOR EVICTION STATUS 
  scale_color_manual(values = Col_EvStatus) +
  
  #SET COLOURS FOR BS_oRIGINAL
  scale_linetype_manual(values = L_BS) +
  
  #MINIMAL THEME
  theme_minimal()


  #ADD VLINE KING REMOVAL
if(length(KingRemov_Plot) != 0){
    #SET NEW MIN X AXIS 
    new_min_x <- min(KingRemov_Plot) - 10  # Adjust to start before the marker
  p <- p +
    xlim(c(new_min_x, NA))+
    geom_vline(xintercept = KingRemov_Plot, colour = "red", linetype = "dashed", size = 1)}

  #ADD VLINE LAST PARTURITION 

  #ADD VLINE ISOLATION


p <- p + xlim(c(min(-100, na.rm = TRUE), 100)) + 
  geom_vline(xintercept = -50, colour = "red", linetype = "dashed", size = 1)



#ADD MATING LINE



#ADD ACTIVATION LINE
if(length(Activation) != 0){
  
  p <- p +
    geom_vline(xintercept = Activation, colour = "red")}


#ADD END LINE
if(length(End) != 0){
  p <- p +
    geom_vline(xintercept = End, colour = "blue")}
  
  








+
  
  #SET COLOURS OF POINTS
  scale_colour_manual(values = , aesthetics = "fill") +
  
  #SET SHAPES OF POINTS
  scale_shape_manual(values = Sh_Vol) +
  
  #POINT LABLELS
  geom_text(aes(label=NextParturition_ParturitionDayDiff, vjust= -0.5), size = 3) +
  #check_overlap = TRUE
  
  #Y LABELS
  ylab(label = paste({{HormoneID}},"concentration in ng/ml", sep = " ")) +
  #ylab(label = paste({{HormoneID}}, {{ConcMetric}})) + #How would I deal with this if I wrap plot in a function? args: Hormone = 
  theme(axis.title.y = element_text(size = 7),
        axis.text.y = element_text(size = 7)) +
  
  #REMOVE X LABELS
  theme(axis.title.x=element_blank()) +
  
  #REMOVE LEGENDS
  theme(legend.position = "none")















plot_individual <- function(Data = NULL, 
                            AnimalID = NULL,
                            HormoneID = NULL,
                            ConcMetric = "Original",
                            X = NULL,
                            Xmin = NULL,
                            Xmax = NULL,
                            Response = NULL,
                            Title = NULL
){
  
  #DATA
  #Does it make sense to subset all at once in the most inner loop? 
  ToPlot <- Data_ToPlot %>% filter(AnimalID == {{AnimalID}},
                                   Hormone == {{HormoneID}},
                                   ConcMetric == {{ConcMetric}})
  
  #PLOT DATA 
  p <-  ggplot(ToPlot, aes(x=DatePairing_DayDiff, y = Conc)) + 
    geom_line() +
    
    #ADD POINTS 
    geom_point(size = 2.5, aes(shape = as_factor(UrineVolume), fill = QuantificationCategory)) +
    
    #SET COLOURS OF POINTS
    scale_colour_manual(values = Colour_QuantificationCategory, aesthetics = "fill") +
    
    #SET SHAPES OF POINTS
    scale_shape_manual(values = Shape_UrineVolume) +
    
    #POINT LABLELS
    geom_text(aes(label=NextParturition_ParturitionDayDiff, vjust= -0.5), size = 3) +
    #check_overlap = TRUE
    
    #Y LABELS
    ylab(label = paste({{HormoneID}},"concentration in ng/ml", sep = " ")) +
    #ylab(label = paste({{HormoneID}}, {{ConcMetric}})) + #How would I deal with this if I wrap plot in a function? args: Hormone = 
    theme(axis.title.y = element_text(size = 7),
          axis.text.y = element_text(size = 7)) +
    
    #REMOVE X LABELS
    theme(axis.title.x=element_blank()) +
    
    #REMOVE LEGENDS
    theme(legend.position = "none")
  
  #X AXIS LIMIT
  if(is.null(Xmin) == FALSE | is.null(Xmax) == FALSE ){
    scale_x_continuous(limits = c(Xmin,Xmax), breaks = seq(Xmin,Xmax, by = 3))
  }
  
  #Y AXIS LIMIT 
  if({{HormoneID}} == "P4" & ConcMetric == "Original"){
    p <- p + 
      scale_y_continuous(limits = ~ c(0, max(.x) + 0.5))
  } else if ({{HormoneID}} != "P4" & ConcMetric == "Original"){
    p <- p + 
      scale_y_continuous(limits = ~ c(0, ceiling(max(.x)) + 5))
  } 
  
  
  #ADD MATING LINE
  if(length(Mating) != 0){
    p <- p +
      geom_vline(xintercept = Mating, colour = "black", linetype = "dashed")}
  
  
  #ADD ACTIVATION LINE
  if(length(Activation) != 0){
    p <- p +
      geom_vline(xintercept = Activation, colour = "red")}
  
  
  #ADD END LINE
  if(length(End) != 0){
    p <- p +
      geom_vline(xintercept = End, colour = "blue")}
  
  
  #ADD TITLE
  if(Title == "Yes"){
    p <- p +  ggtitle(paste({{AnimalID}})) + 
      theme(plot.title = element_text(hjust = 0.5))}
  
  return(p)
  
}


```



#For vertical lines showing successful mating on figures 






```{r Activation info}

#For vertical lines showing reproductive activation (first ovulation) on figures

#for plotting we only going to show activation criteria 1 for E2 
E2_Activation_Info <- RA %>% 
  #RETAIN ONLY CRITERIA E2_1
  filter(Criteria == "E2_1") %>% 
  #RETAIN RA WITHIN 60 DAYS 
  filter(ActivationDay <=60)

```


```{r Dataset}

#Data 
Data_ToPlot <- IB_Hormones_AnalysesDataset %>%
  
  #RETAIN E2 AND P4 []
  filter(Hormone %in% c("E2","P4"),
         SamplingPeriod_Months == "2M") %>% 
  
  #RENAME TREATMENT 
  mutate(Treatment = case_when(Treatment == "UFR" ~ "Unfamiliar Kin",
                               Treatment == "UFUR" ~ "Unfamiliar Non-Kin",
                               Treatment == "FR" ~ "Familiar Kin",
                               Treatment == "FUR" ~ "Familiar Kin")) %>% 
  
  #ADD REPRODUCTIVE ACTIVATION DAY
  left_join(., RA %>% 
              filter(Criteria == "E2_1") %>% 
              select(AnimalID,
                     ActivationDay)) %>%
  
  #ADD PLOT CAT 
  #to sort graph 
  mutate(PlotCat = case_when(PlotCat == "FamLow" ~ 1,
                             PlotCat == "FamMedium" ~ 2,
                             PlotCat == "FamHigh" ~ 3)) %>% 
  
  
  #RENAME HORMONES
  mutate(HormoneFullName = case_when(Hormone  == "E2" ~ "Estradiol",
                                     Hormone  == "P4" ~ "Progesterone")) %>% 
  
  #ARRANGE 
  arrange(Treatment,
          PlotCat,
          ActivationDay,
          Hormone,
          AnimalID)


#Vector of individuals to loop over
Animal_ToPlot <- Data_ToPlot %>% 
  distinct(AnimalID) %>% 
  pull() 

#Vector of hormones to loop over
Hormones_ToPlot <- Data_ToPlot %>% 
  distinct(Hormone) %>% 
  pull()

```






