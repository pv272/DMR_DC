



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
DC_Info_Urine_F <- read.csv("DC_Info_Urine_F_20240816.csv") %>% 
  rename(SampleID = UrineNumber)
View(DC_Info_Urine_F )


#Hormone concentration 
#write csv
DC_Conc_All <-read.csv("DC_Conc_All_20240814.csv") %>% 
  mutate(Conc = as.numeric(Conc))

names(DC_Info_Urine_F)
names(DC_Conc_All)
#Hormone Info to plot 
# I will use non-conservative value
# Remove sample that have not been analysed
Data_ToPlot <- DC_Info_Urine_F %>% 
  inner_join(., DC_Conc_All, by = c("UrineNumber" = "SampleID")) %>%
  
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
names(Data_ToPlot)



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

str(PPPart_plot)

names(Litter_Info)
#Post pairing parturition
PPPart <- DC_Info_F %>% 
  
  #DISTINCT
  distinct(Sampling_Cat,
            Treatment,
            ExperimentalGroup,
            EvictionStatus,
            BS_Original,
           AnimalID,
           PairingDate) %>% 
  left_join(., Litter_Info) %>% 
  
  #RETAIN PARTURITION AFTER PAIRING
  filter(ParturitionDate > PairingDate) %>%
  
  #MMANUALLY CORRECT TUSKER
  mutate(Sampling_Cat = case_when(ExperimentalGroup == "Tusker" ~ "Slow_Post",
                                 TRUE ~ Sampling_Cat)) %>% 
  
  #SELECTIVE RETAIN
  filter(AnimalID != "DRF004")


#For some categories, I will only retain the first parturition date 
PPPart_First <- PPPart %>%
  #RETAIN CATEGORIES FOR WHICH ONE WANT TO KEEP THE FIRST PPPart
  filter(Sampling_Cat %in% c("Rapid_Conc", "Rapid_PreFirst")| 
           Sampling_Cat %in% c("Slow_Conc", "Slow_PreFirst") & EvictionStatus == "Evictor") %>% 
  slice_min(ParturitionDate)


#For Other, I will retain all parturitions
PPPart_All <- PPPart %>% 
  filter(Sampling_Cat %in% c("Slow_Conc", "Slow_PreFirst") & EvictionStatus == "Evictee"| 

           Sampling_Cat == "Slow_None" |
           Sampling_Cat == "Slow_Post" | 
           Sampling_Cat == "None")


#Bind all rows
PPPart_ToPlot <- bind_rows(PPPart_First,
                           PPPart_All) %>% 
  #ARRANGE
  arrange(Treatment, 
          ExperimentalGroup,
          ParturitionDate)
View(PPPart_plot)

#Which group have had more than 2 breeders 
#Check where I would have an equivalent in data prep
View(PPPart_plot %>% 
       group_by(ExperimentalGroup) %>% 
       mutate(BrCount = sum(n_distinct(AnimalID))) %>% 
       ungroup() %>% 
       filter(BrCount > 1))


# Set colour --------------------------------------------------------------

#Set colour for quantification category 
Col_QTcat <- setNames(c('red', 'orange', 'green'), c("ND","D","Q"))



#Set colour for eviction status 
Col_EvStatus <- setNames(c('#696969', 'black', 'black'), c("Evictee","InGroup","Evictor"))


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
#Breeder / Helper =>  LineType

#I want add sampling cat as colour of point or shape of points? 


#I want loop over Sampling_Cat, then Treatment


#Show all king removal 
#Show all latest parturition in original group that occurred within 100 days of isolation or wheer the king was removed (except amobo that was used for Corona from queen treatment and gave birth 600 days before isolation)



#Data set
TilPairing_ToPlot <- Data_ToPlot %>% 
  filter(UrinePairing_DayDiff <= 0)
names(TilPairing_ToPlot)

levels(TilPairing_ToPlot$Sampling_Cat)


#King removal
#Relative to isolation
#If present, this is going to set the earliest ppoint in the graph
KingRemov_Plot <- DC_Info_F %>% 
  mutate(KingRemoval = as.numeric(KingRemovalDate - IsolationDate)) %>% 
  distinct(ExperimentalGroup,
           KingRemoval) %>% 
  filter(ExperimentalGroup == "Kasteel") %>% 
  pull(KingRemoval)


#Last group parturition 
#Relative to isolation 
#This is going to set the earliest point in the graph if there was no king removal
#Relative to Isolation 
LastPart_Plot <- DC_Info_F %>% 
  mutate(LastParturition = as.numeric(LastOrGroupPartIsol_DayDiff)) %>% 
  #RETAIN KING REMOVAL DATE OR WITHIN 100 days
  filter(!is.na(KingRemovalDate) | LastParturition > -100) %>%
  #DISTINCT
  distinct(ExperimentalGroup,
         LastParturition) %>% 
  filter(ExperimentalGroup == "Kasteel") %>% 
  pull(LastParturition)


#Pairing
#Relative to isolation
Pairing_Plot <- DC_Info_Group %>% 
  #ADD PAIRING ISOLATION DATE
  mutate(Pairing = as.numeric(PairingDate - IsolationDate)) %>% 
  select(ExperimentalGroup,
         Pairing) %>% 
  filter(ExperimentalGroup == "Kasteel") %>% 
  pull(Pairing)
View(Pairing_Plot)


p <- ggplot(TilPairing_ToPlot %>% 
              filter(ExperimentalGroup == "Kasteel", Hormone == "E2"), 
            aes(x = UrineIsol_DayDiff, y = Conc, group = AnimalID, 
                color = EvictionStatus, linetype = BS_Original, 
                fill = QTcat)) +
  
  # ADD LINE
  geom_line(size = 1) +
  
  # ADD POINTS with shape manually set for each AnimalID
  geom_point(size = 3, aes(shape = AnimalID, fill = QTcat)) +
  
  # SET COLORS FOR EVICTION STATUS (LINE COLORS)
  scale_color_manual(values = Col_EvStatus) +
  
  # SET LINE TYPE FOR BS_ORIGINAL
  scale_linetype_manual(values = L_BS) +
  
  # SET COLORS FOR QTcat (POINT FILL COLORS)
  scale_fill_manual(values = Col_QTcat) +
  
  # MANUALLY SET SHAPES FOR EACH AnimalID
  scale_shape_manual(values = c(21, 24)) +  # Assign shapes 21 and 24 to the two animals
  
  # CUSTOM LEGEND TITLES
  labs(shape = "AnimalID", fill = "QTcat", color = "Eviction Status") +
  
  # MINIMAL THEME
  theme_minimal()

# Print the plot
print(p)

#ADD VLINE KING REMOVAL
#RED
if(length(KingRemov_Plot) != 0){
    #SET NEW MIN X AXIS 
    new_min_x <- min(KingRemov_Plot) - 2  # Adjust to start before the marker
  p <- p +
    xlim(c(new_min_x, NA))+
    geom_vline(xintercept = KingRemov_Plot, colour = "red", linetype = "dashed", size = 1)}


#ADD VLINE LAST PARTURITION
#BLUE
#The selection of parturition to be shown as been made in the filter
if(length(LastPart_Plot) != 0){
  #SET NEW MIN X AXIS 
  new_min_x <- min(new_min_x, min(LastPart_Plot) - 2)  # Adjust to start before the marker
  p <- p +
    xlim(c(new_min_x, NA))+
    geom_vline(xintercept = LastPart_Plot, colour = "blue", linetype = "dashed", size = 1)}


  #ADD VLINE PAIRING
if(length(Pairing_Plot) != 0){
  p <- p +
    geom_vline(xintercept = Pairing_Plot, colour = "Brown", linetype = "dashed", size = 1)}







# Til pairing loop --------------------------------------------------------


#Vector of hormones to loop over
Hormones_ToPlot <- Hormones_ToPlot %>% 
  distinct(Hormone) %>% 
  pull()

names(Hormones_ToPlot)


#Vector of Experimental group to loop over




for(j in (1:length(Hormones_ToPlot))) {









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












