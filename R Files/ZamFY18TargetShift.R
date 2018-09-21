pacman::p_load(dplyr,readr,tidyverse)

# FILE PATHS
dir.create("Data") #create a folder to store Tableau Fle
datapath<-"Data"

# NAME TABLEAU DATASET
datafile<-"FY18Q3.Zambia.Site.2018.08.30.txt"


#read in Tableau Dataset
df<-read_tsv(file.path(datapath,datafile))



##CREATE subset of the SAFE and EQUIP targets from working file as a different DF, change mech & partner info to ZPCTII, then re-append, then halve
targetSafe_CBNW <- df %>% 
  filter(ResultsOrTargets=="Targets" & period == "10/1/2017",
         SNU %in% c("Copperbelt Province", "NorthWestern Province"),
         `Implementing Mechanism Name`%in% c("SAFE"))

targetZPCT_CBNW <- df %>% 
  filter(ResultsOrTargets=="Targets" & period == "10/1/2017",
         SNU %in% c("Copperbelt Province", "NorthWestern Province"),
         `Implementing Mechanism Name`%in% c("SAFE")) %>% 
  mutate(`Implementing Mechanism Name`= "The Zambia Prevention, Care and Treatment Partnership II (ZPCT II)",
         `Prime Partner` = "FHI 360",
         `Mechanism ID` = "10203")


targetSEQUIP_Lua <- df %>% 
  filter(ResultsOrTargets=="Targets" & period == "10/1/2017",
         SNU %in% c("Luapula Province"),
         `Implementing Mechanism Name`%in% c("EQUIP"))

targetZPCT_Lua <-df %>% 
  filter(ResultsOrTargets=="Targets" & period == "10/1/2017",
         SNU %in% c("Luapula Province"),
         `Implementing Mechanism Name`%in% c("EQUIP")) %>% 
  mutate(`Implementing Mechanism Name`= "The Zambia Prevention, Care and Treatment Partnership II (ZPCT II)",
         `Prime Partner` = "FHI 360",
         `Mechanism ID` = "10203")


FY18targetnew <-bind_rows(targetSafe_CBNW,targetSEQUIP_Lua,targetZPCT_CBNW,targetZPCT_Lua)



#halve indicators
FY18targetnew <-FY18targetnew %>% 
  mutate(values = as.numeric(values/2))


#remove snapshot or annual indicator targets ZPCT-II
FY18targetnew <- FY18targetnew %>%
  filter(!indicator %in% c("TX_CURR", "TX_RET", "TX_PVLS") | `Mechanism ID` != "10203")


#create df for Safe and EQUIP snapshot or annual indicaotrs, and set them to normal
SAFE_EQUIPotherTarget <- FY18targetnew %>% 
  filter(indicator %in% c("TX_CURR", "TX_RET", "TX_PVLS"))

SAFE_EQUIPotherTarget <-SAFE_EQUIPotherTarget %>% 
  mutate(values = as.numeric(values*2))


#remove Safe and Equip snapshot or annual from the halved dataset
FY18targetnew <-FY18targetnew %>% 
  filter(!indicator %in% c("TX_CURR", "TX_RET", "TX_PVLS"))



########filter df to remove EQUIP & SAFE FY18 targets ###########
df_new <-df %>%
  filter(ResultsOrTargets!="Targets" | period != "10/1/2017" |
                  SNU !="Copperbelt Province" |  `Implementing Mechanism Name` !="SAFE")

df_new2 <-df_new %>%
  filter(ResultsOrTargets!="Targets" | period != "10/1/2017" |
           SNU !="NorthWestern Province" |  `Implementing Mechanism Name` !="SAFE")


df_new3 <-df_new2 %>%
  filter(ResultsOrTargets!="Targets" | period != "10/1/2017" |
           SNU !="Luapula Province" |  `Implementing Mechanism Name` !="EQUIP")


###append new targets and df
final <-bind_rows(df_new3,FY18targetnew,SAFE_EQUIPotherTarget)


#note - must change properties of values when in tableau from whole numbers to decimals in order to display properly#
write_tsv(final, file.path(datapath,paste0("FY18.ZAMsite.IM.final", Sys.Date(),".txt")))
         
  
         
  
