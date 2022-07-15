#Script to convert mysid lenghts and plus counts to estimates of BPUE
#load packages
library(tidyverse)
library(tibble)
library(dplyr)
library(readxl)
#read in 
biomass_macro<-read_excel("data/ZoopSynth_biomass.xlsx", 
                          sheet = "ZoopSynth Macro-zooplankton")
#Making Macro BPUE
#pull in mysid counts and lengths from EMP to get macro BPUE
Macro_lengths<-read.delim("Data/MysidAmphipodLengths.txt")
Macro_pluscounts<-read.delim("Data/MysidAmphipodPlusCounts.txt")

Macro_lf<-Macro_lengths%>% #calculate length frequencies
  dplyr::group_by(SampleDate,Station,SpeciesCode,SizeGroup,Size)%>%
  tally()

Macro_lengths_counted<-Macro_lengths%>% #calculate number counted for each taxa
  group_by(SampleDate,Station,SpeciesCode,SizeGroup)%>%
  tally()

Macro_totals<-Macro_lengths_counted%>% #calculate totals between length measurements and pluscounts
  full_join(Macro_pluscounts)
Macro_totals$Total<-(Macro_totals$n+Macro_totals$PlusCount)/Macro_totals$LabSubsample
Macro_totals<-dplyr::rename(Macro_totals,NumberMeasured=n)

#calculate adjusted frequency by taking the length frequency for each length (n) 
#divided by the number of that mysid measured in that sample, 
#multiplied by the total number of that mysid counted in the sample
Macro_lf<-Macro_lf%>%inner_join(Macro_totals)
Macro_lf$SampleDate<-as.Date(Macro_lf$SampleDate,"%m/%d/%Y") #fix dates real quick
Macro_lf$adj_freq<-(Macro_lf$n/Macro_lf$NumberMeasured)*Macro_lf$Total

#join species lookup data with macro length frequencies to link with biomass lookup table
species_lu<- read_excel("Data/MysidAmphipodSpeciesLookUp.xlsx")
Macro_lf<-Macro_lf%>%inner_join(species_lu)
Macro_lf$Taxlifestage<-paste(Macro_lf$SpeciesName,"Adult",sep=" ")

#Now join Macro_lf with biomass_macro and calculate dry weight for each size
biomass_macro<-biomass_macro%>%rename(SpeciesName=Taxname)
Macro_biomass<-Macro_lf%>%left_join(biomass_macro)
Macro_biomass$dry_weight<-(Macro_biomass$a)*(Macro_biomass$Size^Macro_biomass$b) #calculate length dry weights
Macro_biomass$Total_dw<-Macro_biomass$dry_weight*Macro_biomass$adj_freq #calculate total dry weight for that taxa in that sample based on adj length frequencies

#the above is still broken up by each taxa size
Macro_BPUE_total<-Macro_biomass%>%
  group_by(Station,SampleDate,SpeciesName)%>%
  dplyr::summarise(Total_dw=sum(Total_dw))

Macro_BPUE_total$carbon<-Macro_BPUE_total$Total_dw*0.4 #I can't remember where the 0.4 carbon conversion rate comes from

#convert to BPUE
volumes<-read_excel("Data/Mysid_Qry1 Volume.xlsx")
Macro_BPUE_total<-Macro_BPUE_total%>%inner_join(volumes)
Macro_BPUE_total$BPUE<-Macro_BPUE_total$carbon/Macro_BPUE_total$Volume
Macro_BPUE_total$BPUE<-Macro_BPUE_total$BPUE*1e3 #change milligrams to micrograms to fit with meso units

#test