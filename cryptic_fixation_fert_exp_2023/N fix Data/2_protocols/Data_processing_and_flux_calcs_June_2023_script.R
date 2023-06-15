#### fert N fix data processing and flux calcs

library(tidyverse)
library(readxl)
### import sample dry mass data
drymass <- read_excel("C:/Users/rh176228/Desktop/cryptic_fixation/cryptic_fixation_fert_exp_2023/0_data/N fix Data/0_data/N_fix_sample_masses/JuneSampleMasses_REH_6_8.xlsx")
### check sample counts from each plot, we didn't find any lichen in Berry or Glade stands therefore we should
### be missing 36 lichen samples plus 4 lichen blanks - 246 - 40 = 206 samples total
### get rid of NA rows not sure why they are included
drymass <- drymass %>% filter(moist_perc != "NA")
drymass %>% group_by(niche) %>% summarize(sample_count = length(moist_perc))

## missing one peca, which peca? 
drymass %>% filter(niche %in% c("peca", "peca (actually clacar)"))

## that's right we couldn't find a peca blank in our 50 kg treatment at Huck 2
## what do these data look like? 
summary(drymass)
### moist per looks good no extreme outliers, wood samples have really high moist percents as expected

## import flux data
library(readr)
fluxes <- read_csv("C:/Users/rh176228/Desktop/cryptic_fixation/cryptic_fixation_fert_exp_2023/N fix Data/0_data/N_fix_GC_data/fert_Nfix_GC_raw_6_14.csv")

## remove standards
fluxes <- fluxes %>% filter(plot!= "standard")
## change known column to ppm
fluxes <-  rename(fluxes, ppm = known )

## 209 samples = 205 + 4 bag blanks, nice!
## filter out bag blanks
bag_blanks <- fluxes %>% filter(plot %in% c( "Bag A", "Bag B"))

## filter out sample blanks
niche_blanks <- fluxes %>% filter(plot_no == "Blank")
## 25 blanks makes sense cause we didnt collect peca blanks from Berry and Glade which would be 4, 
## plus the missing peca blank from huck is 5

## calculate average blank by niche and bag blank averages
niche_blank_sum <- niche_blanks %>% group_by(niche,  treatment) %>%
  summarize(mean_blank_ppm = mean(ppm))
## add peca 50 blank row
niche_blank_sum <- niche_blank_sum %>% ungroup() %>%
  add_row(niche = "peca", treatment="50", mean_blank_ppm = 4.312252)

bag_blank_avg <- bag_blanks %>% group_by(plot) %>% 
  summarize(mean_ppm = mean(ppm))

## make df of just fluxes, then add bag blank column, then merge with niche blank sums
fluxes <- fluxes %>% filter(plot_no != "Blank") %>% filter(treatment == "C"|50)

## huck sample sused bag A, the rest used Bag B
fluxes <- fluxes %>% mutate(bag_blank = (ifelse(plot == "Huck", 4.461, 4.566)))
fluxes <- left_join(fluxes, niche_blank_sum, by = c("niche", "treatment"))

fluxes <- fluxes %>% mutate(blank_corrected_ppm = ppm - mean_blank_ppm+bag_blank)
## change negatives to zeroes: 
fluxes$blank_corrected_ppm[fluxes$blank_corrected_ppm < 0] <- 0

