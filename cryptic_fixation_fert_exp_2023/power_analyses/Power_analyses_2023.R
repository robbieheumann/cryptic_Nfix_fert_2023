### POWER ANALYSES


library(tidyverse)
library(readr)

if(!require(pwr)){install.packages("pwr")}

final_2022_season_fluxes <- read_csv("C:/Users/rh176228/Box/GYE Nitrogen Fixation Folder/Robbie Data/Summer 2022/seasonal_cryptic_fixation_2/0_Data/final_2022_season_fluxes.csv")
## rename this df "rates" 
rates <- final_2022_season_fluxes
## remove summer BiscBas soil NW which for some reason never had dry mass recorded,
## as well as the extra flux calc at the bottom of the doc that was just from dragging an equation one cell too far in excel...
rates <- rates %>% filter(flux_nmol_C2H4_g_h != 'NA')

### cryptic rate variability by nihe by season
cnf_var <- rates %>%
  group_by(season, niche) %>%
  summarize(sd = sd(flux_nmol_C2H4_g_h),
            mean = mean(flux_nmol_C2H4_g_h),
            median = median(flux_nmol_C2H4_g_h))
cnf_var_wide <- cnf_var %>% pivot_wider(names_from = season, values_from = c(mean, median, sd))
cnf_var_wide <- cnf_var_wide %>% mutate(spring_fall_diff = mean_spring-mean_fall, spring_summer_diff = mean_spring-mean_summer)
cnf_var_wide <- cnf_var_wide %>% mutate(spring_fall_diff_perc = spring_fall_diff/mean_spring*100, spring_summer_diff_perc = spring_summer_diff/mean_spring*100)

cnf_var_effect_sizes <- cnf_var_wide %>% select(niche, spring_summer_diff_perc, mean_spring, mean_summer, sd_spring, sd_summer, sd_summer_wet)

m1 <- cnf_var_effect_sizes$mean_spring
m2 <- cnf_var_effect_sizes$mean_summer
s1 <- cnf_var_effect_sizes$sd_spring
s2 <- cnf_var_effect_sizes$sd_summer

Cohen.d = (m1 - m2)/sqrt(((s1^2) + (s2^2))/2)
library(pwr)

for (i in seq_along(Cohen.d)) {
  result <- pwr.t.test(
    n = NULL,
    d = Cohen.d[i],           
    sig.level = 0.05,
    power = 0.80,
    type = "two.sample",
    alternative = "two.sided"
  )
  
  print(result)
}
### so 440 samples for lichen, 3 for litter, 70 for moss, 11 for soil and 5 for wood

## so for summer 2023 fert plots smapling, if we have 9 plots, and we are sampling two subplots in each, thus 18 sampling sites,
## we multiply by 18 for each per site sampling count. IE 3 litter samples = 3*18 = 54 samples = 26 per group
## maybe we go 5 for heterotrophs (1 sample only blank) and 8*18 = 144 samples 72 per group for moss and lichens
5*18+8*18
## total of 234 samples

