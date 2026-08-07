# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, ggplot2)  

# Data --------------------------------------------------------------------

# mask
mask_dat <- read_csv('data/processed/train_mask.csv')

# Recode priming effect ---------------------------------------------------

mask_dat %>% 
  group_by(participant, congruent, soa) %>% 
  count() %>% 
  pivot_wider(names_from = congruent, values_from = n)
  

mask_dat_recoded <- 
  mask_dat %>%
  group_by(participant, session) %>% 
  mutate(session_median_rt = median(rt)) %>% 
  ungroup() %>% 
  mutate(rt_recoded = case_when(congruent & rt < session_median_rt ~ TRUE,
                                !congruent & rt > session_median_rt ~ TRUE,
                                TRUE ~ FALSE))

# save data
write_csv(mask_dat_recoded, 'data/processed/train_mask_recoded.csv')
