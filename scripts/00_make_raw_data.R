# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr)  

# Data --------------------------------------------------------------------

# Control is the first session, before training participants
dat_control <- 
  dir('../data/raw/', pattern = 'control', full.names = TRUE) %>% 
  map(read_csv) %>% 
  bind_rows()

# Train is sessions 2 onwards, after training them
dat_train <- 
  dir('../data/raw/', full.names = TRUE) %>% 
  str_subset(string = ., pattern = 'control', negate = TRUE) %>% 
  map(read_csv, 
      col_types = cols(participant = col_character(), 
                       session = col_character(), 
                       rt = col_character(),
                       accuracy = col_character())) %>% 
  bind_rows()

# Rename participant column -----------------------------------------------

dat_control <- 
  dat_control %>% 
  mutate(participant = sprintf('%03d', as.integer(participant)))

dat_train <- 
  dat_train %>% 
  mutate(participant = sprintf('%03d', as.integer(participant)),
         session = sprintf('%02d', as.integer(session)))

# Keep only experiment blocks --------------------------------------------

dat_control <- dat_control %>% filter(block_type == 'experiment', trial_type == 'decision') 
dat_train <- dat_train %>% filter(block_type == 'experiment', trial_type == 'decision') 

# Count participants and session -----------------------------------------

dat_control %>% 
  select(participant) %>% 
  distinct() %>% 
  count(participant, name = 'sessions')

dat_train %>% 
  select(participant, session) %>% 
  distinct() %>% 
  count(participant, name = 'sessions')

# Select relevant columns -------------------------------------------------

dat_control <- 
  dat_control %>% 
  select(participant, block_count, trial_type, trial_count, trial_aborted, soa, congruent, task, prime_presence, 
         prime_direction, mask_direction, stim_position, mask_answer, mask_rt, prime_answer, prime_rt, mask_accuracy, prime_accuracy)

dat_train %>% 
  select(participant, session, block_type, trial_count, trial_aborted, trial_type, soa, congruent, task, 
         prime_direction, mask_direction, stim_position, answer, rt)


# Save data ---------------------------------------------------------------

write_csv(x = dat_control, 'data/raw_control.csv')
write_csv(x = dat_train, 'data/raw_train.csv')
