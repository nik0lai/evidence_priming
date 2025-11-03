# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, ggplot2)  

# Data --------------------------------------------------------------------

dat_control <- 
  read_csv('data/raw_control.csv')

dat_train <- 
  read_csv('data/raw_train.csv')

# Count participant -------------------------------------------------------

dat_control %>% 
  select(participant) %>% 
  distinct() %>% 
  count(participant, name = 'sessions')

dat_train %>% 
  select(participant, session) %>% 
  distinct() %>% 
  count(participant, name = 'sessions')

# Control session ---------------------------------------------------------

# Mask discrimination
control_mask_data <-
  dat_control %>% 
  filter(task == 'mask') %>% 
  filter(!trial_aborted) %>% 
  filter(prime_presence == 'present') %>% 
  filter(as.logical(mask_accuracy)) %>% 
  select(participant, soa, prime_direction, mask_direction, congruent, mask_answer, mask_rt, mask_accuracy) %>% 
  mutate(mask_accuracy = as.logical(mask_accuracy),
         mask_rt = as.double(mask_rt)) %>% 
  rename(answer = mask_answer, 
         rt = mask_rt, 
         accuracy = mask_accuracy)

# trim edges
control_mask_data <-
  control_mask_data %>%
  group_by(participant, congruent) %>% 
  filter(between(rt, quantile(rt, .01), quantile(rt, .99))) %>% 
  ungroup()

# save data
control_mask_data %>% 
  write_csv('data/processed/control_mask.csv')

# Prime detection
control_prime_detection <- 
  dat_control %>% 
  filter(task == 'mask') %>% 
  filter(!trial_aborted) %>% 
  select(participant, soa, prime_presence,prime_answer, prime_rt, prime_accuracy) %>% 
  mutate(prime_accuracy = as.logical(prime_accuracy),
         prime_rt = as.double(prime_rt)) %>% 
  rename(answer = prime_answer, 
         rt = prime_rt, 
         accuracy = prime_accuracy)

# save data
control_prime_detection %>% 
  write_csv('data/processed/control_prime_det.csv')

# Prime discrimination
control_prime_data <-
  dat_control %>% 
  filter(task == 'prime') %>% 
  select(participant, soa, prime_direction, mask_direction, congruent, prime_answer, prime_rt, prime_accuracy) %>% 
  mutate(prime_accuracy = as.logical(prime_accuracy),
         prime_rt = as.double(prime_rt)) %>% 
  rename(answer = prime_answer, 
         rt = prime_rt, 
         accuracy = prime_accuracy)

# save data
control_prime_data %>% 
  write_csv('data/processed/control_prime_disc.csv')

# Training session --------------------------------------------------------

train_mask_data <-
  dat_train %>% 
  filter(task == 'mask') %>% 
  filter(!trial_aborted)  %>% 
  filter(as.logical(accuracy)) %>% 
  select(participant, session, soa, prime_direction, mask_direction, congruent, answer, rt, accuracy) %>% 
  mutate(accuracy = as.logical(accuracy),
         rt = as.double(rt)) %>%
  rename(answer = answer, 
         rt = rt, 
         accuracy = accuracy)

# trim edges
train_mask_data <-
  train_mask_data %>%
  group_by(participant, congruent) %>% 
  filter(between(rt, quantile(rt, .01), quantile(rt, .99))) %>% 
  ungroup()

# save data
train_mask_data %>% 
  write_csv('data/processed/train_mask.csv')

# Prime
train_prime_data <-
  dat_train %>% 
  filter(task == 'prime') %>% 
  select(participant, session, soa, prime_direction, mask_direction, congruent, answer, rt, accuracy) %>% 
  mutate(accuracy = as.logical(accuracy),
         rt = as.double(rt)) %>% 
  rename(answer = answer, 
         rt = rt, 
         accuracy = accuracy)

# save data
train_prime_data %>% 
  write_csv('data/processed/train_prime.csv')
