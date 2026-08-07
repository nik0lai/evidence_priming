# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, ggplot2, patchwork, brms, furrr, dplyr, tidyr, tibble, clipr)  

plan(multisession, workers=10)
source('scripts/functions.R')

# Data --------------------------------------------------------------------

dat_train <- 
  read_csv('data/raw_train.csv')

# Format data -------------------------------------------------------------

# After training sessions (session 2 onwards)
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

# Trim RTs ----------------------------------------------------------------

# percentages to trim
sd2 <- 2
sd2.5 <- 0.5

# Training data

# trim edges 2 SD
train_mask_data_sd2 <-
  train_mask_data %>%
  group_by(participant, congruent) %>% 
  filter(between(rt, quantile(rt, sd2/100), quantile(rt, 1-sd2/100)))

# trim edges 2.5 SD
train_mask_data_sd2.5 <-
  train_mask_data %>%
  group_by(participant, congruent) %>% 
  filter(between(rt, quantile(rt, sd2.5/100), quantile(rt, 1-sd2.5/100)))

# Count trials
p1_training <- 
  train_mask_data_sd2 %>% 
  group_by(participant, soa) %>% 
  count() %>% 
  ggplot(aes(x=factor(soa), y=n, color=participant)) +
  geom_point() + geom_line(aes(group=participant)) +
  ggtitle('trial count filter at 2 SD')

p2_training <- 
  train_mask_data_sd2.5 %>% 
  group_by(participant, soa) %>% 
  count() %>% 
  ggplot(aes(x=factor(soa), y=n, color=participant)) +
  geom_point() + geom_line(aes(group=participant)) +
  ggtitle('trial count filter at 2.5 SD')

p1_training + p2_training + plot_layout(guides = 'collect') & geom_hline(yintercept = 300, linetype = 2)

# Direct comparison -------------------------------------------------------

# Get prime data
prime_dat <- read_csv('data/processed/train_prime.csv')

# select columns
prime_dat <- prime_dat %>% 
  select(participant, session, soa, accuracy) %>% 
  mutate(task = 'prime')

# Recode RT data
mask_data <- 
  bind_rows(
    train_mask_data_sd2 %>% mutate(trim = '2sd'),
    train_mask_data_sd2.5 %>% mutate(trim = '2.5sd') 
  ) %>% 
  group_by(participant, session, trim) %>% 
  mutate(session_median_rt = median(rt)) %>% 
  ungroup() %>% 
  mutate(rt_recoded = case_when(congruent & rt < session_median_rt ~ TRUE,
                                !congruent & rt > session_median_rt ~ TRUE,
                                TRUE ~ FALSE)) %>% 
  select(participant, session, trim, soa, rt_recoded) %>% 
  rename(accuracy = rt_recoded) %>% 
  mutate(task = 'mask')

# Combine data
dir_comp_data <- 
  bind_rows(
    prime_dat,
    mask_data
  ) %>% 
  mutate(task = factor(task, levels = c('prime', 'mask')))

# Split data
dir_comp_data_2sd <- dir_comp_data %>% 
  filter(trim == '2sd' | is.na(trim)) %>% 
  mutate(trim = '2sd')
dir_comp_data_2.5sd <- dir_comp_data %>% 
  filter(trim == '2.5sd' | is.na(trim)) %>% 
  mutate(trim = '2.5sd')

# nest data
dir_comp_data_2sd <-
  dir_comp_data_2sd %>% 
  group_by(soa, trim) %>% 
  nest() %>% 
  mutate(data = map(data, ~.x %>% mutate(soa = soa, trim = trim)))
dir_comp_data_2.5sd <-
  dir_comp_data_2.5sd %>% 
  group_by(soa, trim) %>% 
  nest() %>% 
  mutate(data = map(data, ~.x %>% mutate(soa = soa, trim = trim)))


# Fit models --------------------------------------------------------------

model_priors <- c(
  prior(normal(0, 5), class = 'Intercept'),
  prior(normal(0, 5), class = 'b'),
  prior(student_t(3, 0, 2.5), class = sd)
)

# Compile model
m <- brm(
  accuracy ~ task + session + (1 | participant),
  data = dir_comp_data_2sd$data[[1]],    # use one of the dataset for compilation
  family = bernoulli(),            # Normal response distribution
  chains = 0,
  prior = model_priors,
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  file = 'model_fits/direct_comparison_train_group/precompiled_direct_group'
)

sample_model <- function(df) {
  
  filename <- sprintf('model_fits/trimming_test/fit_direct_group_%s_%s', unique(df$soa), unique(df$trim))
  file_rds <- paste0(filename, '.rds')

  if (file.exists(file_rds)) {
    model_alt <- readRDS(file_rds)
  } else {
    model_alt <- update(
      object = m,
      newdata = df,
      prior = model_priors,
      recompile = FALSE,
      chains = 4,
      iter = 5000,
      cores = 4,
      file = filename
    )
  }
  
  return(model_alt)
  
}

# run models on new sessions
dir_comp_data_2sd <-
  dir_comp_data_2sd %>%
  ungroup() %>% 
  mutate(bf_alt = future_map(data, ~sample_model(.x), .progress=TRUE))
dir_comp_data_2.5sd <-
  dir_comp_data_2.5sd %>%
  ungroup() %>% 
  mutate(bf_alt = future_map(data, ~sample_model(.x), .progress=TRUE))

# Combine -----------------------------------------------------------------

# Combine all model fits
dir_comp_group_fits <- 
  bind_rows(
    dir_comp_data_2sd,
    dir_comp_data_2.5sd
  ) 

# get bf
dir_comp_group_fits <-
  dir_comp_group_fits %>% 
  mutate(bf = unlist(map(bf_alt, ~get_bf_alt(.x, 'taskmask'))))

# Get effect size ---------------------------------------------------------

dir_comp_group_fits <- 
  dir_comp_group_fits %>% 
  mutate(cohensd = unlist(map(bf_alt, ~round(mean(as_draws_df(.x)$b_taskmask/1.81), 1))))

# Check r-hat values ------------------------------------------------------

# If rhat column is TRUE it means all r-hat values in the model are lower than 1.01
dir_comp_group_fits %>% 
  mutate(rhat = unlist(map(bf_alt, check_all_rhat))) %>% 
  print(n=85)

# Save data ---------------------------------------------------------------

dir_comp_group_fits %>% 
  select(-c(data, bf_alt)) %>% 
  arrange(trim, soa) %>% 
  write_csv('results/train_direct_comparison_group_trim_bf.csv')

# Compare BF and effect size ----------------------------------------------

# read BF and cohens' d 
train_direct_comp_group_bf_d <- 
  read_csv('results/train_direct_comparison_group_bf.csv') 

dir_comp_group_fits %>% 
  select(-c(cohensd, data, bf_alt)) %>%
  bind_rows(train_direct_comp_group_bf_d %>% select(-cohensd) %>% mutate(trim = '2.3sd')) %>% 
  mutate(bf = log(bf)) %>% 
  pivot_wider(names_from = trim, values_from = bf)

dir_comp_group_fits %>% 
  select(-c(cohensd, data, bf_alt)) %>%
  bind_rows(train_direct_comp_group_bf_d %>% select(-cohensd) %>% mutate(trim = '2.3sd')) %>% 
  filter(soa != '0.0125') %>%
  mutate(bf = log(bf)) %>% 
  ggplot(aes(x=factor(soa), y=bf, color=trim, group=trim)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = c(log(10), log(.1)), linetype=2) 

dir_comp_group_fits %>%
  select(-c(bf, data, bf_alt)) %>%
  bind_rows(train_direct_comp_group_bf_d %>% select(-bf) %>% mutate(trim = '2.3sd')) %>%
  ggplot(aes(x=factor(soa), y=cohensd, color=trim, group=trim)) +
  geom_point(position = position_jitter(width = .2)) +
  geom_line() 

# Combine the dfs to make table
comp <- 
  bind_rows(
    dir_comp_group_fits %>% 
      select(-c(cohensd, data, bf_alt)) %>%
      bind_rows(train_direct_comp_group_bf_d %>% select(-cohensd) %>% mutate(trim = '2.3sd')) %>% 
      mutate(stat = 'bf') %>% 
      rename(value = bf),
    dir_comp_group_fits %>% 
      select(-c(bf, data, bf_alt)) %>%
      bind_rows(train_direct_comp_group_bf_d %>% select(-bf) %>% mutate(trim = '2.3sd')) %>% 
      mutate(stat = 'cohensd') %>% 
      rename(value = cohensd)
  )

make_trim_table_group(comp) %>% write_clip()
