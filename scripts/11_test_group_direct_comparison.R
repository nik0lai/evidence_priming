# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, ggplot2, brms, furrr, tibble)  
library(tidybayes)

source('scripts/functions.R')

# Data --------------------------------------------------------------------

# prime
prime_dat <- read_csv('data/processed/train_prime.csv')
# mask
mask_dat <- read_csv('data/processed/train_mask_recoded.csv')

# Prepare data for logistic regression ------------------------------------

# combine data
dat <-
  bind_rows(
    # prime data
    prime_dat %>%
      select(participant, session, soa, accuracy) %>% 
      mutate(task = 'prime'),
    # mask data
    mask_dat %>% 
      select(participant, session, soa, rt_recoded) %>% 
      rename(accuracy = rt_recoded) %>% 
      mutate(task = 'mask')
  ) %>% 
  mutate(accuracy = as.integer(accuracy)) %>% 
  mutate(task = factor(task, levels = c('prime', 'mask')))

# nest data
nested_data <-
  dat %>% 
  group_by(soa) %>% 
  nest() %>% 
  mutate(data = map(data, ~.x %>% mutate(soa = soa)))

# Logistic regression -----------------------------------------------------

# Compile model
m <- brm(
  accuracy ~ task + (1|participant) + (1|session),
  data = nested_data$data[[1]],    # use one of the dataset for compilation
  family = bernoulli(),            # Normal response distribution
  chains = 0,
  prior = c(
    prior(normal(0, 5), class = 'Intercept'),
    prior(normal(0, 5), class = 'b')
  ),
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  file = 'model_fits/direct_comparison_train_group/precompiled_direct_group'
)

sample_model <- function(df) {
  
  filename <- sprintf('model_fits/direct_comparison_train_group/direct_group_%s', unique(df$soa))
  # alternative model
  model_alt = update(object = m, newdata=df, recompile=FALSE, chains=4, iter = 10000, future=TRUE, file=filename)
  
  return(model_alt)
  
}

# Run model ---------------------------------------------------------------

plan(multisession, workers=10)

# Fit model ---------------------------------------------------------------

# run models on new sessions
nested_data <-
  nested_data %>%
  ungroup() %>% 
  mutate(bf_alt = future_map(data, ~sample_model(.x), .progress=TRUE))

# function to get bf10 while checking for extremely small values
get_bf_alt <- function(f) {
  # get hyp object
  hyp_0 <- hypothesis(f, 'taskmask=0')
  # get bf
  bf_null <- hyp_0$hypothesis$Evid.Ratio
  # get alternative bf
  bf_alt <- exp(-log(abs(bf_null)))
  
  return(bf_alt)
}

# get bf
est_data <-
  nested_data %>% 
  mutate(bf = unlist(map(bf_alt, get_bf_alt))) 

# Get effect size ---------------------------------------------------------

est_data <- est_data %>% 
  mutate(cohensd = unlist(map(bf_alt, ~round(mean(as_draws_df(.x)$b_taskmask/1.81), 1))))

# Check r-hat values ------------------------------------------------------

# If rhat column is TRUE it means all r-hat values in the model are lower than 1.01
est_data %>% 
  mutate(rhat = unlist(map(bf_alt, check_all_rhat)))

# Plot posteriors ---------------------------------------------------------

for (r in 1:nrow(est_data)) {
  plot_name <- sprintf('model_traceplots/direct_comparison_train_group/mask_%s.png', est_data[[r, 'soa']])
  p <- plot(est_data[[r, 'bf_alt']][[1]])
  ggsave(plot_name, plot = p[[1]], width = 6, height = 6)
}

# Save data ---------------------------------------------------------------

est_data %>% 
  select(-c(data, bf_alt)) %>% 
  write_csv('results/train_direct_comparison_group_bf.csv')
