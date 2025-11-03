# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, ggplot2, brms, furrr, tibble)  

source('scripts/functions.R')

# Data --------------------------------------------------------------------

# prime
prime_data <- read_csv('data/processed/train_prime.csv')
# mask
mask_dat <- read_csv('data/processed/train_mask.csv')

# Prime task --------------------------------------------------------------

# format data
prime_data <- prime_data %>%
  select(participant, session, soa, prime_direction, answer) %>%
  rename(x_stim=prime_direction, y_behav=answer) %>%
  mutate(across(c(x_stim, y_behav), ~recode(.x, 'left'=0, 'right'=1)))

# nest data
prime_data <- prime_data %>%
  group_by(soa) %>%
  nest() %>%
  mutate(data = map(data, ~.x %>% mutate(soa = soa)))

# Compile model -----------------------------------------------------------

# alternative model
comp_prime <- brm(
  y_behav ~ x_stim + (1|participant) + (1|session),
  data = prime_data$data[[1]],
  family = 'bernoulli',
  prior = c(
    prior(normal(0, 5), class = Intercept),
    prior(normal(0, 5), class = b),
    prior(student_t(3, 0, 2.5), class = sd)
  ),
  sample_prior = TRUE, 
  save_pars = save_pars(all = TRUE), 
  file = 'model_fits/train_group/precompiled_train_prime_group',
  chains = 0
)

get_alt_fit <- function(df) {
  
  filename <- sprintf('model_fits/train_group/train_prime_group_%s', unique(df$soa))
  # alternative model
  model_alt = update(object = comp_prime, newdata=df, recompile=FALSE, chains=4, iter = 10000, future=TRUE, file=filename)
  
  return(model_alt)
  
}

# Fit model ---------------------------------------------------------------

# Run each SOA parallely, use multisession when running from R studio
plan(multisession)

# run models on new sessions
prime_data <-
  prime_data %>%
  arrange(soa) %>% 
  ungroup() %>% 
  mutate(bf_alt = future_map(data, ~get_alt_fit(.x), .progress=TRUE))

# function to get bf10. First gets the BF for the null and then inverts it
# to get BF for an effect
get_bf_alt <- function(f) {
  # get hyp object
  hyp_0 <- hypothesis(f, 'x_stim=0')
  # get bf
  bf_null <- hyp_0$hypothesis$Evid.Ratio
  # get bf alt
  bf_alt <- exp(-log(abs(bf_null)))
  
  return(bf_alt)
}

# get bf
prime_data <-
  prime_data %>% 
  mutate(bf = unlist(map(bf_alt, get_bf_alt)))

# Get effect size ---------------------------------------------------------

prime_data <- 
  prime_data %>% 
  mutate(cohensd = unlist(map(bf_alt, ~round(mean(as_draws_df(.x)$b_x_stim/1.81), 1))))

# Check r-hat values ------------------------------------------------------

# If rhat column is TRUE it means all r-hat values in the model are lower than 1.01
prime_data %>% 
  mutate(rhat = unlist(map(bf_alt, check_all_rhat)))

# Plot posteriors ---------------------------------------------------------

for (r in 1:nrow(prime_data)) {
  plot_name <- sprintf('model_traceplots/train_group/prime_%s.png', prime_data[[r, 'soa']])
  p <- plot(prime_data[[r, 'bf_alt']][[1]])
  ggsave(plot_name, plot = p[[1]], width = 7, height = 8)
}

# Save data ---------------------------------------------------------------

prime_data %>% 
  select(-c(data, bf_alt)) %>% 
  write_csv('results/train_prime_group_bf.csv')

# Mask data --------------------------------------------------------------

# Note that data is already filtered to contain only correct trials.
# The fitlering is done in the 01_filter_data script

# data
mask_data <- 
  read_csv('data/processed/train_mask.csv')

# rename columns and recode values
mask_data <- 
  mask_data %>% 
  rename(x_congruent = congruent, y_rt = rt) %>% 
  mutate(x_congruent = as.integer(x_congruent))

# nest data
mask_data <-
  mask_data %>% 
  arrange(soa) %>% 
  group_by(soa) %>% 
  nest() %>% 
  mutate(data = map(data, ~.x %>% mutate(soa = soa)))

# Compile model -----------------------------------------------------------

# alt model
comp_alt_mask <-
  brm(
    y_rt ~ x_congruent + (1|participant) + (1|session),
    data = mask_data$data[[1]], 
    family = student(), 
    prior = c(
      prior(normal(0, 5), class = Intercept),
      prior(normal(0, 5), class = b),
      prior(student_t(3, 0, 2.5), class = sd)
    ),
    sample_prior = TRUE, 
    save_pars = save_pars(all = TRUE), 
    file = 'model_fits/train_group/precompiled_train_mask_group',
    chains = 0
  )

get_alt_fit_mask <- function(df) {
  
  filename <- sprintf('model_fits/train_group/train_mask_group_%s', unique(df$soa))
  # alternative model
  model_alt = update(object = comp_alt_mask, newdata=df, recompile=FALSE, chains=4, iter = 10000, future=TRUE, file=filename)
  
  return(model_alt)
  
}

# Fit model ---------------------------------------------------------------

# run models on new sessions
mask_data <-
  mask_data %>%
  ungroup() %>% 
  mutate(bf_alt = future_map(data, ~get_alt_fit_mask(.x), .progress=TRUE))

# function to get bf10
get_bf_alt_mask <- function(f) {
  # get hyp object
  hyp_0 <- hypothesis(f, 'x_congruent=0')
  # get bf
  bf_null <- hyp_0$hypothesis$Evid.Ratio
  # get bf alt
  bf_alt <- exp(-log(abs(bf_null)))
  
  return(bf_alt)
}

# get bf
mask_data <-
  mask_data %>% 
  mutate(bf = unlist(map(bf_alt, get_bf_alt_mask)))

# Get effect size ---------------------------------------------------------

mask_data <- 
  mask_data %>% 
  mutate(cohensd = -round(unlist(map(bf_alt, ~mean(as.data.frame(.x)$b_x_congruent / as.data.frame(.x)$sigma))), 1))

# Check r-hat values ------------------------------------------------------

# If rhat column is TRUE it means all r-hat values in the model are lower than 1.01
mask_data %>% 
  mutate(rhat = unlist(map(bf_alt, check_all_rhat)))

# Plot posteriors ---------------------------------------------------------

for (r in 1:nrow(mask_data)) {
  plot_name <- sprintf('model_traceplots/train_group/mask_%s.png', mask_data[[r, 'soa']])
  p <- plot(mask_data[[r, 'bf_alt']][[1]], nvariables = 6)
  ggsave(plot_name, plot = p[[1]], width = 7, height = 8)
}

# Save data ---------------------------------------------------------------

mask_data %>% 
  select(-c(data, bf_alt)) %>%
  write_csv('results/train_mask_group_bf.csv')
