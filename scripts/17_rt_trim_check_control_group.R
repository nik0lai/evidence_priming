# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, ggplot2, patchwork, brms, furrr, dplyr, tidyr, tibble, clipr)  

plan(multisession)
source('scripts/functions.R')

# Data --------------------------------------------------------------------

dat_control <- 
  read_csv('data/raw_control.csv')

# Format data -------------------------------------------------------------

# Control session (first session)
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

# Trim RTs ----------------------------------------------------------------

# percentages to trim
sd2 <- 2
sd2.5 <- 0.5

# Control data

# trim edges 2 SD
control_mask_data_sd2 <-
  control_mask_data %>%
  group_by(participant, congruent) %>% 
  filter(between(rt, quantile(rt, sd2/100), quantile(rt, 1-sd2/100)))

# trim edges 2.5 SD
control_mask_data_sd2.5 <-
  control_mask_data %>%
  group_by(participant, congruent) %>% 
  filter(between(rt, quantile(rt, sd2.5/100), quantile(rt, 1-sd2.5/100)))

# Count trials
p1_control <- 
  control_mask_data_sd2 %>% 
  group_by(participant, soa) %>% 
  count() %>% 
  ggplot(aes(x=factor(soa), y=n, color=participant)) +
  geom_point() + geom_line(aes(group=participant)) +
  ggtitle('trial count filter at 2 SD') +
  ylim(50, 95)

p2_control <- 
  control_mask_data_sd2.5 %>% 
  group_by(participant, soa) %>% 
  count() %>% 
  ggplot(aes(x=factor(soa), y=n, color=participant)) +
  geom_point() + geom_line(aes(group=participant)) +
  ggtitle('trial count filter at 2.5 SD') +
  ylim(50, 95)

p1_control + p2_control + plot_layout(guides = 'collect') & geom_hline(yintercept = 50, linetype = 2)

# Test control session ----------------------------------------------------

model_priors <- c(
  prior(normal(0, 5), class = Intercept),
  prior(normal(0, 5), class = b),
  prior(student_t(3, 0, 2.5), class = sd)
)

mask_data <-
  bind_rows(
    control_mask_data_sd2 %>% mutate(trim = '2sd'),
    control_mask_data_sd2.5 %>% mutate(trim = '2.5sd')
  ) %>%
  rename(x_congruent = congruent, y_rt = rt) %>%
  mutate(x_congruent = as.integer(x_congruent)) %>%
  group_by(trim, soa) %>%
  nest() %>%
  mutate(data = map(data, ~.x %>% mutate(soa = soa, trim = trim))) %>%
  arrange(trim, soa)

# alt model template used only when a cached fit is missing
comp_alt_mask <-
  brm(
    y_rt ~ x_congruent + (1 | participant),
    data = mask_data$data[[1]],
    family = student(),
    prior = model_priors,
    sample_prior = TRUE,
    save_pars = save_pars(all = TRUE),
    file = 'model_fits/control_group/precompiled_control_mask_group',
    chains = 0
  )

get_alt_fit_mask <- function(df) {
  
  filename <- sprintf('model_fits/trimming_test/fit_control_mask_group_%s_%s', unique(df$soa), unique(df$trim))
  file_rds <- paste0(filename, '.rds')

  if (file.exists(file_rds)) {
    model_alt <- readRDS(file_rds)
  } else {
    model_alt <- update(
      object = comp_alt_mask,
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

# Fit model ---------------------------------------------------------------

control_mask_fits <- 
  mask_data %>%
  ungroup() %>% 
  mutate(bf_alt = future_map(data, ~get_alt_fit_mask(.x), .progress=TRUE))


# Get BF ------------------------------------------------------------------

# get bf
control_mask_fits <-
  control_mask_fits %>% 
  mutate(bf = unlist(map(bf_alt, get_bf_alt_mask)))

# Get effect size ---------------------------------------------------------

control_mask_fits <- 
  control_mask_fits %>% 
  mutate(cohensd = -round(unlist(map(bf_alt, ~mean(as.data.frame(.x)$b_x_congruent / as.data.frame(.x)$sigma))), 2))

# Check r-hat values ------------------------------------------------------

# If rhat column is TRUE it means all r-hat values in the model are lower than 1.01
control_mask_fits %>% 
  mutate(rhat = unlist(map(bf_alt, check_all_rhat)))

# Save data ---------------------------------------------------------------

control_mask_fits %>% 
  select(-c(data, bf_alt)) %>% 
  arrange(trim, soa) %>% 
  write_csv('results/control_mask_trim_bf.csv')

# Compare BF and effect size ----------------------------------------------

# read BF and cohen's d from original analysis
control_mask_bf_d <- 
  read_csv('results/control_mask_group_bf.csv')

control_mask_fits %>% 
  select(-c(cohensd, data, bf_alt)) %>%
  bind_rows(control_mask_bf_d %>% select(-cohensd) %>% mutate(trim = '2.3sd')) %>% 
  mutate(bf = log(bf)) %>% 
  pivot_wider(names_from = trim, values_from = bf)

control_mask_fits %>% 
  select(-c(cohensd, data, bf_alt)) %>%
  bind_rows(control_mask_bf_d %>% select(-cohensd) %>% mutate(trim = '2.3sd')) %>% 
  mutate(bf = log(bf)) %>% 
  ggplot(aes(x=soa, y=bf, color=trim, group=trim)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = c(log(10), log(.1)), linetype=2)

control_mask_fits %>% 
  select(-c(bf, data, bf_alt)) %>%
  bind_rows(control_mask_bf_d %>% select(-bf) %>% mutate(trim = '2.3sd')) %>% 
  ggplot(aes(x=soa, y=cohensd, color=trim, group=trim)) +
  geom_point() +
  geom_line() 

# Combine the dfs to make table
comp <- 
  bind_rows(
    control_mask_fits %>% 
      select(-c(cohensd, data, bf_alt)) %>%
      bind_rows(control_mask_bf_d %>% select(-cohensd) %>% mutate(trim = '2.3sd')) %>% 
      mutate(stat = 'bf') %>% 
      rename(value = bf),
    control_mask_fits %>% 
      select(-c(bf, data, bf_alt)) %>%
      bind_rows(control_mask_bf_d %>% select(-bf) %>% mutate(trim = '2.3sd')) %>% 
      mutate(stat = 'cohensd') %>% 
      rename(value = cohensd)
  )

make_trim_table_group(comp)
write_clip(make_trim_table_group(comp))
