# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, ggplot2, brms, furrr, tibble, psycho, tidybayes)  

source('scripts/functions.R')

# Data --------------------------------------------------------------------

# prime
prime_data <- read_csv('data/processed/train_prime.csv')
prime_data_first <- read_csv('data/processed/control_prime_disc.csv')

# Prime task --------------------------------------------------------------

# format data
prime_data <- prime_data %>%
  select(participant, session, soa, prime_direction, answer) %>%
  rename(x_stim=prime_direction, y_behav=answer) %>%
  mutate(across(c(x_stim, y_behav), ~recode(.x, 'left'=0, 'right'=1)))

prime_data_first <- 
  prime_data_first %>%
  select(participant, soa, prime_direction, answer) %>%
  rename(x_stim=prime_direction, y_behav=answer) %>%
  mutate(across(c(x_stim, y_behav), ~recode(.x, 'left'=0, 'right'=1))) %>% 
  mutate(session = '00')

# Combine data
pd <- full_join(prime_data, prime_data_first)

# Calculate criterion per session -----------------------------------------

# nest data to get nested SDT
pd_nested <- pd %>% 
  arrange(participant, session) %>% 
  group_by(participant, session) %>% 
  nest()

# get SDT
pd_sdt_psycho <-
  pd_nested %>%
  ungroup() %>% 
  mutate(
    sdt = map(data, get_sdt_by_soa)
  ) %>% 
  select(participant, session, sdt) %>%
  unnest(sdt)

# plot SDT across sessions
pd_sdt_psycho %>% 
  ggplot(aes(x=session, y=criterion)) +
  facet_wrap(. ~ participant) + 
  geom_point(color='red') +
  geom_line(aes(group=participant), color='red') + 
  geom_point(aes(y=dprime), color='green') +
  geom_line(aes(y=dprime, group=participant), color='green') +
  geom_hline(yintercept = 0, linetype=2)

# Test difference between first and second session ------------------------

# get data of two first sessions
pd_sdt <- pd %>%
  filter(session %in% c("00", "01")) %>%
  mutate(
    session = factor(session, levels = c("00", "01")),
    x_stim  = as.numeric(x_stim),
    y_behav = as.integer(y_behav)
  ) %>%
  group_by(participant) %>%
  ungroup()

# nest data for model fit
pd_nested <- pd_sdt %>%
  group_by(participant) %>%
  nest() %>%
  mutate(data = map(data, ~.x %>% mutate(participant = participant))) %>% 
  ungroup() 

# Pre-compile model -------------------------------------------------------

dat_template <- pd_nested$data[[1]]

fit_sdt_template <- brm(
  y_behav ~ x_stim * session,
  data = dat_template,
  family = bernoulli(link = "probit"),
  chains = 0,
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  prior = c(
    prior(normal(0, 5), class = "b"),
    prior(normal(0, 5), class = "Intercept")
  ),
  file = 'model_fits/criterion/precompiled_criterion'
)

# Function to refit model per participant ---------------------------------

fit_one_participant <- function(dat) {
  
  dat <- dat %>%
    mutate(
      session = factor(session, levels = c("00", "01")),
      x_stim  = as.numeric(x_stim),
      y_behav = as.integer(y_behav)
    )

  file_name <- sprintf('model_fits/criterion/fit_criterion_%s', unique(dat$participant))
  file_rds <- paste0(file_name, '.rds')
  
  if (file.exists(file_rds)) {
    model_fit <- readRDS(file_rds)
  } else {
    model_fit <- update(
      fit_sdt_template,
      newdata = dat,
      chains = 4,
      cores = 4,
      iter = 4000,
      warmup = 1000,
      seed = 123,
      recompile = FALSE,
      refresh = 0,
      file = file_name
    )
  }

  model_fit
}


# Fit model per participant -------------------------------------------

plan(multisession)

pd_fits <- 
  pd_nested %>%
  mutate(
    fit = future_map(
      data,
      fit_one_participant,
      .options = furrr_options(seed = TRUE)
    )
  )

# Extract criterion posterior per participant -----------------------------

extract_criterion_draws <- function(fit, participant_id) {
  
  fit %>%
    spread_draws(
      b_Intercept,
      b_x_stim,
      b_session01,
      `b_x_stim:session01`
    ) %>%
    mutate(
      participant = participant_id,
      # criterion first session
      c_00 = -b_Intercept - b_x_stim / 2,
      # criterion second session
      c_01 = -(b_Intercept + b_session01) -
        (b_x_stim + `b_x_stim:session01`) / 2,
      # criterion difference
      c_diff_01_minus_00 = c_01 - c_00
    )
}

draws_all <- pd_fits %>%
  mutate(
    draws_c = map2(fit, participant, extract_criterion_draws)
  ) %>%
  select(draws_c) %>%
  unnest(draws_c)

criterion_summary <- draws_all %>%
  group_by(participant) %>%
  mean_qi(c_00, c_01, c_diff_01_minus_00) %>%
  ungroup()

criterion_summary

# Test if criterion differs per session -----------------------------------

pd_hypotheses <- pd_fits %>%
  mutate(
    hyp_criterion = map(
      fit,
      ~ hypothesis(
        .x,
        "-session01 - 0.5 * x_stim:session01 = 0"
      )
    )
  )

hypothesis_summary <- pd_hypotheses %>%
  mutate(
    hyp_table = map(hyp_criterion, ~ as.data.frame(.x$hypothesis))
  ) %>%
  select(participant, hyp_table) %>%
  unnest(hyp_table)

hypothesis_summary

hypothesis_summary %>% 
  mutate(Evid.Ratio = 1/Evid.Ratio) %>% 
  arrange(desc(Evid.Ratio))

hypothesis_summary %>% 
  mutate(bf_label = paste0('participant ', as.integer(participant), ': BF~10~ = ', round(1/Evid.Ratio, 3))) %>% 
  pull(bf_label) %>% 
  paste0(collapse = '; ')

# effect size -------------------------------------------------------------

criterion_diff_summary <- draws_all %>%
  group_by(participant) %>%
  mean_qi(c_diff_01_minus_00) %>%
  ungroup()

criterion_diff_summary %>% 
  mutate(across(c(c_diff_01_minus_00, .lower, .upper), ~round(.x, 2)))

# Save data ---------------------------------------------------------------

hypothesis_summary %>% 
  mutate(bf = 1 / Evid.Ratio) %>% 
  select(participant, bf) %>% 
  full_join(
    criterion_diff_summary %>% 
      select(participant, c_diff_01_minus_00, .lower, .upper) %>% 
      rename(crit_diff = c_diff_01_minus_00)
  ) %>% 
  write_csv('results/criterion_300ms_bf_ci.csv')

# Plot posterior criterion difference by participant ----------------------

draws_all %>%
  ggplot(aes(x = c_diff_01_minus_00)) +
  geom_density(fill = "grey80", color = "black", alpha = 0.75, linewidth = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~ participant, scales = "free_y") +
  theme_bw() +
  labs(
    x = "Criterion difference: session 01 - session 00",
    y = "Posterior density"
  ) + xlim(-.8, .8)


# Compare brms criterion with psycho criterion ----------------------------

criterion_model_session <- draws_all %>%
  select(participant, .draw, c_00, c_01) %>%
  pivot_longer(
    cols = c(c_00, c_01),
    names_to = "session",
    values_to = "criterion_brms"
  ) %>%
  mutate(
    session = recode(
      session,
      c_00 = "00",
      c_01 = "01"
    )
  ) %>%
  group_by(participant, session) %>%
  mean_qi(criterion_brms) %>%
  ungroup()

criterion_compare <- pd_sdt_psycho %>%
  filter(session %in% c("00", "01")) %>%
  select(
    participant,
    session,
    criterion_psycho = criterion,
    dprime_psycho = dprime,
    n_hit, n_miss, n_fa, n_cr
  ) %>%
  left_join(
    criterion_model_session,
    by = c("participant", "session")
  ) %>%
  mutate(
    criterion_diff_brms_minus_psycho = criterion_brms - criterion_psycho,
    lower_diff_brms_minus_psycho = .lower - criterion_psycho,
    upper_diff_brms_minus_psycho = .upper - criterion_psycho
  )

criterion_compare %>% 
  select(participant, session, criterion_psycho, criterion_brms) %>% 
  mutate(diff = criterion_brms - criterion_psycho)
