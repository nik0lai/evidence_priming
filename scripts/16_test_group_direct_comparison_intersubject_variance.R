# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, ggplot2, brms, furrr, tibble, patchwork, tidybayes, ggdist)  

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

# Remove 300 ms soa and center SOAs
dat <- 
  dat %>%
  filter(soa != .3) %>% 
  mutate(
    soa_c   = scale(soa, center = TRUE, scale = FALSE)[,1]  # centered
  )

# Check the centering
dat %>% 
  select(soa, soa_c) %>% 
  distinct() %>% 
  arrange(soa)

# Fit model ---------------------------------------------------------------

# Define and run mode;
fit <- brm(
  accuracy ~ soa_c * task + session +
    (1 | participant) +                           # overall level differences
    (0 + soa_c | gr(participant:task, by = task)),  # SOA slope varies by participant×task, separate SD per task
  data   = dat,
  family = bernoulli(),
  prior  = c(
    prior(normal(0, 5), class = Intercept),
    prior(normal(0, 5), class = "b"),
    prior(student_t(3, 0, 2.5), class = "sd")
  ),
  chains = 4, iter = 4000,
  cores = 4,
  save_pars = save_pars(all = TRUE),
  file = 'model_fits/direct_comparison_train_group/variance_comparison'
)

# check model output
fit

# 
check_all_rhat(fit)

# plot params distribution and traceplots
plot(fit, nvariables = 6, ask = FALSE)

# get variable names to test hypothesis
draw_names <- names(as_draws_df(fit))
grep("sd_.*soa", draw_names, value = TRUE)

get_hyp <- function(fit) {
  hypothesis(
    fit,
    "`participant:task__soa_c:taskprime` >
   `participant:task__soa_c:taskmask`",
    class = 'sd'
  )
}

# test if across subject variance is larger in prime
hyp_result <- get_hyp(fit)
hyp_result

# Save data ---------------------------------------------------------------

hyp_result$hypothesis %>% 
  write_csv('results/direct_comparison_group_variance_bf.csv')

# Plot posteriors ---------------------------------------------------------

# centered
draws_c <- as_draws_df(fit)   # or m_session

draws_c %>%
  select(
    prime = `sd_participant:task__soa_c:taskprime`,
    mask  = `sd_participant:task__soa_c:taskmask`
  ) %>% 
  pivot_longer(
    everything(), names_to = 'condition', values_to = 'between_participant_sd'
  ) %>% 
  ggplot(aes(between_participant_sd, condition)) +
  # stat_dots() +
  stat_halfeye(aes(shape = condition)) +
  xlim(0, 30) +
  ylab('Task') +
  xlab('Model-predicted variance (SD) between participants') +
  scale_shape_manual(values=c('prime'=15, 'mask'=16)) + 
  scale_y_discrete(labels = c('prime'='Prime\ndiscrimination', 'mask'='Priming')) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title.x = element_text(hjust = 1))

ggsave('plots/sup_variance.png', height = 3, width = 4.3)
  
# -------------------------------------------------------------------------

pp_check(fit)

# posterior data ----------------------------------------------------------

# new data for 
nd <- dat %>% 
  ungroup() %>% 
  select(participant, soa, soa_c, task) %>% 
  distinct() %>% 
  mutate(session = NA)

# summarize empirical data
emp_data <- dat %>% 
  group_by(participant, task, soa) %>%
  summarise(acc = mean(accuracy), .groups = "drop") %>%
  group_by(task, soa) %>%
  summarise(
    mean_acc = mean(acc),
    sd_acc   = sd(acc),
    .groups = "drop"
  )

plot_data_ppc <- function(fit, nd) {
  
  post_data_c <- epred_draws(fit,
                             newdata = nd,
                             re_formula = NULL,
                             ndraws = 4000)
  
  # predicted data centered
  pred_points_c <- post_data_c %>%
    group_by(task, soa, .draw) %>%
    summarise(epred = mean(.epred), .groups = "drop") %>%
    group_by(task, soa) %>%
    summarise(
      pred_mean = mean(epred),
      pred_low  = quantile(epred, 0.055),  # 89% CI
      pred_high = quantile(epred, 0.945),
      .groups = "drop"
    )
  
  ggplot(emp_data, aes(x = factor(soa), y = mean_acc)) +
    facet_wrap(. ~ task) +
    geom_point(aes(color = task)) +
    geom_errorbar(aes(ymin = mean_acc - sd_acc,
                      ymax = mean_acc + sd_acc,
                      color=task)) +
    # predicted
    geom_point(data=pred_points_c, aes(y=pred_mean), color = 'purple') +
    geom_errorbar(data=pred_points_c,
                  aes(y=pred_mean,
                      ymin = pred_low,
                      ymax = pred_high),
                  color='purple') +
    theme_bw()
  
}

# -------------------------------------------------------------------------

plot_data_ppc(fit, nd = nd)
