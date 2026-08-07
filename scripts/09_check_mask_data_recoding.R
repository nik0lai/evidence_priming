# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, ggplot2, BayesFactor, brms, tidybayes, patchwork)  

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

# Summarize data ----------------------------------------------------------

mask_rt <- 
  mask_dat_recoded %>% 
  mutate(rt = rt * 1000) %>% 
  group_by(participant, session, soa, congruent) %>% 
  reframe(rt = mean(rt)) %>% 
  pivot_wider(names_from = congruent, values_from = rt, names_prefix = 'con_') %>% 
  mutate(priming_rt = con_FALSE - con_TRUE) %>% 
  select(-matches('con'))

mask_accu <- 
  mask_dat_recoded %>% 
  group_by(participant, session, soa) %>% 
  reframe(rt_recoded = mean(rt_recoded)) 

mask <- 
  full_join(mask_rt, mask_accu)

# Correlation per participant ---------------------------------------------

sub_summ <- mask %>%
  group_by(participant, session, soa) %>% 
  reframe(across(c(priming_rt, rt_recoded), mean))

sub_summ_corr <-
  sub_summ %>% 
  group_by(participant) %>% 
  nest() %>% 
  mutate(corr = map(data, ~correlationBF(y = .x$rt_recoded, x = .x$priming_rt)),
         r_posterior = map(corr, ~posterior(.x, iterations = 10000))) %>% 
  mutate(bf = map(corr, extractBF)) %>% 
  unnest(bf) %>% 
  mutate(
    r_median  = map_dbl(r_posterior, ~median(as.data.frame(.x)$rho)),
    r_lower = map_dbl(r_posterior, ~quantile(as.data.frame(.x)$rho, 0.025)),
    r_upper = map_dbl(r_posterior, ~quantile(as.data.frame(.x)$rho, 0.975))
  ) %>% 
  mutate(  label = paste0(
    "BF = ", round(bf, 2),
    "\nr = ", round(r_median, 3)
  ))

sub_summ_corr <- sub_summ_corr %>% 
  mutate(corr_label = paste0('atop(r == ', round(r_median, 2), ', "[', round(r_lower, 2), ', ', round(r_upper, 2), ']")'))

# Save data ---------------------------------------------------------------

sub_summ_corr %>% 
  select(participant, bf, r_median, r_lower, r_upper) %>% 
  write_csv('results/mask_recoding_correlation_bf.csv')

p0 <-
  sub_summ %>%
  mutate(soa = soa * 1000) %>% 
  ggplot(aes(x=priming_rt, y=rt_recoded, shape=factor(soa), color=factor(soa))) +
  geom_point(size=3) +
  geom_text(
    data = sub_summ_corr,
    aes(x = Inf, y = Inf, label = corr_label),
    inherit.aes = FALSE,
    hjust = 1.2,
    vjust = 3.5,
    size = 3,
    parse = TRUE
  ) +
  facet_wrap(. ~ participant, labeller = labeller(participant = ~paste('Participant', as.integer(.x)))) +
  scale_shape_manual(values=1:7) +
  theme_bw() +
  ylab('Accuracy (RT recoded)') +
  xlab('Priming (RT)') +
  guides(shape = guide_legend('SOA (ms)'),
         color = guide_legend('SOA (ms)'))

p0
ggsave('plots/sup_split_halt_correlation.png', width = 8, height = 4, scale=.9)

# Regression --------------------------------------------------------------

library(brms)
library(dplyr)

# Prep data
df <- mask %>%
  mutate(
    priming_rt_std = scale(priming_rt)[,1],
    soa_num = scale(soa)[,1],  # or soa_num = soa if prefer raw
    session = factor(session)
  )

# Fit model
fit <- brm(
  bf(rt_recoded ~ priming_rt_std + soa_num + session + (1 | participant),
     phi ~ 1),
  data = df,
  family = Beta(),  
  chains = 4, cores = 4, iter = 3000, warmup = 1000,
  control = list(adapt_delta = 0.95),
  prior = c(
    prior(normal(0, 5), class = Intercept),
    prior(normal(0, 5), class = "b"),
    prior(student_t(3, 0, 2.5), class = "sd")
  ),
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  file = 'model_fits/mask_recode/mask_recode_regression'
)

# check model fit and traceplots
fit
# plot(fit)

# get hyp object
hyp_0 <- hypothesis(fit, 'priming_rt_std=0')
# get bf
bf_null <- hyp_0$hypothesis$Evid.Ratio
# get bf alt
bf_alt <- exp(-log(abs(bf_null)))

exponent <- round(log10(bf_alt))
paste0("paste(10^", exponent, ')')

hyp_0$hypothesis %>% 
  write_csv('results/mask_recoding_regression.csv')

# check posterior against observed data -----------------------------------

data_obs_epred <- epred_draws(fit, newdata = df, re_formula = NA)

# summarize
data_obs_epred <- data_obs_epred %>% 
  group_by(participant, session, soa, priming_rt) %>% 
  reframe(obs = mean(rt_recoded),
          post = mean(.epred))

# plot across SOA
p1 <- data_obs_epred %>% 
  mutate(soa = soa * 1000) %>% 
  ggplot(aes(x=priming_rt, y=obs, color=factor(soa), shape=factor(soa))) +
  geom_point(size=3) +
  geom_point(aes(y=post), size=3, color='black', alpha=.7) +
  scale_shape_manual(values=1:7) +
  theme_bw() +
  ylab('Accuracy (RT recoded)') +
  xlab('Priming (RT)') +
  guides(shape = guide_legend('SOA (ms)'),
         color = guide_legend('SOA (ms)'))
# plot separately for each SOA
p2 <-
  data_obs_epred %>% 
  mutate(soa = soa * 1000) %>% 
  ggplot(aes(x=priming_rt, y=obs, color=factor(soa), shape=factor(soa))) +
  facet_wrap(. ~ soa, labeller = labeller(soa = ~paste(.x, 'ms'))) + 
  geom_point(size=3) +
  geom_point(aes(y=post), size=3, color='black', alpha=.7) +
  scale_shape_manual(values=1:7) +
  theme_bw() +
  ylab('Accuracy (RT recoded)') +
  xlab('Priming (RT)')

p2 <- p2 +
  theme(legend.position = 'none')
p1 <- p1 + 
  theme(
    legend.position = "inside",
    legend.direction = 'vertical',
    legend.position.inside = c(.85, .28)
  )

p1 + p2
ggsave('plots/sup_split_halt_convergence.png', width = 8, height = 4, scale=1.2)

# Fit reduced model -------------------------------------------------------

# Fit model
fit_reduced <- brm(
  bf(rt_recoded ~ soa_num + session + (1 | participant),
     phi ~ 1),  # phi formula ✓
  data = df,
  family = Beta(),  # default: logit(mu), log(phi) ✓
  chains = 4, cores = 4, iter = 3000, warmup = 1000,
  control = list(adapt_delta = 0.95),
  prior = c(
    prior(normal(0, 5), class = "b"),
    prior(student_t(3, 0, 2.5), class = "sd")
  ),
  sample_prior = TRUE,
  save_pars = save_pars(all = TRUE),
  file = 'model_fits/mask_recode/mask_recode_regression_reduced'
)

# Reduced r2
r2_full <- bayes_R2(fit, summary = FALSE)[, 1]
r2_reduced <- bayes_R2(fit_reduced, summary = FALSE)[, 1]

delta_r2 <- r2_full - r2_reduced
posterior_summary(delta_r2)
posterior_summary(delta_r2) %>% 
  as.data.frame() %>% 
  write_csv('results/mask_recoding_r2.csv')

# Combine plots -----------------------------------------------------------

# combine plots
p0 + p1 + p2



