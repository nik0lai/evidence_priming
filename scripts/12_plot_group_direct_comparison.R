# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, readr, dplyr, tidyr, ggplot2, tibble, stringr, purrr, furrr, patchwork, BayesFactor, brms, ggh4x, bayestestR, ggtext)

source('scripts/functions.R')

# Data --------------------------------------------------------------------

# prime
prime_dat <- read_csv('data/processed/train_prime.csv')
# mask
mask_dat <- read_csv('data/processed/train_mask_recoded.csv')

# Get number of session ---------------------------------------------------

participant_label_prime <-
  prime_dat %>% 
  select(participant, session) %>% 
  distinct() %>% 
  group_by(participant) %>% 
  count() %>%
  mutate(label = sprintf('Subject %s (%s sessions)', as.integer(participant), n)) %>% 
  select(-n) %>% 
  deframe()

# Format data --------------------------------------------------------------

# format SOA and participant/session columns
prime_dat <- make_soa_factor(prime_dat) %>% format_participant_session()
mask_dat <- make_soa_factor(mask_dat) %>% format_participant_session()

# Summary data ------------------------------------------------------------

# prime
prime_dat_summ <- prime_dat %>% 
  group_by(participant, soa) %>% 
  reframe(accuracy = mean(accuracy)) %>% 
  mutate(task = 'prime')

# mask
mask_dat_summ <- mask_dat %>% 
  group_by(participant, soa) %>% 
  reframe(accuracy = mean(rt_recoded)) %>% 
  mutate(task = 'mask')

# combine data
group_summ <- 
  full_join(
    prime_dat_summ,
    mask_dat_summ,
  )

# Get ci
p_load(binom)
group_summ <-
  group_summ %>%
  group_by(task, soa) %>%
  reframe(y = mean_se(accuracy)) %>% 
  unnest(y)
  
# read BF and cohens d
bf_data <- 
  read_csv('results/train_direct_comparison_group_bf.csv') %>% 
  make_soa_factor()

# prime
bf_data <-
  bf_data %>%
  rowwise() %>%
  mutate(bf_pretty = as.character(pretty_bf(bf)),
         label=paste0('atop(bold(BF[10]), "', bf_pretty, '")'),
         d_label = sprintf("italic(d) == %s", cohensd))

# mark unconscious priming
bf_data <- bf_data %>% 
  mutate(ind_task_advantage = bf >= 10 & cohensd > 0)

# mark true unconscious priming
bf_data <- 
  bf_data %>% 
  full_join(
    read_csv('results/train_prime_group_bf.csv') %>% 
      make_soa_factor() %>% 
      mutate(noawareness = bf <= 0.1) %>% 
      select(soa, noawareness)
  ) %>% 
  mutate(unc_prim = ind_task_advantage & noawareness)

color_no_awareness_unc_priming <- "#ff9b0f"
color_unc_priming <- "#3cb371"

y_pos_bf <- .93
y_pos_bf_below <- y_pos_bf - .025
pd <- position_dodge(width = .1)

group_summ %>% 
  ggplot(aes(x=soa, y=y, group=task, color=task, shape=task, linetype=task)) +
  geom_line(aes(group=interaction(task, soa==300)), position = pd) +
  geom_point(data=bf_data, aes(y=.95, x=soa, shape=ind_task_advantage), stroke=1.5, size=4, color=color_unc_priming, inherit.aes = FALSE) +
  geom_point(data=bf_data[1,] %>% mutate(unc_prim = 'unc_prim'), aes(y=2, x=soa, shape=unc_prim), stroke=1.5, size=4, 
             color=color_no_awareness_unc_priming, inherit.aes = FALSE) +
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.1, linetype=1, position = pd) +
  geom_point(size=2, position = pd) +
  geom_hline(yintercept = .5, linetype=2) +
  scale_color_manual(values=c('mask'='black', 'prime'='black'), labels = c('prime'='Prime discrimination', 'mask'='Priming')) +
  scale_shape_manual(values=c('prime'=15, 'mask'=16, `TRUE`=3, `FALSE`=NA, 'unc_prim'=4), 
                     breaks = c(TRUE, 'unc_prim'), labels = c(`TRUE`='Indirect task advantage', 'unc_prim'='Unconscious priming<br>(double *t*-test)')) +
  scale_linetype_manual(values=c('prime'=3, 'mask'=1), labels = str_to_title) +
  geom_text(data=bf_data, aes(x=soa, y=y_pos_bf, label = 'bold(BF[10])'),
            size=2.5, parse = TRUE, inherit.aes = FALSE) +
  geom_text(data=bf_data, aes(x=soa, y=y_pos_bf_below, label = bf_pretty),
            size=3, parse = TRUE, inherit.aes = FALSE) +
  geom_text(data=bf_data, aes(x=factor(soa), y=.48 , label = d_label),
            size=3, parse = TRUE, inherit.aes = FALSE) +
  guides(color = guide_legend('Task', override.aes = list(shape=c(16, 15), size=3, linetype=c(1,3)), order=1),
         shape = guide_legend('Test', override.aes = list(linetype=c(1)), order=2),
         linetype = 'none') + 
  scale_y_continuous(position = 'right', limits = c(.48, 1)) +
  ylab('Proportion correct') + xlab('SOA (ms)') +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.justification = 'right',
        legend.direction = 'vertical',
        legend.margin = margin(t=0,r=-12),
        legend.key.width = unit(30, 'points'),
        legend.text = element_markdown(),

        strip.text = element_text(size=11),
        strip.background = element_part_rect( fill = 'white'),
        axis.title = element_text(size=12),
        axis.text.y = element_text(size=11),
        axis.text.x = element_text(size=9)) 

ggsave("plots/fig5_group_directcomparison.png", width = 5.5, height = 5.5, scale = .8, device=png)

# Plot variance -----------------------------------------------------------

full_join(
  prime_dat_summ,
  mask_dat_summ,
) %>% 
  group_by(task, soa) %>%  
  reframe(y = sd(accuracy)) %>% 
  mutate(task = factor(task, levels=c('prime', 'mask'))) %>% 
  mutate(easy = soa == 300) %>% 
  
  ggplot(aes(x=soa, y=y, group=task, shape=task, linetype=task)) +
  geom_point(size=3) +
  geom_line(aes(group=interaction(task, easy))) +
  ylab('Accuracy Standard Deviation') +
  ggtitle('Variance across participants') +
  scale_shape_manual(values=c('prime'=15, 'mask'=16), labels = c('prime'='Prime discrimination', 'mask'='Priming')) +
  scale_linetype_manual(labels = c('prime'='Prime discrimination', 'mask'='Priming'), values=c('prime'=3, 'mask'=1)) +
  scale_y_continuous(position = 'right') +
  xlab('SOA (ms)') + 
  guides(shape = guide_legend('Task', position = 'inside', nrow = 2),
         linetype = guide_legend('Task', position = 'inside', nrow = 2)) + 
  theme_bw() +
  theme(legend.position = 'none', 
        plot.title = element_text(hjust = 1),
        axis.title = element_text(size=12),
        axis.text.y = element_text(size=11),
        axis.text.x = element_text(size=9))

ggsave('plots/fig5_group_directcomparison_variance.png', height = 2.5, width = 4.34, device=png)
