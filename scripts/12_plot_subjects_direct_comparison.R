# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, readr, dplyr, tidyr, ggplot2, tibble, stringr, purrr, furrr, patchwork, BayesFactor, brms, ggh4x, knitr, clipr)

source('scripts/functions.R')

# Data --------------------------------------------------------------------

# prime
prime_dat <- read_csv('data/processed/train_prime.csv')
# mask
mask_dat <- read_csv('data/processed/train_mask_recoded.csv')


# Get number of session ---------------------------------------------------

participant_label_prime <- make_participant_label(prime_dat)

# Format data --------------------------------------------------------------

# format SOA and participant/session columns
prime_dat <- make_soa_factor(prime_dat) %>% format_participant_session()
mask_dat <- make_soa_factor(mask_dat) %>% format_participant_session()

# Summary data ------------------------------------------------------------

p_load(binom)

summ_data <- full_join(
  
  prime_dat %>% 
    group_by(participant, soa) %>% 
    summarise(
      k = sum(accuracy),   # number correct
      n = n(),             # total trials
      .groups = "drop"
    ) %>%
    mutate(
      mean = k / n,
      lower  = qbeta(0.025, k + 1, n - k + 1),
      upper = qbeta(0.975, k + 1, n - k + 1)
    ) %>% 
    mutate(task = 'prime')
  ,
  
  mask_dat %>% 
    group_by(participant, soa) %>% 
    summarise(
      k = sum(rt_recoded),   # number correct
      n = n(),             # total trials
      .groups = "drop"
    ) %>%
    mutate(
      mean = k / n,
      lower  = qbeta(0.025, k + 1, n - k + 1),
      upper = qbeta(0.975, k + 1, n - k + 1)
    ) %>% 
    mutate(task = 'mask')
  
)

# read BF and cohens d
bf_data <- 
  read_csv('results/train_direct_comparison_bf.csv') %>% 
  make_soa_factor() 

# prime
bf_data <-
  bf_data %>%
  rowwise() %>%
  mutate(
    bf_pretty = pretty_bf(bf),                 
    bf_number = pretty_bf_number(bf, output = 'ggplot'),          
    label     = paste0("atop(bold(BF[10]), ", bf_number, ")"),
    bf_label  = get_label_bf(bf),
    d_label   = sprintf("italic(d) == %s", pretty_d(cohensd))
  )

# sort df
bf_data <- bf_data %>% 
  arrange(participant, soa)

bf_data$y <- c(
  0.89, 0.89, 0.89, 0.89,  0.89, 0.89, 0.89,  # participant 001, soa: 12.5 → 75
  0.89, 0.89, 0.89, 0.60,  0.60, 0.60, 0.89,  # participant 002
  0.89, 0.89, 0.89, 0.89,  0.72, 0.72, 0.89,  # participant 003
  0.89, 0.89, 0.72, 0.785, 0.60, 0.60, 0.60,  # participant 004
  0.89, 0.89, 0.89, 0.89,  0.89, 0.89, 0.89,  # participant 005
  0.89, 0.89, 0.89, 0.60,  0.60, 0.60, 0.60   # participant 006
)

# mark unconscious priming
bf_data <- bf_data %>% 
  mutate(ind_task_advantage = bf >= 10 & cohensd > 0)

# mark true unconscious priming
bf_data <- 
  bf_data %>% 
  full_join(
read_csv('results/train_prime_subjects_bf.csv') %>% 
  make_soa_factor() %>% 
  mutate(noawareness = bf <= 0.1) %>% 
  select(participant, soa, noawareness)
  ) %>% 
  mutate(unc_prim = case_when(ind_task_advantage & noawareness ~ 'unc_prim',
                              TRUE ~ ''))

color_no_awareness_unc_priming <- "#ff9b0f"
color_unc_priming <- "#3cb371"

summ_data %>% 
  ggplot(aes(x=soa, y=mean, group=participant, color=task, shape=task, linetype=task)) +
  facet_wrap(. ~ participant, labeller = labeller(participant=participant_label_prime), 
             ncol=2, dir = 'v') +
  geom_line(aes(group=interaction(task, soa==300))) +
  geom_point(data=bf_data, aes(y=.93, x=soa, shape=ind_task_advantage), stroke=1.5, size=4, color=color_unc_priming, inherit.aes = FALSE) +
  geom_point(data=bf_data, aes(y=.97, x=soa, shape=unc_prim), stroke=1.5, size=4, color=color_no_awareness_unc_priming, inherit.aes = FALSE) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.05, linetype=1,) +
  geom_point(size=2) +
  geom_hline(yintercept = .5, linetype=2) +
  scale_color_manual(values=c('mask'='black', 'prime'='black'), labels = c('prime'='Prime discrimination', 'mask'='Priming')) +
  scale_shape_manual(values=c('prime'=15, 'mask'=16, `TRUE`=3, `FALSE`=NA, 'unc_prim'=4), breaks = c(TRUE, 'unc_prim'), labels = c(`TRUE`='Indirect task advantage', 'unc_prim'='Unconscious priming')) +
  
  scale_linetype_manual(values=c('prime'=3, 'mask'=1), labels = str_to_title) +
  geom_text(data=bf_data, aes(x=soa, y=y , label = 'bold(BF[10])'),
            size=2.5, parse = TRUE, inherit.aes = FALSE) +
  geom_text(data=bf_data, aes(x=soa, y=y-.04 , label = bf_number),
            size=3, parse = TRUE, inherit.aes = FALSE) +
  geom_text(data=bf_data, aes(x=factor(soa), y=.46 , label = d_label),
            size=3, parse = TRUE, inherit.aes = FALSE) +
  guides(color = guide_legend('Task', override.aes = list(linetype=c(1,3)), order=1),
         shape = guide_legend('Indirect task advantage', override.aes = list(linetype=c(1)), order=2),
         linetype = 'none') + 
  ylab('Proportion correct') + xlab('SOA (ms)') +
  theme_bw() +
  theme(legend.position = 'none',
        legend.justification = 'right',
        # legend.margin = margin(t=-22),
        legend.key.width = unit(30, 'points'),
        strip.text = element_text(size=10),
        strip.background = element_part_rect( fill = 'white'),
        axis.title = element_text(size=12),
        axis.text.y = element_text(size=11),
        axis.text.x = element_text(size=9)) +
  ylim(c(.46, 1))

ggsave('plots/fig5_subjects_directcomparison.png', width = 10, height = 9.7, scale = .9, dpi=300, device=png)

# Make table --------------------------------------------------------------

# Build wide-format table with separate BF and d rows
table_md <- build_participant_bf_table(bf_data, convert_soa_to_ms = FALSE)
write_clip(table_md)   

