# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, readr, dplyr, tidyr, ggplot2, tibble, stringr, purrr, furrr, patchwork)

source('scripts/functions.R')

# Data --------------------------------------------------------------------

# prime
prime_dat_disc <- read_csv('data/processed/control_prime_disc.csv')
prime_dat_det <- read_csv('data/processed/control_prime_det.csv')
# mask
mask_dat <- read_csv('data/processed/control_mask.csv') %>% mutate(rt = rt * 1000)

# Format soa --------------------------------------------------------------

prime_dat_det <- prime_dat_det %>% make_soa_factor()
prime_dat_disc <- prime_dat_disc %>% make_soa_factor()
mask_dat <- mask_dat %>% make_soa_factor()

# Mask discrimination group -----------------------------------------------

# subject performance
mask_dat_summary <- mask_dat %>% 
  group_by(participant, soa, congruent) %>% 
  reframe(rt = mean(rt))

# group summary
mask_dat_summary_group <-
  mask_dat_summary %>%
  group_by(soa, congruent) %>%
  reframe(rt = mean_se(rt)) %>%
  unnest(rt)

p_mask_group <- 
  mask_dat_summary_group %>% 
  ggplot(aes(x=soa, y=y, group = congruent, linetype=congruent, shape=congruent)) +
  geom_line(linetype=1, linewidth=.4) +
  geom_errorbar(data=mask_dat_summary_group, aes(x=soa, ymin=ymin, ymax=ymax), linetype=1, width=.1, linewidth=.4) +
  geom_point(shape=16, size=3, color='white') +
  geom_point(size=3, fill='white') +
  theme_classic() +
  ylab('Mask discrimination\n(ms)') + xlab('SOA (ms)') +
  scale_shape_manual(values=c(10, 13), labels = c(`FALSE`='incongruent', `TRUE`='congruent')) +
  guides(
    linetype=guide_legend('Condition', position = 'inside', nrow = 1),
    color=guide_legend('Condition', position = 'inside', nrow = 1),
    shape = guide_legend('Condition', position = 'inside', nrow = 1)
  ) +
  theme(legend.position.inside = c(.5,.98),
        legend.text = element_text(size=7),
        legend.key.size = unit(.5, 'points'),
        legend.margin = margin(t=-10),
        legend.title = element_blank()) 

p_mask_group

# Prime detection ----------------------------------------------------

# subject performance
prime_det_dat_summary <- 
  prime_dat_det %>%
  group_by(participant, soa, prime_presence) %>%
  reframe(prop_present = mean(answer == 'present'))

# group summary
prime_det_dat_summary_group <-
  prime_det_dat_summary %>%
  group_by(soa, prime_presence) %>%
  reframe(prop_present = mean_se(prop_present)) %>%
  unnest(prop_present)

# dodge position
pd_width <- .1
alpha_ss <- .5

p_prime_det <-
  prime_det_dat_summary %>%
  ggplot(aes(x=soa, y=prop_present, color=participant, group=prime_presence)) +
  # single subject
  geom_line(aes(group=interaction(participant, prime_presence)), position = position_dodge2(width = pd_width), alpha=alpha_ss, linetype=3) +
  geom_point(aes(shape=prime_presence), size=1.5, position = position_dodge2(width = pd_width), alpha=alpha_ss) +
  # group
  geom_errorbar(data=prime_det_dat_summary_group, aes(x=soa, ymin = ymin, ymax = ymax), width=.1, inherit.aes = FALSE) +
  geom_line(data=prime_det_dat_summary_group, aes(y=y, x=soa, group=prime_presence), inherit.aes = FALSE, linetype=3) +
  geom_point(data=prime_det_dat_summary_group, aes(y=y, x=soa, shape=prime_presence), size=2, inherit.aes = FALSE) +
  scale_shape_manual(values = c('present'=12, 'absent'=14), labels = str_to_title) +
  guides(color = 'none',
         shape = guide_legend('Prime', position = 'inside')) +
  ylab('Prime detection\n(p. present)') + xlab('SOA (ms)') +
  theme_classic() +
  theme(
    legend.direction = 'horizontal',
    legend.position.inside = c(.5,.98),
    legend.text = element_text(size=7),
    legend.title = element_text(size=9),
    legend.key.size = unit(.5, 'points'),
    legend.margin = margin(t=-10)) 

p_prime_det

# Prime discrimination ----------------------------------------------------

# subject performance
prime_disc_dat_summary <- 
  prime_dat_disc %>%
  group_by(participant, soa) %>%
  reframe(accuracy = mean(accuracy))

# group summary
prime_disc_dat_summary_group <-
  prime_disc_dat_summary %>%
  group_by(soa) %>%
  reframe(accuracy = mean_se(accuracy)) %>%
  unnest(accuracy)

# dodge position
pd_width <- .1
alpha_ss <- .5

p_prime_disc <-
  prime_disc_dat_summary %>%
  ggplot(aes(x=soa, y=accuracy, color=participant)) +
  # single subject
  geom_line(aes(group=participant), position = position_dodge2(width = pd_width), alpha=alpha_ss, linetype=3) +
  geom_point(shape=15, size=2, position = position_dodge2(width = pd_width), alpha=alpha_ss) +
  # group
  geom_line(data=prime_disc_dat_summary_group, aes(y=y, x=soa, group='all'), inherit.aes = FALSE, linetype=3) +
  geom_point(shape=15, data=prime_disc_dat_summary_group, aes(y=y, x=soa), inherit.aes = FALSE) +
  geom_errorbar(data=prime_disc_dat_summary_group, aes(x=soa, ymin = ymin, ymax = ymax), width=.1, inherit.aes = FALSE) +
  geom_hline(yintercept = c(.5), linetype=2, alpha=.4) +
  # geom_hline(yintercept = c(.45, .55), linetype=2, alpha=.4) +
  ylim(0.3,1) +
  ylab('Prime discrimination\n(p. correct)') + xlab('SOA (ms)') +
  theme_classic() +
  theme(legend.position = 'none')

p_prime_disc


# Combine plots -----------------------------------------------------------

p_prime_disc + p_mask_group 
ggsave('plots/fig2_control_group.png', width = 5, height = 2, dpi=300, scale = 1.1)
p_prime_det
ggsave('plots/fig_det_control_exp.png', width = 4, height = 3, dpi=300, scale = 1.1)

# Get BF for paper --------------------------------------------------------

bf_prime <- 
  read_csv('results/control_prime_group_bf.csv')
bf_mask <- 
  read_csv('results/control_mask_group_bf.csv')

paste(
  bf_prime %>%
    make_soa_factor() %>% 
    arrange(soa) %>% 
    rowwise() %>% 
    mutate(bf = str_remove(str_remove(pretty_bf(bf), 'paste\\('), '\\)')) %>%
    mutate(bf = case_when(str_detect(bf, '\\^') ~ paste0(bf, '^'),
                          TRUE ~ bf)) %>% 
    transmute(text = str_c(soa, " ms: BF~10~ = ", bf, ', *d* = ', cohensd)) %>%
    pull(text),
  collapse = "; "
)

paste(
  bf_mask %>%
    make_soa_factor() %>% 
    arrange(soa) %>% 
    rowwise() %>% 
    mutate(bf = str_remove(str_remove(pretty_bf(bf), 'paste\\('), '\\)')) %>%
    mutate(bf = case_when(str_detect(bf, '\\^') ~ paste0(bf, '^'),
                          TRUE ~ bf)) %>% 
    transmute(text = str_c(soa, " ms: BF~10~ = ", bf, ', *d* = ', cohensd)) %>%
    pull(text),
  collapse = "; "
)

