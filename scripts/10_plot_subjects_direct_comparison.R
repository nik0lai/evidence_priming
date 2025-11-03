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

participant_label_prime <-
  prime_dat %>% 
  select(participant, session) %>% 
  distinct() %>% 
  group_by(participant) %>% 
  count() %>%
  mutate(label = sprintf('Participant %s (%s sessions)', as.integer(participant), n)) %>% 
  select(-n) %>% 
  deframe()

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
  mutate(bf_pretty = as.character(pretty_bf(bf)),
         label=paste0('atop(bold(BF[10]), "', bf_pretty, '")'),
         d_label = sprintf("italic(d) == %s", cohensd))

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
  geom_text(data=bf_data, aes(x=soa, y=y-.04 , label = bf_pretty),
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

# Function to format BF into "10^x^" style
format_bf <- function(bf) {
  if (is.na(bf)) return(NA)
  sci <- scales::scientific(bf, digits = 0)
  if (grepl("e", sci)) {
    parts <- strsplit(sci, "e")[[1]]
    exp <- as.numeric(parts[2])
    paste0("10^", exp, "^")
  } else {
    sprintf("%.2f", as.numeric(bf))
  }
}

# Build wide-format table with separate BF and d rows
table_long <- bf_data %>%
  select(participant, soa, bf, cohensd) %>%
  arrange(participant, soa) %>% 
  mutate(
    participant = as.integer(participant),
    soa_ms = paste0(soa, " ms"),
    bf_fmt = vapply(bf, pretty_bf, character(1)),
    d_fmt  = sprintf("%.1f", cohensd)
  ) %>%
  mutate(bf_fmt = str_remove(str_remove(bf_fmt, 'paste\\('), '\\)')) %>%
  mutate(bf_fmt = case_when(str_detect(bf_fmt, '\\^') ~ paste0(bf_fmt, '^'),
                            TRUE ~ bf_fmt)) %>% 
  select(participant, soa_ms, bf_fmt, d_fmt) %>%
  pivot_wider(
    names_from = soa_ms,
    values_from = c(bf_fmt, d_fmt)
  ) %>%
  arrange(participant)

# Make two tables stacked: one for BF, one for d
table_bf <- table_long %>%
  select(participant, starts_with("bf_fmt")) %>%
  rename_with(~gsub("bf_fmt_", "", .x))

table_d <- table_long %>%
  select(participant, starts_with("d_fmt")) %>%
  rename_with(~gsub("d_fmt_", "", .x))

# Bind them with a label column
table_final <- bind_rows(
  table_bf %>% mutate(measure = "BF~10~"),
  table_d %>% mutate(measure = "*d*")
) %>%
  relocate(measure, .after = participant)

table_final <- table_final %>% 
  arrange(participant, desc(measure)) %>% 
  rename(P = participant,
         stat = measure)

names(table_final) <- gsub(" ms", "&nbsp;ms", names(table_final))

# Print as markdown
table_md <- kable(table_final, format = "markdown")
write_clip(table_md)   # now it's in your clipboard
