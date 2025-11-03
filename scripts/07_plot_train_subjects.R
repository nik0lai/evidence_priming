# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, readr, dplyr, tidyr, ggplot2, tibble, stringr, purrr, furrr, patchwork, ggh4x, bayestestR, ggtext, knitr, clipr)

source('scripts/functions.R')

# Data --------------------------------------------------------------------

# prime
prime_dat <- 
  read_csv('data/processed/train_prime.csv')
# mask
mask_dat <- 
  read_csv('data/processed/train_mask.csv')

# Format data --------------------------------------------------------------

# format SOA and participant/session columns
prime_dat <- make_soa_factor(prime_dat) %>% format_participant_session()
mask_dat <- make_soa_factor(mask_dat) %>% format_participant_session()

# Get number of sessions --------------------------------------------------

# both these should be the same

# prime
participant_label_prime <- 
  prime_dat %>% 
  select(participant, session) %>% 
  distinct() %>% 
  group_by(participant) %>% 
  count() %>%
  mutate(label = paste0('Participant ', as.integer(participant), ' (', n, ' sessions)')) %>% 
  select(-n) %>% 
  deframe()

# mask
participant_label_mask <- 
  mask_dat %>% 
  select(participant, session) %>% 
  distinct() %>% 
  group_by(participant) %>% 
  count() %>%
  mutate(label = paste0('Participant ', as.integer(participant), ' (', n, ' sessions)')) %>% 
  
  select(-n) %>% 
  deframe()

# Get BF ------------------------------------------------------------------

# prime
bf_data_prime <- 
  read_csv('results/train_prime_subjects_bf.csv')

# mask
bf_data_mask <- 
  read_csv('results/train_mask_subjects_bf.csv')

# Make dataset indicating SOAs with unconscious priming -------------------

unc_prim_data <- 
  full_join(
    bf_data_prime %>% mutate(chance = bf <= .1) %>% select(participant, soa, chance),
    bf_data_mask %>% mutate(priming = bf >= 10) %>% select(participant, soa, priming)
  ) %>% 
  mutate(unc_prim = chance & priming) %>% 
  make_soa_factor()

# Format BF for plotting --------------------------------------------------

# prime
bf_data_prime <-
  bf_data_prime %>%
  rowwise() %>%
  mutate(bf_pretty = as.character(pretty_bf(bf)),
         label=paste0('atop(bold(BF[10]), "', bf_pretty, '")'),
         bf_label=get_label_bf(bf),
         d_label = sprintf("italic(d) == %s", cohensd))

# mask
bf_data_mask <-
  bf_data_mask %>%
  rowwise() %>%
  mutate(bf_pretty = as.character(pretty_bf(bf)),
         label=paste0('atop(bold(BF[10]), "', bf_pretty, '")'),
         bf_label=get_label_bf(bf),
         d_label = sprintf("italic(d) == %s", cohensd))


# Colors for plotting evidence --------------------------------------------

# colors for plotting
evidence_colors=c('Effect'="#619CFF", 'No-Effect'="#F8766D", 'Indecisive'='black')
color_unc_priming <- "#ff9b0f"

# Summarize data ----------------------------------------------------------

prime_dat_summ <- 
  prime_dat_summ <-
  prime_dat %>% 
  group_by(participant, soa) %>% 
  summarise(
    k = sum(accuracy),   # number correct
    n = n(),             # total trials
    .groups = "drop"
  ) %>%
  mutate(
    y = k / n,
    ymin  = qbeta(0.031, k + 1, n - k + 1),
    ymax = qbeta(0.975, k + 1, n - k + 1)
  )

# add bf to prime data
prime_dat_summ <-
  prime_dat_summ %>%
  full_join(
    bf_data_prime %>%
      select(participant, soa, bf, bf_pretty, label, bf_label, cohensd, d_label) %>% 
      make_soa_factor()
  )

# Manual position 
std_pos <- .93
prime_dat_summ$y_bf <- c(
  std_pos, std_pos, std_pos, std_pos, std_pos, std_pos, 0.7,  # participant 001, soa: 12.5 → 75
  std_pos, std_pos, std_pos, std_pos, 0.7, 0.7, 0.7,  # participant 002
  std_pos, std_pos, std_pos, std_pos, std_pos, std_pos, 0.7,  # participant 003
  std_pos, std_pos, std_pos, std_pos, std_pos, std_pos, 0.7, # participant 004
  std_pos, std_pos, std_pos, std_pos, std_pos, std_pos, 0.7,  # participant 005
  std_pos, std_pos, std_pos, std_pos, .68, .68, 0.7   # participant 006
)

# plot
p_prime <-
  prime_dat_summ %>% 
  left_join(unc_prim_data) %>% 
  ggplot(aes(x=soa, y=y)) +
  facet_wrap(. ~ participant, ncol = 1, labeller = labeller(participant = participant_label_prime), strip.position = 'top') +
  geom_line(linetype=3, aes(group=soa==300), linewidth=.5, alpha=.8, color='black') +
  geom_point(aes(color=bf_label), size=1.5, shape=15) +
  geom_point(aes(y=1.01, shape=unc_prim), stroke=1.5, size=3, color=color_unc_priming) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax, color=bf_label), width =.1) +
  geom_hline(yintercept = .5, linetype=2, alpha=.5) +
  ylab('Prime discrimination (proportion correct)') + xlab('SOA (ms)\nPrime task') +
  ylim(c(0.42, 1.12)) + 
  stat_unique(aes(x=as.factor(soa), y=y_bf, label="bold(BF[10])", color=bf_label), size=3, parse=TRUE, geom='text') +
  stat_unique(aes(x=as.factor(soa), y=y_bf-.1, label=bf_pretty, color=bf_label), size=3, parse=TRUE, geom='text') +
  stat_unique(aes(x=as.factor(soa), y=.45, label=d_label, color=bf_label), size=2.8, parse=TRUE, geom='text') +
  scale_color_manual(values = evidence_colors) +
  scale_shape_manual(values = c(`TRUE`=4, `FALSE`=NA), breaks = c(TRUE), label = c(`TRUE`=''), drop=FALSE) +
  guides(color = guide_legend('Evidence', override.aes = list(label='', shape=15, size=6, linetype=0), order = 1),
         shape = guide_legend('Unconscious priming<br>(double *t*-test)', order = 2)) +
  theme_bw() +
  theme(legend.position = 'bottom', 
        legend.direction = 'horizontal',
        legend.title = element_markdown(),
        strip.text = element_text(size=14),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.y = element_text(hjust = .6),
        strip.text.x = element_text(size=10),
        strip.background = element_part_rect(side = "tbl", fill = 'white'),
  ) 

# Mask RT plot ------------------------------------------------------------

# separation between same soa congruents
pos_dodge <- 1

mask_priming <- full_join(
  mask_dat %>% 
    group_by(participant, soa, congruent) %>% 
    reframe(rt = mean(rt*1000)) %>% 
    pivot_wider(names_from = congruent, values_from = rt) %>% 
    mutate(rt = `FALSE`-`TRUE`) %>% 
    select(participant, soa, rt),
  mask_dat %>% 
    select(participant, soa, congruent, rt) %>% 
    group_by(participant, soa) %>% 
    mutate(rt = rt*1000) %>% 
    nest() %>% 
    mutate(ci = map(data, ~t.test(rt ~ congruent, data=.x))) %>% 
    mutate(ci_low = unlist(map(ci, ~.x$conf.int))[1],
           ci_high = unlist(map(ci, ~.x$conf.int))[2]) %>% 
    select(participant, soa, ci_low, ci_high)
)

# add bf to prime data
mask_priming <- 
  mask_priming %>%
  full_join(
    bf_data_mask %>%
      select(participant, soa, bf, bf_pretty, label, bf_label, d_label) %>% 
      make_soa_factor()
  )

std_pos <- 90
mask_priming$y_bf <- c(
  std_pos, std_pos, std_pos, std_pos,  std_pos, std_pos, std_pos,  # participant 001, soa: 12.5 → 75
  std_pos, std_pos, std_pos, 31,  31, 31, std_pos,  # participant 002
  std_pos, std_pos, std_pos, 31,  31, 31, std_pos,  # participant 003
  std_pos, std_pos, 31, 31, 31, 31, 31,  # participant 004
  std_pos, std_pos, std_pos, 31,  31, 31, 31,  # participant 005
  std_pos, std_pos, std_pos, std_pos,  31, 31, 31   # participant 006
)

p_mask <-
  mask_priming %>% 
  left_join(unc_prim_data) %>% 
  ggplot(aes(x=soa, y=rt)) +
  facet_wrap(. ~ participant, ncol = 1, labeller = labeller(participant = participant_label_mask), scales = 'free_y', strip.position = 'top') +
  geom_line(linetype=1, aes(group=soa==300), linewidth=.5, alpha=.8, color='black') +
  geom_errorbar(aes(ymin=ci_low, ymax=ci_high, color=bf_label), width=.1, position = position_dodge(width = pos_dodge)) +
  geom_point(aes(color=bf_label), size= 1.5, position = position_dodge2(width = pos_dodge)) +
  geom_point(aes(y=110, shape=unc_prim), stroke=1.5, size=3, show.legend = FALSE, color=color_unc_priming) +
  scale_color_manual(values = evidence_colors) +
  geom_hline(yintercept = 0, linetype=2, alpha=.5) +
  stat_unique(aes(x=as.factor(soa), y=y_bf, label="bold(BF[10])", color=bf_label), size=3, parse=TRUE, geom='text') +
  stat_unique(aes(x=as.factor(soa), y=y_bf-18, label=bf_pretty, color=bf_label), size=3, parse=TRUE, geom='text') +
  stat_unique(aes(x=as.factor(soa), y=-10, label=d_label, color=bf_label), size=2.58, parse=TRUE, geom='text') +
  scale_alpha_manual(values=c(`TRUE`=1, `FALSE`=.3), labels = c(`TRUE`='prime not seen', `FALSE`='prime seen')) +
  scale_shape_manual(values = c(`TRUE`=4, `FALSE`=NA), breaks = c(TRUE)) +
  scale_y_continuous(position = 'right', limits = c(-16.2, 131)) +
  # ggtitle('Mask task') +
  guides(
    color = 'none',
    shape = 'none') +
  ylab('Priming effect (ms)') + xlab('SOA (ms)\nMask task') +
  theme_bw() +
  theme(legend.position = 'bottom', 
        legend.direction = 'horizontal',
        strip.text = element_text(size=14),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.y = element_text(hjust = .6),
        strip.text.x = element_text(size=10, color = 'transparent'),
        strip.background = element_part_rect(side = "tbr", fill = 'white'),
  ) 

# Combine plots -----------------------------------------------------------

p_prime + plot_spacer() + p_mask + plot_layout(widths = c(4, -.48, 4), guides = 'collect') & theme(legend.position = 'bottom')
ggsave('plots/fig4_train_subjects.png', width = 12, height = 18,  dpi = 300, scale=.6, device = png)


# Make markdown tables for paper ------------------------------------------

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
table_long <-
  bf_data_prime %>%
  select(participant, soa, bf, cohensd) %>%
  arrange(participant, soa) %>% 
  mutate(
    participant = as.integer(participant),
    soa_ms = paste0(soa * 1000, " ms"),
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

# Masking -----------------------------------------------------------------

# Build wide-format table with separate BF and d rows
table_long <- bf_data_mask %>%
  select(participant, soa, bf, cohensd) %>%
  arrange(participant, soa) %>% 
  mutate(
    participant = as.integer(participant),
    soa_ms = paste0(soa * 1000, " ms"),
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
