# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, readr, dplyr, tidyr, ggplot2, tibble, stringr, purrr, furrr, patchwork, glue, clipr)

source('scripts/functions.R')

# Data --------------------------------------------------------------------

# prime
prime_dat <- 
  read_csv('data/processed/train_prime.csv')
# mask
mask_dat <- 
  read_csv('data/processed/train_mask.csv')

# Mark 300 ms  ------------------------------------------------------------

prime_dat <- prime_dat %>% mutate(easy = soa == 0.3)
mask_dat <- mask_dat %>% mutate(easy = soa == 0.3)

# Format data --------------------------------------------------------------

# format SOA and participant/session columns
prime_dat <- make_soa_factor(prime_dat) %>% format_participant_session()
mask_dat <- make_soa_factor(mask_dat) %>% format_participant_session()

# Read bf
bf_data_prime <-
  read_csv('results/train_prime_group_bf.csv') %>%
  rowwise() %>%
  mutate(bf_pretty = as.character(pretty_bf(bf)),
         label=paste0('atop(bold(BF[10]), "', bf_pretty, '")'),
         bf_label=get_label_bf(bf),
         d_label = sprintf("italic(d) == %s", cohensd)) %>% 
  make_soa_factor()

prime_dat_summ <-
  prime_dat %>% 
  group_by(participant, soa, easy) %>% 
  reframe(accuracy = mean(accuracy))

prime_dat_summ_group <-
  prime_dat_summ %>%
  group_by(soa, easy) %>%
  reframe(y = mean_se(accuracy)) %>%
  unnest(y)

# Test --------------------------------------------------------------------

# plot
bf_label='black'
p_prime <-
  prime_dat_summ_group %>% 
  filter(soa!=.3) %>% 
  ggplot(aes(x=as.factor(soa), y=y)) +
  # group summary
  geom_point(shape=15, size=3, color='black') +
  geom_line(linetype=3, aes(group=easy), alpha=.8, color='black') +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), color='black', width =.1) +
  # subject summary
  geom_point(data=prime_dat_summ, aes(x=soa, y=accuracy, color=participant), size=2.5, shape=15, alpha=.6, show.legend = FALSE) +
  geom_line(data=prime_dat_summ, aes(x=soa, y=accuracy, color=participant, group=interaction(participant, easy)), linetype=3, alpha=.3, show.legend = FALSE) +
  geom_hline(yintercept = c(.5), linetype=2, alpha=.4) +
  ylab('Prime discrimination (p. correct)') + xlab('SOA (ms)') +
  theme_classic() +
  theme(axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.y = element_text(hjust = .6)
  ) 

# Mask --------------------------------------------------------------------

# subject performance
mask_dat_summary <- 
  mask_dat %>% 
  mutate(rt = rt *1000) %>% 
  group_by(participant, soa, congruent, easy) %>% 
  reframe(rt = mean(rt))

# group summary
mask_dat_summary_group <-
  mask_dat_summary %>%
  group_by(soa, congruent, easy) %>%
  reframe(rt = mean_se(rt)) %>%
  unnest(rt)

p_mask <-
  mask_dat_summary_group %>% 
  ggplot(aes(x=soa, y=y, group = congruent, linetype=congruent, shape=congruent)) +
  geom_line(aes(group=interaction(congruent, easy)), linetype=1, linewidth=.4) +
  geom_errorbar(data=mask_dat_summary_group, aes(x=soa, ymin=ymin, ymax=ymax), linetype=1, width=.1, linewidth=.4) +
  geom_point(shape=16, size=3, color='white') +
  geom_point(size=3, fill='white') +
  theme_classic() +
  ylab('Mask discrimination (ms)') + xlab('SOA (ms)') +
  scale_shape_manual(values=c(10, 13), labels = c(`TRUE`='congruent', `FALSE`='incongruent')) +
  guides(
    linetype=guide_legend('Condition', position = 'inside', nrow = 1),
    color=guide_legend('Condition', position = 'inside', nrow = 1),
    shape = guide_legend('Condition', position = 'inside', nrow = 1)
  ) +
  theme(axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.y = element_text(hjust = .6),
        legend.position.inside = c(.44, .05),
        legend.text = element_text(margin = margin(l=0)),
        legend.margin = margin(t=-10),
        legend.title = element_blank()) 

# Priming effect single subject -------------------------------------------

# subject effect
priming_effect_data <- 
  mask_dat_summary %>% 
  pivot_wider(names_from = congruent, values_from = rt) %>% 
  mutate(priming = `FALSE` - `TRUE`)

# group effect
priming_effect_data_summary <-
  priming_effect_data %>%
  group_by(soa, easy) %>%
  reframe(priming = mean_se(priming)) %>%
  unnest(priming)


# Read bf
bf_data_mask <-
  read_csv('results/train_mask_group_bf.csv') %>%
  rowwise() %>%
  mutate(bf_pretty = as.character(pretty_bf(bf)),
         label=paste0('atop(bold(BF[10]), "', bf_pretty, '")'),
         bf_label=get_label_bf(bf),
         d_label = sprintf("italic(d) == %s", cohensd)) %>% 
  make_soa_factor()

# dodge position
pd_width <- .1
alpha_ss <- .5

p_priming <- 
  priming_effect_data %>% 
  arrange(participant, soa) %>% 
  ggplot(aes(x=soa, y=priming, color=participant)) +
  # single subject
  geom_line(aes(group=interaction(participant, easy)), position = position_dodge2(width = pd_width), alpha=alpha_ss) +
  geom_point(shape=16, position = position_dodge2(width = pd_width), size=2.5, alpha=alpha_ss) +
  # group
  geom_line(data=priming_effect_data_summary, aes(y=y, x=soa, group=easy), inherit.aes = FALSE) +
  geom_point(data=priming_effect_data_summary, aes(y=y, x=soa), size=3, shape=16, inherit.aes = FALSE) +
  geom_errorbar(data=priming_effect_data_summary, aes(x=soa, ymin = ymin, ymax = ymax), width=.1, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype=2, alpha=.4) +
  ylab('Priming effect (ms)') + xlab('SOA (ms)') +
  scale_y_continuous(breaks = scales::breaks_pretty(n=6)) +
  theme_classic() +
  theme(
    legend.position = 'none',
    axis.text.y = element_text(size=10),
    axis.text.x = element_text(size=10),
    axis.title.y = element_text(hjust = .6)
  ) 

# Combine mask reaction time plots ----------------------------------------


(p_prime + p_mask + p_priming) 

ggsave('plots/fig3_train_group.png', width = 12, height = 3.5, dpi = 300, device=png, scale=.7)


# Print BFs for paper -----------------------------------------------------

paste(
  bf_data_prime %>%
    arrange(soa) %>% 
    rowwise() %>% 
    mutate(bf = pretty_bf(bf)) %>% 
    transmute(text = str_c(soa, " ms: ", bf, ', *d* = ', cohensd)) %>%
    pull(text),
  collapse = "; "
) %>% 
  write_clip()  

paste(
  bf_data_mask %>%
    arrange(soa) %>% 
    rowwise() %>% 
    mutate(bf = pretty_bf(bf)) %>% 
    transmute(text = str_c(soa, " ms: ", bf, ', *d* = ', cohensd)) %>%
    pull(text),
  collapse = "; "
) %>% 
  write_clip()  


# Print results paragraph -------------------------------------------------


format_result_ms <- function(data, soa_value) {
  row <- data %>% 
    filter(as.character(soa) == as.character(soa_value))
  
  glue(
    "{row$soa} ms: {pretty_bf(row$bf)}, *d* = {pretty_d(row$cohensd)}"
  )
}

format_all_results_ms <- function(data) {
  data %>%
    arrange(as.numeric(as.character(soa))) %>%
    rowwise() %>%
    transmute(
      text = glue("{soa} ms: {pretty_bf(bf)}, *d* = {pretty_d(cohensd)}")
    ) %>%
    pull(text) %>%
    paste(collapse = "; ")
}

prime_results <- format_all_results_ms(bf_data_prime)
mask_results  <- format_all_results_ms(bf_data_mask)

paper_paragraph <- glue(
  "For ease of comparison with the results in @vorbergDifferentTimeCourses2003 and from the initial session, we first show the group results before turning to the single subject data. Figures [-@fig:fig3-groupResults]A, [-@fig:fig3-groupResults]B and [-@fig:fig3-groupResults]C depict the group-level results of the prime and mask discrimination tasks. Unlike @vorbergDifferentTimeCourses2003, at the group level, participants were able to discriminate the direction of the prime above chance across all SOAs, and unlike in the original study this ability also strongly increased with SOA ({prime_results}; see Figure [-@fig:fig3-groupResults]A). However, note that, in terms of effect size, participants performed barely above-chance at the two shortest SOAs (12.5 and 25 ms: *d* ≤ 0.3). The mask discrimination results perfectly echoed the original results by @vorbergDifferentTimeCourses2003: reaction times were, on average, longer for incongruent compared with congruent trials (see [-@fig:fig3-groupResults]B), and this priming effect difference became more pronounced as the SOA increased ({mask_results}; see Figure [-@fig:fig3-groupResults]C)."
)

cat(paper_paragraph)
