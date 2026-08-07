# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, readr, dplyr, tidyr, ggplot2, tibble, stringr, purrr, furrr, patchwork, ggh4x, bayestestR, ggtext, knitr, clipr, ggdist, splithalf, glue)

source('scripts/functions.R')

# Data --------------------------------------------------------------------

# prime
prime_dat <- 
  read_csv('data/processed/train_prime.csv')
# mask
mask_dat <- 
  read_csv('data/processed/train_mask_recoded.csv')

# Format data --------------------------------------------------------------

# format SOA and participant/session columns
prime_dat <- make_soa_factor(prime_dat) %>% format_participant_session()
mask_dat <- make_soa_factor(mask_dat) %>% format_participant_session()

# permutation split half --------------------------------------------------


# prime data
pd <- 
  prime_dat %>% 
  select(participant, session, soa, accuracy) 

# run permutations
acc_reliability_prime <- splithalf(
  data            = pd,          # your tibble
  outcome         = "accuracy",              # use accuracy as outcome
  score           = "average",             # “mean” will compute % accuracy per half
  halftype        = "random",           # random splits
  permutations    = 5000,               # many random splits
  var.ACC         = "accuracy",         # your accuracy column
  var.condition   = "soa",              # stratify splits by SOA
  var.participant = "participant",      # participant ID
  conditionlist = pd$soa %>% levels()
  # plot = TRUE
)

# summary 
acc_reliability_prime

# summarize correlation
prime_corr_summary <-
  acc_reliability_prime$estimates %>% 
  group_by(condition, iteration) %>% 
  reframe(spearmanbrown = median(spearmanbrown)) %>% 
  group_by(condition) %>% 
  median_qi(spearmanbrown, .width = c(.8, .95)) %>% 
  unnest(spearmanbrown) 

# 
p_prime <- 
  acc_reliability_prime$estimates %>% 
  mutate(condition = factor(condition, levels = pd$soa %>% levels())) %>% 
  ggplot(aes(x=condition, y=spearmanbrown)) +
  geom_point(alpha=.1, position = position_jitter(width = .1),) +
  geom_pointinterval(data=prime_corr_summary, aes(ymin=.lower, ymax=.upper), color='brown3') +
  # stat_pointinterval(color='brown3') +
  theme_bw() +
  xlab('SOA (ms)') +  
  ylab('Spearman–Brown corrected correlation') +
  geom_text(data=prime_corr_summary, 
            aes(x=condition, y=spearmanbrown, label = round(spearmanbrown, 2)),
            position = position_nudge(x = 0.35))

# this could be reported as: using 8000 random splits, the spearman-brown corrected 
# reliability  estimate for the 12.5 condition was 0.75, 95% CI [0.31, 0.97]"
pd_report <-
  prime_corr_summary %>% 
  filter(.width==.95) %>% 
  arrange(as.numeric(condition)) %>% 
  mutate(across(c('spearmanbrown', '.lower', '.upper'), ~round(.x, 2))) %>% 
  mutate(text = case_when(condition == 12.5 ~  paste0(condition, 'ms = ', spearmanbrown, ' 95% CI [', .lower, ', ', .upper, ']'),
                          TRUE ~  paste0(condition, 'ms = ', spearmanbrown, ' [', .lower, ', ', .upper, ']')))

pd_report$text %>% paste(collapse = '; ')

# save correlations
pd_report %>% 
  select(condition, spearmanbrown, .lower, .upper, .width) %>% 
  rename(soa = condition) %>% 
  write_csv('results/prime_reliability_correlations.csv')


# mask data ----------------------------------------------
md <- 
  mask_dat %>% 
  select(participant, session, soa, rt_recoded) 

# run permutations
acc_reliability_mask <- splithalf(
  data            = md,          # your tibble
  outcome         = "accuracy",              # use accuracy as outcome
  score           = "average",             # “mean” will compute % accuracy per half
  halftype        = "random",           # random splits
  permutations    = 5000,               # many random splits
  var.ACC         = "rt_recoded",         # your accuracy column
  var.condition   = "soa",              # stratify splits by SOA
  var.participant = "participant",      # participant ID
  conditionlist   = md$soa %>% levels()
  # ,plot = TRUE
)

# summary 
acc_reliability_mask

acc_reliability_mask$plot +
  scale_x_discrete(limits = c('12.5', '25', '37.5', '50', '62.5', '75', '300'))

# summarize correlation
mask_corr_summary <- 
  acc_reliability_mask$estimates %>% 
  group_by(condition, iteration) %>% 
  reframe(spearmanbrown = median(spearmanbrown)) %>% 
  group_by(condition) %>% 
  median_qi(spearmanbrown, .width = c(.8, .95)) %>% 
  unnest(spearmanbrown) 

# 
p_mask <-
  acc_reliability_mask$estimates %>% 
  mutate(condition = factor(condition, levels = pd$soa %>% levels())) %>% 
  ggplot(aes(x=condition, y=spearmanbrown)) +
  geom_point(alpha=.1, position = position_jitter(width = .1)) +
  geom_pointinterval(data=mask_corr_summary, aes(ymin=.lower, ymax=.upper), color='brown3') +
  # stat_pointinterval(color='brown3') +
  theme_bw() +
  xlab('SOA (ms)') +
  ylab('Spearman–Brown corrected correlation') +
  geom_text(data=mask_corr_summary, 
            aes(x=condition, y=spearmanbrown, label = round(spearmanbrown, 2)),
            position = position_nudge(x = 0.35))

# this could be reported as: using 8000 random splits, the spearman-brown corrected 
# reliability  estimate for the 12.5 condition was 0.75, 95% CI [0.31, 0.97]"
acc_reliability_mask$final_estimates

# this could be reported as: using 8000 random splits, the spearman-brown corrected 
# reliability  estimate for the 12.5 condition was 0.75, 95% CI [0.31, 0.97]"
md_report <- 
  mask_corr_summary %>% 
  filter(.width==.95) %>% 
  arrange(as.numeric(condition)) %>% 
  mutate(across(c('spearmanbrown', '.lower', '.upper'), ~round(.x, 2))) %>% 
  mutate(text = case_when(condition == 12.5 ~  paste0(condition, 'ms = ', spearmanbrown, ' 95% CI [', .lower, ', ', .upper, ']'),
                          TRUE ~  paste0(condition, 'ms = ', spearmanbrown, ' [', .lower, ', ', .upper, ']')))

md_report$text %>% paste(collapse = '; ')

# save correlations
md_report %>% 
  select(condition, spearmanbrown, .lower, .upper, .width) %>% 
  rename(soa = condition) %>% 
  write_csv('results/mask_reliability_correlations.csv')

# Combine plots -----------------------------------------------------------

p <- p_prime / p_mask + plot_layout(guides = 'collect', axis_titles = 'collect')
p
ggsave('plots/sup_split_reliability.png', width = 6.5, height = 4)


# Print results -----------------------------------------------------------

pretty_rel <- function(x) {
  as.character(round(x, 2))
}

format_reliability_result <- function(data, soa_value, width = 0.95, show_ci = FALSE) {
  row <- data %>% 
    filter(condition == as.character(soa_value), .width == width)
  
  ci_text <- if (show_ci) {
    glue("95% CI [{pretty_rel(row$.lower)}, {pretty_rel(row$.upper)}]")
  } else {
    glue("[{pretty_rel(row$.lower)}, {pretty_rel(row$.upper)}]")
  }
  
  glue(
    "{row$condition} ms = {pretty_rel(row$spearmanbrown)} {ci_text}"
  )
}

format_all_reliability <- function(data, width = 0.95) {
  soa_order <- c("12.5", "25", "37.5", "50", "62.5", "75", "300")
  
  data %>%
    filter(.width == width) %>%
    mutate(condition = factor(condition, levels = soa_order)) %>%
    arrange(condition) %>%
    mutate(show_ci = row_number() == 1) %>%
    rowwise() %>%
    transmute(
      text = format_reliability_result(
        data = data,
        soa_value = as.character(condition),
        width = width,
        show_ci = show_ci
      )
    ) %>%
    pull(text) %>%
    paste(collapse = "; ")
}

# Create reliability strings ------------------------------------------------

prime_reliability_results <- format_all_reliability(prime_corr_summary)
mask_reliability_results  <- format_all_reliability(mask_corr_summary)

# Check output if needed ----------------------------------------------------

cat(mask_reliability_results)

# Paragraph -----------------------------------------------------------------

paper_paragraph <- glue(
  "In the prime discrimination task, the Spearman-Brown corrected reliability estimate was at least 0.74 in all SOAs except for the 300 ms SOA, for which it was 0.6 ({prime_reliability_results}). In the mask discrimination task (RT-recoded responses), the reliability was the lowest for the two shortest SOAs ({format_reliability_result(mask_corr_summary, 12.5, show_ci = TRUE)}; {format_reliability_result(mask_corr_summary, 25)}), whereas for the rest it was above 0.9 ({format_reliability_result(mask_corr_summary, 37.5, show_ci = TRUE)}; {format_reliability_result(mask_corr_summary, 50)}; {format_reliability_result(mask_corr_summary, 62.5)}; {format_reliability_result(mask_corr_summary, 75)}; {format_reliability_result(mask_corr_summary, 300)})."
)

cat(paper_paragraph)
