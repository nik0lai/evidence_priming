# Packages
if (!require('pacman', quietly = TRUE)) install.packages('pacman'); library('pacman', quietly = TRUE)
p_load(magrittr, purrr, dplyr, readr, tidyr, stringr, ggplot2, patchwork)  

# Priming -----------------------------------------------------------------

# Hand-estimated data from the figure (RT in ms, SOA in ms)
# Error bars look like SEs of ~6–11 ms.
df <- tribble(
  ~SOA, ~Condition,      ~RT, ~SE,   # RT in ms, SE ~ 7–12 ms by eye
  14,  "congruent",     348,   9,
  28,  "congruent",     334,   9,
  42,  "congruent",     322,   8,
  56,  "congruent",     312,   8,
  70,  "congruent",     300,   8,
  84,  "congruent",     298,   9,
  14,  "incongruent",   352,   9,
  28,  "incongruent",   370,  10,
  42,  "incongruent",   382,  10,
  56,  "incongruent",   396,  11,
  70,  "incongruent",   415,  11,
  84,  "incongruent",   420,  12
)

# Quick look
print(df)

# Plot (style kept simple; feel free to restyle)
p_mask <- df %>% 
  ggplot(aes(x=SOA, y=RT, group=Condition, linetype=Condition, shape=Condition)) +
  geom_line(aes(group=Condition), linetype=1, linewidth=.4) +
  geom_point(shape=16, size=3, color='white') +
  geom_point(size=3, fill='white') +
  geom_errorbar(aes(ymin = RT - SE, ymax = RT + SE), linetype=1, width=.1, linewidth=.4) +
  theme_classic() +
  ylab('Mask discrimination (ms)') + xlab('SOA (ms)') +
  coord_cartesian(ylim = c(250, 450)) +
  scale_x_continuous(breaks = c(14, 28, 42, 56, 70, 84)) +
  scale_y_continuous(breaks = c(250, 350, 450)) +
  scale_shape_manual(values=c(10, 13)) +
  guides(
    linetype=guide_legend('Condition', position = 'inside', nrow = 1),
    color=guide_legend('Condition', position = 'inside', nrow = 1),
    shape = guide_legend('Condition', position = 'inside', nrow = 1)
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(size=10),
        axis.title.y = element_text(hjust = .6),
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank(),
        legend.position.inside = c(.49, .05),
        legend.margin = margin(t=-10),
        legend.title = element_blank()
  )

# Awareness ---------------------------------------------------------------

library(tidyverse)

# Recreated pattern: low start, rise to near .55, slight dip, rise again
df_acc <- tribble(
  ~SOA, ~Accuracy, ~SE,
  14,    0.47,   0.018,  # close to lower dotted line
  28,    0.53,   0.017,  # rises toward top line
  42,    0.51,   0.016,  # near the top line
  56,    0.47,   0.016,  # slight dip
  70,    0.52,   0.017   # back up near the top line
)

p_prime <-
  ggplot(df_acc, aes(SOA, Accuracy)) +
  geom_line(size = 0.5) +
  geom_point(size = 3, shape=15) +
  # geom_errorbar(aes(ymin = Accuracy - SE, ymax = Accuracy + SE), width = 2) +
  scale_x_continuous(breaks = c(14, 28, 42, 56, 70)) +
  scale_y_continuous(limits = c(0.2,1), breaks = c(0.5, 1)) +
  geom_hline(yintercept = c(.5), linetype=2, alpha=.4) +
  labs(x = "SOA (ms)", y = 'Prime awareness (p. correct)') +
  theme_classic() +
  theme(axis.text.x = element_text(size=10),
        axis.title.y = element_text(hjust = .6),
        # axis.text.y=element_blank(),
        # axis.ticks.y=element_blank(),
        legend.margin = margin(t=-10),
        legend.title = element_blank()
  )

# Combine plots -----------------------------------------------------------

p_prime + p_mask & 
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12))
ggsave('plots/fig1_vorberg_data.png', width = 6, height = 3, device=png, scale = 0.9, dpi=300)




