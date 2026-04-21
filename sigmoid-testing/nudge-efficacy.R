library(pracma)
library(ggplot2)
library(cols4all)
library(tidyr)
library(dplyr)
library(metR)

nudge_threshold <- Vectorize(
  function(t, a, nudge) {
    if (t == 0) return (t)
    if (a == 0) return (t)
    c <- logit(t, a = a)
    sigmoid(c + nudge, a = a)
  }
)

df <- expand_grid(
  `Initial probability` = 1:100 / 100,
  Efficacy = 0:50 / 25,
  Nudge = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1)) |>
  mutate(`Nudged probability` = nudge_threshold(`Initial probability`, Efficacy, Nudge),
         `Probability multiplier` = `Nudged probability` / `Initial probability`)

ggplot(data = df, aes(x = `Initial probability`, y = Efficacy, z = `Nudged probability`)) +
  geom_tile(aes(fill = `Nudged probability`)) +
  scale_fill_continuous_c4a_seq("-matplotlib.magma", range = 2:3 / 3) +
  geom_contour(breaks = c(0.01, 1:19/20, 0.99), colour = "black", linewidth = 0.35) + 
  geom_text_contour(breaks = c(0.01, 1:19/20, 0.99), skip = 0, size = 3, stroke = 0.2, stroke.colour = "#ddcc9940") +
  coord_cartesian(expand = FALSE) +
  facet_wrap( ~ Nudge, labeller = label_both) +
  ggtitle("Nudged probability by initial probability, efficacy, and nudge") +
  theme_bw() + 
  theme(legend.position = c(0.825, 0.15))

ggsave("/Users/david/Documents/work/mwlr-moving-the-middle/abm/sigmoid-testing/nudge-efficacy.png", dpi = 300)

ggplot(data = df |> filter((Efficacy * 10) %% 1 == 0),
       aes(x = `Initial probability`, y = `Probability multiplier`, colour = `Nudge`, group = `Nudge`)) +
  geom_line() +
  scale_colour_continuous_c4a_seq("-matplotlib.magma") +
  # scale_y_log10() +
  facet_wrap( ~ Efficacy, labeller = label_both) +
  ggtitle("Probability multiplier by initial probability and efficacy") +
  theme_bw() +
  theme(legend.position = c(0.85, 0.15))

ggsave("/Users/david/Documents/work/mwlr-moving-the-middle/abm/sigmoid-testing/nudge-efficacy-2.png", dpi = 300)


