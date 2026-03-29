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
  p0 = 0:20 / 20,
  Efficacy = 0:25 / 10,
  Nudge = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1)) |>
  mutate(`Nudged p` = nudge_threshold(p0, Efficacy, Nudge))

ggplot(data = df, aes(x = p0, y = Efficacy, z = `Nudged p`)) +
  geom_raster(aes(fill = `Nudged p`)) +
  scale_fill_continuous_c4a_seq("-matplotlib.magma", range = 2:3 / 3, breaks = 0:4 / 4) +
  geom_contour(breaks = c(1:19/20, 0.99), colour = "black", linewidth = 0.35) + 
  geom_text_contour(breaks = c(1:19/20, 0.99), skip = 0, size = 3, stroke = 0.2, stroke.colour = "#ddcc9940") +
  coord_cartesian(expand = FALSE) +
  facet_wrap( ~ Nudge, labeller = label_both) +
  theme_bw() + 
  theme(legend.position = c(0.8, 0.15))

ggsave("nudge-efficacy.png", dpi = 300)
