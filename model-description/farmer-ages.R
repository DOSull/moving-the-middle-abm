library(extraDistr)
library(ggplot2)
library(dplyr)

set.seed(1)

get_age_df <- function(num_farmers = 250,
                       age_range = 70,
                       alpha = 5,
                       beta = 4, 
                       min_age = 20) {
  age_draw <- data.frame(Age = 20 + rbbinom(num_farmers, age_range, alpha, beta)) |>
    count(Age)
  data.frame(Age = min_age + 0:age_range) |>
    left_join(age_draw) |>
    mutate(n = if_else(is.na(n), 0, n),
           pdf = dbbinom(0:age_range, age_range, alpha, beta)) |>
    rename(`Number of farmers` = n)
}

ages_init <- get_age_df()
ggplot(ages_init) +
  geom_col(aes(x = Age, y = `Number of farmers`), fill = "grey") +
  geom_line(aes(x = Age, y = exp_n), colour = "red") +
  theme_minimal()

ages_succession <- get_age_df(age_range = 45, alpha = 4, beta = 2)
ggplot(ages_succession) +
  geom_col(aes(x = Age, y = `Number of farmers`), fill = "grey") +
  geom_line(aes(x = Age, y = exp_n), colour = "red") +
  theme_minimal()

ages_all <- 
  ages_init |> 
  select(Age, exp_n) |> 
  left_join(ages_succession |> select(Age, exp_n), 
            by = join_by(Age)) |> 
  rename(Initial = exp_n.x, Succession = exp_n.y) |> 
  mutate(Succession = if_else(is.na(Succession), 0, Succession)) |> 
  pivot_longer(c(Initial, Succession))

ggplot(ages_all) +
  geom_line(aes(x = Age, y = value, colour = name, group = name)) +
  scale_colour_brewer(palette = "Set1") +
  ylab('Probability density') +
  theme_minimal() +
  theme(legend.title = element_blank())
