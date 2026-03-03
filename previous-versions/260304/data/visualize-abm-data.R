library(dplyr)
library(ggplot2)

farmers <- read.csv("farmers.csv") |>
  select(1, 5:8)

farms <- read.csv("farms.csv") |>
  select(1:15) %>%
  mutate(across(4:6, ~ round(. / size_ha))) |>
  left_join(farmers)

ggplot(farms) +
  geom_point(aes(x = age, y = current_debt, colour = inherited)) +
  # geom_point(aes(x = age, y = current_debt, group = farm_type, colour = farm_type)) +
  facet_wrap( ~ farm_type, ncol = 2)
  # scale_colour_manual(values = c("violet", "dodgerblue", "green3", "orange2"))





 
holdings <- read.csv("holdings-10.csv") %>%
  mutate(across(8:15, ~ round(. / 16 / size)))

holdings |>
  select(current_income_no_variance, current_costs_no_variance, LUC, farm_type) |>
  group_by(LUC, farm_type) |>
  summarise(profit = max(current_income_no_variance - current_costs_no_variance)) |> View()

ggplot(holdings) +
  geom_boxplot(aes(y = gross_return)) +
  facet_grid(farm_type ~ LUC)

ggplot(holdings, aes(x = LUC, y = current_costs_no_variance, colour = farm_type)) +
  geom_point() +
  geom_smooth() +
  scale_colour_manual(values = c("violet", "dodgerblue", "green3", "orange2"))

