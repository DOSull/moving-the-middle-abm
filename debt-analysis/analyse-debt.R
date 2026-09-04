library(tidyverse)
library(ggplot2)
library(here)

folder <- str_glue("{here()}/debt-analysis")

# df1 <- read.csv(str_glue("{folder}/farmers-default.csv")) |>
#     left_join(read.csv(str_glue("{folder}/farms-default.csv")), keep = FALSE) |>
#     mutate(model = "20-80% mortgage, 50% inheritance")

# df2 <- read.csv(
#     str_glue("{folder}/farmers-low-inheritance-high-mortgage.csv")) |>
#     left_join(
#         read.csv(str_glue("{folder}/farms-low-inheritance-high-mortgage.csv")), 
#         keep = FALSE) |>
#     mutate(model = "65% avg mortgage, 5% inheritance")

df1 <- read.csv(
    str_glue("{folder}/farmers-low-discount.csv")) |>
    left_join(
        read.csv(str_glue("{folder}/farms-low-discount.csv")), 
        keep = FALSE) |>
    mutate(model = "low discount")

df2 <- read.csv(
    str_glue("{folder}/farmers-low-discount-dairy-premium.csv")) |>
    left_join(
        read.csv(str_glue("{folder}/farms-low-discount-dairy-premium.csv")), 
        keep = FALSE) |>
    mutate(model = "low discount dairy premium")

cuts <- c(-1, 0.01, 0.25, 0.5, 0.75, 1, 2, 3, 5, 8, 100) * 1e6
labels = c(
    "<10K", "<250K", "251-\n500K", "501-\n750K", "750K-\n1M",
    "1-2M", "2-3M", "3-5M", "5-8M", ">8M" )

df <- bind_rows(df1, df2) |>
    mutate(
        debt_service_percent = 100 * debt_payments / current_income,
        `Debt level` = cut(
            current_debt,
            cuts,
            labels = labels, ordered_result = TRUE
        )
    )

sector_colours = c("mediumpurple3", "dodgerblue2", "forestgreen", "darkorange2")

debt_by_sector <- df |>
    group_by(farm_type, model) |>
    summarise(debt_service_percent = mean(debt_service_percent))

ggplot(debt_by_sector) +
    geom_col(
        aes(x = farm_type, y = debt_service_percent, fill = farm_type),
        position = position_dodge()) +
    scale_fill_manual(values = sector_colours) +
    facet_wrap(~ model) +
    xlab("Farm type") +
    ylab("Debt payments as % income") +
    guides(fill = "none") +
    theme_minimal()
# ggsave(str_glue("{folder}/debt-by-sector.png"), dpi = 600,
#        width = 7, height = 5)

ggplot(df) +
    geom_bar(
        aes(
            x = `Debt level`, 
            y = 100 * after_stat(count) / sum(after_stat(count)),
            group = farm_type, fill = farm_type
        )
    ) +
    scale_fill_manual(values = sector_colours) +
    xlab("Debt level") +
    ylab("% Farms") +
    facet_wrap(~ model) +
    guides(fill = "none") +
    theme_minimal()
# ggsave(str_glue("{folder}/debt-levels.png"), dpi = 600, 
#        width = 9, height = 5)
