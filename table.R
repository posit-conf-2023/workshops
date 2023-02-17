library(tidyverse)
library(gt)
library(glue)

workshops <- read_csv("workshops.csv") |>
  mutate(
    day_1 = glue("[{day_1}](https://posit-conf-2023.github.io/workshops/workshops/{day_1_slug}/)"),
    day_2 = glue("[{day_2}](https://posit-conf-2023.github.io/workshops/workshops/{day_2_slug}/)")
  ) |>
  select(-room, -contains("slug"))

workshops |>
  filter(type == "2-day") |>
  select(-type) |>
  gt() |>
  cols_merge(
    rows = is.na(day_2),
    columns = c(day_1, day_2),
    pattern = "{1}"
  ) |>
  cols_width(day_1 ~ px(800)) |>
  cols_align(day_1, align = "center") |>
  cols_label(day_1 = "Both Days")

workshops |>
  filter(type == "1-day") |>
  select(-type) |>
  gt() |>
  fmt_markdown() |>
  cols_width(
    day_1 ~ px(400),
    day_2 ~ px(400)
  ) |>
  cols_align(
    align = "left",
    columns = c(day_1, day_2)
  ) |>
  tab_style(
    style = cell_borders(
      sides = c("right"),
      style = "solid"
    ),
    locations = cells_body(
      columns = day_1,
      rows = everything()
    )
  ) |>
  cols_label(
    day_1 = "Day 1",
    day_2 = "Day 2"
  )
