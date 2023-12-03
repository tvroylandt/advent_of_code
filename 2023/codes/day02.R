# ---------------- #
# Day 02 #
# ---------------- #

library(tidyverse)

# Part one ----------------------------------------------------------------

# get the right format
df_bag <- 
  tibble(string = readLines("2023/inputs/day02.txt")) |>
  separate(string, into = c("id", "sets"), sep = ": ") |>
  separate_longer_delim(sets, delim = ";") |>
  group_by(id) |>
  mutate(sets_nb = row_number()) |>
  ungroup() |>
  separate_longer_delim(sets, delim = ",") |>
  mutate(sets = str_trim(sets),
         id = as.numeric(str_remove(id, "Game "))) |>
  separate(sets, into = c("nb", "color"), sep  = " ") |>
  pivot_wider(
    names_from = color,
    values_from = nb,
    values_fill = list(nb = 0),
    values_fn = as.numeric
  )

# compare
df_bag_filter <- df_bag |> 
  filter(!(green <= 13 & red <= 12 & blue <=14)) |> 
  distinct(id)

# remove
df_bag_id <- df_bag |> 
  filter(!id %in% df_bag_filter$id) |> 
  distinct(id)

# answer
sum(df_bag_id$id)

# 2377

# Part two ----------------------------------------------------------------
df_bag_power <- df_bag |> 
  group_by(id) |> 
  summarise(across(c(green, red, blue), max))|> 
  mutate(power = green * red * blue)

# answer
sum(df_bag_power$power)

# 71220

