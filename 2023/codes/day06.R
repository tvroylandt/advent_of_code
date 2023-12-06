# ---------------- #
# Day 06 #
# ---------------- #

library(tidyverse)

# Part one ---------------------------------------------------------------
df_races <-
  tibble(string = readLines("2023/inputs/day06.txt")) |> 
  separate(string, into = c("type", "values"), sep = ":") |> 
  separate_longer_delim(values, delim = " ") |> 
  filter(values != "") |> 
  group_by(type) |> 
  mutate(race_id = row_number()) |> 
  ungroup() |> 
  pivot_wider(names_from = type, values_from = values) |> 
  mutate(push = map(Time, \(x)seq(0, x, 1))) |> 
  unnest_longer(push) |>
  mutate(across(everything(), as.numeric),
         time_remaining = Time-push,
         distance_run = push * time_remaining) |> 
  filter(distance_run > Distance) |> 
  count(race_id)

# answer
prod(df_races$n)

# 608902

# Part two ----------------------------------------------------------------
df_races_bis <-
  tibble(string = readLines("2023/inputs/day06.txt")) |> 
  separate(string, into = c("type", "values"), sep = ":") |> 
  mutate(values = str_remove_all(values, " ")) |> 
  pivot_wider(names_from = type, values_from = values) |> 
  mutate(push = map(Time, \(x)seq(0, x, 1))) |> 
  unnest_longer(push) |>
  mutate(across(everything(), as.numeric),
         time_remaining = Time-push,
         distance_run = push * time_remaining) |> 
  filter(distance_run > Distance) 

# answer
dim(df_races_bis)[1]

# 46173809
