# ---------------- #
# Day 03 #
# ---------------- #

library(tidyverse)

# Part one ----------------------------------------------------------------

# get the right format
df_engine <-
  tibble(string = readLines("2023/inputs/day03.txt")) |>
  rowid_to_column(var = "col")

# get positions
df_engine_pos <- df_engine |>
  separate_longer_delim(string, ".") |>
  # filter(col == 36)|>
  filter(string != "") |>
  rename(numbers_symbols = string) |>
  separate(
    numbers_symbols,
    into = c("numbers1", "numbers2"),
    sep = "([^0-9])",
    remove = FALSE
  ) |>
  pivot_longer(c("numbers1", "numbers2"),
               names_to = 'dum',
               values_to = "numbers") |>
  filter(!is.na(numbers)) |>
  mutate(symbols = str_remove_all(numbers_symbols, "[0-9]")) |>
  left_join(df_engine, by = "col") |>
  mutate(pos_numbers = str_locate_all(
    string,
    paste0(
      "(?<!\\d)",
      numbers,
      "(?=($|\\.|\\*|\\+|\\%|\\/|\\#|\\$|\\@|\\-|\\&|\\=))"
    )
  ),
  pos_symbols = str_locate_all(string, fixed(symbols)))

# separate + get all pos
df_engine_pos_numbers <- df_engine_pos |>
  filter(numbers != "" & !is.na(numbers)) |>
  select(col, numbers, pos_numbers) |>
  unnest_longer(c(pos_numbers)) |>
  mutate(
    row_nb = map2(pos_numbers[, 1], pos_numbers[, 2], seq),
    numbers = as.numeric(numbers),
    start = pos_numbers[, 1]
  ) |>
  unnest_longer(row_nb) |>
  rename(col_nb = col) |>
  select(col_nb, row_nb, numbers, start) |>
  distinct()

df_engine_pos_symbols <- df_engine_pos |>
  filter(symbols != "" & !is.na(symbols)) |>
  unnest_longer(pos_symbols) |>
  mutate(row_sym = pos_symbols[, 1]) |>
  rename(col_sym = col) |>
  select(col_sym, row_sym, symbols) |>
  distinct()

# compare positions
# same row
df_all <- df_engine_pos_numbers |>
  mutate(dummy = 1) |>
  full_join(
    df_engine_pos_symbols |>
      mutate(dummy = 1),
    by = c("dummy"),
    relationship =
      "many-to-many"
  )

df_all_part_one <- df_all |>
  filter((abs(row_nb - row_sym)) <= 1 &
           abs(col_nb - col_sym) <= 1) |>
  distinct(col_nb, start, numbers) |>
  arrange(col_nb, start)

# answer
sum(df_all_part_one$numbers)

# 530849

# Part two ----------------------------------------------------------------
df_all_part_two <- df_all |> 
  filter(symbols == "*") |> 
  filter((abs(row_nb - row_sym)) <= 1 &
           abs(col_nb - col_sym) <= 1) |> 
  distinct(col_nb, start, numbers, col_sym, row_sym) |> 
  group_by(col_sym, row_sym) |> 
  add_count() |> 
  mutate(id = row_number()) |> 
  filter(n == 2) |> 
  select(col_sym, row_sym, id, numbers) |> 
  pivot_wider(names_from =  id, names_prefix = "nb_", values_from = numbers) |> 
  mutate(product = nb_1 * nb_2)

# answer
sum(df_all_part_two$product)

# 84900879
