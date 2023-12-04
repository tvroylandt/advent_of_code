# ---------------- #
# Day 04 #
# ---------------- #

library(tidyverse)

# Part one ---------------------------------------------------------------
# clean cards
df_cards <-
  tibble(string = readLines("2023/inputs/day04.txt")) |>
  separate(string, into = c("card_id", "rest"), sep = ":") |>
  separate(rest, into = c("win", "have"), sep = "\\|") |>
  separate_longer_delim(win, delim = " ") |>
  separate_longer_delim(have, delim = " ") |>
  mutate(
    win = as.numeric(str_trim(win)),
    have = as.numeric(str_trim(have)),
    card_id = as.numeric(str_remove(card_id, "Card "))
  ) |>
  filter(!is.na(win) & !is.na(have))

# get matching
df_cards_match <- df_cards |>
  filter(win == have) |>
  group_by(card_id) |>
  mutate(nb = row_number()) |>
  summarise(max_nb = max(nb) - 1) |>
  mutate(total = 2 ^ max_nb) |>
  ungroup()

# answer
sum(df_cards_match$total)

# 22193

# Part two ----------------------------------------------------------------
# get cards drawn by card
df_cards_draw <- df_cards |>
  filter(win == have) |>
  count(card_id, name = "win_ok") |>
  mutate(seq = map2(card_id + 1, card_id + win_ok, seq)) |>
  unnest_longer(seq) |>
  select(-win_ok)

df_cards_init <- df_cards |>
  distinct(card_id)

df_answer <- tibble()

# loop
for (i in 1:length(df_cards_init$card_id)) {
  df_carts_to_add <- df_cards_init |>
    bind_rows(df_answer) |>
    filter(card_id == i) |>
    inner_join(df_cards_draw, by = "card_id", relationship =
                 "many-to-many")
  
  df_answer <- df_answer |>
    bind_rows(df_carts_to_add |>
                select(seq) |>
                rename(card_id = seq))
}

# bind init
df_answer <- df_answer |>
  bind_rows(df_cards_init)

# answer
length(df_answer$card_id)

# 5625994
