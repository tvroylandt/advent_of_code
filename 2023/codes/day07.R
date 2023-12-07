# ---------------- #
# Day 07 #
# ---------------- #

library(tidyverse)

# Part one ---------------------------------------------------------------
df_cards <-
  tibble(string = readLines("2023/inputs/day07.txt")) |>
  separate(string, into = c("cards", "bid"), sep = " ") |>
  mutate(cards_sep = str_split(cards, ""),
         cards_pos = cards_sep) |>
  unnest_longer(cards_sep) |>
  # compute first strength based on number of identical cards
  count(cards, bid, cards_sep, cards_pos, name = "nb_identical") |>
  group_by(cards, bid, cards_pos) |>
  arrange(cards, bid, cards_pos, desc(nb_identical)) |>
  slice_head(n = 2) |>
  mutate(row_nb = row_number()) |>
  select(-cards_sep) |> 
  ungroup() |>
  pivot_wider(names_from = row_nb,
              names_prefix = "ord_",
              values_from = nb_identical) |>
  mutate(
    first_strength = case_when(
      ord_1 == 5 ~ 5,
      ord_1 == 4 ~ 4,
      ord_1 == 3 & ord_2 == 2 ~ 3,
      ord_1 == 3 ~ 2,
      ord_1 == 2 & ord_2 ==2 ~ 1,
      ord_1 == 2 ~ 0,
      ord_1 == 1 ~ -1
    )
  ) |>
  select(-ord_1,-ord_2) |>
  # compute second strength based on position
  unnest_wider(cards_pos, names_sep = "_") |>
  mutate(across(starts_with("cards_pos"),
                \(x)fct_rev(
                  fct_relevel(x, "A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")
                ))) |>
  # arrange and rank
  arrange(first_strength,
          cards_pos_1,
          cards_pos_2,
          cards_pos_3,
          cards_pos_4,
          cards_pos_5) |>
  mutate(multiplier = row_number(),
         bid_multiplied = as.numeric(bid) * multiplier)

# answer
sum(df_cards$bid_multiplied)

# 252656917

# Part two ----------------------------------------------------------------

# answer
