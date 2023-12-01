# ---------------- #
# Day 01 #
# ---------------- #

library(tidyverse)

# Part one ----------------------------------------------------------------
# extracting digits
# then first and last
df_digits <- tibble(string = readLines("2023/inputs/day01.txt")) |>
  mutate(
    digits = str_extract_all(string, "\\d"),
    first_digit = map_chr(digits, \(x)pluck(x, 1)),
    last_digit = map_chr(digits, \(x)pluck(x,-1)),
    calibration_value = as.numeric(paste0(first_digit, last_digit))
  )

# answer
sum(df_digits$calibration_value)

# 54968

# Part two ----------------------------------------------------------------
# function to extract digit
extract_digit <- function(string) {
  df_digits <- map_dfr(
    c(
      # to avoid having to get the digit again
      "1",
      "2",
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "nine"
    ),
    \(x)tibble(pattern = x, detect = str_locate_all(string, x))
  ) |>
    unnest(detect) |>
    mutate(pattern = str_replace_all(
      pattern,
      c(
        "one" = "1",
        "two" = "2",
        "three" = "3",
        "four" = "4",
        "five" = "5",
        "six" = "6",
        "seven" = "7",
        "eight" = "8",
        "nine" = "9"
      )
    ))
  
  # min and max
  tibble(
    first_digit = df_digits |>
      filter(min(detect[, 1]) == detect[, 1]) |>
      pull(pattern),
    last_digit = df_digits |>
      filter(max(detect[, 1]) == detect[, 1]) |>
      pull(pattern)
  )
}

# apply
df_digits_corr <- df_digits |>
  select(string) |>
  mutate(digits_pos = map(string, extract_digit)) |>
  unnest_wider(digits_pos) |>
  mutate(calibration_value = as.numeric(paste0(first_digit, last_digit)))

# answer
sum(df_digits_corr$calibration_value)

# 54094
