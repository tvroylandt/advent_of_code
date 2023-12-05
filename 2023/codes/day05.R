# ---------------- #
# Day 05 #
# ---------------- #

library(tidyverse)

# Part one ---------------------------------------------------------------
df_almanac <-
  tibble(string = readLines("2023/inputs/day05.txt"))

# get list of seeds
df_seeds <- df_almanac |>
  slice(1) |>
  separate_longer_delim(string, delim = " ") |>
  filter(string != "seeds:") |>
  rename(seed = string) |>
  mutate(seed = as.numeric(seed))

# clean the rest
df_almanac_clean <- df_almanac |>
  slice(-1) |>
  mutate(categ = case_when(str_detect(string, ":") ~ string)) |>
  tidyr::fill(categ) |>
  filter(string != "" & string != categ) |>
  separate(string,
           into = c("dest_init", "source_init", "length"),
           sep = "\\s") |>
  separate(categ,
           into = c("source_type", "dest_type"),
           sep = "-to-") |>
  mutate(
    dest_type = str_remove(dest_type, " map:"),
    across(c("dest_init", "source_init", "length"), as.numeric),
    source_dest = pmap(
      list(source_init, dest_init, length),
      \(source_init, dest_init, length)tibble(
        source = seq(source_init, source_init + length - 1),
        dest = seq(dest_init, dest_init + length -
                     1)
      )
    )
  ) |>
  select(source_type, dest_type, source_dest)

# join everything - not subtle
filter_almanac <- function(data, source_type) {
  # filter
  df_almanac_filter <- df_almanac_clean |>
    filter(source_type == {
      {
        source_type
      }
    })
  
  # filter init list
  list_filter <- data |>
    pull({
      {
        source_type
      }
    })
  
  # rename
  dest_name <- unique(df_almanac_filter$dest_type)
  
  df_almanac_rename <- df_almanac_filter |>
    select(source_dest) |>
    mutate(source_dest = map(source_dest,
                             \(x)filter(x, source %in% list_filter))) |>
    unnest(source_dest) |>
    rename({
      {
        source_type
      }
    } := source,
    {
      {
        dest_name
      }
    } := dest)
  
  # join
  data |>
    left_join(df_almanac_rename, by = source_type) |>
    mutate({
      {
        dest_name
      }
    } := coalesce(!!sym(dest_name),!!sym(source_type)))
}

data_filtered <-
  df_seeds |>
  filter_almanac("seed") |>
  filter_almanac("soil") |>
  filter_almanac("fertilizer") |>
  filter_almanac("water") |>
  filter_almanac("light") |>
  filter_almanac("temperature") |>
  filter_almanac("humidity")

# answer
min(data_filtered$location)

# 196167384

# Part two ----------------------------------------------------------------
# get list of seeds again
df_seeds_bis <- df_seeds |>
  mutate(type = rep(c("seed", "length"), length(df_seeds$seed)/2)) |> 
  pivot_wider(names_from = type, values_from = seed) |> 
  unnest(c(seed, length)) |> 
  mutate(seed = map2(seed, length,
                     \(x, y)seq(x, x+y))) |> 
  select(-length) |> 
  unnest_longer(seed)

data_filtered_bis <-
  df_seeds_bis |>
  filter_almanac("seed") |>
  data_filtered_bis |> 
  filter_almanac("soil") |>
  filter_almanac("fertilizer") |>
  filter_almanac("water") |>
  filter_almanac("light") |>
  filter_almanac("temperature") |>
  filter_almanac("humidity")

# answer
min(data_filtered$location)

# not for today

