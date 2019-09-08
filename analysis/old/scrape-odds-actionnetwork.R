
library("tidyverse")
# url <-
#   "https://api.actionnetwork.com/web/v1/scoreboard/nfl?"
# args <-
#   list(
#     # bookids = c(15, 34, 3, 55, 1),
#     bookids = "15,32,3,55,1",
#     date = 20180908
#   )
# httr::modify_url(url = url, query = args)
# resp <- httr::GET(url = url, query = args)

resp <- httr::GET(url = "https://api.actionnetwork.com/web/v1/scoreboard/nfl?bookIds=15,34,3,55,1&date=20180913")
resp
cont <- httr::content(resp, as = "parsed")
# cont

# json ----
# j <-
#   jsonlite::fromJSON(rawToChar(resp$content))
# j
# 
# sapply(j, is.data.frame)
# j$games[sapply(j$games, is.list)] -> y
# j$games$odds %>% purrr::map_dfr(rbind) %>% as_tibble() -> z
# z
# z %>% distinct()
# 
# for(e in j$games) {
#   cat(e, sep = "\n")
#   # cat(names(e), sep = "\n")
# }
# 
# cont[is.list(cont)]
# sapply(cont$games, is.list)

# manual ----
.separate_name_col_at <-
  function(data,
           col = "name",
           cols_out = c(col, paste0("idx_", col)),
           sep = "\\.(?=[0-9]{2})",
           remove = FALSE,
           ...) {
    col_sym <- sym(col)
    # col_out1_sym <- sym(cols_out[1])
    col_out2_sym <- sym(cols_out[2])
    data %>%
      separate(!!col_sym,
               into = cols_out,
               sep = sep,
               remove = remove,
               ...) %>%
      mutate_at(vars(!!col_out2_sym), funs(as.integer))
  }

.add_time_col_at <-
  function(data, col = "games.odds.inserted", col_out = "timestamp") {
    col_sym <- sym(col)
    col_out_sym <- sym(col)
    data %>%
      mutate(!!col_out_sym  := str_replace_all(!!col_sym, "[0-9]{3}\\+.*", "")) %>% 
      mutate_at(vars(!!col_out_sym), funs(lubridate::ymd_hms))
  }

.clean_time_col_at <-
  purrr::partial(.add_time_col_at, col_out = col)

data <-
  .convert_list_to_tbl(cont) %>% 
  mutate(rn = row_number())

data_long0 <-
  data %>%
  filter(!str_detect(name, "^league")) %>% 
  select(-matches("rn")) %>% 
  mutate(grp = if_else(name == "games.id", value, NA_character_)) %>% 
  fill(grp) %>% 
  group_by(grp, name) %>% 
  mutate(n = n(), grp_rn = row_number()) %>% 
  ungroup() %>% 
  rename(name_orig = name) %>% 
  mutate(name = if_else(n > 1, paste0(name_orig, ".", sprintf("%02.0f", grp_rn)), name_orig)) %>% 
  ungroup() %>% 
  select(grp, name, name_orig, value)
data_long0

# NOTE: Need to programmatically identify this value, but it seems like it should always be `23` for this data source.
# rgx_max <- "[.]23$"
nms_max <-
  data_long0 %>%
  .separate_name_col_at() %>% 
  select(-value, -grp) %>% 
  filter(!is.na(idx_name)) %>% 
  group_by(name) %>% 
  filter(idx_name == max(idx_name)) %>% 
  ungroup() %>% 
  arrange(desc(idx_name)) %>% 
  filter(idx_name == max(idx_name))

nms_max
nms_max %>% filter(str_detect(name, "^.*(?!odds).*$"))
# rgx_max <- nms_max %>% slice(1) %>% pull(idx_name) %>% paste0("[.]", .)
# rgx_nms_max <-
#   paste0("(^", paste0(nms_max, ".[0-9]", collapse = ")|(^"), ")") %>% 
#   str_replace_all("\\.", "[.]")
# rgx_nms_max

data_wide_max <-
  data_long0 %>%
  # filter(str_detect(name, rgx_nms_max)) %>% 
  semi_join(nms_max %>% select(name_orig)) %>% 
  select(-name_orig) %>% 
  .separate_name_col_at(col = "name") %>% 
  rename(idx_grp = idx_name) %>% 
  spread(name, value, drop = FALSE) %>% 
  .add_time_col_at() %>% 
  group_by(grp) %>% 
  mutate_at(vars(idx_grp), funs(row_number(games.odds.inserted))) %>%
  ungroup() %>% 
  arrange(games.odds.inserted, games.odds.book_id, grp)
data_wide_max

# Debugging...
data_wide_max %>% count(grp)
data_wide_max %>% count(idx_grp)

data_wide_nomax <-
  data_long0 %>%
  filter(!str_detect(name, rgx_nms_max)) %>% 
  spread(name, value)
data_wide_nomax %>% 
  select(matches("odds.ml_"))

# old ----
data_wide <-
  data_long0 %>%
  spread(name, value)

data_wide %>% glimpse()

data_wide_t <-
  data_wide %>%
  gather(name, value, -grp) %>% 
  spread(grp, value)
data_wide_t

data_wide_and_long <-
  data_wide %>%
  gather(name, value, matches("[0-9]{2}$")) %>% 
  select(grp, name, value, everything()) %>% 
  .separate_name_col_at(col = "name") %>% 
  spread(name, value) %>% 
  rename(idx_grp = idx_name)

data_wide_and_long %>% 
  arrange(grp, idx_grp) %>% 
  select(grp, idx_grp, games.teams.abbr, games.odds.inserted)


data_wide_and_long %>%
  select(grp, idx_grp, games.odds.inserted) %>% 
  .add_time_col_at() %>% 
  arrange(timestamp, grp)

data_wide_and_long %>% 
  filter(idx_name == max(idx_name))

# data_wide_noupdate <-
#   data_wide_t %>%
#   filter(!str_detect(name, "[0-9]$")) %>% 
#   gather(grp, value, -name) %>% 
#   spread(name, value)
# data_wide_noupdate

data_wide_t_update <-
  data_wide_t %>%
  filter(str_detect(name, "[0-9]$"))
data_wide_t_update

name_sep <-
  data_wide_t_update %>%
  select(name) %>% 
  .separate_name_col_at()
name_sep
name_sep_max <-
  name_sep %>% 
  group_by(grp) %>% 
  summarise_at(vars(idx_name), funs(max)) %>% 
  ungroup()
name_sep_max %>% count(idx_name)

# # games.odds.[xxx] <- Completed games?
# name_sep_max %>% filter(idx_name == 23)
# # games.odds.ml_[home/away] <- Completed games?
# name_sep_max %>% filter(idx_name == 18)
# # league.calendar_info.reg.[xxx]
# name_sep_max %>% filter(idx_name == 17)
# # league.calendar_info.[post/pre].[xxx]
# name_sep_max %>% filter(idx_name == 5)
# # games.boxscore.linescore.[xxx], games.value_stats.[home/away]_value_breakdown.[type/value]
# name_sep_max %>% filter(idx_name == 4)
# # games.teams.[xxx]
# name_sep_max %>% filter(idx_name == 2)

data_wide_t_update %>% 
  gather(grp, value, -name) %>% 
  .split_name_col_at()

# tidy way ----
data0 <-
  cont %>% 
  .convert_list_to_tbl_cleanly_at() %>% 
  mutate(idx = row_number()) %>% 
  select(idx, everything())

summ1 <-
  data0 %>%
  filter(is.na(name2)) %>% 
  count(name1, sort = TRUE)
summ1

summ2 <-
  data0 %>%
  filter(is.na(name3)) %>% 
  count(name1, name2, sort = TRUE)
summ2

summ3 <-
  data0 %>%
  filter(is.na(name4)) %>% 
  count(name1, name2, name3, sort = TRUE)
summ3

summ4 <-
  data0 %>%
  count(name1, name2, name3, name4, sort = TRUE)
summ4

summ3 %>%
  inner_join(data0 %>% select(-name4, -value) %>% distinct())
data0 %>% select(-name4, -value) %>% distinct() %>% inner_join(summ3)

val_top <- data0 %>% slice(1) %>% pull(name2)
data0 %>%
  filter(is.na(name3)) %>% 
  mutate(isgrp = if_else(name2 == val_top & is.na(name3), TRUE, FALSE)) %>% 
  filter(isgrp) %>% 
  mutate(grp = row_number(idx)) %>% 
  left_join(data0) %>% 
  fill(grp) %>% 
  group_by(grp) %>% 
  select(grp, name2, value) %>% 
  spread(name2, value)

data0 %>%
  filter(str_detect(name3, "spread_[a|h]"))
data0 %>%
  filter(!is.na(name4)) %>% 
  count(name3, name4, sort = TRUE)
