
html <- url %>% xml2::read_html()
sched_raw <- espn2::get_scores_nfl(year = 2006, limit = 500, as = "minimal")


temp <-
  sched_raw %>%
  .separate_cols_max_espn_at(col = "name")
calendar_raw <-
  temp %>%
  filter(name1 == "leagues" & name2 == "calendar") %>%
  filter(
    name3 == "label" |
      (name3 == "entries" &
         name4 == "label") |
      (name3 == "entries"  & name4 %in% c("startDate", "endDate"))
  )

calendar_raw