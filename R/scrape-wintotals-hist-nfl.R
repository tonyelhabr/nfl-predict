


path_dl <-
  file.path("data-raw", paste0(basename(.URL_WINTOTALS_NFL_SPORTSODDSHISTORY), ".html"))
# download.file(url = .URL_WINTOTALS_NFL_SPORTSODDSHISTORY, path_dl)
data <- do_get_wintotals_nfl_sportsoddshistory(path = path_dl)
data %>% teproj::export_path(config$path_wintotals_hist_nfl_sportsoddshistory)

juju_games <-
  readr::read_csv(
    "https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/legacy_data/season_play_by_play/pbp_2017.csv"
  ) %>%
  # Filter down only to the pass attempts to JuJu based on his GSIS ID 00-0033857:
  filter(Receiver_ID == "00-0033857",
         PassAttempt == 1) %>%
  # Only select the GameID and PassOutcome columns:
  select(GameID, PassOutcome) %>%
  # Calculate the number of receptions, targets, and catch rate in each game:
  group_by(GameID) %>%
  summarise(
    receptions = length(which(PassOutcome == "Complete")),
    targets = n(),
    catch_rate = receptions / targets
  ) %>%
  # Calculate cumulative stats:
  mutate(
    total_receptions = cumsum(receptions),
    total_targets = cumsum(targets),
    total_catch_rate = total_receptions / total_targets,
    # Columns to be used later:
    index = 1:n(),
    game_index = paste("game_", index, sep = ""),
    game_index = fct_relevel(
      factor(game_index),
      "game_1",
      "game_2",
      "game_3",
      "game_4",
      "game_5",
      "game_6",
      "game_7",
      "game_8",
      "game_9",
      "game_10",
      "game_11",
      "game_12",
      "game_13"
    )
  )
juju_games
p_grid <- seq(from = 0, to = 1, by = .05)
prior <- rep(1, 21)
likelihood <- dbinom(x = juju_games$receptions[1],
                     size = juju_games$targets[1],
                     prob = p_grid)
likelihood
bayes_numerator <- likelihood * prior
bayes_numerator
posterior <- bayes_numerator / sum(bayes_numerator)
posterior

game_posteriors <-
  map_dfc(c(1:nrow(juju_games)),
          function(x) {
            p_grid <- seq(from = 0, to = 1, by = .05)
            prior <- rep(1, 21)
            likelihood <- dbinom(
              x = juju_games$total_receptions[x],
              size = juju_games$total_targets[x],
              prob = p_grid
            )
            bayes_numerator <- likelihood * prior
            posterior <- bayes_numerator / sum(bayes_numerator)
            # Return this as a data frame:
            result <- data.frame(posterior)
            colnames(result) <- paste("game_", x, sep = "")
            return(result)
          })
game_posteriors
game_posteriors_bayes <-
  data.frame(p_grid = p_grid, prior = rep(1 / 21, 21)) %>%
  bind_cols(game_posteriors) %>%
  # Gather the columns so the data is long, one row for each week and grid value
  gather(key = "game_index", value = "posterior_prob", -p_grid) %>%
  # Relevel the game_index variable:
  mutate(
    game_index = fct_relevel(
      factor(game_index),
      "prior",
      "game_1",
      "game_2",
      "game_3",
      "game_4",
      "game_5",
      "game_6",
      "game_7",
      "game_8",
      "game_9",
      "game_10",
      "game_11",
      "game_12",
      "game_13"
    )
  )
game_posteriors_bayes


wintotals_hist_nfl <-
  config$path_wintotals_hist_nfl_sportsoddshistory %>%
  teproj::import_path_cleanly()
wintotals_hist_nfl

n_gm_per_season <- 16L
n_w_sportsbook <- 7L
n_w <- 5L
prob <- n_w_sportsbook / n_gm_per_season
probs <- rep(prob, n_gm_per_season)
dbinom(x = n_w, size = n_gm_per_season, prob = probs)
rbinom(n_gm_per_season, size = n_gm_per_season, prob = prob)
dbinom(6, size = 8, prob = 0.5)
pbinom(6, size = 8, prob = 0.5)
?binom.test
binom.test(6, n = 8, p = 0.5)
binom.test(12, n = 16, p = 0.4375)
