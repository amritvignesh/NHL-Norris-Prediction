library(hockeyR)
library(dplyr)
library(vip)
library(gt)

og_stats <- data.frame()

for (year in 2015:2024) {
  szn_stats <- get_skater_stats_hr(year) 
  og_stats <- rbind(og_stats, szn_stats)
}

stats <- og_stats %>%
  filter(position == "D") %>%
  group_by(season, player) %>%
  summarize(season, player, team = paste(team_abbr, collapse = ", "), games = sum(games_played), goals = sum(goals), assists = sum(assists), pt_shares = sum(hr_point_shares), plus_minus = sum(plus_minus), shots = sum(shots_on_goal), time = sum(time_on_ice), blocks = sum(blocks), hits = sum(hits)) %>%
  distinct(season, player, .keep_all = TRUE) 

stats <- stats %>%
  mutate(norris = 0) %>%
  group_by(season) %>%
  mutate(game_pct = games/max(games)) %>%
  ungroup() %>%
  filter(game_pct >= 0.5)

stats$norris[which(stats$season == "2014-15" & stats$player == "Erik Karlsson")] <- 1
stats$norris[which(stats$season == "2015-16" & stats$player == "Drew Doughty")] <- 1
stats$norris[which(stats$season == "2016-17" & stats$player == "Brent Burns")] <- 1
stats$norris[which(stats$season == "2017-18" & stats$player == "Victor Hedman")] <- 1
stats$norris[which(stats$season == "2018-19" & stats$player == "Mark Giordano")] <- 1
stats$norris[which(stats$season == "2019-20" & stats$player == "Roman Josi")] <- 1
stats$norris[which(stats$season == "2020-21" & stats$player == "Adam Fox")] <- 1
stats$norris[which(stats$season == "2021-22" & stats$player == "Cale Makar")] <- 1
stats$norris[which(stats$season == "2022-23" & stats$player == "Erik Karlsson")] <- 1

stats_train <- stats %>% filter(season != "2023-24")
stats_test <- stats %>% filter(season == "2023-24")

norris_reg <- glm(norris ~ goals + assists + pt_shares + plus_minus + shots + blocks + hits, data = stats_train, family = "binomial")

stats_train <- stats_train %>%
  ungroup() %>%
  mutate(prediction = predict(norris_reg, stats_train, type = "response")) %>%
  group_by(season) %>%
  mutate(award_prob = prediction/sum(prediction)) %>%
  mutate(award_won = ifelse(norris == 1, "WON", "")) %>%
  ungroup() 

stats_test <- stats_test %>%
  ungroup() %>%
  mutate(prediction = predict(norris_reg, stats_test, type = "response")) %>%
  mutate(award_prob = prediction/sum(prediction)) %>%
  ungroup() 

final_data_top15_train <- stats_train %>%
  group_by(season) %>%
  arrange(-award_prob) %>%
  filter(row_number() <= 15) %>%
  mutate(award_prob = round(award_prob, 3)) %>%
  select(season, player, team, award_prob, award_won) %>%
  ungroup()

final_data_top15_2024 <- stats_test %>%
  arrange(-award_prob) %>%
  filter(row_number() <= 15) %>%
  mutate(award_prob = round(award_prob, 3)) %>%
  select(player, team, award_prob)

vip(norris_reg)

subfolder_path <- "norris/"
dir.create(subfolder_path, showWarnings = FALSE)

szns <- unique(final_data_top15_train$season)

for (szn in szns) {
  per_year <- final_data_top15_train %>%
    filter(season == szn)
  table <- per_year %>% gt() %>% 
    cols_align(
      align = "center",
      columns = c(season, player, team, award_prob, award_won)
    ) %>%
    data_color(
      columns = award_prob,
      colors = scales::col_numeric(
        palette = paletteer::paletteer_d(
          palette = "ggsci::blue_material"
        ) %>% as.character(),
        domain = NULL
      )
    ) %>%
    cols_label(
      season = md("**Season**"),
      player = md("**Player**"),
      team = md("**Team**"),
      award_prob = md("**Norris Probability**"),
      award_won = md("**Norris Result**")
    ) 
  filename <- paste0(szn, "norris.png")
  gtsave(table, file.path(subfolder_path, filename))
}

table_2024 <- final_data_top15_2024 %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(player, team,  award_prob)
  ) %>%
  data_color(
    columns = award_prob,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    team = md("**Team**"),
    award_prob = md("**Norris Probability**"),
  ) %>%
  tab_header(
    title = md("**2023-24 NHL Norris Trophy Probability**"),
    subtitle = "Based on Norris NHL Data from 2014/15 - 2022/23"
  )
gtsave(table_2024, "norris/2023-24norris.png")



