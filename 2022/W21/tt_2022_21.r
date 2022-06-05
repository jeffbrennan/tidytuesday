source('tt_fnc.R')

# read data -----------------------------------------------------------------------------------
sevens = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/sevens.csv')
fifteens = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/fifteens.csv')


# explore -------------------------------------------------------------------------------------
## sevens --------------------------------------------------------------------------------------
Describe_Char_Cols(sevens)

sevens |> filter(score_1 %in% c('L', 'W', '?')) |> View()



## fifteens ------------------------------------------------------------------------------------




# clean ---------------------------------------------------------------------------------------

# TODO: add continent / country lookup based on venue name
sevens_clean = sevens |> 
  mutate(across(contains('score'), as.integer)) |> 
  select(-row_id, -notes) |>
  group_by(date, team_1, team_2, venue, tournament, stage) |> 
  mutate(game_id = cur_group_id()) |> 
  ungroup()

fifteens_clean = fifteens |> 
  select(-test_no, -home_away_win) |> 
  rename(t1_game_no = home_test_no,
         t2_game_no = away_test_no,
         series = series_no,
         margin = margin_of_victory) |> 
  group_by(date, team_1, team_2, venue, series) |> 
  mutate(game_id = cur_group_id()) |> 
  ungroup()


sevens_clean |> group_by(game_id) |> filter(n()>1) |> View()

## combine -------------------------------------------------------------------------------------
results = DF_Combine_Diagnostics(sevens_clean, fifteens_clean)


combined_df = sevens_clean |> 
  mutate(game_type = 'sevens') |> 
  plyr::rbind.fill(fifteens_clean |> mutate(game_type = 'fifteens')) |> 
  group_by(team_1) |> 
  arrange(date) |> 
  mutate(t1_num_games = row_number()) |> 
  group_by(team_2) |> 
  arrange(date) |> 
  mutate(t2_num_games = row_number()) |> 
  mutate(game_num_diff = abs(t1_num_games - t2_num_games)) |> 
  ungroup()

combined_df = sevens_clean |> 
  mutate(game_type = 'sevens') |> 
  plyr::rbind.fill(fifteens_clean |> mutate(game_type = 'fifteens')) |> 
  group_by(team_1) |> 
  arrange(date) |> 
  mutate(t1_num_games = row_number()) |> 
  group_by(team_2) |> 
  arrange(date) |> 
  mutate(t2_num_games = row_number()) |> 
  mutate(game_num_diff = abs(t1_num_games - t2_num_games)) |> 
  ungroup()

combined_df_long = combined_df |> 
  mutate(across(everything(), as.character)) |> 
  pivot_longer(names(combined_df)[str_detect(names(combined_df), '\\d{1}')]) |> 
  mutate(team_num = str_detect(name, '1')) |> 
  group_by(row_number()) |> 
  # couldnt get case_when() to work
  mutate(game_result = ifelse(value == winner,
                            'winner',
                            ifelse(value == loser,
                                   'loser',
                                   NA
                                  )
                            )
        ) |>         
  group_by(game_id, team_num) |> 
  tidyr::fill(game_result, .direction = 'updown')


# viz -----------------------------------------------------------------------------------------

## margin over time ----------------------------------------------------------------------------
combined_df |>
  mutate(year_month = str_c(as.character(year(date)),
                           '-',
                           as.character(month(date)))) |> 
  group_by(year_month, game_type) |> 
  summarize(margin = mean(margin, na.rm = TRUE)) |>
  ungroup()  |> 
  mutate(date = as.Date(str_c(year_month, '-01'), '%Y-%m-%d')) |> 
  ggplot(aes(x = date, y = margin, color = game_type)) +
  geom_point() + 
  geom_line() + 
  geom_smooth() + 
  facet_wrap(~game_type, ncol = 1) + 
  theme_pubr()

combined_df |>
  mutate(year = year(date)) |> 
  mutate(decade = floor(year/10)*10) |>
  mutate(year5 = ifelse(year %>% as.character() %>% substring(4) %>% as.integer() < 5,
                        decade,
                        as.integer(str_c(substring(decade, 1, 3), '5'))
  )
  ) |> 
  group_by(year5, game_type) |> 
  # summarize(margin = mean(margin, na.rm = TRUE)) |> 
  ungroup()  |> 
  # mutate(date = as.Date(str_c(year_month, '-01'), '%Y-%m-%d')) |> 
  ggplot(aes(x = as.character(decade), y = margin)) +
  geom_boxplot(fill = 'white') +
  geom_violin(aes(fill = game_type)) + 
  geom_jitter(width= 0.05, alpha = 0.5) +
  facet_wrap(~game_type, ncol = 1, scales = 'free') + 
  stat_compare_means() +
  theme_pubr()

ggsave(filename = '2022/W21/viz/margin_by_year.png', width = 6, height = 9)


# home turf advantage -------------------------------------------------------------------------


# correlation between margin & experience -----------------------------------------------------
  ggplot(combined_df) + 
  aes(x = game_num_diff, y = margin) + 
  geom_point() + 
  facet_wrap(~game_type) +
  geom_smooth() + 
  theme_pubr()






