source('tt_fnc.R')

# read data -----------------------------------------------------------------------------------
euro_votes = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision-votes.csv')
euro = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')


# exploration ---------------------------------------------------------------------------------
## euro ----------------------------------------------------------------------------------------
# final rounds 1956 - 2003
# semi/grand final separation begins 2004
# 2020 grand final outlier of only 6 songs
euro |> group_by(year, section) |> summarize(n = n()) |> print(n=Inf)

## euro votes ----------------------------------------------------------------------------------
# duplicates have 0 votes
euro_votes |> filter(duplicate == 'x') |> pull(points) |> summary()

# televoting began in 2016 | jury data exists for all years
euro_votes |> 
  group_by(jury_or_televoting) |>
  summarize(
            min_year = min(year, na.rm = TRUE),
            max_year = max(year, na.rm = TRUE)
           )

euro_votes |>
  select(from_country, year) |> 
  distinct() |> 
  group_by(from_country) |> 
  summarize(n = n()) |> 
  arrange(desc(n)) |> 
  print(n=Inf)

# cleaning ------------------------------------------------------------------------------------
# emoji_links = euro |> 
#   select(artist_country, country_emoji) |> 
#   distinct() |>
#   mutate(emoji_lookup = str_c('flag-', str_to_lower(artist_country))) |> 
#   mutate(emoji_lookup = str_squish(emoji_lookup)) |>
#   group_by(row_number()) |> 
#   mutate(url = emoji_to_link(emoji_lookup),
#          emoji_label = link_to_img(url))

# fwrite(emoji_links, 'emoji_links.csv')

emoji_links = fread('2022/W20/emoji_links.csv') |> 
  mutate(emoji_label = link_to_img(url))

euro_cleaned = euro |>
  left_join(emoji_links)

world = map_data('world')
euro_map = euro_cleaned |> 
  left_join(world |> select(region, lat, long),
            by = c('artist_country' = 'region'))

# plots ---------------------------------------------------------------------------------------
## world map -----------------------------------------------------------------------------------------
world_map = ggplot() +
  geom_map(
    data = euro_map,
    map = world,
    aes(long, lat, map_id = artist_country, fill = total_points)
  ) +
  scale_fill_viridis() + 
  theme_void()

ggsave(plot = world_map,
       filename = '2022/W20/viz/map_votes.png')

# bar plot ------------------------------------------------------------------------------------
bar_plot_2022_final = euro_cleaned |>
  filter(year == '2022' & section == 'grand-final') |>
  ggplot(aes(x = total_points,
             y = reorder(artist_country, (total_points)),
             fill = total_points
            )
       ) + 
  geom_bar(stat = 'identity') + 
  geom_richtext(aes(label=emoji_label), fill = 'white',  label.size = NA, x=-25) +
  scale_x_continuous(limits = c(-30, 650)) +
  labs(title = '2022 Grand Final Results',
       subtitle = 'Eurovision - Turin, Italy',
       y = '') +
  theme_pubr() + 
  theme(legend.position = 'none')

bar_plot_2022_final

ggsave(plot = bar_plot_2022_final,
       filename = '2022/W20/viz/bar_plot_2022_final.png',
       height = '10',
       width = '8',
       dpi = 600)
