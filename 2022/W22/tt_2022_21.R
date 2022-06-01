# setup ---------------------------------------------------------------------------------------
source('tt_fnc.R')
Make_Folder(getwd(), '2022/W22/viz/')

# read data ----------------------------------------------------------------------------------------
poll = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')

# explore ------------------------------------------------------------------------------------------

## poll ----------------------------------------------------------------------------------------
poll |> head(20)
summary(poll)

# 100 companies w/ 5 years of historical data
Describe_Char_Cols(poll)

# 19 industries w/ heavy weight on Retail + Tech
Viz_Count_Bar(poll, 'industry', x_expansion = 1.05)

## reputation ----------------------------------------------------------------------------------



# clean --------------------------------------------------------------------------------------------
# poll |> pull(industry) |> unique() |> clipr::write_clip()
industry_groups = readxl::read_excel('2022/W22/helpers/industry_groups.xlsx')

poll_clean = poll |> 
  group_by(company) |> 
  mutate(company_id = str_pad(cur_group_id(), width = 3, pad = '0')) |> 
  ungroup() |> 
  left_join(industry_groups, by = 'industry')

poll_reshape = poll_clean |>
  select(-year, -rank, -rq, -change) |> 
  rename(rank = `2022_rank`,
         rq = `2022_rq`) |>
  mutate(year = '2022') |> 
  distinct() |> 
  plyr::rbind.fill(poll_clean |> select(-`2022_rq`, `2022_rank`) |> filter(!is.na(rq)))

poll_reshape |> filter(is.na(year)) |> View()


# viz ----------------------------------------------------------------------------------------------
## score by industry group + year ---------------------------------------------------------------------

industry_year = ggplot(poll_reshape, aes(x = rq, y = year, fill = industry_group)) + 
  geom_boxplot() + 
  geom_point(aes(x = rq), color = 'black') +
  facet_wrap(~industry_group, ncol = 1) + 
  labs(y = '') + 
  theme_pubr() + 
  theme(legend.position = 'none')
    
ggsave(plot = industry_year, filename = '2022/W22/viz/rq_industry_year.png', width = 9, height = 16, dpi = 600)
