library(tidyverse)
library(data.table)
library(ggpubr)
library(diffobj)
library(lubridate)
library(tidytext)
library(extrafont)

# setup ---------------------------------------------------------------------------------------
windowsFonts('Cambria' = windowsFont('Cambria'))
windowsFonts('Futura' = windowsFont('Futura'))

loadfonts(device = "win")
setwd(dirname(rstudioapi::getSourceEditorContext()[['path']]))

# read data -----------------------------------------------------------------------------------
nyt_full = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')
nyt_title = fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')


# combine & clean -----------------------------------------------------------------------------
nyt = nyt_full |>
  left_join(nyt_title |> select(id, total_weeks, first_week, debut_rank, best_rank), by = c('title_id' = 'id'))


nyt_str_fix = nyt |>
  select(author, title) |>
  distinct() |>
  rename(author_orig = author,
         title_orig = title) |>
  mutate(author_blank = str_squish(author_orig) == '' & str_detect(title_orig, 'by')) |>
  group_by(row_number()) |>
  mutate(author = ifelse(author_blank,
                          str_match_all(title_orig, ',  by (.*) \\(')[[1]][,2],
                          author_orig
                        )
        ) |>
  mutate(title = ifelse(author_blank,
                        str_match_all(title_orig, '(.*),  by')[[1]][,2],
                        title_orig
                       )
        ) |>
  ungroup() |>
  mutate(across(c('author', 'title'), str_to_upper)) |>
  mutate(across(c('author', 'title'), ~str_replace_all(., '[[:punct:]]', ''))) |>
  mutate(across(c('author', 'title'), ~iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT'))) |>
  mutate(author = str_replace_all(author, '.*\\bBY\\b', '')) |>
  mutate(across(c('author', 'title'), str_squish))


nyt_cleaned = nyt |>
  rename(author_orig = author,
         title_orig = title
        ) |>
  left_join(nyt_str_fix |>
              select(author_orig, author, title_orig, title),
            by = c('author_orig', 'title_orig')) |>
  mutate(Decade = floor(year/10) * 10) |>
  mutate(Author_Count = str_detect(author, '\\bAND\\b') + 1) |>
  group_by(title_id) |> 
  mutate(Debut_Best_Diff = max(debut_rank) - min(best_rank)) |>
  mutate(Blockbuster = max(debut_rank) == max(best_rank))


# exploratory plots
# highest rank by decade

# sentiment analysis
# TODO: figure out way to identify adverbs and convert them to base verbs to improve match rate
# TODO: identify mismatches like MISS MARY => negative(MISS)
nyt_sentiment = nyt_cleaned |>
  select(title, title_id) |>
  distinct() |>
  separate(title, into = paste0('word', seq(1:100)), sep = ' ', remove = FALSE) |>
  pivot_longer(!c(title, title_id)) |>
  filter(!is.na(value)) |>
  select(-name) |>
  rename(word = value) |>
  left_join(get_sentiments('bing') |>
              mutate(word = str_to_upper(word))) |>
  group_by(title_id) |>
  mutate(pos_total = sum(sentiment == 'positive', na.rm = TRUE),
         neg_total = sum(sentiment == 'negative', na.rm = TRUE)) |>
  mutate(pos_ratio = pos_total / n(),
         neg_ratio = neg_total / n()) |>
  mutate(overall_sentiment = case_when(pos_total > neg_total ~ 'positive',
                                       neg_total > pos_total ~ 'negative',
                                       TRUE ~ 'neutral'))


nyt_all = nyt_cleaned |> 
  left_join(nyt_sentiment |>
              select(title_id, contains('pos'), contains('neg'), overall_sentiment) |> 
              distinct(),
            by = 'title_id')



# blockbuster ---------------------------------------------------------------------------------

nyt_all |> 
  select(title_id, title, Decade, Blockbuster) |>
  mutate(Decade = as.character(Decade)) |> 
  distinct() |> 
  group_by(Decade, Blockbuster) |> 
  summarize(n = n()) |>
  group_by(Decade) |> 
  mutate(pct = (n / sum(n)) * 100) |> 
  filter(Blockbuster) |> 
  # mutate(pct = str_c(round(pct * 100, 2), '%')) |> 
  ggplot(aes(x = Decade, y = pct)) + 
  geom_bar(stat='identity', fill = '#2ac2d5') +
  geom_text(aes(label=str_c(round(pct,2), '%\nn=', n)), position = position_dodge(width=1)) +
  labs(title = 'Blockbusted',
       subtitle = 'percentage of books that debuted at their best rank, by decade',
       y = '%') +
  theme_pubr() + 
  theme(legend.position = 'none')

# sleeper hits --------------------------------------------------------------------------------
# TODO: make line plot of top 10 sleepers





# sentiment comparison ------------------------------------------------------------------------
nyt_all |> 
  select(title_id, title, Decade, overall_sentiment) |>
  mutate(Decade = as.character(Decade)) |> 
  distinct() |> 
  group_by(Decade, overall_sentiment) |> 
  summarize(n = n()) |>
  group_by(Decade) |> 
  mutate(pct = (n / sum(n)) * 100) |>
  filter(overall_sentiment == 'negative') |> 
  ggplot(aes(x = Decade, y = pct, fill = 'salmon')) + 
  geom_bar(position = 'dodge', stat='identity') +
  labs(title = 'Negativity Rising',
       subtitle = 'percentage of titles with primarily negative sentiment, by decade',
       y = '%') +
  geom_text(aes(label=str_c(round(pct,2),'%\nn=',n)),
            position = position_dodge(1),
            vjust = -0.25,
            size = 6, 
            color = 'gray20', 
            family = 'Futura') +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  theme_pubr() + 
  theme(legend.position = 'none') + 
  theme(plot.title = element_text(size=20),
        plot.subtitle = element_text(size=15))
