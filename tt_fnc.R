# data manipulation
library(tidyverse)
library(data.table)

library(glue)
library(lubridate)
library(diffobj)

# viz 
library(ggtext)
library(tidytext)
library(extrafont)
library(viridis)
library(ggpubr)

# scraping
library(rvest)

# functions -----------------------------------------------------------------------------------
emoji_to_link = function(x) {
  tryCatch({ 
    img_url = paste0("https://emojipedia.org/", x) |> 
      read_html() |> 
      html_node(xpath = '//*/img') |> 
      html_attr('src')
    
    return(img_url)
  },
  error = function(e) { 
    return(NA)
  })
}

link_to_img = function(x, size = 25) {
  paste0("<img src='", x, "' width='", size, "'/>")
}

Make_Folder = function(path, folder_name) { 
  dir.create(file.path(path, folder_name), showWarnings = FALSE)
}


# summary viz ---------------------------------------------------------------------------------
Describe_Char_Cols = function(df) {
  char_df = df |> select_if(is.character)
  char_cols = names(char_df)
  
  map(char_cols, ~df |>
        group_by(!!sym(.)) |>
        summarize(n = n()) |>
        arrange(desc(n)) |> 
        mutate(is_numeric = !is.na(as.numeric(!!sym(.)))) |> 
        View(.))
  
  df |> View('orig_df')
}

Viz_Count_Bar = function(df, char_var, x_expansion=1) { 
  
  summary_df = df |> 
    group_by(!!sym(char_var)) |> 
    summarize(n = n()) |> 
    mutate(across(!!sym(char_var), as.factor)) |> 
    ungroup() |> 
    mutate(pct = n / sum(n, na.rm = TRUE))
  
  viz = ggplot(summary_df, 
               aes(x = n,
                   y = reorder(!!sym(char_var), n)
                   )
               ) + 
    geom_bar(stat = 'identity', fill = 'dodgerblue') + 
    geom_text(aes(label = glue('{n} ({round(pct * 100, 2)}%)'), hjust=-0.1)) +
    coord_cartesian(xlim = c(min(summary_df$n),
                             max(summary_df$n, na.rm = TRUE) * x_expansion)) +
    labs(title = char_var,
         subtitle = glue('{nrow(summary_df)} groups'),
         y = '') + 
    theme_pubr()
  return(viz)
}

DF_Combine_Diagnostics = function(df1, df2) { 
  df1_name = deparse(substitute(df1))
  df1_missing = setdiff(names(df2), names(df1))
  
  df2_name = deparse(substitute(df2))
  df2_missing = setdiff(names(df1), names(df2))
  
  print(glue('{length(df1_missing)} cols in "{df1_name}" but not "{df2_name}"'))
  print(glue('{length(df2_missing)} cols in "{df2_name}" but not "{df1_name}"'))
  
  results =  list(
    'df1_results' = list('df_name' = df1_name,
                         'missing_cols' = df1_missing
    ),
    'df2_results' = list('df_name' = df2_name,
                         'missing_cols' = df2_missing
    )
  )
  return(results)
}


# setup ---------------------------------------------------------------------------------------
windowsFonts('Cambria' = windowsFont('Cambria'))
windowsFonts('Futura' = windowsFont('Futura'))
loadfonts(device = "win")
