library(tidyverse)
library(lubridate)

master_path <- '../data/original/newscope-news/'
master_files <- list.files(master_path)
master_files <- paste(master_path, master_files, sep="")

additional_path <- '../data/original/additional_news/'
additional_files <- list.files(additional_path)
additional_files <- paste(additional_path, additional_files, sep="")

files <- append(master_files, additional_files)

main_df <- tibble()
count <- length(files)
i <- 1

for (file in files) {
  df <- read_csv(file, col_types = cols(
    versionCreated = col_datetime(format = ""),
    text = col_character(),
    storyId = col_character(),
    sourceCode = col_character()
  ))
  
  df <- df %>% mutate(
    ric = if_else(file %>% str_detect('../data/original/newscope-news/'),
                  file %>% str_remove('_news.csv') %>% str_remove('../data/original/newscope-news/'),
                  file %>% str_remove('_news.csv') %>% str_remove('../data/original/additional_news/'))
  )
  
  if (nrow(main_df) == 0) {
    main_df <- df
  } else {
    main_df <- main_df %>% union_all(df)
  }
  cat(i, '/', count, '\n')
  i <- i + 1
    
}

main_df <- main_df %>% distinct()

# Read sp500 file
sp500 <- read_csv('../data/sp500_constituents_rics.csv')

# Fix dates
sp500 <- sp500 %>% mutate(
  from = ymd(from),
  thru = ymd(thru)
)

start_of_period = ymd("2018-08-03")
end_of_period = ymd("2019-11-03")
sp500 <- sp500 %>% filter(from >= start_of_period | is.na(thru))
sp500 <- sp500 %>% select(from, thru, co_conm, co_tic, co_cusip, co_sic, RIC) %>% rename(ric = RIC)
sp500 <- sp500 %>% mutate(
  from = if_else(from <= start_of_period, start_of_period, from),
  thru = if_else(is.na(thru), end_of_period, thru)
)

df <- main_df %>% left_join(sp500)

df <- df %>% filter(versionCreated >= from & versionCreated <= thru + days(1))
write_csv(df, '../data/news_master_file.csv')