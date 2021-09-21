library(tidyverse)
library(lubridate)
library(RcppRoll)
library(stargazer)
library(tseries)
library(tidytext)

# Read the sentiment data and market data
daily_sentiment <- read_csv('../data/daily_sentiment_file.csv')
market_data <- read_csv('../data/sp500_market_data.csv')
news_df <- read_csv('../data/news_master_file.csv')

# Calculate how many news per ric per day -> to get no news days
news <- news_df %>% mutate(date = as_date(versionCreated)) %>%
  select(date, everything()) %>% 
  group_by(ric, date) %>% tally() %>% rename(news = n)


# daily_sentiment <- daily_sentiment %>% rename(se = se_neg) %>% select(-se_all, -se_np)
market_data <- market_data %>% rename(
  iv30 = ivol_30,
  iv60 = ivol_60,
  iv90 = ivol_90,
  ret = return,
  vol = volume
  
)

# Join the sentiment data to market data and clean the dataset
main_df <- daily_sentiment %>% 
  right_join(market_data, by = c('date', 'ric')) %>% 
  select(-co_conm, -co_cusip, -from, -thru, -co_tic) %>% 
  select(date, ric, co_sic, ret, vol, iv30, iv60, iv90, everything())

# Join the number of news
main_df <- main_df %>% left_join(news)
main_df <- main_df %>% mutate(news = replace_na(news, 0))

# Sort based on ric and date
main_df <- main_df %>% arrange(ric, date)

# Due to merging, there are now some dates without sentiment that are 'no-news' days
main_df <- main_df %>% mutate_at(c('total_words', 'se_all', 'se_neg', 'se_np'), replace_na, 0)

# Calculate volatility, 30 day volatility equals 20 trading days
main_df <- main_df %>% group_by(ric) %>% 
  mutate(v30 = roll_sdr(ret, n = 20, na.rm = T) * sqrt(252)) %>%
  ungroup()

main_df <- main_df %>% mutate(se_neg = se_neg * -1)
write_csv(main_df, '../data/main_data.csv')
