library(tidyverse)
library(lubridate)
library(tidytext)
library(SentimentAnalysis)
library(ggwordcloud)
library(readxl)

# ========================== News ==========================

# Read news data 
news_df <- read_csv('../data/news_master_file.csv')
#news_df <- news_df %>% select(-co_cusip, -co_sic)

# Early closing days
early_closing <- read_excel('../data/original/early_closing.xlsx')
early_closing <- early_closing %>% mutate(date = as.Date(date))

news <- news_df %>%
  mutate(hour = hour(versionCreated),
         minute = minute(versionCreated)) %>%
  select(versionCreated, hour, minute, everything())

holidays <- news %>% mutate(date = as.Date(versionCreated)) %>% inner_join(early_closing)
regular_days <- news %>% mutate(date = as.Date(versionCreated)) %>% anti_join(early_closing)

before_closing <- holidays %>% 
  filter(hour < 13) %>% 
  select(versionCreated, date, everything(), -hour, -minute)

after_closing <- holidays %>% 
  filter(hour >= 13) %>% 
  mutate(date = date + days(1)) %>% 
  select(versionCreated, date, everything(), -hour, -minute)

holidays <- before_closing %>% bind_rows(after_closing)

# ==== Include after hours ====

sp500 <- read_csv('../data/exchange_info.csv')
sp500 <- sp500 %>% mutate(date = ymd(date))
sp500 <- sp500 %>% select(date, CUSIP, EXCHCD, TICKER) %>%
  rename(co_exc = EXCHCD,
         co_cusip = CUSIP) %>%
  group_by(co_cusip) %>% filter(date == max(date)) %>%
  select(-date, -TICKER)

# CRSP cusip does not have check sum
news <- news %>%
  mutate(co_cusip = str_sub(co_cusip, 0, -2))

news <- news %>% left_join(sp500)

" Exchange codes
    1 = NYSE
    3 = NASDAQ
    5 = NASDAQ
"

# Split to nyse and nasdaq
nyse <- news %>% filter(co_exc == 1 | is.na(co_exc))
nasdaq <- news %>% filter(co_exc != 1)

# NYSE
# News that happend during trading hours
nyse_during_trading <- nyse %>%
  filter(hour < 16) %>%
  mutate(date = as.Date(versionCreated)) %>%
  select(versionCreated, date, everything(), -hour, -minute)

# News that happend after 16:30
nyse_after_trading <- nyse %>%
  filter(hour >= 16) %>%
  mutate(date = as.Date(versionCreated + days(1))) %>%
  select(versionCreated, date, everything(), -hour, -minute)

nyse_df <- nyse_during_trading %>% bind_rows(nyse_after_trading)

# NASDAQ
# News that happend during trading hours
nasdaq_during_trading <- nasdaq %>%
  filter(hour < 20) %>%
  mutate(date = as.Date(versionCreated)) %>%
  select(versionCreated, date, everything(), -hour, -minute)

# News that happend after 20:00
nasdaq_after_trading <- nasdaq %>%
  filter(hour >= 20) %>%
  mutate(date = as.Date(versionCreated + days(1))) %>%
  select(versionCreated, date, everything(), -hour, -minute)

nasdaq_df <- nasdaq_during_trading %>% bind_rows(nasdaq_after_trading)

df <- nyse_df %>% bind_rows(nasdaq_df)


# ==== Include only core trading hours ====
# 
# # News that happend during trading hours
# during_trading <- regular_days %>%
#   filter(hour < 16) %>%
#   mutate(date = as.Date(versionCreated)) %>%
#   select(versionCreated, date, everything(), -hour, -minute)
# 
# # News that happend after 16:30
# after_trading <- regular_days %>%
#   filter(hour >= 16) %>%
#   mutate(date = as.Date(versionCreated + days(1))) %>%
#   select(versionCreated, date, everything(), -hour, -minute)
# 
# df <- during_trading %>% bind_rows(after_trading)
# df <- df %>% bind_rows(holidays)

# ==== Weekends ====
# weekends <- df %>% filter(weekdays(date) %in% c('Saturday', 'Sunday'))
# weekdays <- df %>% filter(!weekdays(date) %in% c('Saturday', 'Sunday'))
# 
# weekends <- weekends %>% 
#   mutate(date = if_else(weekdays(date) == 'Saturday', date + days(2), date + days(1)))
# 
# df <- weekends %>% bind_rows(weekdays)

# ==== Sentiment extraction ====

# Read sentiment wordlists Loughrain and Mcdonald
lm_positive <- tibble(DictionaryLM$positive) %>%
  mutate(sentiment = 1, dict = 'pos') %>%
  rename(word = `DictionaryLM$positive`)

lm_negative <- tibble(DictionaryLM$negative) %>% 
  mutate(sentiment = -1, dict = 'neg') %>% 
  rename(word = `DictionaryLM$negative`)

lm_uncertain <- tibble(DictionaryLM$uncertainty) %>% 
  mutate(sentiment = -1, dict = 'un') %>% 
  rename(word = `DictionaryLM$uncertainty`)


# Combine positive and uncertain words
lm_dictionary <- lm_positive %>% bind_rows(lm_negative) %>% bind_rows(lm_uncertain)


# Filter headlines containing the company name or ticker
df <- df %>% 
  mutate(co_conm = co_conm %>%
           str_remove('CORP') %>% str_squish() %>%
           str_remove('INC') %>% str_squish() %>%
           str_remove('CO') %>% str_squish() %>% 
           str_remove('LTD') %>% str_squish()) %>% 
  filter(
    text %>% str_detect(regex(co_conm, ignore_case = T)) |
    text %>% str_detect(regex(co_tic, ignore_case = T)))

# Remove NYSE ORDER IMBALANCE notices, removes approx 45k rows
df <- df %>%filter(!text %>% str_detect(regex('NYSE ORDER IMBALANCE', ignore_case = T)))

# Convert to tidy text format
df <- df %>% unnest_tokens(word, text)

# Remove stop words 
df <- df %>% anti_join(stop_words)
# Count the total words per headline
number_of_words <- df %>% group_by(date, ric, storyId) %>% tally() %>% rename(total_n = n)
# Join the total words to main_df
df <- df %>% inner_join(number_of_words)

# Join sentiment words
main_df <- df %>% inner_join(lm_dictionary)

# Calculate news specific sentiment for all words
news_level_sentiment_all <- main_df %>%
  group_by(date, storyId, ric, total_n) %>% 
  summarise(sentiment = sum(sentiment)) %>% ungroup() %>% 
  mutate(dict = 'all')

# Calculate news specific sentiment for neg and pos words
news_level_sentiment_neg_pos <- main_df %>%
  filter(dict %in% c('neg', 'pos')) %>% 
  group_by(date, storyId, ric, total_n) %>% 
  summarise(sentiment = sum(sentiment)) %>% ungroup() %>% 
  mutate(dict = 'neg_pos')

# Calculate news specific sentiment for only neg
news_level_sentiment_neg <- main_df %>%
  filter(dict == 'neg') %>% 
  group_by(date, storyId, ric, total_n) %>% 
  summarise(sentiment = sum(sentiment)) %>% ungroup() %>% 
  mutate(dict = 'neg')

# Combine
news_level_sentiment <- news_level_sentiment_all %>%
  bind_rows(news_level_sentiment_neg) %>%
  bind_rows(news_level_sentiment_neg_pos)

# Calculate daily sentiment from news specific sentiments
daily_sentiment <- news_level_sentiment %>% 
  group_by(date, ric, dict) %>% 
  summarise(
    sentiment = sum(sentiment) / sum(total_n))

# Calculate total words per day per ric
daily_words <- news_level_sentiment %>% 
  group_by(date, ric) %>% 
  summarise(total_words = sum(total_n))


daily_words <- news_level_sentiment %>% 
  select(date, storyId, ric, total_n) %>% distinct() %>%
  group_by(date, ric) %>% 
  summarise(total_words = sum(total_n))

# Combine with daily words
daily_sentiment <- daily_sentiment %>% inner_join(daily_words)  

# Transform sentiment column to multiple columns
daily_sentiment <- daily_sentiment %>%
  spread(dict, sentiment) %>% 
  rename(
    se_all = all,
    se_neg = neg,
    se_np = neg_pos
  ) %>% 
  arrange(ric, date)

" Fill missing days so that the data contains all days from the period
  Also, days without no news are stored to column 'no_news', (some of these might public holidays)
  TODO it has be checked that there is no dates from over or under the period "
daily_sentiment <- daily_sentiment %>%
  group_by(ric) %>% 
  complete(date = seq.Date(min(date), max(date), by='day')) %>% ungroup()

" Remove weekends from the dataset, this will still leave public holidays, but they will
  be filtered out since the price data does not contain them "
daily_sentiment <- daily_sentiment %>%
  mutate(weekday = weekdays(date)) %>% 
  filter(!weekday %in% c('Saturday', 'Sunday')) %>% 
  select(date, ric, total_words, everything(), -weekday)

# Find all the 'sentiment' words
sentiment_words <- main_df %>%
  group_by(date, ric) %>%
  summarise(words = paste(word, collapse = ', ')) %>% arrange(ric, date)

# Join the words
daily_sentiment <- daily_sentiment %>% left_join(sentiment_words)
# Replace all NA's

daily_sentiment <- daily_sentiment %>% mutate_if(is.numeric, replace_na, 0)
# Write the final output
write_csv(daily_sentiment, '../data/daily_sentiment_file.csv')


# ==== Plotting ====

# All words
all_words <- df %>% group_by(word) %>% tally() %>% arrange(n %>% desc()) %>% 
  filter(str_detect(word, '[^0-9]'))

all_words <- all_words %>% 
  anti_join(news %>% select(co_conm) %>% distinct() %>% mutate(co_conm = str_to_lower(co_conm)) %>% 
  unnest_tokens(word, co_conm)) %>% head(50) %>% mutate(angle = if_else(rbinom(n(), 1, 0.2) == 0,0,90))


# All with sentiment
all_sentiment_words <- main_df %>% group_by(word) %>% tally() %>%
  arrange(n %>% desc()) %>% head(50) %>% mutate(angle = if_else(rbinom(n(), 1, 0.2) == 0,0,90))

# Negative
negative_words <- main_df %>% filter(dict == 'neg') %>% group_by(word) %>% tally() %>% 
  arrange(n %>% desc()) %>% head(50) %>% mutate(angle = if_else(rbinom(n(), 1, 0.2) == 0,0,90))

# Positive
positive_words <- main_df %>% filter(dict == 'pos') %>% group_by(word) %>% tally() %>%
  arrange(n %>% desc()) %>% head(50) %>% mutate(angle = if_else(rbinom(n(), 1, 0.2) == 0,0,90))

# # Uncertain
# uncertain_words <- main_df %>% filter(dict == 'un') %>% group_by(word) %>% tally() %>% 
#   arrange(n %>% desc()) %>% head(50)


# Plot metrics
plot_width <- 4
plot_height <- 3.5


# All words
ggplot(all_words, aes(label = word, size = n, angle = angle)) + 
  geom_text_wordcloud_area(shape = 'circle') +
  theme_minimal() +
  ggsave('../thesis/figures/all_words_wordcloud.pdf',
         width = plot_width, height = plot_height, dpi = 'retina')

# All sentiment words
ggplot(all_sentiment_words, aes(label = word, size = n, angle = angle)) + 
  geom_text_wordcloud_area() +
  scale_size_area() +
  theme_minimal() +
  ggsave('../thesis/figures/all_sentiment_wordcloud.pdf',
    width = plot_width, height = plot_height, dpi = 'retina')
  

# Negative sentiment words
ggplot(negative_words, aes(label = word, size = n, angle = angle)) + 
  geom_text_wordcloud_area() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave('../thesis/figures/negative_wordcloud.pdf',
         width = plot_width, height = plot_height, dpi = 'retina')

# Positive sentiment words
ggplot(positive_words, aes(label = word, size = n, angle = angle)) +
  geom_text_wordcloud_area() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
ggsave('../thesis/figures/positive_wordcloud.pdf',
       width = plot_width, height = plot_height, dpi = 'retina')


# # Uncertain sentiment words
# ggplot(uncertain_words, aes(label = word, size = n)) + 
#   geom_text_wordcloud() +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   ggsave('../thesis/figures/uncertain_wordcloud.pdf',
#          width = plot_width, height = plot_height, dpi = 'retina')

# Cropt all figures in figures folder
system("for f in ../thesis/figures/*
        do
          pdfcrop --margins 5 $f $f
        done")

