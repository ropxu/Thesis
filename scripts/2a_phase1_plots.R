library(tidyverse)
library(tidytext)
library(SentimentAnalysis)
library(RcppRoll)
library(lubridate)
library(ggpubr)
library(scales)

# Read news data
df <- read_csv('../data/news_master_file.csv')
df <- df %>% select(-co_cusip, -co_sic)

# Read sentiment wordlists Loughrain and Mcdonald
positive <- tibble(DictionaryLM$positive) %>% 
  mutate(sentiment = 1) %>% 
  rename(word = `DictionaryLM$positive`)

negative <- tibble(DictionaryLM$negative) %>% 
  mutate(sentiment = -1) %>% 
  rename(word = `DictionaryLM$negative`)

# The final list containing both positive and negative words
lm_dictionary <- positive %>% bind_rows(negative)

# Convert to tidy text format
df <- df %>% unnest_tokens(word, text)

# Remove stop words and count total words
df <- df %>% anti_join(stop_words)
number_of_words <- df %>% group_by(versionCreated, ric, storyId) %>% tally() %>% rename(total_n = n)
df <- df %>% inner_join(number_of_words)

# Join sentiment words
df <- df %>% inner_join(lm_dictionary)

# Aggregate sentiment over a week
df <- df %>% mutate(
  year = year(versionCreated),
  week = week(versionCreated),
  date = date(versionCreated)
)

total_sentiment <- df %>% group_by(year, week) %>%
  summarise(sentiment = sum(sentiment) / sum(total_n)) %>% 
  arrange(year, week) %>%  
  ungroup() %>% 
  mutate(date = row_number())
  
raw_plot <- ggplot(total_sentiment, aes(date, sentiment, fill='red')) +
  geom_col(show.legend = F) +
  #scale_x_date(labels = date_format("%m-%Y")) +
  #scale_x_discrete(breaks=) +
  theme_minimal() +
  theme(panel.grid.major = element_blank()) +
  labs(title = 'Raw news sentiment',
       y = 'Fraction of negative words',
       x = 'Week')


rolling_sentiment <- total_sentiment %>% mutate(
  sent_mean = roll_mean(sentiment, 8, fill=NA, align='right'),
  sent_var = roll_var(sentiment, 8, fill=NA, align='right'),
  sent = replace_na((sentiment - sent_mean) / sent_var,0)
)
  



scaled_plot <- ggplot(rolling_sentiment, aes(date, sent, fill=if_else(sent < 0, 'green', 'red'))) +
  geom_col(show.legend = F) +
  #scale_x_date(labels = date_format("%m-%Y")) +
  #scale_x_discrete(breaks=) +
  theme_minimal() +
  theme(panel.grid.major = element_blank()) +
  labs(title='Scaled news sentiment',
       y = 'Sentiment',
       x = 'Week')

ggarrange(raw_plot, scaled_plot, ncol=1) + 
  ggsave('../presentations/combined_plot.pdf', height = 5, width = 8)

                

