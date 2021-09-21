library(tidyverse)
library(lubridate)
library(bdscale)
library(knitr)

news <- read_csv('../data/news_master_file.csv')
main_df <- read_csv('../data/main_data_no_after_hours.csv')


# ==== News by hour ====

plot_width <- 5.5
plot_height <- 3

news %>% group_by(hour = hour(versionCreated)) %>% tally() %>% ggplot(aes(hour, n)) +
  geom_col(fill = '#112222') +
  theme_linedraw() +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  scale_x_continuous(breaks = seq(0,23,2)) +
  theme(
    panel.grid = element_blank()
  ) +
  labs(y = 'Number of news',
       x = 'Hour') +
  ggsave('../thesis/figures/news_per_hour.pdf',
         device = 'pdf', dpi = 'retina', width = plot_width, height = plot_height)


# ==== News by day ====

daily <- news %>% group_by(weekday = weekdays(versionCreated)) %>% tally()
daily$weekday <- factor(daily$weekday, 
        levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(daily, aes(weekday, n)) +
  geom_col(fill = '#112222', width = 0.7) +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  theme_linedraw() +
  theme(
    panel.grid = element_blank()
  ) +
  labs(y = 'Number of news', 
       x = element_blank()) +
  ggsave('../thesis/figures/news_per_day.pdf',
         device = 'pdf', dpi = 'retina', width = plot_width, height = plot_height)


main_df %>% mutate(sent = if_else(se_neg != 0, 1, 0))  %>% 
  group_by(sent) %>% tally()

# ==== Descriptive statistics ====
main_df %>% #filter(se_neg != 0) %>% 
  group_by(
    months = month(date),
    month = months(date),
    year = year(date),
  ) %>% add_tally() %>% group_by(months, month, year, n) %>% nest() %>% 
  mutate(
    ngg = map_dbl(data, ~sum(if_else(.$se_neg==0,0,1))),
    mean = map_dbl(data, ~mean(.$se_neg, na.rm = T)),
    sd = map_dbl(data, ~sd(.$se_neg, na.rm = T)),
    min = map_dbl(data, ~min(.$se_neg, na.rm = T)),
    max = map_dbl(data, ~max(.$se_neg, na.rm = T)),
    quantiles = map(data, ~quantile(.$se_neg, probs = c(0.25, 0.5, 0.75), na.rm = T)),
    quantiles = map(quantiles, ~bind_rows(.) %>% gather())) %>% unnest(quantiles) %>% ungroup() %>% 
  spread(key, value) %>% arrange(year, months) %>% select(-months, -data) %>% 
  select(month, year, n, ngg, mean, sd, min, max) %>% 
  rename(Month = month, Year = year, N = n, Mean = mean, `Std.dev.` = sd, Min = min, Max = max) %>% 
  knitr::kable(format = 'latex',
               digits = 3,
               booktabs=T, caption = '') %>% 
  kableExtra::kable_styling(position = 'center', full_width = T) 
  # cat(., file = '../thesis/tables/sentiment_descriptives.tex')

# holidays <- read_csv('../data/original/early_closing.csv')  


# ==== Data plots ====
colors <- c('#004c6d', '#517590', '#8aa1b4')
colors <- c('#004c6d', '#004c6d', '#004c6d')
labels <- c('ret' = 'Mean return',
            'se' = 'Mean % of negative words',
            'se_scaled' = 'Scaled negative words')



df <- main_df %>% select(date, ret, se_neg) %>% 
  group_by(date) %>% 
  summarise(ret = mean(ret, na.rm = T)/100,
            se = mean(se_neg, na.rm = T)) %>% drop_na() %>% 
  #mutate(se_scaled = (se - mean(se)) / sd(se)) %>% 
  gather('name', 'value', -date)



aapl <- tidyquant::tq_get('AAPL', get='stock.prices') %>% 
  filter(date >= '2018-08-06' & date <= '2019-11-01') %>% pull(date)


df %>% 
  ggplot() +
  geom_col(aes(date, value, fill=name), width = 1) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(rows = vars(name), scales = 'free_y', labeller = labeller(name = labels)) +
  scale_fill_manual(values = colors) + 
  scale_x_bd(business.dates = aapl, 
             scales::date_format(format = "%Y-%m-%d"), max.major.breaks = 10) +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'none',
        strip.background = element_rect(fill = 'white', color='white'),
        strip.text = element_text(colour = 'black')) +
  labs(y = '') +
  ggsave('../thesis/figures/ret_and_sentiment.png', width = 8, height = 4,
         dpi = 'retina')

news <- read_csv('../data/news_master_file.csv')



news %>% group_by(date = as.Date(versionCreated)) %>% 
  tally() %>% 
  mutate(weekday = weekdays(date)) %>% 
  filter(!weekday %in% c('Saturday', 'Sunday')) %>% 
  ggplot(aes(date, n)) +
  scale_x_bd(business.dates = aapl, 
             scales::date_format(format = "%Y-%m-%d"), max.major.breaks = 10) +
  scale_y_continuous(labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
  geom_line(color = 'black') +
  theme_classic() +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.grid.major = element_blank()) +
  labs(x = "Date", y = 'Number of news') +
  ggsave('../thesis/figures/news_timeseries.pdf', dpi = 'retina',
         width = 8, height = 5)



df %>% group_by(name) %>% 
  mutate(cumret = (value / lag(value))) 
  

ggplot() +
  geom_line(aes(date, cumret, color = name)) +
  facet_grid(rows = vars(name), scale = 'free_y')
  

df <- main_df %>% group_by(date) %>% 
  summarise(mean = mean(se_neg, na.rm = T))


labels = c('iv30' = 'Avg 30d implied volatility', 'ret' = 'Avg cumulative return')

main_df %>% 
  group_by(date) %>% 
  summarise(
    ret = mean(ret, na.rm =T),
    iv30 = mean(iv30, na.rm = T)) %>% 
  mutate(ret = cumprod(replace_na(ret, 0) / 100 + 1)-1, iv30 = iv30 / 100) %>% 
  drop_na() %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(date, value, color = name)) +
  geom_line(color = '#0f0f0f') +
  scale_y_continuous(labels = scales::percent) + 
  theme_linedraw() + 
  theme(
    panel.grid = element_blank(),
    legend.position = 'none',
    strip.background = element_rect(fill = 'white', color='white'),
    strip.text = element_text(colour = 'black')) +
  facet_grid(rows = vars(name), scale = 'free_y', labeller = labeller(name = labels)) +
  labs(x = 'Date', y = '') +
  ggsave('../thesis/figures/iv_ret.pdf',
         device = 'pdf', dpi = 'retina', width = plot_width, height = 4)


labels = c('ret' = 'Avg cumulative return', 'se_neg' = '30d rolling avg sentiment')

main_df %>% group_by(date) %>% 
  summarise(se_neg = mean(se_neg, na.rm = T), ret = mean(ret, na.rm = T)) %>% 
  mutate(se_neg = RcppRoll::roll_meanr(se_neg, n=20, na.rm = T),
         ret = cumprod(replace_na(ret, 0) / 100 + 1)-1) %>% 
  drop_na() %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(date, value, color=name)) +
  geom_line(color = '#0f0f0f') +
  theme_linedraw() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + 
  theme(
    panel.grid = element_blank(),
    legend.position = 'none',
    strip.background = element_rect(fill = 'white', color='white'),
    strip.text = element_text(colour = 'black')) +
  facet_grid(rows = vars(name), scale='free_y', labeller = labeller(name = labels)) +
  labs(x = 'Date', y = '') +
  ggsave('../thesis/figures/sent_ret.pdf',
         device = 'pdf', dpi = 'retina', width = plot_width, height = 4)



main_df %>% 
  group_by(date) %>% 
  summarise(
    ret = mean(ret, na.rm =T),
    iv30 = mean(iv30, na.rm = T)) %>% 
  mutate(ret = cumprod(replace_na(ret, 0) / 100 + 1)-1, iv30 = iv30 / 100) %>% 
  drop_na() %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(date, value, color = name)) + 
  geom_line() +
  theme_linedraw() 
  theme(panel.grid = element_blank())


lm(ret ~ se_neg, data=main_df) %>% summary()
lm(ret ~ iv30, data=main_df) %>% summary()
