library(tidyverse)
library(lubridate)
library(sandwich)
library(lmtest)

main_df <- read_csv('../data/main_data.csv')

main_df <- main_df %>% group_by(ric) %>% 
  mutate(lag_se = lag(se_neg),
         lag_se1 = lag(se_neg,1),
         lag_se2 = lag(se_neg,2),
         lag_se3 = lag(se_neg,3),
         lag_se4 = lag(se_neg,4),
         lag_se5 = lag(se_neg,5),
         lag_ret = lag(ret),
         se_all = se_all * -1,
         d_iv = iv30 / lag(iv30) - 1,
         lag_iv = lag(d_iv),
         lag_iv30 = lag(iv30),
         year = year(date),
         lag_news = lag(news)) %>% ungroup() %>% 
  filter(d_iv != Inf) %>% filter(lag_iv != Inf)



# 1.
model1 <- lm(iv30 ~ lag_se, data=main_df)
coeftest(model1, vcov = vcovHC(model1, type = 'HC0'))

# 2.
model2 <- lm(se_neg ~ lag_iv30, data=main_df)
coeftest(model2, vcov = vcovHC(model2, type = 'HC0'))


# 3.
lm(ret ~ lag_se, data=main_df) %>% summary()

lm(ret ~ d_iv, data=main_df) %>% summary()
lm(ret ~ se_neg, data=main_df) %>% summary()

nested_df <- main_df %>% group_by(month = month(date), year = year(date)) %>% 
  nest()

iv_se_model <- function(df) {
  lm(iv30 ~ lag_se, data=df)
}

se_iv_model <- function(df) {
  lm(se_neg ~ lag_iv30, data=df)
}


get_adjR2 <- function(df) {
  df <- df %>% mutate(glance = map(model, broom::glance)) %>% unnest(glance)
}





nested_df %>% 
  mutate(model = map(data, iv_se_model),
         tidy = map(model, broom::tidy),
         glance = map(model, broom::glance)) %>% 
  unnest(tidy) %>% select(month, year, data, model, term, glance, estimate, statistic) %>% 
  pivot_wider(names_from = term, values_from = c('estimate', 'statistic')) %>% 
  rename(Intercept = `estimate_(Intercept)`,
         BadNews = `estimate_lag_se`,
         t_stat_Intercept = `statistic_(Intercept)`,
         t_stat_BadNews = statistic_lag_se) %>% unnest(glance) %>%
  select(year, month, Intercept, t_stat_Intercept, BadNews, t_stat_BadNews, adj.r.squared) %>% 
  knitr::kable(format = 'latex', digits = 3, booktabs = T) %>% 
  kableExtra::kable_styling(full_width = T)




nested_df %>% 
  mutate(model_se = map(data, se_iv_model),
         tidy = map(model_se, broom::tidy)) %>% 
  unnest(tidy) %>% select(month, year, data, model_se, term, estimate, statistic) %>% 
  pivot_wider(names_from = term, values_from = c('estimate', 'statistic'))




main_df %>% group_by(date) %>% 
  summarise(ret = mean(ret, na.rm = T)/100,
            se = mean(se_neg, na.rm = T),
            `Implied Volatility` = mean(iv30, na.rm = T)/100) %>% 
  mutate(`Cumulative Return` = cumprod(ret + 1)) %>%
  gather('name', 'value', `Cumulative Return`, `Implied Volatility`, -date) %>% 
  ggplot(aes(date, value, group = name)) +
  geom_line() +
  #scale_y_continuous(labels = scales::percent) +
  facet_grid(rows = vars(name), scales = 'free_y') +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'white', color='white'),
        strip.text = element_text(colour = 'black'),
        panel.grid = element_blank()) +
  labs(y = '', x = '')
print(plot)
dev.off()

library(tikzDevice)

news %>% filter(ric == 'FB.O') %>% filter(text %>% str_detect('Facebook')) %>% pull(text)

SentimentAnalysis::DictionaryLM$positive %>% enframe() %>% filter(value == 'acquire')



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
  labs(y = '')



?panelvar::pvarfeols

panelvar::pvarfeols(dependent_vars = c("se_neg", "ret", 'vol'),
          lags = 1,
          #exog_vars = c("month"),
          #transformation = "demean",
          data = main_df %>% mutate(vol = log(vol)) %>% as.data.frame(),
          panel_identifier = c("ric", "year"))

lm(d_iv ~ lag_ret + abs(lag_ret), data=main_df %>% drop_na()) %>% summary()
lm(ret ~  lag_iv + lag_se, data=main_df %>% drop_na(lag_iv, ret, lag_se)) %>% summary()

library(lmtest)
library(sandwich)

bptest(reg)

coeftest(reg, vcov = vcovHC(reg))
coeftest(reg)


model <- function(df) {
  lm(d_iv ~ lag_ret + abs(if_else(lag_ret < 0, lag_ret, 0)), data=df)
}

main_df %>% drop_na() %>% ungroup() %>% 
  group_by(month = month(date), year = year(date)) %>% nest() %>% 
  mutate(model = map(data, model),
         tidy = map(model, broom::tidy)) %>% 
  unnest(tidy) %>% select(month, year, data, model, term, estimate) %>% 
  pivot_wider(names_from = term, values_from = estimate)



main_df %>% select(d_iv)
main_df %>% head()
