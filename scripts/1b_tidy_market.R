library(tidyverse)
library(lubridate)

# List of S&P500 constinuents
sp500 <- read_csv('../data/sp500_constituents_rics.csv')

# Fix dates
sp500 <- sp500 %>% mutate(
  from = ymd(from),
  thru = ymd(thru)
)

# Filter last 15 month's constituents
# start_of_period = now() %>% as.Date() - months(15)
start_of_period = ymd("2018-08-03")
end_of_period = ymd("2019-11-03")
sp500 <- sp500 %>% filter(from >= start_of_period | is.na(thru))
sp500 <- sp500 %>% select(from, thru, co_conm, co_tic, co_cusip, co_sic, RIC) %>% rename(ric = RIC)
sp500 <- sp500 %>% mutate(
  from = if_else(from <= start_of_period, start_of_period, from),
  thru = if_else(is.na(thru), end_of_period, thru)
)

# Get cusips for exchange information
cusips <- sp500 %>% pull(co_cusip)
fileConn<-file('../data/cusips.txt')
writeLines(cusips, fileConn)
close(fileConn)


# --------------- Implied volatlity ---------------

ivol_df <- read_csv('../data/original/impl_vol.csv')
ivol_df <- ivol_df %>% rename(
  date = Date, 
  ivol_30 = `30 Day At-The-Money Implied Volatility Index for Call Options`,
  ivol_60 = `60 Day At-The-Money Implied Volatility Index for Call Options`,
  ivol_90 = `90 Day At-The-Money Implied Volatility Index for Call Options`
)

ivol <- ivol_df %>%
  mutate(
      date = ymd(date),
      co_tic = str_remove(Instrument, 'ATMIV.U')
      ) %>% 
  inner_join(sp500 %>% select(co_tic, ric, from, thru)) %>% 
  select(date, co_tic, ric, Instrument, everything())

# ============== Some test for the data ==============

# Companies that might cause problems when retrieving the ivol 
ivol %>% filter(co_tic != ric %>% str_remove("\\.(.*)")) %>%
  select(co_tic, ric) %>% unique()

# There are companies with multiple stock classes, for example Google has A and C
a <- ivol_df %>% group_by(Instrument) %>% tally() %>% ungroup()
b <- ivol %>% group_by(Instrument) %>% tally() %>% ungroup() %>% rename(m = n)
a %>% inner_join(b) %>%
  filter(n != m) %>%
  mutate(diff = m-n)


# ============== Some test for the data ==============

ivol <- ivol %>% filter(date >= from & date <= thru)

# Companies with less than 30 days ivol values
ivol %>% group_by(Instrument) %>% tally() %>% arrange(n) %>% 
  filter(n <= 30) %>%
  mutate(co_tic = Instrument %>% str_remove('ATMIV.U')) %>% 
  pull(co_tic)

ivol <- ivol %>% select(-from, -thru)

# --------------- Returns & Volume ---------------
market_data <- read_csv('../data/original/sp500_data.csv')
market_data <- market_data %>% rename(
  ric = Instrument,
  return = `Daily Total Return`,
  volume = `Accumulated Volume`,
  date = Date,
  date2 = Date_1
)

returns <- market_data %>% select(date, ric, return) %>% distinct()
volume <- market_data %>% select(date2, ric, volume) %>% distinct() %>% rename(date = date2)
nrow(returns) - nrow(volume)

sp500_market_data <- returns %>% inner_join(volume)
sp500_market_data <- sp500_market_data %>% 
  mutate(date = ymd(date))


sp500_market_data <- sp500_market_data %>% filter(!is.na(date))

# Combine implied vol with returns and volume
sp500_market_data <- sp500_market_data %>% full_join(ivol) %>% 
  select(-Instrument) %>% 
  select(date, co_tic, ric, everything())

sp500_market_data <- sp500_market_data %>% select(-co_tic) %>% left_join(sp500)

write_csv(sp500_market_data, '../data/sp500_market_data.csv')

# Clean environment
rm(list=ls()[! ls() %in% c("sp500_market_data")])
