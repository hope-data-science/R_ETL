
library(pacman)

p_load(tidyverse,dtplyr,data.table)

read_rds("R语言练习/stock-market-data.rds") -> data #请自行设置路径

# 1.哪些股票的代码中包含"8"这个数字？

# data.table
data[str_detect(symbol, "8"), 
     unique(symbol)]

# dtplyr
data %>% 
  lazy_dt() %>% 
  filter(str_detect(symbol,"8")) %>% 
  distinct(symbol) %>% 
  as_tibble()

# 2.每天上涨和下跌的股票各有多少?

# data.table
data[, 
     .(num = uniqueN(symbol)), 
     keyby = .(date, updown = ifelse(close - pre_close > 0, "UP", "DOWN"))]

# dtplyr
data %>% 
  lazy_dt() %>% 
  mutate(updown = ifelse(close - pre_close > 0, "UP", "DOWN")) %>% 
  count(date,updown) 

# 3.每天每个交易所上涨、下跌的股票各有多少？

# data.table
data[, .(num = uniqueN(symbol)), 
     keyby = .(date, 
               exchange = str_sub(symbol, start = -2, end = -1), 
               updown = ifelse(close - pre_close > 0, "UP", "DOWN"))
     ]

# dtplyr
data %>% 
  lazy_dt() %>% 
  mutate(exchange = str_sub(symbol, start = -2, end = -1)) %>% 
  mutate(updown = ifelse(close - pre_close > 0, "UP", "DOWN")) %>% 
  group_by(date,exchange) %>% 
  count(updown,name = "num") %>% 
  as_tibble()

# 4.沪深300成分股中，每天上涨、下跌的股票各有多少？

# data.table
data[index_w300 > 0, 
     .(num = uniqueN(symbol)), 
     keyby = .(date, updown = ifelse(close - pre_close > 0, "UP", "DOWN"))
     ]

# dtplyr
data %>% 
  lazy_dt() %>% 
  filter(index_w300 > 0) %>% 
  mutate(updown = ifelse(close - pre_close > 0, "UP", "DOWN")) %>% 
  group_by(date) %>% 
  count(updown) %>% 
  as_tibble()

# 5.每天每个行业各有多少只股票？

# data.table
data[, .(stk_num = uniqueN(symbol)), 
     keyby = .(date, industry)
     ]

# dtplyr
data %>% 
  lazy_dt() %>% 
  group_by(date,industry) %>% 
  distinct(symbol) %>% 
  summarise(stk_num = n()) %>% 
  tibble()

# 6. 股票数最大的行业和总成交额最大的行业是否总是同一个行业？

# 每天股票数最大行业
data %>% 
  lazy_dt() %>% 
  distinct(date,industry,symbol) %>% 
  count(date,industry,sort = T) %>% 
  group_by(date) %>% 
  slice(1) %>% 
  as_tibble() -> A

# 每天成交额最大的行业
data %>% 
  lazy_dt() %>% 
  group_by(date) %>% 
  count(industry,wt = amount,sort = T) %>% 
  slice(1) %>% 
  as_tibble() -> B

# 是不是同一行业？
A %>% 
  inner_join(B,by = "date") %>% 
  mutate(same = industry.x == industry.y) -> same_or_not

same_or_not %>% 
  pull(same) %>% 
  table()

# 7.每天涨幅超过5%、跌幅超过5%的股票各有多少？

# data.table
data[, ':='(ret = (close - pre_close)/pre_close)
     ][ret > 0.05 | ret < -0.05, 
       .(symbol_amount = uniqueN(symbol)), 
       keyby = .(date, updown = ifelse(ret > 0.05, "up5%+", "down5%+"))
       ]

# dtplyr
data %>% 
  lazy_dt() %>% 
  mutate(ret = (close - pre_close)/pre_close) %>% 
  filter(ret > 0.05 | ret < -0.05) %>% 
  mutate(updown = ifelse(ret > 0.05, "up5%+", "down5%+")) %>% 
  group_by(date,updown) %>% 
  distinct(symbol) %>% 
  summarise(symbol_amount = n()) %>% 
  as_tibble()
  
  
  




