## ----echo=FALSE----------------------------------------------------------
library(knitr)
opts_chunk$set(eval = F)

## ------------------------------------------------------------------------
nr_of_rows <- 1e7

df <- data.frame(
    Logical = sample(c(TRUE, FALSE, NA), prob = c(0.85, 0.1, 0.05), nr_of_rows, replace = TRUE),
    Integer = sample(1L:100L, nr_of_rows, replace = TRUE),
    Real = sample(sample(1:10000, 20) / 100, nr_of_rows, replace = TRUE),
    Factor = as.factor(sample(labels(UScitiesD), nr_of_rows, replace = TRUE))
  )

## ------------------------------------------------------------------------
library(pacman)
p_load(data.table,tidyverse)

## ------------------------------------------------------------------------
df %>% as_tibble -> dt
rm(df)   #df不用了，因此移除掉来节省空间

## ------------------------------------------------------------------------
bind_rows(dt,dt,dt,dt,dt) -> dt5
rm(dt)  #dt不用了，移除掉节省空间

## ------------------------------------------------------------------------
dt5 %>% 
  object.size() %>% 
  print(unit = "auto")

## ------------------------------------------------------------------------
dt5 %>%
  mutate(sum = Integer + Real,prod = Integer * Real) %>%
  group_by(Logical,Integer,Factor) %>%
  summarise(n = n(),
            median = median(Real),
            sum_avg = mean(sum),
            prod_avg = mean(prod))

## ------------------------------------------------------------------------
system.time(dt5 %>%
  mutate(sum = Integer + Real,prod = Integer * Real) %>%
  group_by(Logical,Integer,Factor) %>%
  summarise(n = n(),
            median = median(Real),
            sum_avg = mean(sum),
            prod_avg = mean(prod)))

## ------------------------------------------------------------------------
dt5 %>%
  mutate(sum = Integer + Real,prod = Integer * Real) %>%
  as.data.table() %>%
  .[,.(n = .N,
        median = median(Real),
            sum_avg = mean(sum),
            prod_avg = mean(prod)),
    by = .(Logical,Integer,Factor)] %>%
  as_tibble

## ------------------------------------------------------------------------
system.time(dt5 %>%
  mutate(sum = Integer + Real,prod = Integer * Real) %>%
  as.data.table() %>%
  .[,.(n = .N,
        median = median(Real),
            sum_avg = mean(sum),
            prod_avg = mean(prod)),
    by = .(Logical,Integer,Factor)] %>%
  as_tibble)

## ------------------------------------------------------------------------
my_count = function(df,...){
  dt <- as.data.table(df) 
  dt[,.(n = .N),by = ...] %>% as_tibble()
}

## ------------------------------------------------------------------------
p_load(microbenchmark)

microbenchmark(dt5 %>% count(Integer) -> a,
               dt5 %>% my_count(Integer) -> b,
               times = 5,unit = "s")

setequal(a,b)  #看看两者得到的效果是不是一样的

