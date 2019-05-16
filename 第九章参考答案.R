
# 第九章参考答案

## 数据生成

nr_of_rows <- 2e3

df <- data.frame(
  Logical = sample(c(TRUE, FALSE, NA), prob = c(0.85, 0.1, 0.05), nr_of_rows, replace = TRUE),
  Integer = sample(1L:100L, nr_of_rows, replace = TRUE),
  Real = sample(sample(1:10000, 20) / 100, nr_of_rows, replace = TRUE),
  Factor = as.factor(sample(labels(UScitiesD), nr_of_rows, replace = TRUE))
)

## 1

head(df)
str(df)
summary(df)

as_tibble(df) -> dt
dt

## 2

dt %>% 
  filter(Integer < 29)

## 3

dt %>% 
  filter(Logical == TRUE,Integer > 29)

## 4

dt %>% 
  filter(is.na(Logical))

## 5

dt %>% 
  select(1:2) %>% 
  slice(1:4)

## 6

dt %>% 
  filter(Logical == FALSE) %>% 
  select(Real,Factor)

## 7

dt %>% 
  rename(a = Logical,b = Integer,
         c = Real, d = Factor) -> dt
dt

## 8

dt %>% 
  mutate(e = b * c)

## 9

dt %>% 
  mutate(one = 1)

## 10

dt %>% 
  arrange(b,c)

## 11

dt %>% 
  filter(a == TRUE) %>% 
  arrange(b)

## 12

dt %>% 
  filter(a == TRUE) %>% 
  arrange(desc(b))

## 13

dt %>% 
  transmute_all(is.na) %>% 
  summarise_all(sum)

## 14

dt %>% 
  filter(a == FALSE,b > 29) %>% 
  select(a,b,c) %>% 
  select(b,everything())

## 15

dt %>% 
  distinct(d)

## 16

dt %>% 
  count(a)

dt %>% 
  count(a,d) 

## 17

dt %>% 
  group_by(d) %>% 
  summarise(mean(c))

## 18

dt %>% 
  filter(a == FALSE) %>% 
  group_by(b,d) %>% 
  summarise(mean(c),median(c))

## 19

dt %>% 
  filter(a == FALSE) %>% 
  group_by(d) %>% 
  summarise(max(c),min(c))

## 20

dt %>% 
  group_by(d) %>% 
  select(b,c,d) %>% 
  mutate(c_new = c - first(c))

## 21

dt %>% 
  filter(a == FALSE) %>% 
  select(b,c,d) %>% 
  group_by(d) %>% 
  filter(c == max(c) | c == min(c)) %>% 
  arrange(b) %>% 
  print(n = Inf)

## 22

dt %>% 
  mutate(rank_c = min_rank(c),rank_c_desc = min_rank(desc(c))) %>% 
  group_by(d) %>% 
  filter(b == max(b)) %>% 
  ungroup()

## 23

dt %>% 
  top_n(5,b)

dt %>% 
  top_n(5,desc(b))

## 24

dt %>% 
  group_by(d) %>% 
  summarise(n = n()) -> d_count

dt %>% 
  left_join(d_count,by = "d")

