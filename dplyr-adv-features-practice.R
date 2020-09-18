library(tidyverse)
library(nycflights13)

flights %>% 
  relocate(carrier) %>% 
  head(2)

flights %>% 
  relocate(carrier, .after=day)

flights %>% 
  relocate(carrier, .before=air_time) 

flights %>% 
  relocate(carrier, .after =last_col()) 


flights %>% 
  relocate(carrier, .end =last_col()) 

flights %>% 
  relocate(c(carrier, tailnum, origin, dest), .after=dep_time)

flights %>% 
  summarize(mean(dep_delay, na.rm = T))

flights %>% 
  summarize(mean_delay=mean(dep_delay, na.rm = T))

# Find unique no of carriers 

flights %>% 
  summarize(c_dis=n_distinct(carrier),t_dis=n_distinct(tailnum), o_dis=n_distinct(origin), d_dis=n_distinct(dest))

flights %>% 
  summarize_at(vars(carrier,tailnum,origin,dest), n_distinct)

flights %>% 
  summarize_if(is.character, n_distinct)

flights %>% 
  summarize(across(where(is.character), n_distinct))

flights %>% 
  summarize_at(vars(tailnum, origin, carrier), n_distinct)

flights %>% 
  summarize(across(c(tailnum,origin,carrier), n_distinct))


# find mean of those variable which are numeric and ends with delay

# do the above for just vars that ends with "delay"

flights %>% 
  summarize(across(ends_with("delay"), mean,na.rm=T))

flights %>% 
  summarize(across(where(is.numeric) & ends_with("delay"), mean,na.rm=T))

# does not
flights %>% 
  summarize(across(mean(arr_delay),sd(arr_delay)))

# It works
flights %>% 
  summarize(across(arr_delay, c(mean,median,sd),na.rm=T))

# Incorporate appropriate column names

flights %>% 
  summarize(across(arr_delay, c(mean,median,sd),na.rm=T), )



flights %>%
  drop_na(c(arr_delay,dep_delay)) %>% 
  group_by(origin) %>% 
  #filter(n() > 1) %>% 
  summarize(across(c(arr_delay, dep_delay), c(avg=mean, sdev=sd)))


flights %>%
  drop_na(c(arr_delay,dep_delay)) %>% 
  group_by(origin) %>% 
  #filter(n() > 1) %>% 
  summarize(across(c(arr_delay, dep_delay), c(mean, sd), .names="{col}_{fn}"))


flights %>%
  drop_na(c(arr_delay,dep_delay)) %>% 
  group_by(origin) %>% 
  #filter(n() > 1) %>% 
  summarize(across(c(arr_delay, dep_delay), c(avg=mean, sdev=sd), .names="{col}_{fn}"))


flights %>%
  drop_na(c(arr_delay,dep_delay)) %>% 
  group_by(origin) %>% 
  #filter(n() > 1) %>% 
  summarize(across(c(arr_delay, dep_delay), c(avg=mean, sdev=sd), .names="{fn}_{col}"))



flights %>%
  drop_na(dep_delay) %>%
  group_by(origin) %>%
  summarise(range=range(dep_delay))


flights %>%
  drop_na(dep_delay) %>%
  group_by(origin) %>%
  summarise(range=range(dep_delay)) %>%
  mutate(name=rep(c("min","max"), length(unique(origin)) )) %>% 
  pivot_wider(names_from = name, values_from=range)




decile <- seq(0, 1, 0.1)

flights %>%
  drop_na(dep_delay) %>%
  summarise(deciles = quantile(dep_delay, decile))


flights %>%
  drop_na(dep_delay) %>%
  group_by(origin) %>%
  summarise(deciles = quantile(dep_delay, decile)) %>%
  mutate(name = rep(paste0("dec_", decile), length(unique(origin)))) %>%
  pivot_wider(names_from = name, values_from = deciles)













































