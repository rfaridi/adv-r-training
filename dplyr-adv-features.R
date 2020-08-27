## ----setup, include = FALSE---------------------------------------------------
library(knitr)
opts_chunk$set(message=F, 
		   warning=F, 
		   comment=NA)


## ---- include=F---------------------------------------------------------------
library(tidyverse)
library(nycflights13)
library(flipbookr)


## ----child='./dplyr-adv-features-p1-relocate.rmd', eval=T---------------------

## ----relocate, include = FALSE------------------------------------------------
flights %>% 
    relocate(carrier)  %>% 
    head(2)
flights %>% 
    relocate(carrier, .after=day) %>% 
    head(2)
flights %>% 
    relocate(carrier, .before=air_time) %>% 
    head(2)
flights %>% 
    relocate(carrier, .after =last_col()) %>% 
    head(2)


## ----relocate2, include = FALSE-----------------------------------------------
flights %>% 
    relocate(is.numeric, .after =last_col()) %>% 
    head(2)
flights %>% 
    relocate(is.character, .after = is.numeric) %>% 
    head(2)



## ----child='./dplyr-adv-features-p2-across.rmd', eval=T-----------------------

## ----across, include = FALSE--------------------------------------------------
flights %>% 
    summarize_if(is.character, n_distinct)
flights %>% 
    summarize(across(where(is.character), n_distinct))
flights %>% 
    summarize_at(vars(tailnum, origin, carrier), n_distinct)
flights %>% 
    summarize(across(where(is.numeric) & ends_with("delay"), mean,na.rm=T))
flights%>% 
    group_by(origin) %>% 
    filter(n() > 1) %>% 
    summarize(across(c(tailnum, carrier), n_distinct))


## ----across2, include = FALSE-------------------------------------------------

flights %>% 
    group_by(origin) %>% 
    filter(n() > 1) %>% 
    summarize(across(c(arr_delay,dep_delay),mean))

flights %>% 
    group_by(origin) %>% 
    filter(n() > 1) %>% 
    summarize(across(c(arr_delay,dep_delay),mean,na.rm=T))

flights %>% 
    drop_na(c(arr_delay,dep_delay)) %>% 
    group_by(origin) %>% 
    filter(n() > 1) %>% 
    summarize(across(c(arr_delay,dep_delay),mean))

flights %>% 
    drop_na(c(arr_delay,dep_delay)) %>% 
    group_by(origin) %>% 
    filter(n() > 1) %>% 
    summarize(across(c(arr_delay,dep_delay),mean,.names = "mean_{col}"))

flights %>% 
    drop_na(c(arr_delay,dep_delay)) %>% 
    group_by(origin) %>% 
    filter(n() > 1) %>% 
    summarize(across(c(arr_delay,dep_delay),c(avg=mean,sdev=sd),.names = "{col}_{fn}"))

flights %>% 
    group_by(origin) %>% 
    filter(n() > 1) %>% 
    summarize(across(c(arr_delay,dep_delay),c(avg=~mean(.x,na.rm=T),sdev=~sd(.x,na.rm=T)),.names = "{col}_{fn}"))



## ----across3, include = FALSE-------------------------------------------------

flights %>% 
    drop_na(dep_delay) %>% 
    group_by(origin) %>% 
    summarise(range=range(dep_delay))
flights %>% 
    drop_na(dep_delay) %>% 
    group_by(origin) %>% 
    summarise(range=range(dep_delay)) %>% 
    mutate(name=rep(c("min","max"), length(unique(origin)))) %>% 
    pivot_wider(names_from=name, values_from=range)
# alternative 

flights %>% 
    drop_na(dep_delay) %>% 
    group_by(origin) %>% 
    summarise(min=min(dep_delay), max=max(dep_delay)) 
# May the second one much easier
# But useful in longer output 


## ----across4, include = FALSE-------------------------------------------------
decile <- seq(0, 1, 0.1)
flights %>% 
  drop_na(dep_delay) %>% 
  group_by(origin) %>% 
  summarise(deciles = quantile(dep_delay, decile)) %>% 
  mutate(name = rep(paste0("dec_", decile), length(unique(origin)))) %>% 
  pivot_wider(names_from = name, values_from = deciles)


