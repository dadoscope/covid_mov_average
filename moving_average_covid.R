get_moving_average <- function(dat, x){
   ndays = nrow(dat)
   moving_average = c()
   for(i in 1:(ndays-x+1)){
      ini = i
      end = i+(x-1)
      moving_average = c(moving_average, mean(dat$deaths[ini:end]))
   }   
   return(data.frame(dat[-c(1:(x-1)),],moving_average = moving_average))
}

plot_moving_average <- function(dat_death_ba_7, state = "BAHIA", x = 7){
   mov_av_ba_7 %>% ggplot(aes(x = date, y = moving_average))
}


library(tidyverse)
library(lubridate)

link = "https://brasil.io/dataset/covid19/caso/?place_type=city&format=csv"

dat = read.csv(link, stringsAsFactors = FALSE)
dat <- dat %>% mutate(date = ymd(date))


dat_death_ba = dat %>% filter(state == "BA") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE)) %>% mutate(deaths = c(0,diff(deaths)))
dat_death_sp = dat %>% filter(state == "SP") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE)) %>% mutate(deaths = c(0,diff(deaths)))
dat_death_mg = dat %>% filter(state == "MG") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE)) %>% mutate(deaths = c(0,diff(deaths)))
dat_death_df = dat %>% filter(state == "DF") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE)) %>% mutate(deaths = c(0,diff(deaths)))
dat_death_rj = dat %>% filter(state == "RJ") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE)) %>% mutate(deaths = c(0,diff(deaths)))
dat_death_ma = dat %>% filter(state == "MA") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE)) %>% mutate(deaths = c(0,diff(deaths)))
dat_death_ce = dat %>% filter(state == "CE") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE)) %>% mutate(deaths = c(0,diff(deaths)))
dat_death_am = dat %>% filter(state == "AM") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE)) %>% mutate(deaths = c(0,diff(deaths)))

x = 7
mov_av_ba_7 = get_moving_average(dat_death_ba, x)
mov_av_sp_7 = get_moving_average(dat_death_sp, x)
mov_av_rj_7 = get_moving_average(dat_death_rj, x)
mov_av_mg_7 = get_moving_average(dat_death_mg, x)
mov_av_df_7 = get_moving_average(dat_death_df, x)
mov_av_am_7 = get_moving_average(dat_death_am, x)
mov_av_ce_7 = get_moving_average(dat_death_ce, x)
mov_av_ma_7 = get_moving_average(dat_death_ma, x)

x = 5
mov_av_ba_5 = get_moving_average(dat_death_ba, x)
mov_av_sp_5 = get_moving_average(dat_death_sp, x)
mov_av_rj_5 = get_moving_average(dat_death_rj, x)
mov_av_mg_5 = get_moving_average(dat_death_mg, x)
mov_av_df_5 = get_moving_average(dat_death_df, x)
mov_av_am_5 = get_moving_average(dat_death_am, x)
mov_av_ce_5 = get_moving_average(dat_death_ce, x)
mov_av_ma_5 = get_moving_average(dat_death_ma, x)


x = 9
mov_av_ba_9 = get_moving_average(dat_death_ba, x)
mov_av_sp_9 = get_moving_average(dat_death_sp, x)
mov_av_rj_9 = get_moving_average(dat_death_rj, x)
mov_av_mg_9 = get_moving_average(dat_death_mg, x)
mov_av_df_9 = get_moving_average(dat_death_df, x)
mov_av_am_9 = get_moving_average(dat_death_am, x)
mov_av_ce_9 = get_moving_average(dat_death_ce, x)
mov_av_ma_9 = get_moving_average(dat_death_ma, x)

x = 14
mov_av_ba_14 = get_moving_average(dat_death_ba, x)
mov_av_sp_14 = get_moving_average(dat_death_sp, x)
mov_av_rj_14 = get_moving_average(dat_death_rj, x)
mov_av_mg_14 = get_moving_average(dat_death_mg, x)
mov_av_df_14 = get_moving_average(dat_death_df, x)
mov_av_am_14 = get_moving_average(dat_death_am, x)
mov_av_ce_14 = get_moving_average(dat_death_ce, x)
mov_av_ma_14 = get_moving_average(dat_death_ma, x)


mov_av_ba_7$tw = "7"
mov_av_ba_7$state = "BAHIA"
mov_av_ba_5$tw = "5"
mov_av_ba_5$state = "BAHIA"
mov_av_ba_9$tw = "9"
mov_av_ba_9$state = "BAHIA"
mov_av_ba_14$tw = "14"
mov_av_ba_14$state = "BAHIA"
mov_av_ba = rbind(mov_av_ba_5, mov_av_ba_7, mov_av_ba_9, mov_av_ba_14)

mov_av_sp_7$tw = "7"
mov_av_sp_7$state = "SAO PAULO"
mov_av_sp_5$tw = "5"
mov_av_sp_5$state = "SAO PAULO"
mov_av_sp_9$tw = "9"
mov_av_sp_9$state = "SAO PAULO"
mov_av_sp_14$tw = "14"
mov_av_sp_14$state = "SAO PAULO"
mov_av_sp = rbind(mov_av_sp_5, mov_av_sp_7, mov_av_sp_9, mov_av_sp_14)

mov_av_ma_7$tw = "7"
mov_av_ma_7$state = "MARANHAO"
mov_av_ma_5$tw = "5"
mov_av_ma_5$state = "MARANHAO"
mov_av_ma_9$tw = "9"
mov_av_ma_9$state = "MARANHAO"
mov_av_ma_14$tw = "14"
mov_av_ma_14$state = "MARANHAO"
mov_av_ma = rbind(mov_av_ma_5, mov_av_ma_7, mov_av_ma_9, mov_av_ma_14)

mov_av_ce_7$tw = "7"
mov_av_ce_7$state = "CEARA"
mov_av_ce_5$tw = "5"
mov_av_ce_5$state = "CEARA"
mov_av_ce_9$tw = "9"
mov_av_ce_9$state = "CEARA"
mov_av_ce_14$tw = "14"
mov_av_ce_14$state = "CEARA"
mov_av_ce = rbind(mov_av_ce_5, mov_av_ce_7, mov_av_ce_9, mov_av_ce_14)

mov_av = rbind(mov_av_ba, mov_av_ce, mov_av_ma, mov_av_sp)

mov_av_ce %>% filter(tw == "14") %>% 
   group_by(tw)%>% 
   ggplot(aes(x = date, y = moving_average)) + 
   geom_line() + 
   labs(x = "Date", y = "Average deaths", col = "window average", title = "Covid19 daily deaths", subtitle = "Moving average - 14 dias") +
   theme_minimal() + 
   facet_wrap(~ state)

