get_moving_average <- function(dat, x){
   ndays = nrow(dat)
   moving_average = c()
   for(i in 1:(ndays-x+1)){
      ini = i
      end = i+(x-1)
      moving_average = c(moving_average, mean(dat$confirmed[ini:end]))
   }   
   return(data.frame(dat[-c(1:(x-1)),],moving_average = moving_average))
}


library(tidyverse)
library(lubridate)

link = "https://brasil.io/dataset/covid19/caso/?place_type=city&format=csv"

dat = read.csv(link, stringsAsFactors = FALSE)
dat <- dat %>% mutate(date = ymd(date))

dat_confirmed_ba = dat %>% filter(state == "BA") %>% group_by(date) %>% summarise(confirmed = sum(confirmed, na.rm=TRUE)) %>% mutate(confirmed = c(0,diff(confirmed)))
dat_confirmed_sp = dat %>% filter(state == "SP") %>% group_by(date) %>% summarise(confirmed = sum(confirmed, na.rm=TRUE)) %>% mutate(confirmed = c(0,diff(confirmed)))
dat_confirmed_mg = dat %>% filter(state == "MG") %>% group_by(date) %>% summarise(confirmed = sum(confirmed, na.rm=TRUE)) %>% mutate(confirmed = c(0,diff(confirmed)))
dat_confirmed_df = dat %>% filter(state == "DF") %>% group_by(date) %>% summarise(confirmed = sum(confirmed, na.rm=TRUE)) %>% mutate(confirmed = c(0,diff(confirmed)))
dat_confirmed_rj = dat %>% filter(state == "RJ") %>% group_by(date) %>% summarise(confirmed = sum(confirmed, na.rm=TRUE)) %>% mutate(confirmed = c(0,diff(confirmed)))
dat_confirmed_ma = dat %>% filter(state == "MA") %>% group_by(date) %>% summarise(confirmed = sum(confirmed, na.rm=TRUE)) %>% mutate(confirmed = c(0,diff(confirmed)))
dat_confirmed_ce = dat %>% filter(state == "CE") %>% group_by(date) %>% summarise(confirmed = sum(confirmed, na.rm=TRUE)) %>% mutate(confirmed = c(0,diff(confirmed)))
dat_confirmed_am = dat %>% filter(state == "AM") %>% group_by(date) %>% summarise(confirmed = sum(confirmed, na.rm=TRUE)) %>% mutate(confirmed = c(0,diff(confirmed)))


x = 7
mov_av_ba_7 = get_moving_average(dat_confirmed_ba, x)
mov_av_sp_7 = get_moving_average(dat_confirmed_sp, x)
mov_av_rj_7 = get_moving_average(dat_confirmed_rj, x)
mov_av_mg_7 = get_moving_average(dat_confirmed_mg, x)
mov_av_df_7 = get_moving_average(dat_confirmed_df, x)
mov_av_am_7 = get_moving_average(dat_confirmed_am, x)
mov_av_ce_7 = get_moving_average(dat_confirmed_ce, x)
mov_av_ma_7 = get_moving_average(dat_confirmed_ma, x)

x = 5
mov_av_ba_5 = get_moving_average(dat_confirmed_ba, x)
mov_av_sp_5 = get_moving_average(dat_confirmed_sp, x)
mov_av_rj_5 = get_moving_average(dat_confirmed_rj, x)
mov_av_mg_5 = get_moving_average(dat_confirmed_mg, x)
mov_av_df_5 = get_moving_average(dat_confirmed_df, x)
mov_av_am_5 = get_moving_average(dat_confirmed_am, x)
mov_av_ce_5 = get_moving_average(dat_confirmed_ce, x)
mov_av_ma_5 = get_moving_average(dat_confirmed_ma, x)


x = 9
mov_av_ba_9 = get_moving_average(dat_confirmed_ba, x)
mov_av_sp_9 = get_moving_average(dat_confirmed_sp, x)
mov_av_rj_9 = get_moving_average(dat_confirmed_rj, x)
mov_av_mg_9 = get_moving_average(dat_confirmed_mg, x)
mov_av_df_9 = get_moving_average(dat_confirmed_df, x)
mov_av_am_9 = get_moving_average(dat_confirmed_am, x)
mov_av_ce_9 = get_moving_average(dat_confirmed_ce, x)
mov_av_ma_9 = get_moving_average(dat_confirmed_ma, x)

x = 14
mov_av_ba_14 = get_moving_average(dat_confirmed_ba, x)
mov_av_sp_14 = get_moving_average(dat_confirmed_sp, x)
mov_av_rj_14 = get_moving_average(dat_confirmed_rj, x)
mov_av_mg_14 = get_moving_average(dat_confirmed_mg, x)
mov_av_df_14 = get_moving_average(dat_confirmed_df, x)
mov_av_am_14 = get_moving_average(dat_confirmed_am, x)
mov_av_ce_14 = get_moving_average(dat_confirmed_ce, x)
mov_av_ma_14 = get_moving_average(dat_confirmed_ma, x)


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

mov_av_am_7$tw = "7"
mov_av_am_7$state = "AMAZONAS"
mov_av_am_5$tw = "5"
mov_av_am_5$state = "AMAZONAS"
mov_av_am_9$tw = "9"
mov_av_am_9$state = "AMAZONAS"
mov_av_am_14$tw = "14"
mov_av_am_14$state = "AMAZONAS"
mov_av_am = rbind(mov_av_am_5, mov_av_am_7, mov_av_am_9, mov_av_am_14)


mov_av_rj_7$tw = "7"
mov_av_rj_7$state = "RIO DE JANEIRO"
mov_av_rj_5$tw = "5"
mov_av_rj_5$state = "RIO DE JANEIRO"
mov_av_rj_9$tw = "9"
mov_av_rj_9$state = "RIO DE JANEIRO"
mov_av_rj_14$tw = "14"
mov_av_rj_14$state = "RIO DE JANEIRO"
mov_av_rj = rbind(mov_av_rj_5, mov_av_rj_7, mov_av_rj_9, mov_av_rj_14)

mov_av_mg_7$tw = "7"
mov_av_mg_7$state = "MINAS GERAIS"
mov_av_mg_5$tw = "5"
mov_av_mg_5$state = "MINAS GERAIS"
mov_av_mg_9$tw = "9"
mov_av_mg_9$state = "MINAS GERAIS"
mov_av_mg_14$tw = "14"
mov_av_mg_14$state = "MINAS GERAIS"
mov_av_mg = rbind(mov_av_mg_5, mov_av_mg_7, mov_av_mg_9, mov_av_mg_14)

mov_av_df_7$tw = "7"
mov_av_df_7$state = "DISTRITO FEDERAL"
mov_av_df_5$tw = "5"
mov_av_df_5$state = "DISTRITO FEDERAL"
mov_av_df_9$tw = "9"
mov_av_df_9$state = "DISTRITO FEDERAL"
mov_av_df_14$tw = "14"
mov_av_df_14$state = "DISTRITO FEDERAL"
mov_av_df = rbind(mov_av_df_5, mov_av_df_7, mov_av_df_9, mov_av_df_14)

mov_av = rbind(mov_av_ba, mov_av_ce, mov_av_ma, mov_av_sp, mov_av_am, mov_av_rj, mov_av_df, mov_av_mg)

png("evolution_confirmed_brasil.png",width=3200,height=1800,res=300)
mov_av %>% filter(tw == "14") %>% 
   group_by(tw)%>% 
   ggplot(aes(x = date, y = moving_average)) + 
   geom_line() + 
   labs(x = "Date", y = "Average confirmed", col = "window average", title = "Covid19 daily confirmed", subtitle = "Moving average - 14 dias", caption = "by @Dadoscope1") +
   theme_minimal() + 
   facet_wrap(~ state, scales = "free_y")
dev.off()
