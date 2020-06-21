get_moving_average <- function(dat, x){
   ndays = nrow(dat)
   moving_average = c()
   for(i in 1:(ndays-x+1)){
      ini = i
      end = i+(x-1)
      moving_average = c(moving_average, mean(dat$deaths[ini:end],na.rm=TRUE))
   }   
   return(data.frame(dat[-c(1:(x-1)),],moving_average = moving_average,tw=as.character(x)))
}


library(tidyverse)
library(lubridate)

link = "https://brasil.io/dataset/covid19/caso/?place_type=city&format=csv"

dat = read.csv(link, stringsAsFactors = FALSE)
dat <- dat %>% mutate(date = ymd(date))

dat_deaths_ba = dat %>% filter(state == "BA") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_ma = dat %>% filter(state == "MA") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_ce = dat %>% filter(state == "CE") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_pe = dat %>% filter(state == "PE") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_pb = dat %>% filter(state == "PB") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_rn = dat %>% filter(state == "RN") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_pi = dat %>% filter(state == "PI") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_se = dat %>% filter(state == "SE") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_al = dat %>% filter(state == "AL") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_sp = dat %>% filter(state == "SP") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_mg = dat %>% filter(state == "MG") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_rj = dat %>% filter(state == "RJ") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_es = dat %>% filter(state == "ES") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_am = dat %>% filter(state == "AM") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_pa = dat %>% filter(state == "PA") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_rr = dat %>% filter(state == "RR") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_ap = dat %>% filter(state == "AP") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_ro = dat %>% filter(state == "RO") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_to = dat %>% filter(state == "TO") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_ac = dat %>% filter(state == "AC") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_ms = dat %>% filter(state == "MS") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_mt = dat %>% filter(state == "MT") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_go = dat %>% filter(state == "GO") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_df = dat %>% filter(state == "DF") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_rs = dat %>% filter(state == "RS") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_pr = dat %>% filter(state == "PR") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_sc = dat %>% filter(state == "SC") %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state=unique(state)) %>% mutate(deaths = c(0,diff(deaths)))
dat_deaths_br = dat %>% group_by(date) %>% summarise(deaths = sum(deaths, na.rm=TRUE),state="BR") %>% mutate(deaths = c(0,diff(deaths)))

x = 14
mov_av_ba = get_moving_average(dat_deaths_ba, x)
mov_av_ce = get_moving_average(dat_deaths_ce, x)
mov_av_ma = get_moving_average(dat_deaths_ma, x)
mov_av_pi = get_moving_average(dat_deaths_pi, x)
mov_av_rn = get_moving_average(dat_deaths_rn, x)
mov_av_pb = get_moving_average(dat_deaths_pb, x)
mov_av_pe = get_moving_average(dat_deaths_pe, x)
mov_av_se = get_moving_average(dat_deaths_se, x)
mov_av_al = get_moving_average(dat_deaths_al, x)
mov_av_sp = get_moving_average(dat_deaths_sp, x)
mov_av_rj = get_moving_average(dat_deaths_rj, x)
mov_av_mg = get_moving_average(dat_deaths_mg, x)
mov_av_es = get_moving_average(dat_deaths_es, x)
mov_av_df = get_moving_average(dat_deaths_df, x)
mov_av_go = get_moving_average(dat_deaths_go, x)
mov_av_mt = get_moving_average(dat_deaths_mt, x)
mov_av_ms = get_moving_average(dat_deaths_ms, x)
mov_av_am = get_moving_average(dat_deaths_am, x)
mov_av_pa = get_moving_average(dat_deaths_pa, x)
mov_av_ro = get_moving_average(dat_deaths_ro, x)
mov_av_ac = get_moving_average(dat_deaths_ac, x)
mov_av_rr = get_moving_average(dat_deaths_rr, x)
mov_av_ap = get_moving_average(dat_deaths_ap, x)
mov_av_to = get_moving_average(dat_deaths_to, x)
mov_av_sc = get_moving_average(dat_deaths_sc, x)
mov_av_pr = get_moving_average(dat_deaths_pr, x)
mov_av_rs = get_moving_average(dat_deaths_rs, x)
mov_av_br = get_moving_average(dat_deaths_br, x)


mov_av = rbind(mov_av_ma,mov_av_pi,mov_av_ce,mov_av_rn,mov_av_pb,mov_av_pe,mov_av_al,mov_av_se,mov_av_ba,
               mov_av_sp,mov_av_rj,mov_av_es,mov_av_mg,
               mov_av_am,mov_av_pa,mov_av_to,mov_av_ap,mov_av_ac,mov_av_rr,mov_av_ro,
               mov_av_df,mov_av_go,mov_av_mt,mov_av_ms,
               mov_av_sc,mov_av_pr,mov_av_rs)

for(st in unique(sort(mov_av$state))){
   mov_av_st = mov_av %>% filter(state == st)
   p1 <- mov_av_st %>% 
      ggplot(aes(x = lubridate::ymd(date), y = moving_average)) + 
      #   geom_bar(fill="red4",stat="identity") +
      geom_line() + 
      labs(x = "Date", y = "Mortes confirmadas", col = "window average", title = "Mortes confirmadas por Covid19", subtitle = "Moving average - 14 dias", caption = paste("created by @Dadoscope1 @",Sys.Date())) +
      theme_minimal() + 
      facet_wrap(~ state, scale="free_y")
   png(paste0("evolution_deaths_",st,".png"),width=3200,height=1800,res=300)
   print(p1)
   dev.off()
}

##

