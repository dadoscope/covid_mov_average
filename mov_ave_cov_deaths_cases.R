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

theme_set(theme_bw() +  theme_bw(base_size = 18) +  theme(panel.border = element_rect(size = 1.2)))
plasma_pal <- c("red", viridis::plasma(n = 6))

library(forecast)
library(tidyverse)
library(lubridate)
library(data.table)
library(tibble)

setwd("/Users/isiscosta/RScript/moving_average_covid_deaths")
link = "https://data.brasil.io/dataset/covid19/caso.csv.gz"
dat = fread(link, stringsAsFactors = FALSE)
regioes <- c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste")

dat <- dat %>% 
  mutate(date = ymd(date)) %>%
  filter(place_type == "state") %>% 
  mutate(cod_regiao = trunc(city_ibge_code / 10),
         regiao = regioes[cod_regiao])

greens <- c("AM", "AP")
oranges <- c("AC","PA","RO","RR","CE","BA","AL","PB","SE","RN","RJ","GO")
reds <- c("PI","ES","MG","SP","PR","SC","RS","MT","MS","DF")
purples <- c("TO","PE","MA")
colors_uf <- tibble(uf = greens, color = "green4") %>% 
  rbind(tibble(uf = oranges, color = "orange3")) %>%
  rbind(tibble(uf = reds, color = "red3")) %>%
  rbind(tibble(uf = purples, color = "purple"))
allcolors <- unique(colors_uf$color)
for(c in allcolors){
  p1 <- dat %>% 
    inner_join(colors_uf, by = c("state" = "uf")) %>%
    filter(color == c) %>%
    group_by(state) %>%
    arrange(date) %>%
    mutate(confirmed = c(0,diff(confirmed))) %>%
    mutate(confirmed = forecast::ma(confirmed,order=7)) %>%
    ggplot(aes(x=date, y=confirmed)) + 
    geom_line(cex = 1.5, color = c) + 
    facet_wrap(~ state, ncol = 3, scale = "free_y") +
    labs(title = "Médias Móveis de casos confirmados de COVID19",
         #subtitle = paste0("Região ",r),
         x = "Data",
         y = "Casos confirmados",
         col = "")
  png(paste0("moving_average_casos_color_",c,".png"),width=3200,height=1800,res=300)
  print(p1)
  dev.off()
}
  
greens <- c("AC","AM","RO","PA","CE","PE","RJ")
oranges <- c("AP", "MA", "AL", "ES", "SP")
reds <- c("PI","RN","SE","PB","BA", "MG","PR","RS","SC","DF","GO","MS","MT")
purples <- c("TO","RR")
colors_uf <- tibble(uf = greens, color = "green4") %>% 
  rbind(tibble(uf = oranges, color = "orange3")) %>%
  rbind(tibble(uf = reds, color = "red3")) %>%
  rbind(tibble(uf = purples, color = "purple"))
allcolors <- unique(colors_uf$color)
for(c in allcolors){
  p1 <- dat %>% 
    inner_join(colors_uf, by = c("state" = "uf")) %>%
    filter(color == c) %>%
    group_by(state) %>%
    arrange(date) %>%
    mutate(deaths = c(0,diff(deaths))) %>%
    mutate(deaths = forecast::ma(deaths,order=7)) %>%
    ggplot(aes(x=date, y=deaths)) + 
    geom_line(cex = 1.5, col = c) + 
    facet_wrap(~ state, ncol = 3, scale = "free_y") +
    labs(title = "Médias Móveis de mortes por COVID19 confirmadas",
         #subtitle = paste0("Região ",r),
         x = "Data",
         y = "Mortes confirmadas")
  png(paste0("moving_average_mortes_color_",c,".png"),width=3200,height=1800,res=300)
  print(p1)
  dev.off()
}
  
  
### Dados da Espanha

casos_es <- fread("nuevos_casos_diarios_diag.csv")
casos_es <- casos_es %>% 
  mutate(date = paste(DayMonth,Year,sep="/")) %>%
  mutate(date = dmy(date) %>% format("%Y-%m-%d")) %>%
  mutate(date = ymd(date)) %>% 
  select(date,Cases) 

p1 <- casos_es %>% 
  arrange(date) %>%
  mutate(confirmed = forecast::ma(Cases,order=7)) %>% ungroup() %>%
  ggplot(aes(x=date, y=confirmed)) + 
  geom_line(cex = 1.5) +
  labs(title = "Médias Móveis de casos confirmados de COVID19",
       subtitle = "Espanha",
       x = "Data",
       y = "Casos confirmados")
png("casos_espanha.png",width=3200,height=1800,res=300)
print(p1)
dev.off()


p1 <- casos_es %>% 
  mutate(state = "Espanha") %>%
  arrange(date) %>%
  mutate(Cases = forecast::ma(Cases,order=7)) %>% 
  rbind(dat %>% 
          filter(state == "SP") %>%
          group_by(state) %>%
          arrange(date) %>%
          mutate(confirmed = c(0,diff(confirmed))) %>%
          mutate(confirmed = forecast::ma(confirmed,order=7)) %>% 
          ungroup() %>%
          select(date, Cases = confirmed, state)) %>%
  ggplot(aes(x=date, y=Cases, col = state)) + 
  geom_line(cex = 1.5) +
  labs(title = "Médias Móveis de casos confirmados de COVID19",
       subtitle = "Espanha e São Paulo",
       x = "Data",
       y = "Casos confirmados",
       col = "Estado")
png("casos_espanha_saopaulo.png",width=3200,height=1800,res=300)
print(p1)
dev.off()
  


mortes_es <- fread("muertos_diarios_por_coron.csv")
mortes_es <- mortes_es %>% 
  mutate(date = paste(DayMonth,Year,sep="/")) %>%
  mutate(date = dmy(date) %>% format("%Y-%m-%d")) %>%
  mutate(date = ymd(date)) %>% 
  select(date,deaths) 
p1 <- mortes_es %>% 
  mutate(state = "Espanha") %>%
  arrange(date) %>%
  mutate(deaths = forecast::ma(deaths,order=7)) %>% 
  rbind(dat %>% 
          filter(state == "SP") %>%
          group_by(state) %>%
          arrange(date) %>%
          mutate(deaths = c(0,diff(deaths))) %>% 
          mutate(deaths = forecast::ma(deaths,order=7)) %>% 
          ungroup() %>%
          select(date, deaths, state)) %>%
  ggplot(aes(x=date, y=deaths, col = state)) + 
  geom_line(cex = 1.5) +
  labs(title = "Médias Móveis de mortes confirmadas de COVID19",
       subtitle = "Espanha e São Paulo",
       x = "Data",
       y = "Mortes confirmadas",
       col = "Estado")
png("mortes_espanha_saopaulo.png",width=3200,height=1800,res=300)
print(p1)
dev.off()

##