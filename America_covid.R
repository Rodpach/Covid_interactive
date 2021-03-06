library(tidyverse)
library(lubridate)
library(plotly)
library(ggrepel)
library(ggpubr)
library(tabulizer)

fecha = "08 de abril 2020"
regiones = read_csv("https://raw.githubusercontent.com/Rodpach/Covid_interactive/master/Americas.csv")

total = read_csv("https://covid.ourworldindata.org/data/ecdc/full_data.csv")

total = total[,c(1,2,5,6)]

total = total %>% rename(Country = location) %>% rename(Casos = total_cases, Muertes = total_deaths)


total = left_join(total, regiones)

Casos_paises = filter(total, Country %in% c("Spain", "Italy", "South Korea") | Continente == 'Americas')

paises_menos_casos = Casos_paises %>% group_by(Country) %>% summarise(max_casos = max(Casos)) %>% filter(max_casos > 100) %>%
  select(Country) %>% .$Country


Casos_paises = filter(Casos_paises, Country %in% paises_menos_casos)

paises_dias_uncaso = split(Casos_paises,Casos_paises$Country)

paises_dias_uncaso = lapply(paises_dias_uncaso, function(x){
  x = filter(x, Casos > 0)
  dias = x$date[1]  %--% x$date
  x$dia = day(as.period(dias, "day"))
  x
})

paises_dias_uncaso = bind_rows(paises_dias_uncaso)

Casos_paises = left_join(Casos_paises, paises_dias_uncaso)

gg_Dia1_America = ggplot(Casos_paises, aes(x=dia, y = Casos, text = Country))+
  geom_line(aes(color = Country), show.legend = F)+
  geom_point(aes(color = Country), show.legend = F)+
  labs(y = "Casos totales", x = "Dias desde la primera infección", title = paste( "America - World in Data - ", fecha,". Otros países: Italia, Corea del Sur y España", sep = "")) +
  theme(legend.position = 'bottom') +
  theme_bw()

gg_Dia1_America = ggplotly(gg_Dia1_America, tooltip = c("text", "Casos"), dynamicTicks = T)%>% layout(legend = list(orientation = 'v'))

htmlwidgets::saveWidget(gg_Dia1_America, paste(getwd(),"/Covid_interactive/Ita_SK_Spain_Americas.html", sep = ''))

#GGPLOT
umbral = 45


max_dates = dplyr::filter(Casos_paises, dia <= umbral) %>% group_by(Country) %>% summarise(date=max(date)) %>% left_join(Casos_paises)
limite = dplyr::filter(max_dates, !Country %in% c("Italy", "South Korea","Spain")) %>% summarise(max = max(Casos))

limite = limite+50

gg_casos = ggplot(Casos_paises, aes(x=dia, y = Casos, text = Country))+
  geom_line(aes(color = Country))+
  geom_point(aes(color = Country)) +
  geom_text_repel(data = max_dates, aes(x=dia, y = Casos, label= paste(Country, "- Casos:", as.character(Casos))), 
                  angle        = 90,
                  vjust        = -2,
                  hjust = 3,
                  segment.size = 0.4,
                  direction     = "x",
                  segment.color = "black",
                  force = .5) +
  theme_classic()+
  scale_x_continuous(breaks = seq(0,umbral, 2), limits = c(0,umbral))+
  scale_y_continuous(breaks = seq(0,as.numeric(limite), 300), limits = c(0,as.numeric(limite)))+
  scale_color_discrete(breaks = c(""), 
                       labels = c(paste("Italia. Casos:",max_dates[max_dates$Country == "Italy",3], sep = ""))) +
  theme(legend.position = 'top',  axis.title = element_text(size=20)) +
  labs(y = "Casos totales", x = "Días desde la primera infección", title = paste("World in Data - ", fecha,". Umbral de ", umbral " días desde la primera infección.", sep = ""), color = "Otros países:")

#LOG
max_dates = Casos_paises %>% group_by(Country) %>% summarise(date=max(date)) %>% left_join(Casos_paises) %>% mutate(Casos = round(log(Casos), digits = 2))

gg_casos_log = ggplot(Casos_paises, aes(x=dia, y = log(Casos), text = Country))+
  geom_line(aes(color = Country), show.legend = F)+ #, span = 0.5, se =F, 
  geom_point(aes(color = Country), size = 0.4, show.legend = F) +
  geom_text_repel(data = filter(max_dates, dia < max(dia)-15), aes(x=dia, y = Casos, label= Country), 
                  angle        = 90,
                  nudge_y = 15,
                  segment.size = 0.4,
                  direction     = "x",
                  segment.color = "black",
                  force = .5) +
  geom_text_repel(data = filter(max_dates, dia > max(dia)-15), aes(x=dia, y = Casos, label= Country), 
                                               angle        = 90,
                  nudge_y = -9,
                  segment.size = 0.4,
                  direction     = "y",
                  segment.color = "black",
                  force = .5) +
  theme_classic()+
  scale_x_continuous(breaks = seq(0,100, 2))+
  theme(legend.position = 'top',  axis.title = element_text(size=20)) +
  labs(y = "log(Casos totales)", x = "Días desde la primera infección", title = paste("World in Data - ", fecha, ". Días totales desde la primera infección.", sep = ""), color = "Otros países:")

ggarrange(gg_casos, gg_casos_log, nrow = 2)

ggsave(paste(getwd(),'/Covid_interactive/Casos_primera_infeccion_America_umbral.jpg', sep = ""), width = 8.5, height = 8.5*2, units = 'in')

#
gg_America_dates =ggplot(dplyr::filter(total, Country %in% paises_menos_casos), aes(x=date, y=Casos, text = Country))+
  geom_line(aes(color = Country), show.legend = F)+
  geom_point(aes(color = Country), show.legend = F)+
  labs(y = "Casos totales", x = "Fecha", title = paste(regions[4], "America - World in Data -", fecha,". Otros países: Italia, Corea del Sur y España")) +
  theme(legend.position = 'bottom') +
  theme_bw()

gg_America_dates = ggplotly(gg_America_dates, tooltip = c("text", "date", "Casos"), dynamicTicks = T)%>% layout(legend = list(orientation = 'v'))

htmlwidgets::saveWidget(gg_America_dates, paste(getwd(),"/Covid_interactive/America.html", sep = ""))
