library(tidyverse)
library(ggplot2)
library(zoo)
library(ggthemes)
library(plotly)
library(knitr)
library(kableExtra)



# Dados ------------------------------------------------------------------------------------------------

base_covid <- read.delim("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv",
                         sep = ";")


base_covid_sp <- base_covid %>% 
  mutate(nome_munic = iconv(nome_munic, from = "UTF-8", to = "latin1"),
         datahora = lubridate::as_date(datahora))


botucatu <- base_covid_sp %>% filter(nome_munic == "Botucatu")


bauru <- base_covid_sp %>% filter(nome_munic == "Bauru")

# Gráficos ----------------------------------------------------------------------------------------------

botucatu %>% mutate(media_movel = round(rollmeanr(casos_novos, 7, fill = NA))) %>% 
  ggplot(aes(datahora, casos_novos)) + 
  geom_col(aes(colour = "White"), width = 0.8, fill = "#DC143C")+
  geom_line(aes(datahora, media_movel), colour = "Blue", size = 1.2)+
  scale_x_date(breaks = "15 days", date_labels = "%b %d")+
  theme_hc()+
  xlab("")+
  ylab("")+
  ggtitle("Casos Diários de Covid-19 na Cidade de Botucatu (SP)")+
  labs(caption = "AUTOR: CAIO MARTINS", colour = "Blue")+
  theme(axis.text.x = element_text(angle = 45),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))+
  guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE)
  


bauru %>% mutate(media_movel = round(rollmeanr(casos_novos, 7, fill = NA))) %>% 
  ggplot(aes(datahora, casos_novos)) + 
  geom_col(aes(colour = "White"), width = 0.8, fill = "#DC143C")+
  geom_line(aes(datahora, media_movel), colour = "Blue", size = 1.2)+
  scale_x_date(breaks = "15 days", date_labels = "%b %d")+
  theme_hc()+
  xlab("")+
  ylab("")+
  ggtitle("Casos Diários de Covid-19 na Cidade de Bauru (SP)")+
  labs(caption = "AUTOR: CAIO MARTINS")+
  theme(axis.text.x = element_text(angle = 45),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))+
  guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE)



# Gráficos Interativos --------------------------------------------------------------------------

botucatu_mapa <- botucatu %>% 
  mutate(media_movel = round(rollmeanr(casos_novos, 7, fill = NA))) %>% 
  ggplot(aes(datahora, casos_novos)) + 
  geom_col(aes(colour = "White"), width = 0.8, fill = "#DC143C")+
  geom_line(aes(datahora, media_movel), colour = "Blue", size = 1.2)+
  scale_x_date(breaks = "15 days", date_labels = "%b %d")+
  theme_hc()+
  xlab("")+
  ylab("")+
  ggtitle("Casos Diários de Covid-19 na Cidade de Botucatu")+
  theme(axis.text.x = element_text(angle = 45),plot.title = element_text(hjust = 0.5))+
  guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE)


botucatu_interativo <- ggplotly(botucatu_mapa, tooltip = "all")



botucatu_tabela <-  botucatu %>%
  select(nome_munic,
         datahora,
         mes,
         casos,
         casos_novos,
         obitos,
         obitos_novos)


botucatu_tabela <- botucatu_tabela %>% 
  mutate(mes = ifelse(mes < 2, 13, mes)) %>% 
  group_by(mes) %>% 
  summarize(soma = sum(casos_novos)) %>% 
  arrange(mes) %>% 
  rename(Nº_Casos = soma)

botucatu_tabela <- botucatu_tabela %>%
  mutate(mes = case_when(mes == 13 ~ "Janeiro-2021",
                         mes == 2 ~ "Fevereiro_2020",
                         mes == 3 ~ "Março_2020",
                         mes == 4 ~ "Abril_2020",
                         mes == 5 ~ "Maio_2020",
                         mes == 6 ~ "Junho_2020",
                         mes == 7 ~ "Julho_2020",
                         mes == 8 ~ "Agosto_2020",
                         mes == 9 ~ "Setembro_2020",
                         mes == 10 ~ "Outubro_2020",
                         mes == 11~ "Novembro_2020",
                         mes == 12 ~ "Dezembro_2020"))


botucatu_tabela %>%
  kbl() %>%
  kable_classic_2(full_width = F)


botucatu_tabela_obitos <-  botucatu %>%
  select(nome_munic,
         datahora,
         mes,
         casos,
         casos_novos,
         obitos,
         obitos_novos)


botucatu_tabela_obitos <- botucatu_tabela_obitos %>% 
  mutate(mes = ifelse(mes < 2, 13, mes)) %>% 
  group_by(mes) %>% 
  summarize(soma = sum(obitos_novos)) %>% 
  arrange(mes) %>% 
  rename(Nº_Obitos = soma)

botucatu_tabela_obitos <- botucatu_tabela_obitos %>%
  mutate(mes = case_when(mes == 13 ~ "Janeiro-2021",
                         mes == 2 ~ "Fevereiro_2020",
                         mes == 3 ~ "Março_2020",
                         mes == 4 ~ "Abril_2020",
                         mes == 5 ~ "Maio_2020",
                         mes == 6 ~ "Junho_2020",
                         mes == 7 ~ "Julho_2020",
                         mes == 8 ~ "Agosto_2020",
                         mes == 9 ~ "Setembro_2020",
                         mes == 10 ~ "Outubro_2020",
                         mes == 11~ "Novembro_2020",
                         mes == 12 ~ "Dezembro_2020"))


botucatu_tabela_obitos %>%
  kbl() %>%
  kable_classic_2(full_width = F)


