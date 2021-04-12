library(tidyverse)
library(ggplot2)
library(zoo)
library(ggthemes)
library(plotly)
library(knitr)
library(kableExtra)

rsconnect::showLogs()
usethis::use_github()
# Dados ------------------------------------------------------------------------------------------------

base_covid <- read.delim("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv",
                         sep = ";")


base_covid_sp <- base_covid %>% 
  mutate(nome_munic = iconv(nome_munic, from = "UTF-8", to = "latin1"),
         datahora = lubridate::as_date(datahora))


botucatu <- base_covid_sp %>% filter(nome_munic == "Botucatu")


bauru <- base_covid_sp %>% filter(nome_munic == "Bauru")

sp <- base_covid_sp %>% filter(nome_munic == "São Paulo")

# Gráficos ----------------------------------------------------------------------------------------------

botucatu %>% mutate(media_movel = round(rollmeanr(casos_novos, 7, fill = NA))) %>% 
  ggplot(aes(datahora, casos_novos)) + 
  geom_col(aes(), width = 0.7, fill = "#4d94ff")+
  geom_line(aes(datahora, media_movel), colour = "#b30000", size = 1.2)+
  scale_x_date(breaks = "15 days", date_labels = "%b %d")+
  theme_hc()+
  xlab("")+
  ylab("")+
  ggtitle("Casos Diários de Covid-19 na Cidade de Botucatu (SP)")+
  labs(caption = "AUTOR: CAIO MARTINS", colour = "Blue")+
  theme(axis.text.x = element_text(angle = 45, face = "bold"),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))+
  guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE)+
  annotate("text", x = as.POSIXct("2021-01-02"), y = 34, label = "2021")



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












graficos_covid <- base_covid_sp %>% filter(nome_munic == "São Paulo")

dados_mes<- graficos_covid %>% mutate(ano = lubridate::year(datahora),
                                      mes = lubridate::month(mes),
                                      mes = case_when(mes == 1 ~ "Jan",
                                                      mes == 2 ~ "Fev",
                                                      mes == 3 ~ "Mar",
                                                      mes == 4 ~ "Abr",
                                                      mes == 5 ~ "Mai",
                                                      mes == 6 ~ "Jun",
                                                      mes == 7 ~ "Jul",
                                                      mes == 8 ~ "Ago",
                                                      mes == 9 ~ "Set",
                                                      mes == 10 ~ "Out",
                                                      mes == 11~ "Nov",
                                                      mes == 12 ~ "Dez"),
                                      mes = factor(mes, levels = c("Jan",
                                                                   "Fev",
                                                                   "Mar",
                                                                   "Abr",
                                                                   "Mai",
                                                                   "Jun",
                                                                   "Jul",
                                                                   "Ago",
                                                                   "Set",
                                                                   "Out",
                                                                   "Nov",
                                                                   "Dez"), 
                                                   ordered = TRUE))

dados_mes %>% 
  filter(nome_munic == "São Paulo") %>% 
  separate(datahora, "mes_ano", 7) %>% 
  group_by(mes_ano, ano) %>% 
  summarize(obitos_mes = sum(obitos_novos)) %>% 
  mutate(mes_ano = lubridate::as_date(as.yearmon(mes_ano)),
         ano = as_factor(ano)) %>% 
  ggplot(aes(mes_ano, obitos_mes, fill = ano))+
  geom_col()+
  xlab("")+
  ylab("")+
  theme_hc()+
  scale_fill_manual(name = "ano", values = c("#ff4d4d","#668cff"))+
  theme(axis.text.x = element_text(size = 11.5),
        axis.text.y = element_text(size = 15.0))+
  scale_y_continuous(breaks = scales::pretty_breaks())+
  scale_x_date(date_labels = "%b", breaks = "1 month")

dados_mes %>% filter(nome_munic == "São Paulo") %>% 
  mutate(ano = factor(ano)) %>% 
  group_by(mes, ano) %>%
  summarize(casos_mes = sum(casos_novos)) %>% 
  ggplot(aes(mes, casos_mes, fill = ano))+
  geom_col()+
  xlab("")+
  ylab("")+
  theme_hc()+
  scale_fill_manual(name = "ano", values = c("#ff4d4d","#668cff"))+
  theme(axis.text.x = element_text(size = 11.5),
        axis.text.y = element_text(size = 15.0))
  scale_x_continuous(breaks = scales::date_trans(mes))+
  scale_y_continuous(breaks = scales::pretty_breaks())












