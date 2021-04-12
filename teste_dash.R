---
  title: "Covid-19 em Botucatu e Região"
output: 
  flexdashboard::flex_dashboard:
  orientation: rows
social: menu
source_code: embed
runtime: shiny 
---
  
  
  
  ```{r global, include=FALSE}
options(OutDec = ",")  

library(flexdashboard)
library(tidyverse)
library(ggplot2)
library(zoo)
library(ggthemes)
library(knitr)
library(kableExtra)
library(shiny)
base_covid <- read.delim("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv",
                         sep = ";")

base_covid_sp <- base_covid %>% 
  mutate(nome_munic = iconv(nome_munic, from = "UTF-8", to = "latin1"),
         datahora = lubridate::as_date(datahora))


graficos_covid <- base_covid_sp %>% filter(nome_munic == "Botucatu" | nome_munic == "Bauru")


```{r reactive}
# Create reative dataframe to filter by inputs
df_municipios_react <- reactive({
  
  
  # Apply inputs as filter criteria
  
  
  df <- graficos_covid %>% 
    filter(municipio %in% input$cidades) %>%
    
    
    
    return(df)
})
```

```

Inputs {.sidebar}
-----------------------------------------------------------------------
  
  \newline

### COVID-19 - BOTUCATU E REGIÃO

#### Neste arquivo apresento os dados da Covid-19 na cidade de Botucatu e Bauru.

#### O gráfico apresenta o número de CASOS DIÁRIOS da doença, exemplificado pelas barras em vermelho, e a média móvel dos últimos 7 dias em azul.
\newline
\newline

#### Selecione abaixo a cidade
```{r}
#selectInput("municipios",
#           label = "Cidade: ",
#          choices = unique(graficos_covid$nome_munic),
#         selected = 1)

selectInput("cidades",
            label = "Escolha a cidade que deseja visualizar: ",
            choices = sort(unique(df_municipios_react$cidades))
            selected = 1)

```

\newline

Os dados foram retirados do repositório do SEADE no Github:
  \newline
https://github.com/seade-R/dados-covid-sp


\newline
AUTOR: CAIO MARTINS







Row{data-height=320}
-------------------------------------
  
  ### NÚMERO DE CASOS DIÁRIOS
  
  ```{r}

renderPlot({
  graficos_covid %>% filter(nome_munic == input$cidades) %>%
    mutate(media_movel = round(rollmeanr(casos_novos, 7, fill = NA))) %>% 
    ggplot(aes(datahora, casos_novos)) + 
    geom_col(aes(colour = "White"), width = 0.8, fill = "#DC143C")+
    geom_line(aes(datahora, media_movel), colour = "Blue", size = 1.2)+
    scale_x_date(breaks = "15 days", date_labels = "%b %d")+
    theme_hc()+
    xlab("")+
    ylab("")+
    #ggtitle("Casos Diários de Covid-19 na Cidade de Botucatu (SP)")+
    #labs(caption = "AUTOR: CAIO MARTINS", colour = "Blue")+
    theme(axis.text.x = element_text(angle = 0, size = 14.0))+
    #plot.title = element_text(hjust = 0.5),
    #plot.caption = element_text(hjust = 0.5))+
    guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE)
  
})



```

Row{}
-------------------------------------
  
  ### NÚMERO DE CASOS POR MÊS
  
  ```{r}

casos_botucatu <- graficos_covid %>% filter(nome_munic == "Botucatu") %>% 
  mutate(mes = ifelse(mes < 2, 13, mes)) %>% 
  group_by(mes) %>% 
  summarize(soma = sum(casos_novos)) %>% 
  arrange(mes) %>% 
  rename(Nº_CASOS = soma) %>% 
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
                         mes == 12 ~ "Dezembro_2020")) %>% 
  kable("html", escape = F) %>%
  kable_styling(bootstrap_options = c("striped", "condensed","hover")) %>%
  scroll_box()

casos_bauru <- graficos_covid %>% filter(nome_munic == "Bauru") %>% 
  mutate(mes = ifelse(mes < 2, 13, mes)) %>% 
  group_by(mes) %>% 
  summarize(soma = sum(casos_novos)) %>% 
  arrange(mes) %>% 
  rename(Nº_CASOS = soma) %>% 
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
                         mes == 12 ~ "Dezembro_2020")) %>% 
  kable("html", escape = F) %>%
  kable_styling(bootstrap_options = c("striped", "condensed","hover")) %>%
  scroll_box()



renderUI( {
  data <- ifelse(input$cidades %in% c("Bauru"), casos_bauru, casos_botucatu)
  HTML(data)
})



```

### NÚMERO DE ÓBITOS POR MÊS 

```{r}


obitos_botucatu<- graficos_covid %>% filter(nome_munic == "Botucatu") %>% 
  mutate(mes = ifelse(mes < 2, 13, mes)) %>% 
  group_by(mes) %>% 
  summarize(soma = sum(obitos_novos)) %>% 
  arrange(mes) %>% 
  rename(Nº_OBITOS = soma) %>% 
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
                         mes == 12 ~ "Dezembro_2020")) %>% 
  kable("html", escape = F) %>%
  kable_styling(bootstrap_options = c("striped", "condensed","hover")) %>%
  scroll_box()


obitos_bauru<- graficos_covid %>% filter(nome_munic == "Bauru") %>% 
  mutate(mes = ifelse(mes < 2, 13, mes)) %>% 
  group_by(mes) %>% 
  summarize(soma = sum(obitos_novos)) %>% 
  arrange(mes) %>% 
  rename(Nº_OBITOS = soma) %>% 
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
                         mes == 12 ~ "Dezembro_2020")) %>% 
  kable("html", escape = F) %>%
  kable_styling(bootstrap_options = c("striped", "condensed","hover")) %>%
  scroll_box()


renderUI( {
  data <- ifelse(input$cidades %in% c("Bauru"), obitos_bauru, obitos_botucatu)
  HTML(data)
})  





```