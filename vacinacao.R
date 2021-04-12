library(tidyverse)
library(ggplot2)
library(zoo)
library(ggthemes)
library(plotly)
library(knitr)
library(kableExtra)
library(abjutils)
library(geobr)

# INTERNACAO E LEITOS COVID -----------


internacoes <- read.delim("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/plano_sp_leitos_internacoes.csv",
                          sep = ";")


internacoes <- internacoes %>% mutate(nome_drs = iconv(nome_drs, from = "UTF-8", to = "latin1"),
                                      datahora = lubridate::as_date(datahora))

bauru <- internacoes %>% filter(str_detect(nome_drs, "Bauru"))


bauru <- bauru %>% mutate(media_movel = rollmean(pacientes_uti_ultimo_dia, 7,fill = NA, align = "right"))


# VACINACAO SP ------


vacinacao <- read.delim('https://www.saopaulo.sp.gov.br/wp-content/uploads/2021/03/20210311_vacinometro.csv',
                        sep = ";")


vacinacao <- vacinacao %>% 
  rename(nome_municipio = ï..Ds.Municipio.Maiusc,
         numero_vacinados = Contagem.de.Id.Vacinacao) %>% 
  mutate(nome_municipio = iconv(nome_municipio, from = 'UTF-8', to = 'latin1'),
         Dose = iconv(Dose, from = 'UTF-8', to = 'latin1')) %>% 
  pivot_wider(names_from = Dose, values_from = numero_vacinados) %>% 
  rename(primeira_dose = `1° Dose`,
         segunda_dose = `2° Dose`)


vacinacao <- vacinacao %>% select(nome_municipio, primeira_dose, segunda_dose) %>% 
  arrange(desc(primeira_dose))



municipios_sp <- read_municipality(code_muni = "SP", year = 2018)
municipios_sp<- municipios_sp %>% mutate(name_muni = str_to_upper(name_muni, locale = "pt")) %>% 
  rename(nome_municipio = name_muni)

vacinacao <- left_join(vacinacao, municipios_sp, by = "nome_municipio")

pop_sp_10 <- read.delim("C:\\Users\\mcaio\\Desktop\\Nova pasta\\pop_sp_2010.csv", sep = ";")

pop_sp_10 <- pop_sp_10 %>% rename(nome_municipio = Municipios) %>% 
  mutate(nome_municipio = str_to_upper(nome_municipio, locale = "pt"))

vacinacao <- left_join(vacinacao, pop_sp_10)

vacinacao <- vacinacao %>% select(nome_municipio, primeira_dose, segunda_dose, code_muni, Populacao, geom)

vacinacao <- vacinacao %>% mutate(percentual_aplicado = round((primeira_dose/Populacao)*100,2))


vacinacao <- vacinacao %>% rename(Municípios = nome_municipio,
                                  '1ªDose' = primeira_dose,
                                  '2ªDose' = segunda_dose,
                                  'Percentual Aplicado' = percentual_aplicado)

vacinacao %>% select(Municípios, `1ªDose`, `2ªDose`, `Percentual Aplicado`) %>% 
  kbl(caption = "Vacinação nos Municípios de São Paulo") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))









teste <- "https://www.saopaulo.sp.gov.br/wp-content/uploads/2021/03/20210311_vacinometro.csv"

basename(teste)

download.file(teste)

url <- 'https://www.saopaulo.sp.gov.br/wp-content/uploads/2021/03/20210311_vacinometro.csv'
tf <- tempfile(pattern = 'test', fileext = '.csv')
download.file(url, tf)
df <- read.delim(tf, sep = ";")
