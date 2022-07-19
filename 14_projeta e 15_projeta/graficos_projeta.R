setwd("16 - Projeta Inpe")
library(tidyverse)
library(lubridate)
library(readxl)

# NÃO SERVEM: 
# Precipitação total anual
# 


# Incluir temperatura ja existente ----------------------------------------

mensal_mirante_ponto <- read_delim("../14 - Warming stripes/mensal_mirante_ponto.csv", 
                                   delim = ";", escape_double = FALSE, col_types = cols(`NUMERO DE DIAS COM PRECIP. PLUV, MENSAL(número)` = col_skip(), 
                                                                                        `PRECIPITACAO TOTAL, MENSAL(mm)` = col_skip(), 
                                                                                        `UMIDADE RELATIVA DO AR, MEDIA MENSAL(%)` = col_skip(), 
                                                                                        ...8 = col_skip()), trim_ws = TRUE, 
                                   skip = 9)  |> 
  mutate(ano = as.numeric(substr(`Data Medicao`,1,4))) |> 
  rename(Minima = `TEMPERATURA MINIMA MEDIA, MENSAL(°C)`, Maxima = `TEMPERATURA MAXIMA MEDIA, MENSAL(°C)`,
         Media = `TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`) |> 
  pivot_longer(2:4, names_to="Metrica", values_to="value") |> 
  mutate(value = as.numeric(value)) |> 
  group_by(ano, Metrica) |> 
  summarise(value = mean(value)) |> 
  filter(ano < 2020) |>  
  mutate(Cenario = "Mirante")

# Temperatura Cenários -------------------------------------------------------------

temperatura_minima_media <- read_excel("data/temperatura_minima_media.xlsx") |> 
  select(1,7,8) |> 
  mutate(Metrica = "Minima")|> 
  mutate(ano = ymd(ano)) |> 
  mutate(ano = as.integer(substr(ano,1,4)))

temperatura_maxima_media <- read_excel("data/temperatura_maxima_media.xlsx") |> 
  select(1,7,8) |> 
  mutate(Metrica = "Maxima")|> 
  mutate(ano = ymd(ano)) |> 
  mutate(ano = as.integer(substr(ano,1,4)))

temperatura_media_media <- read_excel("data/temperatura_media_media.xlsx") |> 
  select(1,7,8) |> 
  mutate(Metrica = "Media")|> 
  mutate(ano = ymd(ano)) |> 
  mutate(ano = as.integer(substr(ano,1,4)))

temperaturas <- temperatura_minima_media |> 
rbind(temperatura_maxima_media) |> 
  rbind(temperatura_media_media)

temperaturas |> 
  ggplot()+
  geom_line(aes(x=ano, y=value, col=Metrica))+
  facet_wrap(~Cenario)+ylim(0,35)


# Umidade -----------------------------------------------------------------

umidade_relativa_media <- read_excel("data/umidade_relativa_media.xlsx") |> 
  select(1,7,8)

umidade_relativa_media |> 
  mutate(ano = as.integer(substr(ano,1,4))) |> 
  filter(ano<2099) |> 
  ggplot()+
  geom_line(aes(x=ano, y=value, col=Cenario))+
  scale_x_continuous(breaks=seq(1960,2100,20))

umidade_relativa_media |> 
  mutate(ano = as.integer(substr(ano,1,4))) |>
  filter(ano<2099) |> 
  filter(Cenario != "RCP 4.5") |> 
  ggplot(aes(x=ano))+
  geom_col(aes(y=value,fill=Cenario))+
  #geom_point(aes(y=value,col=Cenario, shape=Cenario))+
  scale_x_continuous(breaks=seq(1960,2100,20))+
  geom_hline(yintercept=c(71.8,65.3,63.1))+ylim(0,80)

umidade_relativa_media |> 
  mutate(ano = as.integer(substr(ano,1,4))) |> 
  filter(ano<2099) |> 
  filter(Cenario != "RCP 8.5") |> 
  ggplot(aes(x=ano))+
  geom_col(aes(y=value,fill=Cenario))+
  #geom_point(aes(y=value,col=Cenario, shape=Cenario))+
  scale_x_continuous(breaks=seq(1960,2100,20))+
  geom_hline(yintercept=c(71.8,65.3,63.1))+ylim(0,80)

umidade_relativa_media |> 
  mutate(ano = as.integer(substr(ano,1,4))) |> 
  filter(ano<2051) |> 
  group_by(Cenario) |> 
  summarise(media = mean(value))



# Umidade mensal ----------------------------------------------------------

umidade_relativa_mensal <- read_excel("data/umidade_relativa_mensal.xlsx") |> 
  select(1,7,8) |> 
  mutate(ano = as.integer(substr(mes,1,4))) |> 
  mutate(meses = as.integer(substr(mes,6,7))) |> 
  filter(meses >= 5 & meses <= 8) |> 
  group_by(ano, Cenario) |> 
  summarise(Umidade_media = mean(value))

umidade_relativa_mensal |> 
  filter(Cenario != "RCP 4.5") |> 
  ggplot(aes(x=ano, y=Umidade_media))+
  geom_col(col="gray80", width=.2)+
  geom_point(aes(col=Cenario, shape=Cenario))+
  scale_x_continuous(breaks=seq(1960,2100,20))+
  geom_hline(yintercept=c(71.8,65.3,63.1))


# Estiagem ----------------------------------------------------------------

maximo_diasconsecutivos_comchuva <- read_excel("data/maximo_diasconsecutivos_comchuva.xlsx") |> 
  select(1,7,8) |> 
  mutate(ano = as.integer(substr(ano,1,4)))

maximo_diasconsecutivos_comchuva |> 
  mutate(ano = as.integer(substr(ano,1,4))) |> 
  filter(Cenario != "RCP 8.5") |> 
  ggplot()+
  geom_col(aes(x=ano, y=value, fill=Cenario))+
  scale_x_continuous(breaks=seq(1960,2100,20))

maximo_diasconsecutivos_semchuva <- read_excel("data/maximo_diasconsecutivos_semchuva.xlsx") |> 
  select(1,7,8) |> 
  mutate(ano = as.integer(substr(ano,1,4)))

maximo_diasconsecutivos_semchuva |> 
  mutate(ano = as.integer(substr(ano,1,4))) |> 
  filter(Cenario != "RCP 8.5") |> 
  ggplot()+
  geom_col(aes(x=ano, y=value, fill=Cenario))+
  scale_x_continuous(breaks=seq(1960,2100,20))+ylim(0,90)+
  geom_hline(yintercept=30, linetype="longdash")

maximo_diasconsecutivos_semchuva |> 
  mutate(ano = as.integer(substr(ano,1,4))) |> 
  filter(Cenario != "RCP 4.5") |> 
  ggplot()+
  geom_col(aes(x=ano, y=value, fill=Cenario))+
  scale_x_continuous(breaks=seq(1960,2100,20))+ylim(0,90)+
  geom_hline(yintercept=30, linetype="longdash")


# Precipitação por dias de chuva ------------------------------------------

precipitacao_dividida_pordiasdechuva <- read_excel("data/precipitacao_dividida_pordiasdechuva.xlsx") |> 
  select(1,7,8) |> 
  mutate(ano = as.integer(substr(ano,1,4)))

precipitacao_dividida_pordiasdechuva |> 
  mutate(ano = as.integer(substr(ano,1,4))) |> 
  filter(Cenario != "RCP 4.5") |> 
  ggplot()+
  geom_col(aes(x=ano, y=value, fill=Cenario))+
  scale_x_continuous(breaks=seq(1960,2100,20))


# Precipitação total ------------------------------------------------------

precipitacao_total_anual <- read_excel("data/precipitacao_total_anual.xlsx") |> 
  select(1,7,8) |> 
  mutate(ano = as.integer(substr(ano,1,4)))

precipitacao_total_anual |> 
  filter(Cenario!="RCP 8.5") |> 
  ggplot()+
  geom_col(aes(x=ano, y=value, fill=Cenario))+
  scale_x_continuous(breaks=seq(1960,2100,20))+ylim(0,2700)

precipitacao_total_anual |> 
  filter(Cenario!="RCP 4.5") |> 
  ggplot()+
  geom_col(aes(x=ano, y=value, fill=Cenario))+
  scale_x_continuous(breaks=seq(1960,2100,20))+ylim(0,2700)


# Chuvas extremas ---------------------------------------------------------

chuva_extrema <- read_excel("data/precipitacao_dias_sucederam_percentil_95porcento.xlsx") |> 
  select(1,7,8) |> 
  mutate(ano = as.integer(substr(ano,1,4)))

chuva_extrema |> 
  filter(Cenario!="RCP 4.5") |> 
  ggplot()+
  geom_col(aes(x=ano, y=value, fill=Cenario))+
  scale_x_continuous(breaks=seq(1960,2100,20))+ylim(0,1250)

chuva_extrema |> 
  filter(Cenario!="RCP 8.5") |> 
  ggplot()+
  geom_col(aes(x=ano, y=value, fill=Cenario))+
  scale_x_continuous(breaks=seq(1960,2100,20))+ylim(0,1250)

