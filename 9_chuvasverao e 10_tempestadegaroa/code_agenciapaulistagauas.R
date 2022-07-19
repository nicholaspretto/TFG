setwd("7 - SP Precipitacao historica")

library(tidyverse)
library(readxl)
library(lubridate)

iag <- read_delim("E3-035_Chuva_Diaria_Serie_20220524_181715.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                  na = "---")


# Gráfico bonito porém meaningless ----------------------------------------

iag |> 
  pivot_longer(2:32, names_to = "Dia", values_to = "Precipitacao_mm") |> 
  mutate(Data = dmy(paste0(Dia, "/", `Mês/Ano`))) |> 
  ggplot(aes(x=Data, y= as.numeric(Precipitacao_mm))) + 
  geom_col(fill="blue")


# Dias com mais de 50mm de chuva  ---------------------------------------------------

iag |> 
  pivot_longer(2:32, names_to = "Dia", values_to = "Precipitacao_mm") |> 
  mutate(Data = dmy(paste0(Dia, "/", `Mês/Ano`))) |>
  mutate(Ano = as.numeric(substr(`Mês/Ano`, 4,8))) |> 
  mutate(qtde = 1) |> 
  mutate(Tipo = case_when(Precipitacao_mm >= 100 ~ "Mais de 100mm",
                          Precipitacao_mm >= 75 & Precipitacao_mm < 100 ~ "Entre 75 e 100mm",
                          Precipitacao_mm >= 50 & Precipitacao_mm < 75 ~ "Entre 50 e 75mm",
                          TRUE ~ "Menos de 50mm")) |> 
  mutate(Tipo = factor(Tipo, levels=c("Mais de 100mm", "Entre 75 e 100mm", "Entre 50 e 75mm"))) |> 
  filter(Tipo != "Menos de 50mm") |> 
  mutate(Decada = paste0(substr(Ano, 1, 3), "0")) |> 
  filter(Decada != 1930) |> 
  group_by(Decada,Tipo) |> 
  summarise(qtde=sum(qtde)) |> 
  ggplot(aes(x=Decada, y=qtde, fill=Tipo)) + 
  geom_col()+
  geom_text(aes(x=Decada, y=qtde+2, label=qtde))+
  scale_fill_manual(values=c("#003e87", "#3b63a8", "#859bd5"))


# Gráfico por mês e por ano -----------------------------------------------

#Por verão
iag |> 
  mutate(Ano = substr(`Mês/Ano`, 4,8)) |> 
    mutate(Mes = substr(`Mês/Ano`,1,2)) |> 
    filter(Mes == "01" | Mes == "02" | Mes == "03" | Mes == "12") |> 
  mutate(Mes = as.numeric(Mes),
         Ano = as.numeric(Ano)) |> 
  mutate(Ano = case_when(Mes == 12 ~ Ano+1,
                         TRUE ~ Ano)) |> 
  filter(Ano < 2020, Ano > 1936) |> 
  group_by(Ano) |> 
  summarise(Precipitacao_verao = sum(`Chuva total`)) |> 
  ggplot(aes(x=Ano, y=Precipitacao_verao))+
  geom_col(fill="blue", width=1)+
  scale_y_reverse()

mean((iag |> 
       mutate(Ano = substr(`Mês/Ano`, 4,8)) |> 
       mutate(Mes = substr(`Mês/Ano`,1,2)) |> 
       filter(Mes == "01" | Mes == "02" | Mes == "03" | Mes == "12") |> 
       mutate(Mes = as.numeric(Mes),
              Ano = as.numeric(Ano)) |> 
       mutate(Ano = case_when(Mes == 12 ~ Ano+1,
                              TRUE ~ Ano)) |> 
       filter(Ano < 2020, Ano > 1936) |> 
       group_by(Ano) |> 
       summarise(Precipitacao_verao = sum(`Chuva total`)))$Precipitacao_verao)


# Gráfico garoas ----------------------------------------------------------


iag |> 
  pivot_longer(2:32, names_to = "Dia", values_to = "Precipitacao_mm") |> 
  mutate(Data = dmy(paste0(Dia, "/", `Mês/Ano`))) |>
  mutate(Ano = as.numeric(substr(`Mês/Ano`, 4,8))) |> 
  mutate(qtde = 1) |> 
  mutate(Tipo = case_when(Precipitacao_mm <= 5 & Precipitacao_mm > 0 ~ "Garoa",
                          TRUE ~ "Outros")) |> 
  filter(Tipo == "Garoa") |> 
  group_by(Ano) |> 
  summarise(total = sum(qtde)) |> 
  ggplot(aes(x=Ano, y=total))+
  geom_col(aes(fill=total))+
  #geom_smooth()+
  scale_x_continuous(breaks=c(1936,1950,1970,1990,2010,2019))+
  ylim(c(0,250))+
  scale_fill_gradient(
    low = "#fcc0b8", 
    high = "#003e87")+
  labs(x="Ano", y="Número de dias com precipitação entre 0 e 5mm (garoa)")
  