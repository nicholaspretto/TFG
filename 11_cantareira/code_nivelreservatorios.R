setwd("12 - Nivel reservatorios")

library(tidyverse)
library(readxl)
library(lubridate)

# Cantareira --------------------------------------------------------------

cantareira_completo <- read_excel("cantareira/sistema_completo.xlsx")

cantareira_completo |> 
  mutate(Data = dmy(Data)) |> 
  ggplot(aes(x=Data, y=`Volume (%)`))+
  geom_area()

cantareira_completo |> 
  mutate(Dia=1) |> 
  mutate(Volume = case_when(`Volume (%)`>= 60 ~ "normal",
                            T ~ "outros")) |> 
  group_by(Volume) |> 
  summarise(Soma=sum(Dia))
  

# Com dados do pacote Mananciais ------------------------------------------

# install.packages("devtools")
#devtools::install_github("beatrizmilz/mananciais")
library(mananciais)


# Guarapiranga ------------------------------------------------------------

guarapiranga <- mananciais::dados_mananciais() |> 
  filter(sistema == "Guarapiranga")

guarapiranga |> 
  ggplot(aes(x=data, y=volume_porcentagem))+
  geom_area()

# Alto Tietê --------------------------------------------------------------

altotiete <- mananciais::dados_mananciais() |> 
  filter(sistema == "Alto Tietê")

altotiete |> 
  ggplot(aes(x=data, y=volume_porcentagem))+
  geom_area()


# Cantareira com dados da Beatriz Milz ------------------------------------

cantareira <- mananciais::dados_mananciais() |> 
  filter(sistema == "Cantareira")

cantareira |> 
  ggplot(aes(x=data, y=volume_porcentagem))+
  geom_area()

cantareira |> 
  mutate(tipo = case_when(volume_porcentagem >= 60 ~ "Normal",
                          volume_porcentagem >= 40 & volume_porcentagem < 60 ~ "Atenção",
                          volume_porcentagem >= 30 & volume_porcentagem < 40 ~ "Restrição",
                          volume_porcentagem >= 20 & volume_porcentagem < 30 ~ "Alerta",
                          volume_porcentagem >= 0 & volume_porcentagem < 20 ~ "Fase especial",
                          TRUE ~ "Volume morto")) |> 
  ggplot(aes(x=data, y=volume_porcentagem, fill=tipo))+
  geom_area()
