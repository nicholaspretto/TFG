setwd("8 - Emissoes gas efeito estufa")

library(tidyverse)
library(lubridate)
library(basedosdados)

dados <- read_csv("D:/Arquivos R/Bases de dados grandes/Emissões CO2 por municipio/dados.csv") |> 
  filter(`TERRITÓRIO` == "SP") |> 
  filter(`IBGE` == "3550308") 

options(scipen=9999)

dados |> 
  pivot_longer(14:32, names_to="Ano", values_to="CO2equivalente") |>  
  mutate(CO2equivalente = str_replace(CO2equivalente, ",", ".")) |> 
  mutate(CO2equivalente = as.numeric(CO2equivalente)) |>  
  group_by(Ano) |> 
  summarise(CO2equivalente = sum(CO2equivalente)) |> 
  ggplot(aes(x=Ano, y=CO2equivalente)) +
  geom_col()



dados2 <- read_csv("municipio.csv") |> 
  filter(id_municipio=="3550308")

dados2 |>
  filter(tipo_emissao == "Emissão") |> 
  mutate(Tipo_emissao = case_when(nivel_2 == "Efluentes Liquidos" ~ "Efluentes líquidos",
                                  nivel_2 == "Resíduos Sólidos" ~ "Resíduos sólidos",
                                  nivel_2 == "Emissões pela Queima de Combustíveis" & nivel_3 == "Transportes" ~ "Queima de combustíveis",
                                  nivel_2 == "Emissões pela Queima de Combustíveis" ~ "Queima de combustíveis",
                                  TRUE ~ "Outros")) |> 
  mutate(Tipo_emissao = factor(Tipo_emissao, levels = c("Outros", "Efluentes líquidos", "Resíduos sólidos", "Queima de combustíveis", "Queima de combustíveis (Transportes)"))) |> 
  group_by(ano, Tipo_emissao) |> 
  summarise(emissao = sum(emissao)) |> 
  ggplot(aes(x=ano, y=emissao, fill=Tipo_emissao))+
  geom_col()+
  ylim(c(0,22500000))

# Pais inteiro ------------------------------------------------------------

dadosinteiros <- read_csv("D:/Arquivos R/Bases de dados grandes/Emissões CO2 por municipio/dados.csv") |> 
  filter(GÁS == "CO2e (t) GWP-AR5") |> 
  filter(`IBGE` != "3550308") |> 
  pivot_longer(14:32, names_to="Ano", values_to="CO2equivalente") |>  
  mutate(CO2equivalente = str_replace(CO2equivalente, ",", ".")) |> 
  mutate(CO2equivalente = as.numeric(CO2equivalente)) |>  
  mutate(Usosolo = case_when(`NIVEL 2` == "Alterações de Uso do Solo" & `TIPO DE EMISSÃO` == "Emissão" ~ "Emissão por alteração no uso do solo",
                             T ~ "Outros")) |> 
  group_by(Ano) |> 
  summarise(CO2equivalente = sum(CO2equivalente)) |> 
  mutate(Regiao = "Resto do Brasil")

dadosinteiros2 <- read_csv("D:/Arquivos R/Bases de dados grandes/Emissões CO2 por municipio/dados.csv") |> 
  filter(GÁS == "CO2e (t) GWP-AR5") |> 
  filter(`IBGE` != "3550308") |> 
  pivot_longer(14:32, names_to="Ano", values_to="CO2equivalente") |>  
  mutate(CO2equivalente = str_replace(CO2equivalente, ",", ".")) |> 
  mutate(CO2equivalente = as.numeric(CO2equivalente)) |>  
  group_by(Ano, `NIVEL 1`) |> 
  summarise(CO2equivalente = sum(CO2equivalente)) |> 
  mutate(Regiao = "Resto do Brasil")

dadossp <- dados |> 
  pivot_longer(14:32, names_to="Ano", values_to="CO2equivalente") |>  
  mutate(CO2equivalente = str_replace(CO2equivalente, ",", ".")) |> 
  mutate(CO2equivalente = as.numeric(CO2equivalente)) |>  
  group_by(Ano,`NIVEL 1`) |> 
  summarise(CO2equivalente = sum(CO2equivalente)) |> 
  mutate(Regiao = "São Paulo") |> 
  mutate(`NIVEL 1` = "São Paulo") |> 
  group_by(Ano,`NIVEL 1`) |> 
  summarise(CO2equivalente = sum(CO2equivalente))
  
library(ggstream)
library(streamgraph)

rbind(dadossp, dadosinteiros2) |>
  mutate(Ano = as.numeric(Ano)) |> 
  ggplot(aes(x=Ano,y=CO2equivalente, fill=`NIVEL 1`)) + 
  geom_stream()

rbind(dadossp, dadosinteiros2) |>
  mutate(Ano = as.numeric(Ano)) |> 
  mutate(`NIVEL 1` = case_when(`NIVEL 1` == "São Paulo" ~ "aaSão Paulo", T~`NIVEL 1`)) |> 
  ggplot(aes(x=Ano,y=CO2equivalente, fill=`NIVEL 1`)) + 
  geom_col()


rbind(dadossp, dadosinteiros2) |>
  mutate(Ano = as.numeric(Ano)) |> 
    streamgraph(key=`NIVEL 1`, value=CO2equivalente, date=Ano)

write.csv(dados_unidos, "dados_para_rawgraphs.csv")

write.csv((dados_unidos |> 
  select(1:3) |> 
  pivot_wider(names_from=`NIVEL 1`, values_from=CO2equivalente)), "dados_para_flourish.csv")
  
