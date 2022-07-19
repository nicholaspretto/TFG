setwd("10 - Desastres registrados")

library(tidyverse)
library(lubridate)
library(readxl)

danos_2013 <- read_excel("defesacivil_perdas/danos_2013.xls", 
                         skip = 4) |> mutate(Ano=2013)
danos_2014 <- read_excel("defesacivil_perdas/danos_2014.xls", 
                         skip = 4) |> mutate(Ano=2014)
arquivo <- rbind(danos_2013, danos_2014)
danos_2015 <- read_excel("defesacivil_perdas/danos_2015.xls", 
                         skip = 4) |> mutate(Ano=2015)
arquivo <- rbind(arquivo, danos_2015)
danos_2016 <- read_excel("defesacivil_perdas/danos_2016.xls", 
                         skip = 4) |> mutate(Ano=2016)
arquivo <- rbind(arquivo, danos_2016)
danos_2017 <- read_excel("defesacivil_perdas/danos_2017.xls", 
                         skip = 4) |> mutate(Ano=2017)
arquivo <- rbind(arquivo, danos_2017)
danos_2018 <- read_excel("defesacivil_perdas/danos_2018.xls", 
                         skip = 4) |> mutate(Ano=2018)
arquivo <- rbind(arquivo, danos_2018)
danos_2019 <- read_excel("defesacivil_perdas/danos_2019.xls", 
                         skip = 4) |> mutate(Ano=2019)
arquivo <- rbind(arquivo, danos_2019)
danos_2020 <- read_excel("defesacivil_perdas/danos_2020.xls", 
                         skip = 4) |> mutate(Ano=2020)
arquivo <- rbind(arquivo, danos_2020)
danos_2021 <- read_excel("defesacivil_perdas/danos_2021.xls", 
                         skip = 4) |> mutate(Ano=2021)
arquivo <- rbind(arquivo, danos_2021)


arquivo |> 
  mutate(Codigo5 = substr(COBRADE, 1,5)) |> 
    mutate(Tipo = case_when(Codigo5%in%c(12100,12200,12300,13214,13215) ~ "Desastres ligados à água",
                            Codigo5%in%c(14110, 14120) ~ "Seca e estiagem",
                            Codigo5%in%c(15110) ~ "Doenças virais",
                            TRUE ~  "wOutros")) |> 
  filter(Tipo != "Doenças virais") |> 
  filter(Status == "Reconhecido") |> 
  filter(UF == "SP") |> 
  ggplot(aes(x=Ano, y=DH_Mortos, fill=Tipo))+
  geom_col()


arquivo |> 
  mutate(Codigo5 = substr(COBRADE, 1,5)) |> 
  mutate(Tipo = case_when(Codigo5%in%c(12100,12200,12300,13214,13215) ~ "Desastres ligados à água",
                          Codigo5%in%c(14110, 14120) ~ "Seca e estiagem",
                          Codigo5%in%c(15110) ~ "Doenças virais",
                          TRUE ~  "wOutros")) |> 
  filter(Tipo != "Doenças virais") |> 
  filter(Status == "Reconhecido") |> 
  filter(UF == "SP") |> 
  ggplot(aes(x=Ano, y=DH_Mortos, fill=Tipo))+
  geom_col()


anos_unificados <- read_excel("anos_unificados.xlsx", 
                              sheet = "Planilha1")

prausarqgis <- anos_unificados |> 
           mutate(qtde=1) |> 
           group_by(`Código IBGE`, Desastre) |> 
           summarise(Qtde = sum(qtde)) |> 
           mutate(tipo = case_when(Desastre %in% c("ENXURRADAS","TEMPESTADE LOCAL/CONVECTIVA - CHUVAS INTENSAS",
                                                   "CHUVAS INTENSAS","INUNDAÇÕES","ENCHENTES","ALAGAMENTOS") ~ "Desastres com água",
                                   Desastre == "ESTIAGEM"  ~ "Estiagem",
                                   Desastre %in% c("TEMPESTADE LOCAL/CONVECTIVA - TORNADOS", "TEMPESTADE LOCAL/CONVECTIVA - VENDAVAL", 
                                                   "TORNADOS", "VENDAVAL", "VENDAVAL EXTREMAMENTE INTENSO") ~ "Desastres com vento",
                  TRUE ~ "Outros")) |> 
  select(1,3,4) |> 
  group_by(`Código IBGE`, tipo) |> 
  summarise(Qtde=sum(Qtde)) |> 
  pivot_wider(names_from=tipo, values_from=Qtde) |> 
  mutate(`Desastres com água` = replace_na(`Desastres com água`, 0),
         `Desastres com vento` = replace_na(`Desastres com vento`, 0),
         Estiagem = replace_na(Estiagem,0),
         Outros = replace_na(Outros, 0))

#writexl::write_xlsx(prausarqgis,path = "tiposdesastres_qgis.xlsx")

library(treemapify)

anos_unificados |> 
  mutate(qtde=1) |> 
  group_by(`Código IBGE`, Desastre) |> 
  summarise(Qtde = sum(qtde)) |> 
  mutate(tipo = case_when(Desastre %in% c("ENXURRADAS","TEMPESTADE LOCAL/CONVECTIVA - CHUVAS INTENSAS",
                                          "CHUVAS INTENSAS","INUNDAÇÕES","ENCHENTES","ALAGAMENTOS", "INUNDAÇÕES LITORÂNEAS") ~ "Desastres com água",
                          Desastre == "ESTIAGEM"  ~ "Estiagem",
                          Desastre %in% c("TEMPESTADE LOCAL/CONVECTIVA - TORNADOS", "TEMPESTADE LOCAL/CONVECTIVA - VENDAVAL", 
                                          "TORNADOS", "VENDAVAL", "VENDAVAL EXTREMAMENTE INTENSO") ~ "Desastres com vento",
                          TRUE ~ "Outros")) |> 
  mutate(Desastre = case_when(Desastre == "TEMPESTADE LOCAL/CONVECTIVA - CHUVAS INTENSAS" ~ "CHUVAS INTENSAS",
                              Desastre == "TEMPESTADE LOCAL/CONVECTIVA - TORNADOS" ~ "TORNADOS",
                              Desastre == "VENDAVAL EXTREMAMENTE INTENSO" ~ "VENDAVAL",
                              Desastre == "GEADAS" ~ "GEADA",
                              Desastre == "TEMPESTADE LOCAL/CONVECTIVA - VENDAVAL" ~ "VENDAVAL",
                              Desastre == "GRANIZOS" ~ "GRANIZO",
                              Desastre == "DESLIZAMENTOS DE SOLO E/OU ROCHA" ~ "DESLIZAMENTOS",
                              Desastre == "INUNDAÇÕES LITORÂNEAS" ~ "INUNDAÇÕES",
                              Desastre == "EROSÃO CONTINENTAL LAMINAR" ~ "EROSÃO",
                              Desastre == "EROSÃO DE MARGEM FLUVIAL" ~ "EROSÃO",
                              Desastre %in% c("INCÊNDIOS EM ALGOMERADOS RESIDENCIAIS", "INCÊNDIOS EM ÁREAS NÃO PROTEGIDAS") ~ "INCÊNDIOS",
                              TRUE ~ Desastre)) |> 
  group_by(tipo, Desastre) |> 
  summarise(Qtde = sum(Qtde)) |> 
  ggplot(aes(area=Qtde, fill=tipo, subgroup=tipo, group=Desastre))+
  geom_treemap()+
  geom_treemap_text(aes(label=paste0(Desastre, "\n", Qtde)))  
  

anos_unificados |> 
  mutate(qtde=1) |> 
  group_by(`Código IBGE`, Desastre) |> 
  summarise(Qtde = sum(qtde)) |> 
  mutate(tipo = case_when(Desastre %in% c("ENXURRADAS","TEMPESTADE LOCAL/CONVECTIVA - CHUVAS INTENSAS",
                                          "CHUVAS INTENSAS","INUNDAÇÕES","ENCHENTES","ALAGAMENTOS", "INUNDAÇÕES LITORÂNEAS") ~ "Desastres com água",
                          Desastre == "ESTIAGEM"  ~ "Estiagem",
                          Desastre %in% c("TEMPESTADE LOCAL/CONVECTIVA - TORNADOS", "TEMPESTADE LOCAL/CONVECTIVA - VENDAVAL", 
                                          "TORNADOS", "VENDAVAL", "VENDAVAL EXTREMAMENTE INTENSO") ~ "Desastres com vento",
                          TRUE ~ "Outros")) |> 
  mutate(Desastre = case_when(Desastre == "TEMPESTADE LOCAL/CONVECTIVA - CHUVAS INTENSAS" ~ "CHUVAS INTENSAS",
                              Desastre == "TEMPESTADE LOCAL/CONVECTIVA - TORNADOS" ~ "TORNADOS",
                              Desastre == "VENDAVAL EXTREMAMENTE INTENSO" ~ "VENDAVAL",
                              Desastre == "GEADAS" ~ "GEADA",
                              Desastre == "TEMPESTADE LOCAL/CONVECTIVA - VENDAVAL" ~ "VENDAVAL",
                              Desastre == "GRANIZOS" ~ "GRANIZO",
                              Desastre == "DESLIZAMENTOS DE SOLO E/OU ROCHA" ~ "DESLIZAMENTOS",
                              Desastre == "INUNDAÇÕES LITORÂNEAS" ~ "INUNDAÇÕES",
                              Desastre == "EROSÃO CONTINENTAL LAMINAR" ~ "EROSÃO",
                              Desastre == "EROSÃO DE MARGEM FLUVIAL" ~ "EROSÃO",
                              Desastre %in% c("INCÊNDIOS EM ALGOMERADOS RESIDENCIAIS", "INCÊNDIOS EM ÁREAS NÃO PROTEGIDAS") ~ "INCÊNDIOS",
                              TRUE ~ Desastre)) |> 
  group_by(tipo) |> 
  summarise(Qtde = sum(Qtde))

294+31+59+35
294/419*100
59/419*100
