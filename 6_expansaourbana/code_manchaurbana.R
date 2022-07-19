setwd("19 - Mancha urbana")

library(tidyverse)

manchaurbana <- read_csv("manchaurbana.csv") |> 
  # SPRAREA está em m². Transformar em km²=dividir por 10^6
  mutate(km2 = SPRAREA/1000000) |> 
  # Reagrupar períodos
  mutate(PERIODO2 = case_when(PERIODO == "1553 a 1881" | PERIODO == "1882 a 1914" ~ "1553 a 1914",
                              PERIODO == "1915 a 1929" | PERIODO == "1930 a 1949" ~ "1915 a 1949",
                              PERIODO == "1950 a 1962" | PERIODO == "1963 a 1974" ~ "1950 a 1974",
                              PERIODO == "1975 a 1980" | PERIODO == "1981 a 1985" ~ "1975 a 1985",
                              T ~ "1986 a 2002")) |> 
  mutate(TEMPO = case_when(PERIODO == "1553 a 1881" | PERIODO == "1882 a 1914" ~ (1914-1553+1),
                              PERIODO == "1915 a 1929" | PERIODO == "1930 a 1949" ~ (1949-1915+1),
                              PERIODO == "1950 a 1962" | PERIODO == "1963 a 1974" ~ (1974-1950+1),
                              PERIODO == "1975 a 1980" | PERIODO == "1981 a 1985" ~ (1985-1975+1),
                              T ~ (2002-1986+1))) |> 
  # Fazer as devidas somas
  group_by(PERIODO2, TEMPO) |> 
  summarise(SOMA_KM2 = sum(km2))



# Gráfico -----------------------------------------------------------------

manchaurbana |> 
  ggplot(aes(x=PERIODO2, y=TEMPO)) + 
  geom_col()

manchaurbana |> 
  ggplot(aes(x=PERIODO2, y=SOMA_KM2))+
  geom_col()


# Tentativa nova ----------------------------------------------------------

manchaurbana2 <- read_csv("manchaurbana.csv") |> 
  # SPRAREA está em m². Transformar em km²=dividir por 10^6
  mutate(km2 = SPRAREA/1000000) |> 
  # Pegar último ano de referência
  mutate(ANO= as.numeric(substr(PERIODO, 8, 11))) |> 
    # Fazer as devidas somas
  group_by(PERIODO, ANO) |> 
  summarise(KM2 = sum(km2)) |> 
  ungroup() |> 
  mutate(SOMA_KM2 = cumsum(KM2))

manchaurbana2 |> 
  filter(ANO<2000) |> 
  ggplot(aes(x=ANO))+
  geom_line(aes(y=SOMA_KM2))+
  geom_point(aes(y=SOMA_KM2))+
  geom_text(aes(y=SOMA_KM2 + 100, label=paste0("+",round(KM2,1) ,"km²")))
