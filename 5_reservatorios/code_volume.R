setwd("17 - Volume represas")

library(tidyverse)
library(readxl)

represa_volume <- read_excel("data/represa_volume.xlsx")

represa_volume |> 
  group_by(Sistema) |> 
  summarise(m3 = sum(Maximo_10na6_m3)) |> 
  ggplot(aes(x=Sistema, y=1, size=m3))+
  geom_point()

represa_volume |> 
  group_by(Sistema) |> 
  mutate(m3 = sum(Maximo_10na6_m3)) |> 
  ungroup() |> 
  ggplot()+
  geom_col(aes(x=reorder(Sistema, m3), y=Maximo_10na6_m3, fill=Represa), width=0.2)

represa_volume |> 
  group_by(Sistema) |> 
  summarise(m3 = sum(Maximo_10na6_m3)) |> 
  ungroup() |> 
  ggplot()+
  geom_col(aes(x=reorder(Sistema, m3), y=m3, fill=Sistema), width=0.2)
