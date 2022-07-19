library(readxl)
library(nexo.utils)
library(tidyverse)
library(sf)

dados_benevolo <- read_excel("../Datasets_TFG2/dados_benevolo.xlsx") |> 
  separate(Latlon, c("Lat", "Lon"), sep = "," ) |> 
  na.omit()


dados_benevolo |> 
  mutate(Um = 1) |> 
  group_by(Agua) |> 
  summarise(Soma = sum(Um)) |> 
  ggplot(aes(x=Agua, y=Soma, fill=Agua))+
  geom_col()+
  coord_flip()


  
nexo.utils::mapCountries |> 
  ggplot()+
  geom_sf()+
  geom_point(data=dados_benevolo, aes(x=as.numeric(Lon), y=as.numeric(Lat)), size=2)

dados_benevolo |> 
  ggplot(aes(x=Lon, y=Lat)) +
  geom_point()
