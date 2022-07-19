setwd("20 - Resiliencia urbana")

library(tidyverse)

resiliencia <- read_excel("Infografico_Infraestrutura.xlsx")


resiliencia |> 
  mutate(V_contribution = case_when(Contribution == "Negligible" ~ 0.5,
                                    Contribution == "Small" ~ 1.5,
                                    Contribution == "Moderate" ~ 5,
                                    Contribution == "High" ~ 10)) |> 
  mutate(Confiabilidade = case_when(Quality == "Positive" & Confiability == "Low" ~ "Positivo Baixo",
                           Quality == "Positive" & Confiability == "Medium" ~ "Positivo Médio",
                           Quality == "Positive" & Confiability == "High" ~ "Positivo Alto",
                           Quality == "Negative" & Confiability == "Low" ~ "Negativo Baixo",
                           Quality == "Negative" & Confiability == "Medium" ~ "Negativo Médio",
                           Quality == "Negative" & Confiability == "High" ~ "Negativo Alto")) |> 
  ggplot(aes(y=Componente,x=Adaptacao))+
  geom_point(aes(size=V_contribution, col=Confiabilidade))
