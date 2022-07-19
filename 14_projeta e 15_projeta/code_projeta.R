setwd("16 - Projeta Inpe")

library(httr)
library(tidyverse)
library(lubridate)

set_config(config(ssl_verifypeer = FALSE))
options(RCurlOptions = list(ssl_verifypeer = FALSE))
options(rsconnect.check.certificate = FALSE)

years99 <- readxl::read_xlsx("data/anos99.xlsx")
years99$ano <- as.character(years99$ano)
years98 <- readxl::read_xlsx("data/anos98.xlsx")
years98$ano <- as.character(years98$ano)
years45 <- readxl::read_xlsx("data/anos45.xlsx")
years45$ano <- as.character(years45$ano)
meses99 <- readxl::read_xlsx("data/meses99.xlsx")
meses99$mes <- as.character(meses99$mes)
meses1961 <- readxl::read_xlsx("data/meses1961.xlsx")
meses1961$mes <- as.character(meses1961$mes)

# HISTÓRIA DAS CHUVAS EXTREMAS

# Precipitação total anual ------------------------------------------------

PRCPTOT_45 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/4/YEARLY/4/1/2006/12/2099/PRCPTOT/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows() |> 
  mutate(Cenario = "RCP 4.5") |> 
  cbind(years98)

PRCPTOT_85 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/6/YEARLY/4/1/2006/12/2099/PRCPTOT/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 8.5") |> 
  cbind(years98)

PRCPTOT_hist <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/16/YEARLY/4/1/1961/12/2005/PRCPTOT/-23.5501/-46.636/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "Historico") |> 
  cbind(years45)

writexl::write_xlsx(rbind(rbind(PRCPTOT_45, PRCPTOT_85), PRCPTOT_hist), "data/precipitacao_total_anual.xlsx")

# Precipitação total anual dos dias que excederam o percentil 95% ---------

R95p_45 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/4/YEARLY/4/1/2006/12/2099/R95p/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 4.5")|> 
  cbind(years98)

R95p_85 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/6/YEARLY/4/1/2006/12/2099/R95p/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 8.5")|> 
  cbind(years98)

r95p_hist <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/16/YEARLY/4/1/1961/12/2005/R95p/-23.5501/-46.636/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "Historico") |> 
  cbind(years45)

writexl::write_xlsx(rbind(rbind(R95p_45, R95p_85),r95p_hist), "data/precipitacao_dias_sucederam_percentil_95porcento.xlsx")

# Precipitação anual dividida pelos dias de chuva -------------------------

SDII_45 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/6/YEARLY/4/1/2006/12/2099/SDII/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 4.5")|> 
  cbind(years98)

SDII_85 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/4/YEARLY/4/1/2006/12/2099/SDII/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 8.5")|> 
  cbind(years98)

SDII_hist <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/16/YEARLY/4/1/1961/12/2005/SDII/-23.5501/-46.636/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "Historico") |> 
  cbind(years45)

writexl::write_xlsx(rbind(rbind(SDII_45, SDII_85),SDII_hist), "data/precipitacao_dividida_pordiasdechuva.xlsx")

# HISTÓRIA DA ESTIAGEM

# Máximo de dias consecutivos com chuva -----------------------------------

CWD_85 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/6/YEARLY/4/1/2006/12/2099/CWD/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 8.5")|> 
  cbind(years98)

CWD_45 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/4/YEARLY/4/1/2006/12/2099/CWD/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 4.5")|> 
  cbind(years98)

CWD_hist <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/16/YEARLY/4/1/1961/12/2005/CWD/-23.5501/-46.636/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "Historico") |> 
  cbind(years45)

writexl::write_xlsx(rbind(rbind(CWD_45, CWD_85), CWD_hist), "data/maximo_diasconsecutivos_comchuva.xlsx")

# Número máximo de dias consecutivos sem chuva ----------------------------

CDD_45 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/4/YEARLY/4/1/2006/12/2099/CDD/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 4.5")|> 
  cbind(years98)

CDD_85 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/6/YEARLY/4/1/2006/12/2099/CDD/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 8.5")|> 
  cbind(years98)

CDD_hist <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/16/YEARLY/4/1/1961/12/2005/CDD/-23.5501/-46.636/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "Historico") |> 
  cbind(years45)

writexl::write_xlsx(rbind(rbind(CDD_45, CDD_85),CDD_hist), "data/maximo_diasconsecutivos_semchuva.xlsx")

# AUMENTO DAS TEMPERATURAS

# Temperatura mínima média anual ------------------------------------------


MNTP_anual85 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/6/YEARLY/4/1/2006/12/2099/MNTP/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 8.5") |> 
  cbind(years99)

MNTP_anual45 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/4/YEARLY/4/1/2006/12/2099/MNTP/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 4.5") |> 
  cbind(years99)

MNTP_hist <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/16/YEARLY/4/1/1961/12/2005/MNTP/-23.5501/-46.636/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "Historico") |> 
  cbind(years45)

writexl::write_xlsx(rbind(rbind(MNTP_anual45, MNTP_anual85), MNTP_hist), "data/temperatura_minima_media.xlsx")

# Temperatura máxima média anual ------------------------------------------

MXTP_anual_RCP85 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/6/YEARLY/4/1/2006/12/2099/MXTP/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 8.5") |> 
  cbind(years99)

MXTP_anual_RCP45 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/4/YEARLY/4/1/2006/12/2099/MXTP/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 4.5") |> 
  cbind(years99)

MXTP_hist <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/16/YEARLY/4/1/1961/12/2005/MXTP/-23.5501/-46.636/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "Historico") |> 
  cbind(years45)

writexl::write_xlsx(rbind(rbind(MXTP_anual_RCP45, MXTP_anual_RCP85),MXTP_hist), "data/temperatura_maxima_media.xlsx")


# Temperatura média média anual -------------------------------------------

TP2M_85 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/6/YEARLY/4/1/2006/12/2099/TP2M/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 8.5") |> 
  cbind(years99)
  

TP2M_45 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/4/YEARLY/4/1/2006/12/2099/TP2M/-23.5501/-46.636/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 4.5") |> 
  cbind(years99)

TP2M_hist <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/16/YEARLY/4/1/1961/12/2005/TP2M/-23.5501/-46.636/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "Historico") |> 
  cbind(years45)

writexl::write_xlsx(rbind(rbind(TP2M_85, TP2M_45),TP2M_hist), "data/temperatura_media_media.xlsx")

# UMIDADE


# Umidade relativa do ar --------------------------------------------------

UR2M_45 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/4/YEARLY/4/1/2006/12/2099/UR2M/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 4.5") |> 
  cbind(years99)

UR2M_85 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/6/YEARLY/4/1/2006/12/2099/UR2M/-23.5501/-46.6336/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 8.5") |> 
  cbind(years99)

UR2M_hist <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/16/YEARLY/4/1/1961/12/2005/UR2M/-23.5501/-46.636/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "Historico") |> 
  cbind(years45)

writexl::write_xlsx(rbind(rbind(UR2M_45, UR2M_85), UR2M_hist), "data/umidade_relativa_media.xlsx")

# Umidade por mês

UR2M_mensal_45 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/4/MONTHLY/3/1/2006/12/2099/UR2M/-23.5501/-46.636/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 4.5") |> 
  cbind(meses99)

UR2M_mensal_85 <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/6/MONTHLY/3/1/2006/12/2099/UR2M/-23.5501/-46.636/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "RCP 8.5") |> 
  cbind(meses99)

UR2M_mensal_hist <- GET("https://projeta.cptec.inpe.br/api/v1/public/ETA/16/MONTHLY/3/1/1961/12/2005/UR2M/-23.5501/-46.636/") |> 
  content() |> 
  bind_rows()|> 
  mutate(Cenario = "Historico") |> 
  cbind(meses1961)

writexl::write_xlsx(rbind(rbind(UR2M_mensal_45, UR2M_mensal_85), UR2M_mensal_hist), "data/umidade_relativa_mensal.xlsx")
