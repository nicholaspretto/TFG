setwd("14 - Warming stripes")
library(tidyverse)
library(lubridate)
library(RColorBrewer)
mensal_mirante <- read_delim("mensal_mirante_ponto.csv", 
                                   delim = ";", escape_double = FALSE, na = "null", 
                                   trim_ws = TRUE, skip = 9) |> 
  mutate(Ano = as.numeric(substr(`Data Medicao`,1,4)))

medias <- mensal_mirante |> 
  filter(Ano<2020) |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))

theme_strip <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(vjust = 3),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold")
  )

col_strip <- brewer.pal(11, "RdBu")
brewer.pal.info

medias |> 
ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÉDIAS SÃO PAULO 1961-2019",
       caption = "Dados: Estação meteorológica do Mirante de Santana, Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip


minimas <- mensal_mirante |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MINIMA MEDIA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))

maximas <- mensal_mirante |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MAXIMA MEDIA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))
  
minimas |> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÍNIMAS SÃO PAULO 1961-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip

maximas |> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÁXIMAS SÃO PAULO 1961-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip



# Outras cidades - subindo dados ----------------------------------------------------------

belem <- read_delim("outrascapitais/belem.csv", 
                    delim = ";", escape_double = FALSE, na = "null", 
                    trim_ws = TRUE, skip = 9) |> 
  mutate(Ano = as.numeric(substr(`Data Medicao`,1,4))) |> 
  filter(Ano<2020, Ano>1961) |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))

belohorizonte <- read_delim("outrascapitais/belohorizonte.csv", 
                    delim = ";", escape_double = FALSE, na = "null", 
                    trim_ws = TRUE, skip = 9) |> 
  mutate(Ano = as.numeric(substr(`Data Medicao`,1,4))) |> 
  filter(Ano<2020) |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))
 
manaus <- read_delim("outrascapitais/manaus.csv", 
                            delim = ";", escape_double = FALSE, na = "null", 
                            trim_ws = TRUE, skip = 9) |> 
  mutate(Ano = as.numeric(substr(`Data Medicao`,1,4))) |> 
  filter(Ano<2020) |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))

portoalegre <- read_delim("outrascapitais/portoalegre.csv", 
                     delim = ";", escape_double = FALSE, na = "null", 
                     trim_ws = TRUE, skip = 9) |> 
  mutate(Ano = as.numeric(substr(`Data Medicao`,1,4))) |> 
  filter(Ano<2020) |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))

curitiba <- read_delim("outrascapitais/curitiba.csv", 
                          delim = ";", escape_double = FALSE, na = "null", 
                          trim_ws = TRUE, skip = 9) |> 
  mutate(Ano = as.numeric(substr(`Data Medicao`,1,4))) |> 
  filter(Ano<2020) |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))

brasilia <- read_delim("outrascapitais/brasilia.csv", 
                       delim = ";", escape_double = FALSE, na = "null", 
                       trim_ws = TRUE, skip = 9) |> 
  mutate(Ano = as.numeric(substr(`Data Medicao`,1,4))) |> 
  filter(Ano<2020) |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))

cuiaba <- read_delim("outrascapitais/cuiaba.csv", 
                       delim = ";", escape_double = FALSE, na = "null", 
                       trim_ws = TRUE, skip = 9) |> 
  mutate(Ano = as.numeric(substr(`Data Medicao`,1,4))) |> 
  filter(Ano<2020) |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))

goiania <- read_delim("outrascapitais/goiania.csv", 
                      delim = ";", escape_double = FALSE, na = "null", 
                      trim_ws = TRUE, skip = 9) |> 
  mutate(Ano = as.numeric(substr(`Data Medicao`,1,4))) |> 
  filter(Ano<2020) |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))

riodejaneiro <- read_delim("outrascapitais/riodejaneiro.csv", 
                      delim = ";", escape_double = FALSE, na = "null", 
                      trim_ws = TRUE, skip = 9) |> 
  mutate(Ano = as.numeric(substr(`Data Medicao`,1,4))) |> 
  filter(Ano<2020) |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))

fortaleza <- read_delim("outrascapitais/fortaleza.csv", 
                           delim = ";", escape_double = FALSE, na = "null", 
                           trim_ws = TRUE, skip = 9) |> 
  mutate(Ano = as.numeric(substr(`Data Medicao`,1,4))) |> 
  filter(Ano<2020) |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))

recife <- read_delim("outrascapitais/recife.csv", 
                        delim = ";", escape_double = FALSE, na = "null", 
                        trim_ws = TRUE, skip = 9) |> 
  mutate(Ano = as.numeric(substr(`Data Medicao`,1,4))) |> 
  filter(Ano<2020) |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))

salvador <- read_delim("outrascapitais/salvador.csv", 
                     delim = ";", escape_double = FALSE, na = "null", 
                     trim_ws = TRUE, skip = 9) |> 
  mutate(Ano = as.numeric(substr(`Data Medicao`,1,4))) |> 
  filter(Ano<2020) |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))

rio_realengo <- read_delim("outrascapitais/rio_realengo.csv", 
                       delim = ";", escape_double = FALSE, na = "null", 
                       trim_ws = TRUE, skip = 9) |> 
  mutate(Ano = as.numeric(substr(`Data Medicao`,1,4))) |> 
  filter(Ano<2020) |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))

rio_santacruz <- read_delim("outrascapitais/rio_santacruz.csv", 
                           delim = ";", escape_double = FALSE, na = "null", 
                           trim_ws = TRUE, skip = 9) |> 
  mutate(Ano = as.numeric(substr(`Data Medicao`,1,4))) |> 
  filter(Ano<2020) |> 
  group_by(Ano) |> 
  summarise(Media = mean(`TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`)) |> 
  mutate(Ano = ymd(paste0(Ano, "-01-01")))

# Outras cidades - Graficos -----------------------------------------------

belem|> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÉDIAS BELÉM 1962-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip

belohorizonte|> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÉDIAS BELO HORIZONTE 1961-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip

manaus |> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÉDIAS MANAUS 1961-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip

portoalegre |> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÉDIAS PORTO ALEGRE 1961-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip

curitiba |> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÉDIAS CURITIBA 1961-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip

brasilia |> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÉDIAS BRASÍLIA 1961-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip

goiania |> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÉDIAS GOIÂNIA 1961-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip

cuiaba |> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÉDIAS CUIABÁ 1961-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip

recife |> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÉDIAS RECIFE 1961-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip

fortaleza |> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÉDIAS FORTALEZA 1961-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip

# Gráficos impraticáveis --------------------------------------------------

riodejaneiro |> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÉDIAS RIO DE JANEIRO 1961-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip

salvador |> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÉDIAS SALVADOR 1961-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip

rio_realengo |> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÉDIAS RIO DE JANEIRO/REALENGO 1961-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip

rio_santacruz |> 
  ggplot(aes(x = Ano, y = 1, fill = Media))+
  geom_tile()+
  scale_x_date(date_breaks = "4 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "MÉDIAS RIO DE JANEIRO/SANTA CRUZ 1961-2019",
       caption = "Dados: Inmet (Instituto Nacional de Meteorologia)")+
  theme_strip
