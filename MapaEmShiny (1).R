library(shiny)
library(httr)
library(tidyverse)
library(sf)
library(janitor)
library(leaflet)

Sys.setlocale("LC_ALL", "pt_BR.UTF-8")


#muda p teu repo
dados = read.csv2("isolamento.csv", fileEncoding="UTF-8")
dados$codigo_municipio_ibge  = as.character(dados$codigo_municipio_ibge )
dados_na <- dados %>% group_by(municipio1 , codigo_municipio_ibge ) %>%
  summarise() 
dados <- dados %>%
  mutate(data = as.Date(paste0(str_extract(data , "\\d+/\\d+"), "/2022"), format = "%d/%m/%Y"))
dados <- dados %>% filter(!is.na(data ))


estado = 'SP'
url <- sprintf("https://servicodados.ibge.gov.br/api/v3/malhas/estados/%s?formato=application/json&qualidade=maxima&intrarregiao=municipio", estado)


response <- GET(url)
save_path_json = 'sp.shp'
if (status_code(response) == 200) {
  teste = content(response, "raw")
  writeBin(content(response, "raw"), save_path_json)
  sprintf("Arquivo salvo com sucesso.\n")
} else {
  sprintf("Erro ao obter dados:", status_code(response), "\n")
}

map <- st_read("sp.shp", quiet = TRUE)
mapReduzido = map[st_distance(map$geometry,map[map$codarea == 3550308,]) < 0.7,]
#mapa_data = left_join(map, dados, by = c("codarea" = "Código.Município.IBGE"))
mapa_data = left_join(mapReduzido, dados, by = c("codarea" = "codigo_municipio_ibge"))

mapa_data <- mapa_data %>%
  mutate_at(vars(media_de_indice_de_isolamento), 
            ~ as.numeric(gsub("%", "", .)) / 100)

pal <- colorBin(palette = "YlOrRd",  # You can also use "Reds", "Blues", or custom colors
                domain = mapa_data$media_de_indice_de_isolamento,  # Use the media column as the domain
                bins = 5) 

mapa_data_na <- left_join(mapReduzido, dados_na, by = c("codarea" = "codigo_municipio_ibge"))
#mapa_data_na = mapa_data_na[st_distance(mapa_data_na$geometry,mapa_data_na[mapa_data_na$codarea == 3550308,]) < 0.7,]

#mapa_data <- mapa_data %>% filter(!is.na(Data))


