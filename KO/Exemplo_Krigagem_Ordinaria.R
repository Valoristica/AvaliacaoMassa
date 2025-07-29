# ******************************************************************************
# Exemplo de Krigagem Ordinária
# Augusto Oliveira (augoliv@gmail.com)
# ******************************************************************************

# ==============================================================================
# LIMPANDO O AMBIENTE R
# ==============================================================================
rm(list = ls())
gc(reset = T)
graphics.off()
rm(list = ls(all = TRUE))

# ==============================================================================
## Carregando os pacotes
# ==============================================================================
if (!require("sf")) install.packages("sf")
library(sf)
if (!require("gstat")) install.packages("gstat")
library(gstat)
if (!require("automap")) install.packages("automap")
library(automap)
if (!require("terra"))  install.packages("terra")
library(terra)
if (!require("raster"))  install.packages("raster")
library(raster)
if (!require("maptiles"))  install.packages("maptiles")
library(maptiles)


# ==============================================================================
# Setando a pasta de trabalho
# ==============================================================================
getwd()
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ==============================================================================
# Função para criar uma grade com tamanho de célula definido dentro de
#   um polígono.
# ==============================================================================
cria_grid <- function(limite_polygon,cell_size = 20)
{
  gridPpts <- sf::st_make_grid(limite_polygon,
                               what = "centers",
                               cellsize = c(cell_size, cell_size))
  gridPpts <- sf::st_sf(gridPpts)

  return(gridPpts[, ncol(gridPpts)])
}

# ==============================================================================
## Carregar limite da Área estuda (será usado na krigagem)
## Exemplo de como carregar um geopackage
# ==============================================================================
contorno <- st_read("./KO/cidade_ecologica_contorno.gpkg")
plot(contorno)

# ==============================================================================
## Carregar lotes (será usado na krigagem)
## Exemplo de como carregar um geopackage
# ==============================================================================
lotes <- st_read("./KO/cidade_ecologica_lotes.gpkg")
plot(lotes[, "iq"])

# ==============================================================================
## Carregar dados de um CSV
# ==============================================================================
df <-
  read.table(
    'kO/cidade_ecologica.csv',
    encoding = "UTF-8",
    header = T,
    sep = ";" ,
    dec = ","
  )

# ==============================================================================
## Transformar os dados em um objeto espacial sf
# ==============================================================================
df_sf <- st_as_sf(df, coords = c("x", "y"), crs = 31984)

# ==============================================================================
# Criar uma grade (grid) para a Área de contorno
# ==============================================================================
grade_10_x_10 <- cria_grid(contorno, cell_size = 10)
plot(grade_10_x_10)

# ==============================================================================
## KRIGAGEM ORDINÁRIA
# ==============================================================================

#-----------------------------------------
# Variograma experimental
#-----------------------------------------
variograma <- variogram(z~1, data=df_sf, cressie=T, cutoff = 350)
plot(variograma, plot.numbers=TRUE)

#-----------------------------------------
# Variograma teórico
#-----------------------------------------

model_vario <- fit.variogram(variograma, vgm(c("Sph", "Gau", "Exp")))
plot(variograma, model_vario, plot.numbers=TRUE)

# Parâmetros do Variograma
nugget <- model_vario$psill[1]
psill <- model_vario$psill[2]
range <- model_vario$range[2]
message('Nugget (pepita):', nugget, '\n',
        'Sill (patamar):', psill, '\n',
        'Range (alcance):', range)
model_vario


#-----------------------------------------
# Ordinary kriging (OK)
#-----------------------------------------
st_crs(grade_10_x_10) <- st_crs(df_sf)
ko <- krige(z~ 1 ,                   # formula
            df_sf,                   # data
            grade_10_x_10,           # newdata
            model_vario              # model
            #, maxdist = 5          # local kriging: apenas 5 vizinhos
)
plot(ko)


#-----------------------------------------
# Ordinary kriging (OK) nos lotes
#-----------------------------------------
st_crs(lotes) <- st_crs(df_sf)
ko_lotes <- krige(z~ 1 ,                   # formula
            df_sf,                   # data
            lotes,           # newdata
            model_vario              # model
            #, maxdist = 5           # local kriging: apenas 5 vizinhos
)
plot(ko_lotes[, "var1.pred"])
plot(ko_lotes[, "var1.var"])
#-----------------------------------------
# Usando o pacote Automap
#-----------------------------------------

st_crs(grade_10_x_10) <- st_crs(df_sf)

ko_auto <- autoKrige(z~ 1 ,                       # formula
                     as_Spatial(df_sf),           # data
                     as_Spatial(grade_10_x_10)    # new data
                     , start_vals=c(nugget, range, psill)
)

# Plotar
plot(ko_auto)

#-----------------------------------------
# Geração do raster
#-----------------------------------------
grade_10_x_10$z <- ko$var1.pred
raster_10_x_10 <- raster::rasterFromXYZ(as_Spatial(grade_10_x_10),
                                              crs = 31984)
plot(raster_10_x_10)

#-----------------------------------------
# Cortar o raster pelo limite da Área
#-----------------------------------------
crs(raster_10_x_10) <- crs(contorno)
raster_10_x_10 <- mask(raster_10_x_10, contorno)
plot(raster_10_x_10)

#-----------------------------------------
# Exportar o raster recortado
#-----------------------------------------
writeRaster(raster_10_x_10,
            "./KO/cidade_ecologica_kriging_10_x_10.tif",
            format = "GTiff",
            overwrite = TRUE)

#-----------------------------------------
# Ler o raster recortado
#-----------------------------------------
raster_10_x_10 <- raster("./KO/cidade_ecologica_kriging_10_x_10.tif")
crs(raster_10_x_10) <- crs(contorno)


#-----------------------------------------
# Salvar a grade com a interpolação e variância
#-----------------------------------------

# Pega as coordenadas
coord <- st_coordinates(grade_10_x_10)

# Cria o dataframe de exportação
df_csv <- data.frame( coord ,
                      ko_auto$krige_output$var1.pred ,
                      ko_auto$krige_output$var1.var ,
                      ko_auto$krige_output$var1.stdev)

# Renomeia as colunas
names(df_csv) <- c('x', 'y', 'predicao', 'variancia', 'desvpad')


# Exportar para CSV
write.csv(df_csv,"./KO/cidade_ecologica_kriging_10_x_10_projecao.csv")

# ==============================================================================
## MAPAS
# ==============================================================================
library(RColorBrewer)
library(tmap)

col = rev(brewer.pal(n=5, "Spectral"))

#-----------------------------------------
# Mapa Bubbles dos Dados Amostrais
#-----------------------------------------
mapa <-
  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
  tm_shape(contorno) +
  tm_layout("Amostra de Terrenos",
            legend.outside=TRUE,
            inner.margins = c(0.1,0.1,0.1,0.1) # bottom, left, top, right
  ) +
  tm_fill("white", alpha = 0) +
  tm_borders("black", lwd = 1.5) +
  tm_shape(df_sf) +
  tm_bubbles("z",
             col = "z",
             style="quantile",
             alpha=.5,
             scale=2,
             palette="-RdYlBu",
             title.size="Valores Unitários",
             title.col="R$/m²"

  ) +
  tm_compass() +
  tm_scalebar()

mapa

#-----------------------------------------
# Mapa da Krigagem Ordinária do Raster
#-----------------------------------------
mapa_ko <-
  tm_basemap(c(Satelite = "Esri.WorldImagery", Politico = "OpenStreetMap.DE")) +
  tm_shape(raster_10_x_10) +
  tm_raster( palette = col, title = "Krigagem Ordinária") +
  tm_layout("Amostra de Terrenos",
            legend.outside=TRUE,
            inner.margins = c(0.1,0.1,0.1,0.1) # bottom, left, top, right
  ) +
  tm_compass() +
  tm_scalebar()
mapa_ko

#-----------------------------------------
# Mapa da Krigagem Ordinária dos Lotes
#-----------------------------------------
lotes$predicao <- ko_lotes$var1.pred
pal <- brewer.pal(7, "OrRd")
plot(lotes["predicao"],
     main = "Krigagem Ordinária - Projeção nos Lotes",
     breaks = "quantile", nbreaks = 7,
     pal = pal)


