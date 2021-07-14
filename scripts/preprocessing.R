# Se descargan los datos y guardan como .rds
library(data.table)
library(sf)
library(ggplot2)

# Ya ejecutado
# conteo_feb <- fread("https://catalogodatos.gub.uy/dataset/95a35fa3-9629-41a6-976a-5fb8c900fdec/resource/b985762d-c1c1-4dd0-bc57-6c3ab53e8ff8/download/autoscope_02_2021_volumen.csv", header = TRUE, sep = ";")
# conteo_mar <- fread("https://catalogodatos.gub.uy/dataset/95a35fa3-9629-41a6-976a-5fb8c900fdec/resource/6df7fc9c-8331-4d93-928c-cd69b48940a2/download/autoscope_03_2021.csv", header = TRUE, sep = ";")
# velocidad_feb <- fread("https://catalogodatos.gub.uy/dataset/64189920-546e-4266-a8ee-476524313661/resource/01ba56d8-2dab-4d8b-8391-d03159314144/download/autoscope_02_2021_velocidad.csv", header = TRUE, sep = ";")
# velocidad_mar <- fread("https://catalogodatos.gub.uy/dataset/64189920-546e-4266-a8ee-476524313661/resource/da56b7db-dcef-41e0-8f16-a00406ae7c1c/download/autoscope_03_2021_velocidad.csv", header = TRUE, sep = ";")
# 
# # Guardar como rds, estos son los datos originales.
# saveRDS(conteo_feb, "data/raw/conteo_feb.rds")
# saveRDS(conteo_mar, "data/raw/conteo_mar.rds")
# saveRDS(velocidad_feb, "data/raw/velocidad_feb.rds")
# saveRDS(velocidad_mar, "data/raw/velocidad_mar.rds")

# Joins
febrero <- conteo_feb[velocidad_feb, 
                      on = c('cod_detector', 'id_carril', 'fecha', 'hora', 'latitud', 'longitud'), nomatch = 0]
marzo <- conteo_mar[velocidad_mar, 
                    on = c('cod_detector', 'id_carril', 'fecha', 'hora', 'latitud', 'longitud'), nomatch = 0]

febrero_marzo <- rbind(febrero, marzo)
rm(list=c("conteo_feb","velocidad_feb", "conteo_mar", "velocidad_mar", "febrero", "marzo"))

# OBSERVACIÓN: Les guardo febrero_marzo en "processed" PERO si estos datos no los van a usar, entonces deberían borrarlos.
# La buena práctica es: se guardan los datos de entrada (raw), se procesan y solo se guardan los datos finales en processed o si fuese
# necesario para el análisis algún conjunto puntual intermedio pero NO todos los datos de pasos intermedios.
# Acá se los guardo para que vean y decidan pero luego limpien.
# En este caso, hay que tener en cuenta que si los datos finales les quedan muy grandes y como no compartimos repositorio local
# tal vez no pueden subir los datos finales por un tema de tamaño (y hasta buena práctica), en ese caso, comenten que hay que ejecutar el procesamiento 
# una vez que se clona el repo. Pero ustedes trabajen con los finales en el Rmd (les va a ahorrar bastante tiempo).

saveRDS(febrero_marzo, "data/processed/febrero_marzo.rds") 
# Osea, se los dejo subido (así corren enseguida el .Rmd) al repo, luego pueden borrarla para que no demore.

# Cargar municipios
municipios <- st_read("data/geo/mdeo_barrios/")
st_crs(x = municipios) <-  5382
# st_crs(municipios) <- sp::CRS("+proj=utm +zone=21+datum=WGS84")
# OBS: "+proj=utm +zone=21+datum=WGS84" es la oficial y la que se supone deberían usar. Si usan este sistema no matchea.
# El problema es saber cual es sistema de coordenadas que usa el catalogo de datos abiertos (y que no especifican)
municipios <- st_transform(municipios, "+init=epsg:4326") # CHEQUEAR coordenadas febrero_marzo o sea el los datos del catalogo

# Join
febrero_marzo_sf <- st_as_sf(febrero_marzo[sample(1:.N, 0.01*.N), ], coords = c("longitud", "latitud")) # MUESTRA
# febrero_marzo_sf <- st_as_sf(febrero_marzo, coords = c("longitud", "latitud")) # TODOS LOS DATOS
st_crs(febrero_marzo_sf) <- st_crs(municipios)

base <- 
  st_join(febrero_marzo_sf, municipios) %>% 
  dplyr::filter(!is.na(velocidad_promedio)) # Sacar posibles missing (no hay ninguno.)
# Acá agregan AREA, PERIMETER y CCZ pero siguen teniendo la geometría asociada a los puntos, no los barrios.

# Si hicieran al revez, les repite la geometria de municipio para cada observación. PERO eso tampoco es lo que quieren.
# Porque repetirían el (mismo) vector de geometría por cada observación
# base <- 
#   st_join(municipios, febrero_marzo_sf) %>% 
#   dplyr::filter(!is.na(velocidad_promedio))
# Si lo quieren en formato data frame + la geometría, por ejemplo:
# base <- cbind(st_drop_geometry(base), st_geometry(base))

# Chequeo.
# ggplot() + 
#   geom_sf(data = municipios) + 
#   geom_sf(data = febrero_marzo_sf) +


# Acá pueden elegir si guardar base ó febrero_marzo lo que vayan a usar pero si es pesada no lo suban al repo y lo ponen el el gitignore
# O si van a generar agrupaciones/resumenes que luego se la van a agregar a municipios y eso lo hacen en el Rmd.
saveRDS(base, "data/processed/febrero_marzo_join.rds")
