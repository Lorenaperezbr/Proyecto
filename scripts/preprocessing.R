# Se descargan los datos y guardan como .rds
library(data.table)

conteo_feb <- fread("https://catalogodatos.gub.uy/dataset/95a35fa3-9629-41a6-976a-5fb8c900fdec/resource/b985762d-c1c1-4dd0-bc57-6c3ab53e8ff8/download/autoscope_02_2021_volumen.csv", header = TRUE, sep = ";")
conteo_mar <- fread("https://catalogodatos.gub.uy/dataset/95a35fa3-9629-41a6-976a-5fb8c900fdec/resource/6df7fc9c-8331-4d93-928c-cd69b48940a2/download/autoscope_03_2021.csv", header = TRUE, sep = ";")
velocidad_feb <- fread("https://catalogodatos.gub.uy/dataset/64189920-546e-4266-a8ee-476524313661/resource/01ba56d8-2dab-4d8b-8391-d03159314144/download/autoscope_02_2021_velocidad.csv", header = TRUE, sep = ";")
velocidad_mar <- fread("https://catalogodatos.gub.uy/dataset/64189920-546e-4266-a8ee-476524313661/resource/da56b7db-dcef-41e0-8f16-a00406ae7c1c/download/autoscope_03_2021_velocidad.csv", header = TRUE, sep = ";")

# Guardar como rds
saveRDS(conteo_feb, "data/raw/conteo_feb.rds")
saveRDS(conteo_mar, "data/raw/conteo_mar.rds")
saveRDS(velocidad_feb, "data/raw/velocidad_feb.rds")
saveRDS(velocidad_mar, "data/raw/velocidad_mar.rds")