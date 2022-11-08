library(here)
library(lubridate)
library(rmarkdown)

setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/Informe covid")

here <- "S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/Informe covid/Informes"

#Sys.setenv(RSTUDIO_PANDOC="C:/Users/kl_93/AppData/Local/pandoc")
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/quarto/bin/tools")


filename <- paste0(today(),"-Informe-COVID.html")

rmarkdown::render(
  input = "Crear_informe.Rmd",
  output_format = "html_document",
  output_file = filename,
  output_dir = here,
  encoding = "UTF-8",
  clean = TRUE,
  quiet = TRUE
)


#https://stackoverflow.com/questions/28432607/pandoc-version-1-12-3-or-higher-is-required-and-was-not-found-r-shiny
#para problemas con pandoc