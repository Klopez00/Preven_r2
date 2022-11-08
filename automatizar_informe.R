
library(here)
library(lubridate)
setwd("S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/Informes centinela")
here <- "S:/Medicina Preventiva/AISLAMIENTOS TRABAJADOS/RECEPCION AISLAMIENTOS/Informes centinela"
#Sys.setenv(RSTUDIO_PANDOC="C:/Users/kl_93/AppData/Local/pandoc")
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/quarto/bin/tools")
filename <- paste0(today(),"-Informe-Aislamientos.html")
rmarkdown::render(
  input = "Informe_centinela.Rmd",
  output_format = "html_document",
  output_file = filename,
  output_dir = here,
  encoding = "UTF-8",
  clean = TRUE,
  quiet = TRUE
)
