if (!require("pacman")) install.packages("pacman")
p_load(here,
       stringr)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

# hämta kommunnamnen för att använda i walk-loopen nedan
dala_kommuner <- hamtaregion_kod_namn(regionkod = hamtakommuner(lan = "20", F, F))$region %>% str_replace("-", "_") %>% paste0("_", .) %>% c("", .)

walk(dala_kommuner, ~ {
  rmarkdown::render(
    input = glue('befolkningsutveckling{.x}.Rmd'),
    output_file = glue("befolkningsutveckling{.x}.html"),
    envir = parent.frame()
  )
})



rmarkdown::render(
  input = 'befolkningsutveckling.Rmd',
  output_file = paste0("befolkningsutveckling.html"),
  envir = parent.frame()
)

# 
# rmarkdown::render(
#   input = 'befolkningsutveckling_Avesta.Rmd',
#   output_file = paste0("befolkningsutveckling_Avesta.html"),
#   envir = parent.frame()
# )
# 
# rmarkdown::render(
#   input = 'befolkningsutveckling_Borlänge.Rmd',
#   output_file = paste0("befolkningsutveckling_Borlänge.html"),
#   envir = parent.frame()
# )
# 
# rmarkdown::render(
#   input = 'befolkningsutveckling_Falun.Rmd',
#   output_file = paste0("befolkningsutveckling_Falun.html"),
#   envir = parent.frame()
# )
# 
# rmarkdown::render(
#   input = 'befolkningsutveckling_Gagnef.Rmd',
#   output_file = paste0("befolkningsutveckling_Gagnef.html"),
#   envir = parent.frame()
# )
# 
# rmarkdown::render(
#   input = 'befolkningsutveckling_Hedemora.Rmd',
#   output_file = paste0("befolkningsutveckling_Hedemora.html"),
#   envir = parent.frame()
# )
# 
# rmarkdown::render(
#   input = 'befolkningsutveckling_Leksand.Rmd',
#   output_file = paste0("befolkningsutveckling_Leksand.html"),
#   envir = parent.frame()
# )
# 
# rmarkdown::render(
#   input = 'befolkningsutveckling_Ludvika.Rmd',
#   output_file = paste0("befolkningsutveckling_Ludvika.html"),
#   envir = parent.frame()
# )
# 
# rmarkdown::render(
#   input = 'befolkningsutveckling_Mora.Rmd',
#   output_file = paste0("befolkningsutveckling_Mora.html"),
#   envir = parent.frame()
# )
# 
# rmarkdown::render(
#   input = 'befolkningsutveckling_Malung_Sälen.Rmd',
#   output_file = paste0("befolkningsutveckling_Malung_Sälen.html"),
#   envir = parent.frame()
# )
# 
# rmarkdown::render(
#   input = 'befolkningsutveckling_Orsa.Rmd',
#   output_file = paste0("befolkningsutveckling_Orsa.html"),
#   envir = parent.frame()
# )
# 
# rmarkdown::render(
#   input = 'befolkningsutveckling_Säter.Rmd',
#   output_file = paste0("befolkningsutveckling_Säter.html"),
#   envir = parent.frame()
# )
# 
# rmarkdown::render(
#   input = 'befolkningsutveckling_Smedjebacken.Rmd',
#   output_file = paste0("befolkningsutveckling_Smedjebacken.html"),
#   envir = parent.frame()
# )
# 
# rmarkdown::render(
#   input = 'befolkningsutveckling_Rättvik.Rmd',
#   output_file = paste0("befolkningsutveckling_Rättvik.html"),
#   envir = parent.frame()
# )
# 
# rmarkdown::render(
#   input = 'befolkningsutveckling_Älvdalen.Rmd',
#   output_file = paste0("befolkningsutveckling_Älvdalen.html"),
#   envir = parent.frame()
# )
# 
# rmarkdown::render(
#   input = 'befolkningsutveckling_Vansbro.Rmd',
#   output_file = paste0("befolkningsutveckling_Vansbro.html"),
#   envir = parent.frame()
# )
