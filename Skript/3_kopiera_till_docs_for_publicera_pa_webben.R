if (!require("pacman")) install.packages("pacman")
p_load(here,
       tidyverse)

walk(dala_kommuner, ~ {
  if (.x == "") {
    file.copy(from = "befolkningsutveckling.html", to = "docs/index.html", overwrite = TRUE)
  } else {
    file.copy(from = glue("befolkningsutveckling{.x}.html"), to = glue("docs/befolkningsutveckling{.x}.html"), overwrite = TRUE)
  }
})







# if(publicera == TRUE){
#   
#   file.copy(from = "befolkningsutveckling_Avesta.html", to = "docs/befolkningsutveckling_Avesta.html", overwrite = TRUE)
#   file.copy(from = "befolkningsutveckling_Borlänge.html", to = "docs/befolkningsutveckling_Borlänge.html", overwrite = TRUE)
#   file.copy(from = "befolkningsutveckling_Falun.html", to = "docs/befolkningsutveckling_Falun.html", overwrite = TRUE)
#   file.copy(from = "befolkningsutveckling_Gagnef.html", to = "docs/befolkningsutveckling_Gagnef.html", overwrite = TRUE)
#   file.copy(from = "befolkningsutveckling_Hedemora.html", to = "docs/befolkningsutveckling_Hedemora.html", overwrite = TRUE)
#   file.copy(from = "befolkningsutveckling_Leksand.html", to = "docs/befolkningsutveckling_Leksand.html", overwrite = TRUE)
#   file.copy(from = "befolkningsutveckling_Malung_Sälen.html", to = "docs/befolkningsutveckling_Malung_Sälen.html", overwrite = TRUE)
#   file.copy(from = "befolkningsutveckling_Orsa.html", to = "docs/befolkningsutveckling_Orsa.html", overwrite = TRUE)
#   file.copy(from = "befolkningsutveckling_Ludvika.html", to = "docs/befolkningsutveckling_Ludvika.html", overwrite = TRUE)
#   file.copy(from = "befolkningsutveckling_Mora.html", to = "docs/befolkningsutveckling_Mora.html", overwrite = TRUE)
#   file.copy(from = "befolkningsutveckling_Säter.html", to = "docs/befolkningsutveckling_Säter.html", overwrite = TRUE)
#   file.copy(from = "befolkningsutveckling_Smedjebacken.html", to = "docs/befolkningsutveckling_Smedjebacken.html", overwrite = TRUE)
#   file.copy(from = "befolkningsutveckling_Rättvik.html", to = "docs/befolkningsutveckling_Rättvik.html", overwrite = TRUE)
#   file.copy(from = "befolkningsutveckling_Älvdalen.html", to = "docs/befolkningsutveckling_Älvdalen.html", overwrite = TRUE)
#   file.copy(from = "befolkningsutveckling_Vansbro.html", to = "docs/befolkningsutveckling_Vansbro.html", overwrite = TRUE)
#   file.copy(from = "befolkningsutveckling.html", to = "docs/index.html", overwrite = TRUE)
#   
# }
