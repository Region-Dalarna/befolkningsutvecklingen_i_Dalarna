# source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
# skapa_hamta_data_skript_pxweb(skickad_url_pxweb = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101J/InOmflytt",
#                               output_mapp = "C:/Users/frkjon/Projekt/befolkningsutvecklingen_i_Dalarna/Skript/",
#                               tabell_namn = "")


# Uppdaterar data som används i rapporten "Läget i Dalarna"
if (!require("pacman")) install.packages("pacman")
p_load(here)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

Output_mapp = here("Data","/")
Output_mapp_figur = here("Figurer","/")
vald_region = "20"
valt_lan = "20"
spara_figur = FALSE
publicera = FALSE
# 
# # Diagram  befolkningsförändring
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_befolkningsforandring_region_kon_ar_SCB.R", encoding="UTF-8")
gg_befolkning = diagram_befolkningsforandring_ar(region_vekt = hamtakommuner(vald_region,tamedriket = FALSE),
                                                spara_figur=spara_figur,
                                                diag_folkmangd = FALSE,
                                                returnera_data = TRUE,
                                                output_mapp_figur = Output_mapp_figur)
# 
# Diagram födda och döda
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_fodelsenetto_region_SCB.R")
gg_fodda_doda = diagram_fodelsenetto(region_vekt = hamtakommuner(vald_region,tamedriket = FALSE),
                                     spara_figur = spara_figur,
                                     tid = "*",
                                     returnera_data = TRUE,
                                     output_mapp_figur = Output_mapp_figur)
# 

# Diagram flyttnetto län
source(here("Skript","diagram_flyttningar_overskott.R"), encoding="UTF-8")
gg_flytt_lan <- diagram_inflytt(spara_figur = spara_figur,
                                returnera_data = TRUE,
                                output_mapp_figur = Output_mapp_figur)

# Diagram ut/inflyttning till län
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_inflyttlan_utflyttlan_SCB.R")
gg_utflytt_lan <- diagram_inflyttlan_utflyttlan(inflyttningsl_klartext = "*",
                                                utflyttningsl_klartext = " Dalarnas län (Utflyttningslän)",
                                                tid = "9999",
                                                spara_figur = spara_figur,
                                                output_mapp_figur = Output_mapp_figur,
                                                returnera_data = TRUE)

gg_inflytt_lan <- diagram_inflyttlan_utflyttlan(inflyttningsl_klartext = " Dalarnas län (Inflyttningslän)",
                                                utflyttningsl_klartext = "*",
                                                tid = "9999",
                                                spara_figur = spara_figur,
                                                output_mapp_figur = Output_mapp_figur,
                                                returnera_data = TRUE)

# # Diagram  flyttnetto
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_flytt_inrikes_utrikes_netto_SCB.R", encoding="UTF-8")
gg_flytt <- diagram_inr_utr_flytt(region_vekt = hamtakommuner(vald_region,tamedriket = FALSE),
                                  spara_figur = spara_figur,
                                  #tid = c(2000:9999),
                                  returnera_data = TRUE,
                                  output_mapp_figur = Output_mapp_figur)

# # Diagram flyttnetto åldersgrupper
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_flytt_inrikes_aldersgrupper_SCB.R", encoding="UTF-8")
gg_flytt_alder <- diagram_inrikes_flytt_alder(region_vekt = hamtakommuner(vald_region,tamedriket = FALSE),
                                              #tid = c(2000:9999),
                                              spara_figur = spara_figur,
                                              output_mapp_figur = Output_mapp_figur)

# Diagram flyttnetto födelseregion
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_flytt_inrikes_bakgrund_SCB.R", encoding="UTF-8")
gg_flytt_bakgrund <- diag_inr_flyttnetto_inr_utr_fodda(region_vekt = hamtakommuner(vald_region,tamedriket = FALSE),
                                                       skriv_diagram = spara_figur,
                                                       output_mapp =  Output_mapp_figur,
                                                       returnera_data = TRUE,
                                                       farg_vekt = diagramfarger("rus_sex")[2:1])

# Kartor över befolkningsutvecklingen
source(here("Skript","kartor.R"), encoding="UTF-8")
gg_kartor <- kartor_befolkning(karta_kommun = TRUE,
                               karta_lan = TRUE,
                               returnera_data = TRUE)

# Diagram fruktsamhet
# Jmf Sverige och Dalarna
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_fruktsamhet_SCB.R", encoding="UTF-8")
gg_fruktsamhet <- diagram_fruktsamhet(region_vekt = c("00",valt_lan),
                                      diag_facet = TRUE, # diag_fokus_tid som facet-diagram istället för ett per region
                                      diag_jmf_lan = FALSE, # Skapa diagram för jämförelse mellan valda regioner
                                      diag_forandring = FALSE, # Skapa diagram för förändringar över tid
                                      spara_figur = spara_figur,
                                      output_mapp_figur = Output_mapp_figur,
                                      vald_period = "*",
                                      facet_skala = "fixed",
                                      returnera_data = TRUE)
# Jmf Dalarnas kommuner
gg_fruktsamhet_kommun <- diagram_fruktsamhet(region_vekt = hamtakommuner(valt_lan,tamedriket = FALSE,tamedlan=FALSE),
                                      diag_facet = TRUE, # diag_fokus_tid som facet-diagram istället för ett per region
                                      diag_jmf_lan = TRUE, # Skapa diagram för jämförelse mellan valda regioner
                                      diag_forandring = FALSE, # Skapa diagram för förändringar över tid
                                      spara_figur = spara_figur,
                                      output_mapp_figur = Output_mapp_figur,
                                      facet_skala = "fixed",
                                      vald_period = "*",
                                      returnera_data = TRUE)

## Befolkningspyramid
source("G:/skript/diagram/diag_befpyramid.R")
gg_befpyramid <- diag_befpyramid(geo_vekt = c(vald_region),
                                 jmfr_linje = "ar",
                                 jmfr_ar = "1968",
                                 output_mapp = Output_mapp_figur)

## Befolkningsprognos
source(here("Skript","befprognos.R"), encoding="UTF-8")

# gg_befprognos <- diagram_befprognos(region_vekt = vald_region,
#                                     diag_aldergrupp = TRUE,
#                                     diag_jmf_region = FALSE,
#                                     output_mapp_figur = Output_mapp_figur)

gg_befprognos <- diagram_befprognos(region_vekt = hamtakommuner(vald_region,tamedriket = FALSE,tamedlan=TRUE),
                                     diag_aldergrupp = TRUE, 
                                     diag_jmf_region = FALSE,
                                     diag_alla = TRUE,
                                     jmf_procent = FALSE,
                                     returnera_data = TRUE,
                                     output_mapp_figur = Output_mapp_figur,
                                     spara_figur = spara_figur)

gg_befprognos_procent <- diagram_befprognos(region_vekt = hamtakommuner(vald_region,tamedriket = FALSE,tamedlan=TRUE),
                                            diag_aldergrupp = FALSE, 
                                            diag_jmf_region = TRUE,
                                            diag_alla = FALSE,
                                            jmf_procent = TRUE,
                                            returnera_data = FALSE,
                                            output_mapp_figur = Output_mapp_figur,
                                            spara_figur = spara_figur)



rmarkdown::render(
  input = 'befolkningsutveckling.Rmd',
  output_file = paste0("befolkningsutveckling.html"),
  envir = parent.frame()
)

#file.copy(from = "befolkningsutveckling.html", to = "Dalarna/index.html", overwrite = TRUE)

rmarkdown::render(
  input = 'befolkningsutveckling_Avesta.Rmd',
  output_file = paste0("befolkningsutveckling_Avesta.html"),
  envir = parent.frame()
)

#file.copy(from = "befolkningsutveckling_Avesta.html", to = "Avesta/index.html", overwrite = TRUE)

rmarkdown::render(
  input = 'befolkningsutveckling_Hedemora.Rmd',
  output_file = paste0("befolkningsutveckling_Hedemora.html"),
  envir = parent.frame()
)

rmarkdown::render(
  input = 'befolkningsutveckling_Ludvika.Rmd',
  output_file = paste0("befolkningsutveckling_Ludvika.html"),
  envir = parent.frame()
)

rmarkdown::render(
  input = 'befolkningsutveckling_Säter.Rmd',
  output_file = paste0("befolkningsutveckling_Säter.html"),
  envir = parent.frame()
)

rmarkdown::render(
  input = 'befolkningsutveckling_Gagnef.Rmd',
  output_file = paste0("befolkningsutveckling_Gagnef.html"),
  envir = parent.frame()
)

rmarkdown::render(
  input = 'befolkningsutveckling_Leksand.Rmd',
  output_file = paste0("befolkningsutveckling_Leksand.html"),
  envir = parent.frame()
)

rmarkdown::render(
  input = 'befolkningsutveckling_Smedjebacken.Rmd',
  output_file = paste0("befolkningsutveckling_Smedjebacken.html"),
  envir = parent.frame()
)

rmarkdown::render(
  input = 'befolkningsutveckling_Borlänge.Rmd',
  output_file = paste0("befolkningsutveckling_Borlänge.html"),
  envir = parent.frame()
)

rmarkdown::render(
  input = 'befolkningsutveckling_Falun.Rmd',
  output_file = paste0("befolkningsutveckling_Falun.html"),
  envir = parent.frame()
)

rmarkdown::render(
  input = 'befolkningsutveckling_Mora.Rmd',
  output_file = paste0("befolkningsutveckling_Mora.html"),
  envir = parent.frame()
)

rmarkdown::render(
  input = 'befolkningsutveckling_Rättvik.Rmd',
  output_file = paste0("befolkningsutveckling_Rättvik.html"),
  envir = parent.frame()
)

rmarkdown::render(
  input = 'befolkningsutveckling_Älvdalen.Rmd',
  output_file = paste0("befolkningsutveckling_Älvdalen.html"),
  envir = parent.frame()
)

rmarkdown::render(
  input = 'befolkningsutveckling_Orsa.Rmd',
  output_file = paste0("befolkningsutveckling_Orsa.html"),
  envir = parent.frame()
)

rmarkdown::render(
  input = 'befolkningsutveckling_Malung_Sälen.Rmd',
  output_file = paste0("befolkningsutveckling_Malung_Sälen.html"),
  envir = parent.frame()
)

rmarkdown::render(
  input = 'befolkningsutveckling_Vansbro.Rmd',
  output_file = paste0("befolkningsutveckling_Vansbro.html"),
  envir = parent.frame()
)

if(publicera == TRUE){
  
  file.copy(from = "befolkningsutveckling_Avesta.html", to = "docs/befolkningsutveckling_Avesta.html", overwrite = TRUE)
  file.copy(from = "befolkningsutveckling_Borlänge.html", to = "docs/befolkningsutveckling_Borlänge.html", overwrite = TRUE)
  file.copy(from = "befolkningsutveckling_Hedemora.html", to = "docs/befolkningsutveckling_Hedemora.html", overwrite = TRUE)
  file.copy(from = "befolkningsutveckling_Ludvika.html", to = "docs/befolkningsutveckling_Ludvika.html", overwrite = TRUE)
  file.copy(from = "befolkningsutveckling_Säter.html", to = "docs/befolkningsutveckling_Säter.html", overwrite = TRUE)
  file.copy(from = "befolkningsutveckling_Gagnef.html", to = "docs/befolkningsutveckling_Gagnef.html", overwrite = TRUE)
  file.copy(from = "befolkningsutveckling_Leksand.html", to = "docs/befolkningsutveckling_Leksand.html", overwrite = TRUE)
  file.copy(from = "befolkningsutveckling_Smedjebacken.html", to = "docs/befolkningsutveckling_Smedjebacken.html", overwrite = TRUE)
  file.copy(from = "befolkningsutveckling_Falun.html", to = "docs/befolkningsutveckling_Falun.html", overwrite = TRUE)
  file.copy(from = "befolkningsutveckling_Mora.html", to = "docs/befolkningsutveckling_Mora.html", overwrite = TRUE)
  file.copy(from = "befolkningsutveckling_Rättvik.html", to = "docs/befolkningsutveckling_Rättvik.html", overwrite = TRUE)
  file.copy(from = "befolkningsutveckling_Älvdalen.html", to = "docs/befolkningsutveckling_Älvdalen.html", overwrite = TRUE)
  file.copy(from = "befolkningsutveckling_Orsa.html", to = "docs/befolkningsutveckling_Orsa.html", overwrite = TRUE)
  file.copy(from = "befolkningsutveckling_Malung_Sälen.html", to = "docs/befolkningsutveckling_Malung_Sälen.html", overwrite = TRUE)
  file.copy(from = "befolkningsutveckling_Vansbro.html", to = "docs/befolkningsutveckling_Vansbro.html", overwrite = TRUE)
  file.copy(from = "befolkningsutveckling.html", to = "docs/befolkningsutveckling.html", overwrite = TRUE)
  
}






