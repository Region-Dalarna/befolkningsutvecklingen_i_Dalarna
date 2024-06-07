# Uppdaterar data som används i rapporten "Läget i Dalarna"
if (!require("pacman")) install.packages("pacman")
p_load(here)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

Output_mapp = here("Data","/")
Output_mapp_figur = here("Figurer","/")
vald_region = "20"
valt_lan = "20"
# 
# # Diagram  befolkningsförändring
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_befolkningsforandring_region_kon_ar_SCB.R", encoding="UTF-8")
gg_befolkning = diagram_befolkningsforandring_ar(region_vekt = vald_region,
                                                spara_figur=FALSE,
                                                diag_folkmangd = FALSE,
                                                returnera_data = TRUE)
# 
# Diagram födda och döda
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_fodelsenetto_region_SCB.R")
gg_fodda_doda = diagram_fodelsenetto(region_vekt = vald_region,
                                     spara_figur = FALSE,
                                     tid = "*",
                                     returnera_data = TRUE)
# 

# Diagram flyttnetto län
source("C:/Users/frkjon/Projekt/befolkningsutvecklingen_i_Dalarna/Skript/diagram_flyttningar_overskott.R")
gg_flytt_lan <- diagram_inflytt(spara_figur=FALSE,
                                returnera_data = TRUE)

# # Diagram  flyttnetto
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_flytt_inrikes_utrikes_netto_SCB.R", encoding="UTF-8")
gg_flytt <- diagram_inr_utr_flytt(region_vekt = vald_region,
                                  spara_figur=FALSE,
                                  #tid = c(2000:9999),
                                  returnera_data = TRUE)

# # Diagram flyttnetto åldersgrupper
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_flytt_inrikes_aldersgrupper_SCB.R", encoding="UTF-8")
gg_flytt_alder <- diagram_inrikes_flytt_alder(region_vekt = vald_region,
                                              #tid = c(2000:9999),
                                              spara_figur = FALSE)

# # Diagram flyttnetto födelseregion
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_flytt_inrikes_bakgrund_SCB.R", encoding="UTF-8")
gg_flytt_bakgrund <- diag_inr_flyttnetto_inr_utr_fodda(region_vekt = vald_region,
                                                       skriv_diagram = FALSE,
                                                       output_mapp =  "G:/skript/jon/Figurer/",
                                                       farg_vekt = diagramfarger("rus_sex")[2:1])

# Kartor över befolkningsutvecklingen
source(here("Skript","kartor.R"), encoding="UTF-8")
gg_kartor <- kartor_befolkning(karta_kommun = TRUE,
                               karta_lan = TRUE,
                               returnera_data = TRUE)

# Diagram fruk
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_fruktsamhet_SCB.R", encoding="UTF-8")
gg_fruktsamhet <- diagram_fruktsamhet(region_vekt = c("00",valt_lan),
                                      diag_facet = TRUE, # diag_fokus_tid som facet-diagram istället för ett per region
                                      diag_jmf_lan = FALSE, # Skapa diagram för jämförelse mellan valda regioner
                                      diag_forandring = FALSE, # Skapa diagram för förändringar över tid
                                      spara_figur=FALSE,
                                      vald_period = "*",
                                      facet_skala = "fixed",
                                      returnera_data = TRUE)

source("G:/skript/diagram/diag_befpyramid.R")
gg_befpyramid <- diag_befpyramid(geo_vekt = vald_region,
                                 jmfr_linje = "ar",
                                 jmfr_ar = "1968",
                                 output_mapp = Output_mapp_figur)




rmarkdown::render(
  input = 'befolkningsutveckling.Rmd',
  output_file = paste0("befolkningsutveckling.html"),
  envir = parent.frame()
)
