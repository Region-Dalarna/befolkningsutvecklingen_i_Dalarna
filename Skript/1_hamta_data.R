# Uppdaterar data som används i rapporten "Läget i Dalarna"
if (!require("pacman")) install.packages("pacman")
p_load(here)

Output_mapp = here("Data","/")
vald_region = "2021"

# Diagram  befolkningsförändring
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_befolkningsforandring_region_kon_ar_SCB.R", encoding="UTF-8")
gg_befolkning = diagram_befolkningsforandring_ar(region_vekt = vald_region,
                                                spara_figur=FALSE,
                                                diag_folkmangd = FALSE,
                                                returnera_data = TRUE)

# Diagram födda och döda
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_fodelsenetto_region_SCB.R")
gg_fodda_doda = diagram_fodelsenetto(region_vekt = vald_region,
                                     spara_figur = FALSE,
                                     tid = "*",
                                     returnera_data = TRUE)

# Diagram  flyttnetto
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_flytt_inrikes_utrikes_netto_SCB.R", encoding="UTF-8")
gg_flytt <- diagram_inr_utr_flytt(region_vekt = vald_region,
                                  spara_figur=FALSE,
                                  #tid = c(2000:9999),
                                  returnera_data = TRUE)

# Diagram flyttnetto åldersgrupper
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_flytt_inrikes_aldersgrupper_SCB.R", encoding="UTF-8")
gg_flytt_alder <- diagram_inrikes_flytt_alder(region_vekt = vald_region,
                                              #tid = c(2000:9999),
                                              spara_figur = FALSE)

# Diagram flyttnetto födelseregion
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_flytt_inrikes_bakgrund_SCB.R", encoding="UTF-8")
gg_flytt_bakgrund <- diag_inr_flyttnetto_inr_utr_fodda(region_vekt = vald_region,
                                                       skriv_diagram = FALSE,
                                                       output_mapp =  "G:/skript/jon/Figurer/",
                                                       farg_vekt = diagramfarger("rus_sex")[2:1])


rmarkdown::render(
  input = 'befolkningsutveckling_kommun.Rmd',
  output_file = paste0("befolkningsutveckling_",vald_region,".html"),
  envir = parent.frame()
)
