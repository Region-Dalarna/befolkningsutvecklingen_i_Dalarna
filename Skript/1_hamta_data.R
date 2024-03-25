# Uppdaterar data som används i rapporten "Läget i Dalarna"
if (!require("pacman")) install.packages("pacman")
p_load(here)

Output_mapp = here("Data","/")
vald_region = "2021"

# Diagram 1, befolkningsförändring
source(here("Skript","diagram_befolkningsforandring.R"), encoding="UTF-8")
gg_befolkning = diagram_befolkning(region_vekt = vald_region,
                          spara_figur=FALSE,
                          diag_folkmangd = FALSE)

# Diagram 2, födda och döda
source(here("Skript","diagram_fodda_doda.R"), encoding="UTF-8")
gg_fodda_doda = diagram_fodda_doda(region_vekt = vald_region,
                          spara_figur=FALSE,
                          diag_fodda = TRUE)

# Diagram 3-5 flyttnetto
source(here("Skript","diagam_inrikes_flytt.R"), encoding="UTF-8")
gg_flytt <- diagram_inrikes_flytt(region_vekt = vald_region,
                                  spara_figur=FALSE,
                                  tid = c(2000:9999))

# Diagram 6-8 flyttnetto åldersgrupper
source(here("Skript","diagram_inr_flyttnetto_aldergrupper.R"), encoding="UTF-8")
gg_flytt_alder <- diagram_inrikes_flytt_alder(region_vekt = vald_region,
                                              spara_figur = FALSE)



rmarkdown::render(
  input = 'befolkningsutveckling_kommun.Rmd',
  output_file = paste0("befolkningsutveckling_",vald_region,".html"),
  envir = parent.frame()
)
