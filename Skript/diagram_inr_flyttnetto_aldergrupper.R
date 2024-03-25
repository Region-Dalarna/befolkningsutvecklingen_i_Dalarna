#test = diagram_inrikes_flytt_alder(spara_figur=FALSE,region_vekt = "20")
diagram_inrikes_flytt_alder <- function(region_vekt = "20", # Val av kommuner
                                  output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                                  #output_mapp_data = NA, # Vart hamnar data om den skall sparas. NA medför att data inte sparas
                                  #filnamn_data = "andel_offentligt.xlsx", # Filnamn för sparad data
                                  vald_farg = diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                  spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                  diag_flyttnetto_alder = TRUE, # Skapa diagram för flyttnetto
                                  diag_alder_fokus = TRUE, # Skapa diagram för flyttnetto uppdelat
                                  alder_grupp = c(20, 30, 40, 50, 60), # Vilka åldersgrupper skall användas. Välj enligt principen upp till första, sedan intervall mellan och sedan från sista
                                  alder_grupp_fokus = "20-29 år", # Vilken åldersgrupp skall fokuseras i diag_alder_fokus. Måste finnas bland grupperna ovan
                                  valda_ar = c("2021","2022","2023"), # Vilka år skall användas i diag_flyttnetto_alder
                                  returnera_figur = TRUE # Om man vill att figuren skall returneras från funktionen
                                  #returnera_data = FALSE, # True om användaren vill returnera data från funktionen
                                  #diag_totalt = TRUE, # Skriver ut diagram för kön totalt
                                  #diag_kon = TRUE # Skriver ut diagram uppdelat på kön
){
  
  # ===========================================================================================================
  #
  # Skript som skapar diagram för andelen som arbetar inom offentlig sektor. Funkar med och utan könsuppdelning men enbart för senaste år
  # Går även att använda olika åldersspann
  # ===========================================================================================================
  
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_flyttningar_region_alder_kon_scb.R")
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna."
  
  gg_list <- list()
  objektnamn <- c()
  
  flytt_df <- hamta_bef_flyttningar_region_alder_kon_scb(region_vekt = region_vekt,
                                                         cont_klartext = c("Inrikes flyttningsöverskott", "Invandringsöverskott"),
                                                         kon_klartext = c("Kvinnor", "Män"),
                                                         alder_koder = "*") %>%
      filter(ålder != "totalt ålder") %>% 
        mutate(alder_grupper = skapa_aldersgrupper(ålder,alder_grupp)) %>% 
          group_by(år,regionkod,region, variabel , alder_grupper) %>% 
            summarize(varde = sum(varde)) %>% 
              ungroup()
  
  
  if(diag_flyttnetto_alder == TRUE){
   
    reg_txt <- flytt_df$region %>% unique() %>% skapa_kortnamn_lan(T)
    
    diagram_titel <- paste0("Inrikes flyttnetto i ", reg_txt)
    diagramfil <- paste0("Flyttnetto_alder_", reg_txt, "_ar_", min(valda_ar), "_", max(valda_ar), ".png")
    objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = flytt_df %>% 
                                   filter(år%in%valda_ar,
                                          variabel == "Inrikes flyttningsöverskott"), 
                                 skickad_x_var = "alder_grupper", 
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "år",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 #x_axis_storlek = 8,
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "",
                                 geom_position_stack = FALSE,
                                 #diagram_facet = FALSE,
                                 legend_vand = FALSE,
                                 dataetiketter = TRUE,
                                 #facet_grp = "kön",
                                 #fokusera_varden = total_list,
                                 manual_color = vald_farg,
                                 output_mapp = output_mapp_figur,
                                 skriv_till_diagramfil = spara_figur,
                                 filnamn_diagram = diagramfil)

    gg_list <- c(gg_list, list(gg_obj))
  }
  
  if(diag_alder_fokus == TRUE){
    
    diagram_titel <- paste0("Inrikes flyttnetto (",alder_grupp_fokus ,") i ", reg_txt)
    diagramfil <- paste0("Inrikes flyttnetto_alder_", reg_txt, "_ar_", min(flytt_df$år), "_", max(flytt_df$år), ".png")
    objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = flytt_df %>% 
                                   filter(alder_grupper == alder_grupp_fokus, 
                                          variabel == "Inrikes flyttningsöverskott"), 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "varde",
                                 #skickad_x_grupp = "år",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 #x_axis_storlek = 8,
                                 stodlinjer_avrunda_fem = FALSE,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "",
                                 geom_position_stack = FALSE,
                                 #diagram_facet = FALSE,
                                 legend_vand = FALSE,
                                 dataetiketter = FALSE,
                                 #facet_grp = "kön",
                                 #fokusera_varden = total_list,
                                 manual_color = vald_farg,
                                 output_mapp = output_mapp_figur,
                                 skriv_till_diagramfil = spara_figur,
                                 filnamn_diagram = diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  names(gg_list) <- objektnamn
  
  if(returnera_figur==TRUE){
    return(gg_list)
  }
  
}
