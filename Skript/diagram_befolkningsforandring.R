#test = diagram_befolkning(spara_figur=FALSE,region_vekt = "2021")
diagram_befolkning <- function(region_vekt = "20", # Val av kommuner
                                  output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                                  #output_mapp_data = NA, # Vart hamnar data om den skall sparas. NA medför att data inte sparas
                                  #filnamn_data = "andel_offentligt.xlsx", # Filnamn för sparad data
                                  vald_farg = diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                  spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                  diag_folkmangd = TRUE, # Skapa diagram för flyttnetto
                                  diag_forandring = TRUE, # Skapa diagram för flyttnetto uppdelat
                                  returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                  returnera_data = TRUE # True om användaren vill returnera data från funktionen
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
  
  #source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_flyttningar_region_alder_kon_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_forandringar_region_period_kon_scb.R")
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna."
  
  gg_list <- list()
  objektnamn <- c()
  
  befolkning_df <- hamta_bef_forandringar_region_alder_kon_scb(region_vekt = region_vekt,
                                                               kon_klartext = NA,
                                                               forandringar_klartext = c("folkmängd", "folkökning"),
                                                               period_klartext = "hela året")
  
  if(returnera_data == TRUE){
    assign("befolkning_df", befolkning_df, envir = .GlobalEnv)
  }
  
  
  if(diag_folkmangd == TRUE){
    
    reg_txt <- befolkning_df$region %>% unique() %>% skapa_kortnamn_lan(T)
    
    diagram_titel <- paste0("Folkmängd i ", reg_txt)
    diagramfil <- paste0("Folkmangd_", reg_txt, "_ar_", min(befolkning_df$år), "_", max(befolkning_df$år), ".png")
    objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = befolkning_df %>% 
                                   filter(förändringar == "folkmängd"), 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "personer",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 #x_axis_storlek = 8,
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "",
                                 geom_position_stack = TRUE,
                                 diagram_facet = FALSE,
                                 legend_vand = TRUE,
                                 manual_color = vald_farg,
                                 output_mapp = output_mapp_figur,
                                 skriv_till_diagramfil = spara_figur,
                                 filnamn_diagram = diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
  }

  
  if(diag_forandring == TRUE){
    # Skapa ny variabel för befolkningsförändring
    befolkning_df_forandring <- befolkning_df %>% 
      filter(förändringar == "folkökning") %>%
      mutate(kategori = ifelse(personer>0,"Ökad befolkning","Minskad befolkning"))
    
    reg_txt <- befolkning_df$region %>% unique() %>% skapa_kortnamn_lan(T)
    
    diagram_titel <- paste0("Befolkningsutveckling i ", reg_txt)
    diagramfil <- paste0("Befolkningsutveckling_", reg_txt, "_ar_", min(befolkning_df$år), "_", max(befolkning_df$år), ".png")
    objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = befolkning_df %>% 
                                   filter(förändringar == "folkökning") %>%
                                   mutate(kategori = ifelse(personer>0,"Ökad befolkning","Minskad befolkning")), 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "personer",
                                 skickad_x_grupp = "kategori",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 #x_axis_storlek = 8,
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "",
                                 geom_position_stack = TRUE,
                                 diagram_facet = FALSE,
                                 legend_vand = TRUE,
                                 manual_color = vald_farg,
                                 output_mapp = output_mapp_figur,
                                 skriv_till_diagramfil = spara_figur,
                                 filnamn_diagram = diagramfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    
  }
  names(gg_list) <- objektnamn
  if(returnera_figur==TRUE)
  return(gg_list)
}
