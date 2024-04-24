#test = diagram_befolkning(spara_figur=FALSE,region_vekt = hamtakommuner("22",tamedriket=FALSE),diag_facet = FALSE)
diagram_befolkning <- function(region_vekt = "20", # Val av kommuner
                               output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                               #output_mapp_data = NA, # Vart hamnar data om den skall sparas. NA medför att data inte sparas
                               #filnamn_data = "andel_offentligt.xlsx", # Filnamn för sparad data
                               vald_farg = diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                               spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                               diag_folkmangd = TRUE, # Skapa diagram för flyttnetto
                               diag_facet = FALSE,
                               etiketter_xaxel = 4, # Intervall för etiketter på x-axeln (ej för Facet där 12 används automatiskt)
                               kon_klartext = NA, # Alternativet är c("kvinnor","män") där det görs en uppdelning på kön
                               diag_forandring = TRUE, # Skapa diagram för flyttnetto uppdelat
                               returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                               returnera_data = TRUE # True om användaren vill returnera data från funktionen
                               #diag_totalt = TRUE, # Skriver ut diagram för kön totalt
                               #diag_kon = TRUE # Skriver ut diagram uppdelat på kön
){
  
  # ===========================================================================================================
  # Diagram för befolkningsutveckling och folkökning. Vid flera regioner går det att välja mellan enskilda diagram eller facet
  # Uppdelning på kön är möjlig
  # Skapad av 2024-04-23
  # Förbättringsmöjligheter: Går för tillfället inte att skapa grupper vid flera regioner
  # ===========================================================================================================
  
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna."
  
  gg_list <- list()
  objektnamn <- c()
  
  befolkning_df <- hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = region_vekt,
                                                        kon_klartext = kon_klartext,
                                                        cont_klartext = c("folkmängd", "folkökning")) %>% 
    rename(variabel = vardekategori)
  
  if(returnera_data == TRUE){
    assign("befolkning_df", befolkning_df, envir = .GlobalEnv)
  }
  
  skapa_diagram <- function(bef, vald_region){
    if(diag_folkmangd == TRUE){
      
      ut_df <- bef %>% 
        filter(regionkod %in% vald_region)
      
      reg_txt <- (ut_df$region %>% unique() %>% skapa_kortnamn_lan(T))[1]
      
      if(length(unique(ut_df$region)) > 1){
        reg_txt <- paste0(reg_txt,"_facet")
      }
      
      diagram_titel <- paste0("Folkmängd i ", reg_txt)
      diagramfil <- paste0("Folkmangd_", reg_txt, "_ar_", min(ut_df$år), "_", max(ut_df$år), ".png")
      objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
      
      if(length(kon_klartext)>1){
        farg = diagramfarger("kon")} else{
          farg = vald_farg[1]
        }
      
      gg_obj <- SkapaStapelDiagram(skickad_df = ut_df %>% 
                                     filter(variabel == "Folkmängd"), 
                                   skickad_x_var = "år", 
                                   skickad_y_var = "varde",
                                   skickad_x_grupp = ifelse(length(kon_klartext)==1,NA,"kön"),
                                   diagram_titel = diagram_titel,
                                   diagram_capt = diagram_capt,
                                   #x_axis_storlek = 8,
                                   x_axis_visa_var_xe_etikett = ifelse(length(unique(ut_df$region)) > 1,12,etiketter_xaxel),
                                   stodlinjer_avrunda_fem = TRUE,
                                   manual_x_axis_text_vjust = 1,
                                   manual_x_axis_text_hjust = 1,
                                   manual_y_axis_title = "",
                                   geom_position_stack = TRUE,
                                   diagram_facet = length(unique(ut_df$region)) > 1,
                                   facet_grp = "region",
                                   facet_scale = "free",
                                   facet_legend_bottom = TRUE,
                                   legend_vand = TRUE,
                                   manual_color =farg ,
                                   output_mapp = output_mapp_figur,
                                   skriv_till_diagramfil = spara_figur,
                                   filnamn_diagram = diagramfil)
      
      gg_list <- c(gg_list, list(gg_obj))
    }
    
    
    if(diag_forandring == TRUE){
      # # Skapa ny variabel för befolkningsförändring
      # befolkning_df_forandring <- befolkning_df %>% 
      #   filter(förändringar == "folkökning") %>%
      #   mutate(kategori = ifelse(personer>0,"Ökad befolkning","Minskad befolkning"))
      ut_df <- bef %>% 
        filter(regionkod %in% vald_region)
      
      reg_txt <- (ut_df$region %>% unique() %>% skapa_kortnamn_lan(T))[1]
      
      if(length(unique(ut_df$region)) > 1){
        reg_txt <- paste0(reg_txt,"_facet")
      }
      
      diagram_titel <- paste0("Befolkningsutveckling i ", reg_txt)
      diagramfil <- paste0("Befolkningsutveckling_", reg_txt, "_ar_", min(ut_df$år), "_", max(ut_df$år), ".png")
      objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
      
      if(length(kon_klartext)>1) vald_farg = diagramfarger("kon")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = ut_df %>% 
                                     filter(variabel == "Folkökning",år>min(år)) %>%
                                     mutate(kategori = ifelse(varde>=0,"Ökad befolkning","Minskad befolkning")), 
                                   skickad_x_var = "år", 
                                   skickad_y_var = "varde",
                                   skickad_x_grupp = ifelse(length(kon_klartext)==1,"kategori","kön"),
                                   diagram_titel = diagram_titel,
                                   diagram_capt = diagram_capt,
                                   #x_axis_storlek = 8,
                                   x_axis_visa_var_xe_etikett = ifelse(length(unique(ut_df$region)) > 1,12,etiketter_xaxel),
                                   stodlinjer_avrunda_fem = TRUE,
                                   manual_x_axis_text_vjust = 1,
                                   manual_x_axis_text_hjust = 1,
                                   manual_y_axis_title = "",
                                   geom_position_stack = TRUE,
                                   diagram_facet = length(unique(ut_df$region)) > 1,
                                   facet_grp = "region",
                                   facet_scale = "free",
                                   facet_legend_bottom = TRUE,
                                   legend_vand = TRUE,
                                   manual_color = vald_farg,
                                   output_mapp = output_mapp_figur,
                                   skriv_till_diagramfil = spara_figur,
                                   filnamn_diagram = diagramfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      
    }
    names(gg_list) <- objektnamn
    return(gg_list)
  }
  
  if (diag_facet) {
    diag <- skapa_diagram(befolkning_df,region_vekt)
    
  } else {
    diag <- map(region_vekt, ~ skapa_diagram(befolkning_df, .x)) %>% flatten()
    
  }
  
  if(returnera_figur==TRUE)
    return(diag)
}
