#test = diagram_inrikes_flytt(spara_figur=FALSE,region_vekt = "2021")
diagram_inrikes_flytt <- function(region_vekt = "20", # Val av kommuner
                                     output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                                     #output_mapp_data = NA, # Vart hamnar data om den skall sparas. NA medför att data inte sparas
                                     #filnamn_data = "andel_offentligt.xlsx", # Filnamn för sparad data
                                     vald_farg = diagramfarger("kon"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                     tid = "*", # Avsluta med 9999 för senaste år
                                     spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                     diag_flyttnetto = TRUE, # Skapa diagram för flyttnetto
                                     diag_uppdelat = TRUE, # Skapa diagram för flyttnetto uppdelat
                                     returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                     returnera_data = FALSE # True om användaren vill returnera data från funktionen
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
                                                         tid_koder = tid)
  
  if(returnera_data == TRUE){
    assign("flytt_df", flytt_df, envir = .GlobalEnv)
  }
  
  
  if(diag_flyttnetto == TRUE){
    
    totalvarden_linjebredd <- 0.8      # gör en linjetjocklek på totallinjerna som är 0,2 % av diff (på raden ovan)
    total_list <- list()
    unika_ar <- unique(flytt_df$år)
    vald_regionkod = region_vekt
    unika_reg <- unique(vald_regionkod)
    unika_reg_txt <- hamtaregion_kod_namn(unika_reg)$region %>% skapa_kortnamn_lan()
    
    for (reg in 1:length(unika_reg)) { 
      for (ar in 1:length(unika_ar)){
        arsvarde <- flytt_df %>% 
          filter(år == unika_ar[ar],
                 regionkod == unika_reg[reg]) %>% 
          group_by(år,regionkod) %>% 
            summarize(varde = sum(varde)) %>% 
                        .$varde 
        #dplyr::pull()
        #arsvarde <- sum(arsvarde, na.rm = TRUE)
        total_list <- c(total_list, list(list(geom = "rect", ymin=arsvarde-totalvarden_linjebredd, ymax=arsvarde+totalvarden_linjebredd, xmin=ar-0.45, xmax=ar+0.45, alpha=1, fill="black")))
        
      } # slut for-loop unika_ar
    } # slut for_loop unika_reg        

    reg_txt <- flytt_df$region %>% unique() %>% skapa_kortnamn_lan(T)
    
    diagram_titel <- paste0("Flyttnetto i ", reg_txt)
    diagramfil <- paste0("Flyttnetto_", reg_txt, "_ar_", min(flytt_df$år), "_", max(flytt_df$år), ".png")
    objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = flytt_df, 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "kön",
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
                                 facet_grp = "kön",
                                 fokusera_varden = total_list,
                                 manual_color = vald_farg,
                                 output_mapp = output_mapp_figur,
                                 skriv_till_diagramfil = FALSE,
                                 filnamn_diagram = diagramfil)
    
    dia_med_legend <- gg_obj +
      geom_line(aes(color="line"))+
      scale_color_manual(name = "", values = c("line" = "black"), labels = "Flyttnetto")+
      theme(legend.key = element_rect(fill = "white"),
            legend.box.just = "bottom")
    
    ggsave(paste0(output_mapp_figur, diagramfil), dia_med_legend, width = 8, height = 6, dpi = 300)

    gg_list <- c(gg_list, list(dia_med_legend))
  }
  
  if(diag_uppdelat == TRUE){
    i = 1
    while(i <= length(unique(flytt_df$variabel))){
      vald_variabel <- unique(flytt_df$variabel)[i]
      
      reg_txt <- ar_alla_kommuner_i_ett_lan(flytt_df$regionkod %>% unique(), returnera_text = TRUE)
      if (reg_txt == FALSE) reg_txt <- flytt_df$region %>% unique() %>% skapa_kortnamn_lan(T) %>% paste0(collapse = ", ")
      
      diagram_titel <- paste0(vald_variabel, " i ", reg_txt)
      diagramfil <- paste0(vald_variabel, "_", reg_txt, "_ar_", min(flytt_df$år), "_", max(flytt_df$år), ".png")
      objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
      
      gg_obj <- SkapaStapelDiagram(skickad_df = flytt_df %>% 
                                     filter(variabel == vald_variabel) %>% 
                                     filter(!is.na(varde),
                                            varde != 0), 
                                   skickad_x_var = "år", 
                                   skickad_y_var = "varde",
                                   skickad_x_grupp = "kön",
                                   diagram_titel = diagram_titel,
                                   diagram_capt = diagram_capt,
                                   #x_axis_storlek = 8,
                                   stodlinjer_avrunda_fem = FALSE,
                                   manual_x_axis_text_vjust = 1,
                                   manual_x_axis_text_hjust = 1,
                                   manual_y_axis_title = "",
                                   geom_position_stack = TRUE,
                                   diagram_facet = FALSE,
                                   facet_grp = "region",
                                   facet_x_axis_storlek = 6,
                                   manual_color = vald_farg,
                                   output_mapp = output_mapp_figur,
                                   skriv_till_diagramfil = spara_figur,
                                   filnamn_diagram = diagramfil)
      
      gg_list <- c(gg_list, list(gg_obj))
      
      i=i+1
      
    }
    
    
  }
  names(gg_list) <- objektnamn
  if(returnera_figur==TRUE) return(gg_list)
}
