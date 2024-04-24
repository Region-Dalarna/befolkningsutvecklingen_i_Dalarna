#test = diagram_fodda_doda(spara_figur=FALSE,region_vekt = "2021")
diagram_fodda_doda <- function(region_vekt = "20", # Val av kommuner
                                  output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                                  #output_mapp_data = NA, # Vart hamnar data om den skall sparas. NA medför att data inte sparas
                                  #filnamn_data = "andel_offentligt.xlsx", # Filnamn för sparad data
                                  vald_farg = diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                  spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                  diag_fodda = TRUE, # Skapa diagram för flyttnetto
                                  visa_totalvarden = TRUE, # Visa totalvärden i diagrammet
                                  etiketter_xaxel = 4, # Intervall för etiketter på x-axeln
                                  tid = "*",
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
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_fodda_moderns_alder_region_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_doda_alder_kon_region_scb.R")
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna."
  
  gg_list <- list()
  objektnamn <- c()
  
  fodda_df <- hamta_fodda_moderns_alder_region_scb(region_vekt = region_vekt,
                                                   alder_moder = "tot",
                                                   tid_koder = tid) %>% 
    select(-'moderns ålder')
  
  doda_df <- hamta_doda_alder_kon_region_scb(region_vekt = region_vekt,
                                             alder = "*",
                                             tid_koder = tid) %>% 
    filter(ålder == "totalt ålder") %>% 
      select(-'ålder')
  
  df <- fodda_df %>% 
    left_join(doda_df, by = c("regionkod","region", "år")) %>% 
      mutate(netto = födda-Döda)%>% 
        pivot_longer(cols = c("födda", "Döda", "netto"),
                     names_to = "variabel",
                     values_to = "varde")
  
  if(returnera_data == TRUE){
    assign("fodda_doda_df", df, envir = .GlobalEnv)
  }
  

  if(diag_fodda == TRUE){
    
    if (visa_totalvarden){
      
      #diff <- max(chart_df$Inrikes_flyttnetto) - min(chart_df$Inrikes_flyttnetto) # ta reda på skillnaden mellan det högsta och lägsta värdet i datasetet
      totalvarden_linjebredd <- 0.5      # gör en linjetjocklek på totallinjerna som är 0,2 % av diff (på raden ovan)
      total_list <- list()
      unika_ar <- unique(df$år)
      vald_regionkod = "2021"
      unika_reg <- unique(vald_regionkod)
      unika_reg_txt <- hamtaregion_kod_namn(unika_reg)$region %>% skapa_kortnamn_lan()
      
      for (reg in 1:length(unika_reg)) { 
        for (ar in 1:length(unika_ar)){
          arsvarde <- df %>% 
            filter(år == unika_ar[ar],
                   regionkod == unika_reg[reg]) %>% 
            filter(variabel == "netto") %>% .$varde 
            #dplyr::pull()
          #arsvarde <- sum(arsvarde, na.rm = TRUE)
          total_list <- c(total_list, list(list(geom = "rect", ymin=arsvarde-totalvarden_linjebredd, ymax=arsvarde+totalvarden_linjebredd, xmin=ar-0.45, xmax=ar+0.45, alpha=1, fill="black")))

        } # slut for-loop unika_ar
      } # slut for_loop unika_reg        
    } else total_list <- NA # slut if-sats visa_totalvärden
    
    reg_txt <- fodda_df$region %>% unique() %>% skapa_kortnamn_lan(T)
    
    diagram_titel <- paste0("Födelsenetto i ", reg_txt)
    diagramfil <- paste0("Födelsenetto_", reg_txt, "_ar_", min(df$år), "_", max(df$år), ".png")
    objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = df %>%
                                   filter(variabel != "netto") %>% 
                                    mutate(varde = ifelse(variabel=="Döda",varde*-1,varde),
                                           variabel = stringr::str_to_title(variabel)), 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "variabel",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 #x_axis_storlek = 8,
                                 x_axis_visa_var_xe_etikett = etiketter_xaxel,
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "",
                                 geom_position_stack = TRUE,
                                 diagram_facet = FALSE,
                                 facet_grp = "variabel",
                                 fokusera_varden = total_list,
                                 legend_vand = TRUE,
                                 manual_color = vald_farg,
                                 output_mapp = output_mapp_figur,
                                 skriv_till_diagramfil = FALSE,
                                 filnamn_diagram = diagramfil)
    
    dia_med_legend <- gg_obj +
      geom_line(aes(color="line"))+
      scale_color_manual(name = "", values = c("line" = "black"), labels = "Födelsenetto")+
      theme(legend.key = element_rect(fill = "white"),
            legend.box.just = "bottom")
    
    if(spara_figur == TRUE){
      ggsave(paste0(output_mapp_figur, diagramfil), dia_med_legend, width = 8, height = 6, dpi = 300)
    }
    #ggsave(paste0(output_mapp_figur, diagramfil), dia_med_legend, width = 8, height = 6, dpi = 300)
    
    gg_list <- c(gg_list, list(dia_med_legend))
  }
  
  # if(diag_uppdelat == TRUE){
  #   i = 1
  #   while(i <= length(unique(flytt_df$variabel))){
  #     vald_variabel <- unique(flytt_df$variabel)[i]
  #     
  #     reg_txt <- ar_alla_kommuner_i_ett_lan(flytt_df$regionkod %>% unique(), returnera_text = TRUE)
  #     if (reg_txt == FALSE) reg_txt <- flytt_df$region %>% unique() %>% skapa_kortnamn_lan(T) %>% paste0(collapse = ", ")
  #     
  #     diagram_titel <- paste0(vald_variabel, " i ", reg_txt)
  #     diagramfil <- paste0(vald_variabel, "_", reg_txt, "_ar_", min(flytt_df$år), "_", max(flytt_df$år), ".png")
  #     objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
  #     
  #     gg_obj <- SkapaStapelDiagram(skickad_df = flytt_df %>% 
  #                                    filter(variabel == vald_variabel) %>% 
  #                                    filter(!is.na(varde),
  #                                           varde != 0), 
  #                                  skickad_x_var = "år", 
  #                                  skickad_y_var = "varde",
  #                                  skickad_x_grupp = "kön",
  #                                  diagram_titel = diagram_titel,
  #                                  diagram_capt = diagram_capt,
  #                                  #x_axis_storlek = 8,
  #                                  stodlinjer_avrunda_fem = TRUE,
  #                                  manual_x_axis_text_vjust = 1,
  #                                  manual_x_axis_text_hjust = 1,
  #                                  manual_y_axis_title = "",
  #                                  geom_position_stack = TRUE,
  #                                  diagram_facet = FALSE,
  #                                  facet_grp = "region",
  #                                  facet_x_axis_storlek = 6,
  #                                  manual_color = vald_farg,
  #                                  output_mapp = output_mapp_figur,
  #                                  skriv_till_diagramfil = spara_figur,
  #                                  filnamn_diagram = diagramfil)
  #     
  #     gg_list <- c(gg_list, list(gg_obj))
  #     
  #     i=i+1
  #     
  #   }
  #   names(gg_list) <- objektnamn
  #   
  # }
  names(gg_list) <- objektnamn
  
  if(returnera_figur == TRUE){
  return(gg_list)
  }
}
