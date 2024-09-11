
#test = diagram_inflytt(spara_figur=FALSE,tid=c(2018:9999))
diagram_inflytt <- function(region_vekt = hamtaAllaLan(tamedriket = FALSE), # Val av kommuner
                                 cont_klartext = c("Inrikes flyttningsöverskott", "Invandringsöverskott"), 
                                 tid = "9999",# Finns från 2000. Vid fler år än ett summeras värdena över vald tidsperiod
                                 output_mapp_figur= "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/", # Vart hamnar figur om den skall sparas
                                 filnamn_data = "flyttningar_netto.xlsx", # Filnamn för sparad data
                                 vald_farg = diagramfarger("rus_tva_fokus"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                 spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                                 returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                 returnera_data = FALSE # True om användaren vill returnera data från funktionen
){
  
  # ===========================================================================================================
  #
  # Skript som skapar diagram för andelen som arbetar inom offentlig sektor. Funkar med och utan könsuppdelning men enbart för senaste år
  # Går även att använda olika åldersspann
  # ===========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  gg_list <- list() # Skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  
  # Data som sourcas från Region Dalarna
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_flyttningar_region_alder_kon_scb.R")
  options(dplyr.summarise.inform = FALSE)
  
  flytt_df <- hamta_bef_flyttningar_region_alder_kon_scb(region_vekt = region_vekt,
                                                         cont_klartext = cont_klartext,
                                                         kon_klartext = NA,
                                                         tid_koder = tid) %>% 
    ungroup()
  
  # if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
  #   list_data <- c(list_data,list("Totalt" = andel_totalt_utskrift))
  # }
  
  if(returnera_data == TRUE){
    assign("flytt_overskott", flytt_df, envir = .GlobalEnv)
  }
  
  skapa_diagram <- function(data,overskott){
    
    data <- data %>% 
      filter(variabel %in% overskott) %>% 
        mutate(region = skapa_kortnamn_lan(region))

    if(length(unique(data$år))==1){
      diagram_titel <- paste0(overskott," ", data$år)
    } else{
      diagram_titel <- paste0(overskott," ", min(data$år),"-",max(data$år))
    } 
  
    if(overskott == "Inrikes flyttningsöverskott"){
      diagramfilnamn <- "inrikes_flyttningsoverskott.png"
    }else{
      diagramfilnamn <- "utrikes_flyttningsoverskott.png"
    }
    
    diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
    
    gg_obj <- SkapaStapelDiagram(skickad_df = data %>%
                                   filter(variabel %in% overskott) %>% 
                                    mutate(fokus = ifelse(region == "Dalarna",1,0)), 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "varde",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 x_axis_sort_value = TRUE,
                                 x_var_fokus = "fokus",
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "",
                                 manual_color = vald_farg,
                                 output_mapp = output_mapp_figur,
                                 skriv_till_diagramfil = spara_figur,
                                 filnamn_diagram = diagramfilnamn)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list) <- diagramfilnamn %>% str_remove(".png")
    return(gg_list)
  }
  
  
  diag <- map(unique(flytt_df$variabel), ~skapa_diagram(flytt_df, .x)) %>% flatten()
  
  
  # if(!is.na(output_mapp_data) & !is.na(filnamn_data)){
  #   write.xlsx(list_data,paste0(output_mapp_data,filnamn_data))
  # }
  if(returnera_figur == TRUE) return(diag)
  
}
