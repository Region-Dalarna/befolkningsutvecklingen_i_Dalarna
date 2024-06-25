diagram_inr_utr_flytt <- function(output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                                  vald_farg = diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                                  inflyttningsl_klartext = "*",			 #  Finns: " Stockholms län (Inflyttningslän)", " Uppsala län (Inflyttningslän)", " Södermanlands län (Inflyttningslän)", " Östergötlands län (Inflyttningslän)", " Jönköpings län (Inflyttningslän)", " Kronobergs län (Inflyttningslän)", " Kalmar län (Inflyttningslän)", " Gotlands län (Inflyttningslän)", " Blekinge län (Inflyttningslän)", " Skåne län (Inflyttningslän)", " Hallands län (Inflyttningslän)", " Västra Götalands län (Inflyttningslän)", " Värmlands län (Inflyttningslän)", " Örebro län (Inflyttningslän)", " Västmanlands län (Inflyttningslän)", " Dalarnas län (Inflyttningslän)", " Gävleborgs län (Inflyttningslän)", " Västernorrlands län (Inflyttningslän)", " Jämtlands län (Inflyttningslän)", " Västerbottens län (Inflyttningslän)", " Norrbottens län (Inflyttningslän)"
                                  utflyttningsl_klartext = " Dalarnas län (Utflyttningslän)",			 #  Finns: " Stockholms län (Utflyttningslän)", " Uppsala län (Utflyttningslän)", " Södermanlands län (Utflyttningslän)", " Östergötlands län (Utflyttningslän)", " Jönköpings län (Utflyttningslän)", " Kronobergs län (Utflyttningslän)", " Kalmar län (Utflyttningslän)", " Gotlands län (Utflyttningslän)", " Blekinge län (Utflyttningslän)", " Skåne län (Utflyttningslän)", " Hallands län (Utflyttningslän)", " Västra Götalands län (Utflyttningslän)", " Värmlands län (Utflyttningslän)", " Örebro län (Utflyttningslän)", " Västmanlands län (Utflyttningslän)", " Dalarnas län (Utflyttningslän)", " Gävleborgs län (Utflyttningslän)", " Västernorrlands län (Utflyttningslän)", " Jämtlands län (Utflyttningslän)", " Västerbottens län (Utflyttningslän)", " Norrbottens län (Utflyttningslän)"
                                  tid = "*", # Avsluta med 9999 för senaste år
                                  spara_figur = TRUE, # Sparar figuren till output_mapp_figur
                                  diag_senaste_ar = TRUE, # Skapar ett diagram för antingen in eller utflytt från ett län till alla andra valda län. Enbart senaste valda år
                                  diag_flera_ar = FALSE, # Skapar ett diagram per vald destination (in- eller utflyttningslan). Enbart intressant om flera år väljs
                                  diag_facet = FALSE, # Sätts till TRUE om man istället vill ha diag_flera_ar som ett facet-diagram
                                  returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                                  returnera_data = FALSE # True om användaren vill returnera data från funktionen
){
  
  # ===========================================================================================================
  # Diagram för inrikes och utrikes flyttar. Vid flera regioner går det att välja mellan enskilda diagram eller facet
  # Skapad: 2024-04-24
  # Förbättringsmöjligheter: Går för tillfället inte att summera  flera regioner
  # ===========================================================================================================
  
  inflyttningsl_klartext = " Dalarnas län (Inflyttningslän)"
  utflyttningsl_klartext = "*"
  if(length(inflyttningsl_klartext) > 1 && length(utflyttningsl_klartext)>1){
    stop("Max 1 län får väljas för antingen inflyttningsl_klartext eller utflyttningsl_klartext")
  }
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_inflyttningslan_utflyttningslan_kon_tid_scb.R")
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna.\nDiagramförklaring: Diagrammet avser flyttningar och inte personer.\nUnder ett år kan en person flytta fler än en gång."
  
  gg_list <- list()
  objektnamn <- c()
  
  inflytt_utflytt_df = hamta_inflyttningslan_utflyttningslan_kon_tid_scb(inflyttningsl_klartext = inflyttningsl_klartext,
                                                                          utflyttningsl_klartext = utflyttningsl_klartext ,
                                                                          kon_klartext = NA,
                                                                          cont_klartext = "*",
                                                                          tid_koder = tid,
                                                                          returnera_df = TRUE)
  
# Byter dåliga namn på regioner till bättre
  inflytt_utflytt_df <- inflytt_utflytt_df %>% 
    mutate(Inflyttningslän = case_when(
      Inflyttningslän == " Stockholms län (Inflyttningslän)" ~ "Stockholm",
      Inflyttningslän == " Uppsala län (Inflyttningslän)" ~ "Uppsala",
      Inflyttningslän == " Södermanlands län (Inflyttningslän)" ~ "Södermanland",
      Inflyttningslän == " Östergötlands län (Inflyttningslän)" ~ "Östergötland",
      Inflyttningslän == " Jönköpings län (Inflyttningslän)" ~ "Jönköping",
      Inflyttningslän == " Kronobergs län (Inflyttningslän)" ~ "Kronoberg",
      Inflyttningslän == " Kalmar län (Inflyttningslän)" ~ "Kalmar",
      Inflyttningslän == " Gotlands län (Inflyttningslän)" ~ "Gotland",
      Inflyttningslän == " Blekinge län (Inflyttningslän)" ~ "Blekinge",
      Inflyttningslän == " Skåne län (Inflyttningslän)" ~ "Skåne",
      Inflyttningslän == " Hallands län (Inflyttningslän)" ~ "Halland",
      Inflyttningslän == " Västra Götalands län (Inflyttningslän)" ~ "Västra Götaland",
      Inflyttningslän == " Värmlands län (Inflyttningslän)" ~ "Värmland",
      Inflyttningslän == " Örebro län (Inflyttningslän)" ~ "Örebro",
      Inflyttningslän == " Västmanlands län (Inflyttningslän)" ~ "Västmanland",
      Inflyttningslän == " Dalarnas län (Inflyttningslän)" ~ "Dalarna",
      Inflyttningslän == " Gävleborgs län (Inflyttningslän)" ~ "Gävleborg",
      Inflyttningslän == " Västernorrlands län (Inflyttningslän)" ~ "Västernorrland",
      Inflyttningslän == " Jämtlands län (Inflyttningslän)" ~ "Jämtland",
      Inflyttningslän == " Västerbottens län (Inflyttningslän)" ~ "Västerbotten",
      Inflyttningslän == " Norrbottens län (Inflyttningslän)" ~ "Norrbotten"
    )) %>% 
    mutate(Utflyttningslän = case_when(
      Utflyttningslän == " Stockholms län (Utflyttningslän)" ~ "Stockholm",
      Utflyttningslän == " Uppsala län (Utflyttningslän)" ~ "Uppsala",
      Utflyttningslän == " Södermanlands län (Utflyttningslän)" ~ "Södermanland",
      Utflyttningslän == " Östergötlands län (Utflyttningslän)" ~ "Östergötland",
      Utflyttningslän == " Jönköpings län (Utflyttningslän)" ~ "Jönköping",
      Utflyttningslän == " Kronobergs län (Utflyttningslän)" ~ "Kronoberg",
      Utflyttningslän == " Kalmar län (Utflyttningslän)" ~ "Kalmar",
      Utflyttningslän == " Gotlands län (Utflyttningslän)" ~ "Gotland",
      Utflyttningslän == " Blekinge län (Utflyttningslän)" ~ "Blekinge",
      Utflyttningslän == " Skåne län (Utflyttningslän)" ~ "Skåne",
      Utflyttningslän == " Hallands län (Utflyttningslän)" ~ "Halland",
      Utflyttningslän == " Västra Götalands län (Utflyttningslän)" ~ "Västra Götaland",
      Utflyttningslän == " Värmlands län (Utflyttningslän)" ~ "Värmland",
      Utflyttningslän == " Örebro län (Utflyttningslän)" ~ "Örebro",
      Utflyttningslän == " Västmanlands län (Utflyttningslän)" ~ "Västmanland",
      Utflyttningslän == " Dalarnas län (Utflyttningslän)" ~ "Dalarna",
      Utflyttningslän == " Gävleborgs län (Utflyttningslän)" ~ "Gävleborg",
      Utflyttningslän == " Västernorrlands län (Utflyttningslän)" ~ "Västernorrland",
      Utflyttningslän == " Jämtlands län (Utflyttningslän)" ~ "Jämtland",
      Utflyttningslän == " Västerbottens län (Utflyttningslän)" ~ "Västerbotten",
      Utflyttningslän == " Norrbottens län (Utflyttningslän)" ~ "Norrbotten"
    )) %>% 
    rename("Antal_flyttar" = `Inrikes omflyttning mellan län `)
  
  if(returnera_data == TRUE){
    assign("inflytt_utflytt_lan_df", inflytt_utflytt_df, envir = .GlobalEnv)
  }
  
  if(diag_senaste_ar == TRUE){
    
    if(length(unique(inflytt_utflytt_df$Inflyttningslän)) == 1){
      diagram_titel<- paste0("Antal inflyttar till ",unique(inflytt_utflytt_df$Inflyttningslän)," år ",max(inflytt_utflytt_df$år))
      diagramfil <- paste0("Inflytt_till_",unique(inflytt_utflytt_df$Inflyttningslän),".png")
      objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
      inflytt_utflytt_df <- inflytt_utflytt_df %>% 
        filter(Utflyttningslän != unique(inflytt_utflytt_df$Inflyttningslän))
    }else{
      diagram_titel<- paste0("Antal utflyttar från ",unique(inflytt_utflytt_df$Utflyttningslän)," år ",max(inflytt_utflytt_df$år))
      diagramfil <- paste0("Utflytt_fran_",unique(inflytt_utflytt_df$Utflyttningslän),".png")
      objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
      inflytt_utflytt_df <- inflytt_utflytt_df %>% 
        filter(Inflyttningslän != unique(inflytt_utflytt_df$Utflyttningslän))
    }
    
    gg_obj <- SkapaStapelDiagram(skickad_df = inflytt_utflytt_df %>% 
                                   filter(år == max(år)), 
                                 skickad_x_var = ifelse(length(unique(inflytt_utflytt_df$Inflyttningslän)) == 1,"Utflyttningslän","Inflyttningslän"), 
                                 skickad_y_var = "Antal_flyttar",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 #x_axis_storlek = 8,
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "Antal flyttar",
                                 x_axis_sort_value = TRUE,
                                 #geom_position_stack = TRUE,
                                 #diagram_facet = length(unique(df$region)) > 1,
                                 #x_axis_visa_var_xe_etikett = ifelse(diag_facet==TRUE,2,NA),
                                 #facet_grp = "region",
                                 #facet_scale = "free",
                                 #facet_legend_bottom = TRUE,
                                 #legend_vand = TRUE,
                                 manual_color = vald_farg,
                                 output_mapp = "output_mapp_figur",
                                 skriv_till_diagramfil = FALSE,
                                 filnamn_diagram = "diagramfil")
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list) <- objektnamn
    
  }
  
  skapa_diagram <- function(df, vald_region){
    
    df <- df %>% filter(regionkod %in% vald_region)
    
    if(diag_flyttnetto == TRUE){
      
      if(diag_facet == FALSE){
        diff <- max(df$varde) - min(df$varde) # ta reda på skillnaden mellan det högsta och lägsta värdet i datasetet
        totalvarden_linjebredd <- 0.002*diff      # gör en linjetjocklek på totallinjerna som är 0,2 % av diff (på raden ovan)
        total_list <- list()
        unika_ar <- unique(df$år)
        vald_regionkod = vald_region
        unika_reg <- unique(vald_regionkod)
        unika_reg_txt <- hamtaregion_kod_namn(unika_reg)$region %>% skapa_kortnamn_lan()
        
        for (reg in 1:length(unika_reg)) { 
          for (ar in 1:length(unika_ar)){
            arsvarde <- df %>% 
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
      } else total_list <- NA
      
      reg_txt <- (df$region %>% unique() %>% skapa_kortnamn_lan(T))[1]
      
      if(length(unique(df$region)) > 1){
        reg_txt <- paste0(reg_txt,"_facet")
        diagram_titel <- paste0("Flyttnetto")
      }else{
        diagram_titel <- paste0("Flyttnetto i ", reg_txt)
      }
      
      #diagram_titel <- paste0("Flyttnetto i ", reg_txt)
      diagramfil <- paste0("Flyttnetto_", reg_txt, "_ar_", min(df$år), "_", max(df$år), ".png")
      #objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
      
      gg_obj <- SkapaStapelDiagram(skickad_df = df, 
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
                                   diagram_facet = length(unique(df$region)) > 1,
                                   x_axis_visa_var_xe_etikett = ifelse(diag_facet==TRUE,2,NA),
                                   facet_grp = "region",
                                   facet_scale = "free",
                                   facet_legend_bottom = TRUE,
                                   legend_vand = TRUE,
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
      names(gg_list) <- diagramfil %>% str_remove(".png")
    }
    
    if(diag_uppdelat == TRUE){
      
      skapa_diagram_uppdelat <- function(df,vald_variabel){
        
        #reg_txt <- ar_alla_kommuner_i_ett_lan(df$regionkod %>% unique(), returnera_text = TRUE)
        #if (reg_txt == FALSE) reg_txt <- df$region %>% unique() %>% skapa_kortnamn_lan(T) %>% paste0(collapse = ", ")
        
        reg_txt <- (df$region %>% unique() %>% skapa_kortnamn_lan(T))[1]
        
        if(length(unique(df$region)) > 1){
          reg_txt <- paste0(reg_txt,"_facet")
          diagram_titel <- paste0(vald_variabel)
        }else{
          diagram_titel <- paste0(vald_variabel," ", reg_txt)
        }
        
        diagramfil <- paste0(vald_variabel, "_", reg_txt, "_ar_", min(df$år), "_", max(df$år), ".png")
        objektnamn_uppdelat <- c(objektnamn_uppdelat,diagramfil %>% str_remove(".png"))
        
        gg_obj <- SkapaStapelDiagram(skickad_df = df %>% 
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
                                     diagram_facet = length(unique(df$region)) > 1,
                                     x_axis_visa_var_xe_etikett = ifelse(diag_facet==TRUE,2,NA),
                                     facet_grp = "region",
                                     facet_scale = "free",
                                     facet_legend_bottom = TRUE,
                                     facet_x_axis_storlek = 6,
                                     manual_color = vald_farg,
                                     output_mapp = output_mapp_figur,
                                     skriv_till_diagramfil = spara_figur,
                                     filnamn_diagram = diagramfil)
        
        
        gg_list_uppdelat <- c(gg_list_uppdelat, list(gg_obj))
        names(gg_list_uppdelat) <- objektnamn_uppdelat
        return(gg_list_uppdelat)
        
      }
      
      diag_uppdelning <- map(unique(df$variabel), ~ skapa_diagram_uppdelat(df , .x)) %>% flatten
      gg_list <- c(gg_list, diag_uppdelning)
    }
    return(gg_list)
  }
  
  
  if (diag_facet) {
    diag <- skapa_diagram(flytt_df,region_vekt)
    
  } else {
    diag <- map(region_vekt, ~ skapa_diagram(flytt_df, .x)) %>% flatten()
    
  }
  
  if(returnera_figur==TRUE) return(diag)
}