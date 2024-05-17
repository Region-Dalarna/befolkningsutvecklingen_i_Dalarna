#test = kartor_befolkning()
kartor_befolkning = function(karta_kommun = TRUE, # Karta över Dalarnas kommuner
                             karta_lan = TRUE, # Karta över Sveriges län
                             returnera_data = TRUE, # Skall data returneras
                             startar = "1968") { # Startår för data. Finns som tidigast 1968
  

  # Paket som används
  if (!require("pacman")) install.packages("pacman")
  p_load(sf,
         here,
         openxlsx,
         tidyverse,
         mapview,
         leafpop,
         plotly,
         tidytext,
         ggiraph,
         showtext)
  
  
  ##### Hämta data till RUS-indikatorerna #######
  
  ########################
  #### Ladda in paket ####
  ########################
  
  #font_add_google("Poppins","poppins")
  gg_list <- list()
  objekt_namn <- c()
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_GIS.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
  
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas, bearbetning av Samhällsanalys, Region Dalarna."
  
  if(karta_kommun == TRUE){
    # Karta över dalarnas kommuner
    region_vekt <- "20"
    
    befolkning_df <- hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = hamtakommuner(region_vekt,tamedlan = FALSE,tamedriket = FALSE),
                                                          tid_koder = c(startar,"9999"),
                                                          cont_klartext = c("folkmängd")) %>% 
      group_by(regionkod,region,år) %>%
        summarize(Folkmängd = sum(Folkmängd))
    
    # Calculate change between 2000 and latest year
    befolkning_df <- befolkning_df %>% 
      mutate(Folkmängd = as.numeric(Folkmängd),
             förändring = round(((Folkmängd - lag(Folkmängd))/Folkmängd)*100,1),
             förändring = ifelse(is.na(förändring),Folkmängd, förändring),
             förändring = ifelse(år == "2000",0, förändring)) %>% 
        filter(år == max(år)) %>% 
          mutate(regionkod = as.integer(regionkod))
    
    if(returnera_data == TRUE){
      assign("befolkning_forandring_kommun", befolkning_df, envir = .GlobalEnv)
    }
    
    mapp_scbadmgranser <- "G:/Samhällsanalys/GIS/grundkartor/KommunerDalarna/"
    
    filnamn_kommuner <- "Dalarna_kommuner.shp"
    
    sokvag_kommuner_sv <- paste0(mapp_scbadmgranser, filnamn_kommuner)
    
    kommuner_sv <- st_read(sokvag_kommuner_sv,options = "ENCODING=WINDOWS-1252")
    
    # lägg ihop kommunpolygonerna med brp per kommun-statistiken
    befutveckling_karta <- left_join(kommuner_sv, befolkning_df, by = c("KOMMUNKOD" = "regionkod"))
    
    
    karta_ej_girafe <- ggplot(befutveckling_karta,
                    aes(fill = förändring,
                        label = KOMMUNNAMN,tooltip=paste("Kommun:",KOMMUNNAMN,"<br>","Befolkningsutveckling",förändring,"%","<br>","Folkmängd" ,år," : ",Folkmängd))) +
      geom_sf_interactive() +
      scale_fill_gradient(low = "#93cec1", high = "#0e5a4c") +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_rect(fill = "white", colour = 'white'),
            plot.caption = element_text(hjust = 0),
            legend.position = "right") +
      labs(title = paste0("Befolkningsutveckling i Dalarnas kommuner"),
           subtitle = paste0("Mellan ",startar," och ",max(befolkning_df$år)),
           yaxis = "",
           fill = "Förändring (%)",
           caption = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna") 
    
    karta_kommun <- girafe(ggobj=karta_ej_girafe)
    
    karta_kommun <-girafe_options(karta_kommun,opts_tooltip(opacity = 1,css = "padding:4pt;font-size:0.7rem;color:black;background-color:rgba(255,255,255,0.7);border-radius:5pt;font-weight: 600",
                                              offx = 50))
    
    objekt_namn <- c(objekt_namn, "karta_kommun")
    gg_list <- c(gg_list, list(karta_kommun))
  
  }
  
  if(karta_lan == TRUE){
    lan_df <- hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = hamtaAllaLan(tamedriket = FALSE),
                                                   tid_koder = c(startar,"9999"),
                                                   cont_klartext = c("folkmängd")) %>% 
      group_by(regionkod,region,år) %>%
      summarize(Folkmängd = sum(Folkmängd))
    
    # Calculate change between 2000 and latest year
    lan_df <- lan_df %>% 
      mutate(Folkmängd = as.numeric(Folkmängd),
             förändring = round(((Folkmängd - lag(Folkmängd))/Folkmängd)*100,1),
             förändring = ifelse(is.na(förändring),Folkmängd, förändring),
             förändring = ifelse(år == "2000",0, förändring)) %>% 
      filter(år == max(år)) %>% 
      mutate(regionkod = as.character(regionkod),
             region = skapa_kortnamn_lan(region))
    
    if(returnera_data == TRUE){
      assign("befolkning_forandring_lan", lan_df, envir = .GlobalEnv)
    }
    
    filnamn_lan <- "Länsgränser_SCB_07.shp"
    
    mapp_scbadmgranser <- "G:/Samhällsanalys/GIS/grundkartor/Adm gränser med kustgränser/"
    
    sokvag_lan_sv <- paste0(mapp_scbadmgranser, filnamn_lan)
    
    lan_sv <- st_read(sokvag_lan_sv,options = "ENCODING=WINDOWS-1252")
    
    # lägg ihop kommunpolygonerna med brp per kommun-statistiken
    befutveckling_karta_lan <- left_join(lan_sv, lan_df, by = c("LNKOD" = "regionkod"))
    
    karta_lan_ej_girafe <- ggplot(befutveckling_karta_lan,
                    aes(fill = förändring,
                        label = LNNAMN,tooltip=paste("Län:",region,"<br>","Befolkningsutveckling",förändring,"%"))) +
      geom_sf_interactive() +
      scale_fill_gradient(low = "#93cec1", high = "#0e5a4c") +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_rect(fill = "white", colour = 'white'),
            plot.caption = element_text(hjust = 0),
            legend.position = "right") +
      labs(title = paste0("Befolkningsutveckling i Sveriges län"),
           subtitle = paste0("Mellan ",startar," och ",max(lan_df$år)),
           yaxis = "",
           fill = "Förändring (%)",
           caption = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna") 
    
    
    karta_lan <- girafe(ggobj=karta_lan_ej_girafe)
    
    karta_lan <-girafe_options(karta_lan,opts_tooltip(opacity = 1,css = "padding:4pt;font-size:0.7rem;color:black;background-color:rgba(255,255,255,0.7);border-radius:5pt;font-weight: 600",
                                              offx = 25,
                                              use_cursor_pos = TRUE))
    
    gg_list <- c(gg_list, list(karta_lan))
    objekt_namn <- c(objekt_namn, "karta_lan")
    
  }
  names(gg_list) <- objekt_namn

  return(gg_list)
}


