#test = diagram_befprognos(spara_figur=FALSE,region_vekt = hamtaAllaLan(tamedriket = FALSE),diag_alla = TRUE)
diagram_befprognos <- function(region_vekt = "20", # Val av kommun/län att fokusera på. Fungerar inte med riket 
                               region_fokus = "20",
                               output_mapp_figur= "G:/skript/jon/Figurer/", # Vart hamnar figur om den skall sparas
                               vald_farg = diagramfarger("rus_sex"), # Vilken färgvektor vill man ha. Blir alltid "kon" när man väljer det diagrammet
                               diag_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning av Samhällsanalys, Region Dalarna\nDiagramförklaring:I prognoserna tas ingen hänsyn till planerat bostadsbyggande,\netableringar av företag eller andra framtida mål och förutsättningar.",
                               prognos_ar = "2040", # Vilket år skall prognosen fokusera på
                               spara_figur = FALSE, # Sparar figuren till output_mapp_figur
                               diag_aldergrupp = TRUE, # Skapa diagram för alla valda år i valt län
                               diag_jmf_region = TRUE, # Skapar ett diagram där total befolkningsförändring jämförs mellan valda regioner (region_vekt)
                               engelsk_titel = NA, # Går att ändra om man vill ha en engelsk titel (funkar för tillfället bara för det tredje diagrammet (diag_jmf_region))
                               diag_facet = FALSE, # Skapa ett facetdiagram där alla valda regioner visas
                               diag_alla = TRUE, # Ett diagram skapas för alla regioner. Om FALSE skapas ett diagram för region_fokus
                               jmf_procent = FALSE, # Skapa diagram för förändring i procent
                               avrunda_fem = TRUE, # Avrunda till närmaste fem på y-axeln
                               returnera_figur = TRUE, # Om man vill att figuren skall returneras från funktionen
                               returnera_data = FALSE){ # True om användaren vill returnera data från funktionen
  
  
  # ===========================================================================================================
  #
  # Skript som skapar diagram för befolkningsprognos. 
  # Tre diagram:
  # 1. Befolkningsförändring i valda regioner
  # 2. Befolkningsförändring i valda regioner för olika åldersgrupper
  # 3. Befolkningsförändring i valda regioner för olika åldersgrupper (facet)
  # Justering 20250109: SCB har ändrat variabelnamn från Folkmängd till Antal. Jag lägger till en mutate för att ändra tillbaka. /Jon 20250109
  # ===========================================================================================================
  
  if("00"%in%region_vekt){
    stop("---- Skriptet fungerar inte med riket ----")
  }
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bef_folkmangd_alder_kon_ar_scb.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_befprogn_region_inrikesutrikes_kon_alder_tid_BefProgRegFakN_scb.R")
  options(dplyr.summarise.inform = FALSE)
  
  gg_list <- list()
  gg_list_map <- list()
  objektnamn <- c()
  objektnamn_map <- c()
  
  prognos_df <- hamta_befprogn_region_inrikesutrikes_kon_alder_tid_scb(region_vekt = region_vekt,			# Val av region.
                                                                       inrikesutrikes_klartext = NA,			 #  NA = tas inte med i uttaget,  Finns: "inrikes födda", "utrikes födda", "inrikes och utrikes födda"
                                                                       kon_klartext = NA,			 #  NA = tas inte med i uttaget,  Finns: "män", "kvinnor"
                                                                       alder_koder = "*",			 
                                                                       tid_koder = prognos_ar) %>% 
    mutate(alder_grupp = skapa_aldersgrupper(ålder,c(0,20,65,80)),
           Folkmängd = Antal) %>% 
      group_by(år, regionkod, region, alder_grupp) %>% 
        summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop")
  
  bef_df <- hamta_bef_folkmangd_alder_kon_ar_scb(region_vekt = region_vekt,
                                                 alder_koder = "*",
                                                 tid_koder = "9999") %>% 
    filter(ålder!="totalt ålder") %>% 
      mutate(alder_grupp = skapa_aldersgrupper(ålder,c(0,20,65,80))) %>% 
        group_by(år, regionkod, region, alder_grupp) %>% 
          summarise(Folkmängd = sum(Folkmängd, na.rm = TRUE), .groups = "drop")
  
  tot <- rbind(bef_df, prognos_df) 
  
  total_forandring <- tot %>% 
    group_by(år,regionkod,region) %>% 
    summarise(Folkmängd = sum(Folkmängd)) %>% 
    mutate(alder_grupp = "Totalt") 
  
  diag_df <- rbind(tot, total_forandring) %>% 
    mutate(region = skapa_kortnamn_lan(region))
  
  ut <- diag_df %>%
    filter(år%in%c(min(tot$år),max(tot$år))) %>% 
      mutate(ar = ifelse(år==min(år),"forsta_ar","sista_ar")) %>% 
        select(-år) %>% 
      pivot_wider(names_from = ar, values_from = Folkmängd) %>% 
        mutate(forandring = (sista_ar - forsta_ar),
               forandring_procent = ((sista_ar - forsta_ar)/forsta_ar)*100) %>% 
          pivot_longer(cols = c(forandring), names_to = "variabel", values_to = "Folkmängd") %>% 
            select(-c(forsta_ar,sista_ar))
  
  # total_forandring <- ut %>% 
  #   group_by(regionkod,region,variabel) %>% 
  #     summarise(Folkmängd = sum(Folkmängd)) %>% 
  #       mutate(alder_grupp = "Totalt") 
  
  # diag_df <- rbind(ut, total_forandring) %>% 
  #   mutate(region = skapa_kortnamn_lan(region))
  
  #diag_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning av Samhällsanalys, Region Dalarna\nDiagramförklaring:I prognoserna tas ingen hänsyn till planerat bostadsbyggande,\netableringar av företag eller andra framtida mål och förutsättningar."


  if(returnera_data == TRUE){
    assign("befprognos_df", ut, envir = .GlobalEnv)
  }

  if(diag_jmf_region == TRUE){
    diagram_titel <- paste0("Befolkningsförändring ",min(tot$år),"-",max(tot$år))
    if(!(is.na(engelsk_titel))) diagram_titel = engelsk_titel
    diagramfil <- ifelse(jmf_procent==TRUE,paste0("befolkningsforandring_jmf_region_procent_", max(tot$år), ".png"),paste0("befolkningsforandring_jmf_region_", max(tot$år), ".png"))
    #diagramfil <- paste0("befolkningsforandring_jmf_region_", max(tot$år), ".png")
    objektnamn <- c(objektnamn,diagramfil %>% str_remove(".png"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = ut %>% 
                                   filter(alder_grupp == "Totalt") %>% 
                                    mutate(fokus = ifelse(regionkod == region_fokus, "1", "0")), 
                                 skickad_x_var = "region", 
                                 skickad_y_var = ifelse(jmf_procent==TRUE,"forandring_procent","Folkmängd"),
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diag_capt,
                                 x_axis_sort_value = TRUE,
                                 x_var_fokus = "fokus",
                                 stodlinjer_avrunda_fem = avrunda_fem,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = ifelse(jmf_procent==TRUE,"procent","Antal"),
                                 skriv_till_diagramfil = spara_figur,
                                 manual_color = diagramfarger("rus_tva_fokus"),
                                 output_mapp = output_mapp_figur,
                                 filnamn_diagram = diagramfil)
    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list) <- objektnamn
  }
  
  if(diag_alla == FALSE && diag_facet == FALSE){
    ut <- ut %>%
      filter(regionkod %in% region_fokus)
    }

  if(diag_aldergrupp == TRUE){
    
    skapa_diagram <- function(data,vald_region){
      
      data <- data %>%
        filter(region %in% vald_region)
      
      if(length(unique(data$region)) > 1){
        vald_region <- paste0(vald_region[1],"_facet_")
        diagram_titel<- paste0("Befolkningsförändring ",min(tot$år),"-",max(tot$år))
      }else{
        diagram_titel <- paste0("Befolkningsförändring i ",vald_region," ",min(tot$år),"-",max(tot$år))
      }
      diagramfil <- ifelse(jmf_procent==TRUE,paste0("befolkningsforandring_procent_",vald_region,"_", max(tot$år), ".png"),paste0("befolkningsforandring_",vald_region,"_", max(tot$år), ".png"))
      #diagramfil <- paste0("befolkningsforandring_",vald_region,"_", max(tot$år), ".png")
      objektnamn_map <- c(objektnamn_map,diagramfil %>% str_remove(".png"))
      
      gg_obj <- SkapaStapelDiagram(skickad_df = data %>% 
                                    mutate(alder_grupp = factor(alder_grupp, levels = c("Totalt","0-19 år","20-64 år","65-79 år","80+ år"))), 
                                   skickad_x_var = "alder_grupp", 
                                   skickad_y_var = ifelse(jmf_procent==TRUE,"forandring_procent","Folkmängd"),
                                   diagram_titel = diagram_titel,
                                   diagram_capt = diag_capt,
                                   stodlinjer_avrunda_fem = avrunda_fem,
                                   manual_x_axis_text_vjust = 1,
                                   manual_x_axis_text_hjust = 1,
                                   diagram_facet = length(unique(data$region)) > 1,
                                   facet_grp = "region",
                                   manual_y_axis_title = ifelse(jmf_procent==TRUE,"procent","Antal"),
                                   skriv_till_diagramfil = spara_figur,
                                   manual_color = diagramfarger("rus_sex")[1],
                                   output_mapp = output_mapp_figur,
                                   filnamn_diagram = diagramfil)
      
      gg_list_map <- c(gg_list_map, list(gg_obj))
      names(gg_list_map) <- objektnamn_map
      return(gg_list_map)
    
    }
    
    if (diag_facet) {
      diag <- skapa_diagram(ut,unique(ut$region))
      
    } else {
      diag <- map(unique(ut$region), ~ skapa_diagram(ut, .x)) %>% flatten()
      
    }
    
    gg_list <- c(gg_list, diag)
    
  }
  
  if(returnera_figur==TRUE){
    return(gg_list)
  }

}
