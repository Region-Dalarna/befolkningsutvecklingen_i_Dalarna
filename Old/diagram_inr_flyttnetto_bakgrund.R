
#test = diag_inr_flyttnetto_inr_utr_fodda(output_mapp =  "G:/skript/jon/Figurer/",farg_vekt = diagramfarger("rus_sex"),region_vekt = "2021")

diag_inr_flyttnetto_inr_utr_fodda <- function(
    region_vekt = "20",
    gruppera_namn = NA,                               # om NA skapas ett diagram per region, annars grupperas de ihop och får namnet som anges här
    facet_diagram = TRUE,                            # om TRUE skapas ett diagram för alla regioner, annars ett diagram för varje region
    farg_vekt = diagramfarger("rd_gron")[c(1,4)],
    diagram_capt = "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nInrikes flyttnetto är skillnaden mellan de som flyttat in till och de som flyttat ut från en kommun/region, från och till andra kommuner/regioner",
    output_mapp = "G:/Samhällsanalys/API/Fran_R/utskrift/",
    skriv_diagram = TRUE,
    skriv_excel = FALSE,
    spara_som_svg = FALSE,                            # TRUE om vi vill spara diagrammet som svg
    visa_totalvarden = TRUE,                          # skriver ut ett streck för netto både inrikes och utrikes födda
    visa_totalvarden_dataetiketter = FALSE,           # skriver ut dataetiketter för totalvärdena
    totalvarden_dataetiketter_farg = "black",         # välj färg på totalstrecken
    totalvarden_dataetiketter_hjust = 20,             # justerar dataetiketter för totalvärden i höjdled
    totalvarden_dataetiketter_textstorlek = 2,        # justerar textstorlek för dataetiketter för totalvärden
    totalvarden_linjetjocklek = 4                     # tjocklek på totalstrecken i tiondels % av hela diffen i datasetet
) {
  
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_text.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
  
  gg_list <- list()
  objektnamn <- c()
  
  if (is.na(farg_vekt[1])) farg_vekt <- diagramfarger("rd_gron")
  
  # =====================================================================================================
  
  url_uttag <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101J/FlyttFodReg"
  
  cont_var <- hamta_klartext_med_kod(url_uttag, c("000001J5", "000001EE"), "contentscode")
  
  varlista <- list(
    Region = region_vekt,
    Fodelseregion = "*",
    ContentsCode = c("000001J5", "000001EE"),
    Tid = '*')
  
  # =============================================== API-uttag ===============================================
  
  px_uttag <- pxweb_get(url = url_uttag, query = varlista) 
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(Region)) %>% 
    rename(regionkod = Region) %>% relocate(regionkod, .before = region) %>% 
    mutate(region = region %>% skapa_kortnamn_lan(byt_ut_riket_mot_sverige = TRUE))     # gör om län till kortnamn och riket till Sverige
  
  if (!is.na(gruppera_namn)){
    px_df <- px_df %>% 
      group_by(år, födelseregion) %>% 
      summarise(Inrikes_flyttnetto = sum(across(all_of(cont_var)))) %>% 
      ungroup() %>% 
      mutate(region = gruppera_namn)
    
  } else {
    px_df <- px_df %>% 
      group_by(år, regionkod, region, födelseregion) %>% 
      summarise(Inrikes_flyttnetto = sum(across(all_of(cont_var)))) %>% 
      ungroup()
  }
  
  
  if (skriv_excel){
    reg_namn <- ifelse(!is.na(gruppera_namn), gruppera_namn, paste0(region_vekt, collapse = "_"))
    excelfil <- paste0("andel_arblosa_", min(arblosa_bakgr$tid), "_", max(arblosa_bakgr$tid) ,".xlsx")
    write.xlsx(px_df, paste0("Flyttnetto_", reg_namn, "_ar", min(chart_df$år), "_", max(chart_df$år), ".xlsx"), overwrite = TRUE)
  }
  # ============================================= Skapa diagram ==============================================
  
  #for (reg in unique(px_df$region)) {
  skapa_diagram <- function(vald_regionkod) {  # skapa en funktion som skapar diagram för varje region
    
    retur_list <- list()
    #vald_regionkod = "20"
    chart_df <- px_df %>% filter(regionkod %in% vald_regionkod) 
    
    reg_txt <- chart_df$region %>% unique() %>% skapa_kortnamn_lan(byt_ut_riket_mot_sverige = TRUE)
    # =================================== visa totalvärden ========================================
    if (visa_totalvarden){
      
      diff <- max(chart_df$Inrikes_flyttnetto) - min(chart_df$Inrikes_flyttnetto) # ta reda på skillnaden mellan det högsta och lägsta värdet i datasetet
      totalvarden_linjebredd <- diff * (totalvarden_linjetjocklek/1000)      # gör en linjetjocklek på totallinjerna som är 0,2 % av diff (på raden ovan)
      total_list <- list()
      unika_ar <- unique(chart_df$år)
      unika_reg <- unique(vald_regionkod)
      unika_reg_txt <- hamtaregion_kod_namn(unika_reg)$region %>% skapa_kortnamn_lan()
      
      for (reg in 1:length(unika_reg)) { 
        for (ar in 1:length(unika_ar)){
          arsvarde <- chart_df %>% 
            filter(år == unika_ar[ar],
                   regionkod == unika_reg[reg]) %>% 
            select(Inrikes_flyttnetto) %>% 
            dplyr::pull()
          arsvarde <- sum(arsvarde, na.rm = TRUE)
          total_list <- c(total_list, list(list(geom = "rect", ymin=arsvarde-totalvarden_linjebredd, ymax=arsvarde+totalvarden_linjebredd, xmin=ar-0.45, xmax=ar+0.45, alpha=1, fill="black")))
          if (visa_totalvarden_dataetiketter) {
            total_list <- c(total_list, list(list(geom = "text", y=arsvarde+totalvarden_dataetiketter_hjust, x = ar, size = totalvarden_dataetiketter_textstorlek, angle=0, fontface = "plain", label =arsvarde, color = totalvarden_dataetiketter_farg)))
          } # slut if-sats om man vill vissa dataetiketter
        } # slut for-loop unika_ar
      } # slut for_loop unika_reg        
    } else total_list <- NA # slut if-sats visa_totalvärden
    
    # ======================= skapa ggplot-objekt =================
    
    facet_diagram = FALSE
    
    diagtitel_txt <- if (facet_diagram) " i" else paste0(" i ", reg_txt)
    
    #ar_alla_kommuner_i_ett_lan(vald_regionkod)
    
    diagram_titel <- paste0("Inrikes flyttnetto", diagtitel_txt)
    diagramfil <- paste0("Flyttnetto_bakgrund", unika_reg_txt %>% paste0(collapse = "_"), "_ar", min(chart_df$år), "_", max(chart_df$år), ".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = chart_df, 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "Inrikes_flyttnetto",
                                 skickad_x_grupp = "födelseregion",
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 #x_axis_storlek = 8,
                                 stodlinjer_avrunda_fem = FALSE,
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 manual_y_axis_title = "",
                                 geom_position_stack = TRUE,
                                 fokusera_varden = total_list,
                                 diagram_facet = facet_diagram,
                                 facet_grp = "region",
                                 skriv_till_diagramfil = !visa_totalvarden,
                                 manual_color = farg_vekt,
                                 diagram_som_svg = spara_som_svg,
                                 output_mapp = "output_mapp",
                                 filnamn_diagram = "diagramfil")
    
    if (visa_totalvarden){
      dia_med_legend <- gg_obj +
        geom_line(aes(color="line"))+
        scale_color_manual(name = "", values = c("line" = "black"), labels = "inrikes flyttnetto totalt")+
        theme(legend.key = element_rect(fill = "white"),
              legend.box.just = "bottom")
    } # slut if-sats visa_totalvarden  
    
    #retur_list <- c(retur_list, dia_med_legend)
    #retur_list <- dia_med_legend
    #names(retur_list)[length(retur_list)] <- diagramfil %>% str_remove(".png")
    #names(retur_list) <- skapa_kortnamn_lan(vald_regionkod)

    if (skriv_diagram) {                           # skriv en diagramfil om så önskas
      skriv_till_diagramfil(dia_med_legend,
                            output_mapp = output_mapp,
                            filnamn_diagram = diagramfil)
    } # slut if-sats
    
    return(dia_med_legend)
  } # slut skapa diagram-funktion för varje region
  
  if (length(region_vekt) == 1) facet_diagram <- FALSE 
  
  if (facet_diagram) {
    gg_list <- skapa_diagram(region_vekt)
    #names(gg_list) <- paste0("facet_",skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt))[2][1],"_mfl")
  } else {
    gg_list <- map(unique(region_vekt), ~skapa_diagram(.x))
    names(gg_list) <- paste0("inflytt_utflytt",region_vekt)
  }
  
  return(gg_list)
} # slut funktion
