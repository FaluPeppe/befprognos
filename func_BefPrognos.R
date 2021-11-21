library(pxweb)
library(httr)             # för att komma förbi brandväggen
library(writexl)
library(dplyr)
library(readr)            # för att använda parse_number
library(ggplot2)
library(png)
library(tidyr)            # för att använda pivot_longer

# en testrad för att testa github i R Studio
# Ladda skript som sköter regionuttaget och kommuner per region
source("G:/Samhällsanalys/Automatisering och R/Skript/API_func.R", encoding = "utf-8", echo = FALSE)
source("G:/Samhällsanalys/Automatisering och R/Skript/func_logga_i_diagram.R", encoding = "utf-8", echo = FALSE)

# För att komma förbi brandvägg - om man har en sådan, annars kan man ta bort dessa två rader
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
set_config(config(ssl_verifypeer = 0L))

SkapaBefPrognosDiagram <- function(aktlan = "20", bara_lan = TRUE, AktuellRegion = NULL,
                                   jmfrtid = 10, JmfrFleraPrognoser = FALSE,
                                   url_tabeller,
                                   output_fold,
                                   logga_path,
                                   logga_storlek,
                                   diagram_capt = "") {
                                  
  # Initierar variabler
  malar <- 0
  startar <- 0
  px_alla <- NULL
  
  # ================================================================================================================
  
  # Fyll regiontabell
  regdf <- hamtaregtab()
  
  # Här väljer vi om vi ska ta med riket och länet också
  # Om vi bara vill ha län och inte kommuner, lägg in en vektor här nedan
  if (bara_lan){
    location_aktRegion <- aktlan
  } else {
    location_aktRegion <- hamtakommuner(aktlan, tamedriket = FALSE, tamedlan = FALSE) 
  }
  
  # plocka ut vanligaste län i vektorn med alla regioner i 
  vanlan <- names(sort(table(substr(location_aktRegion,1,2)),decreasing=TRUE)[1])
  if (is.null(AktuellRegion)) AktuellRegion <- ifelse(length(location_aktRegion) 
                                                      > 1, paste0(regdf$region[regdf$regionkod
                                                                               == vanlan], "s kommuner"), regdf$region[regdf$regionkod
                                                                                                                       == aktlan])
  
  # =============== jämförelse x år framåt från senaste tillgängliga år ==========================
  
  # ========== Hämta befolkningsprognos för tabell(er) i vektor url_tabeller  ====================
  # om vi valt att jämföra flera prognoser loopas hela vektorn med url:er till tabellerna
  # annars kör vi bara första tabellen
  slutloop <- ifelse(JmfrFleraPrognoser, length(url_tabeller), 1) 
  # först hämtar vi startåret för alla tabeller i vektorn, och skapar samtidigt målåret också
  for (url_progn in 1:slutloop) {
    # först plockar vi ut ett pyttelitet api-uttag för att få tidigaste år i prognos att skapa startår från
    px_small <- pxweb_get(url = url_tabeller[url_progn],query = list(
                            Region = "20",ContentsCode = "*", Tid = "*")) 
    px_df_small <- as.data.frame(px_small, column.name.type = "text", variable.value.type = "text")
    startar[url_progn] <- as.character(as.numeric(min(px_df_small$år)) - 1)     # startår är alltid ett år innan första året i prognosen
    malar[url_progn] <- as.character(as.numeric(startar[url_progn]) + jmfrtid)             # målår skapas genom att addera jämförelsetid till startåret
  
  
    # API-uttag för att ta ut målåret som är det vi behöver från prognosen
    px_uttag <- pxweb_get(url = url_tabeller[url_progn],
                          query = list(
                            Region = location_aktRegion,
                            Alder = '*',
                            ContentsCode = '*',
                            Tid = malar[url_progn])) 
    
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    px_df <- as.data.frame(px_uttag) %>% 
      cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
              select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
    px_df <- px_df %>% select(regionkod, region, ålder, år, Folkmängd)
    
    # bind ihop med tidigare
    px_alla <- rbind(px_df, px_alla)
  # slut på loop med alla url:er
  }
  
  # =========================== hämta senaste tillgängliga år i befolkingsstatistiken ============================
  
  url_adress <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  
  # Välj variabler
  px_uttag_bef <- pxweb_get(url = url_adress,
                        query = list(
                          Region = location_aktRegion,
                          Alder = '*',
                          ContentsCode = 'BE0101N1',
                          Tid = startar)) 
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  px_df_bef <- as.data.frame(px_uttag_bef) %>% 
    cbind(regionkod = as.data.frame(px_uttag_bef, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
  # skapa en df med både prognosvärden och befolkningssiffror för senaste året innan prognosen startar
  progn_bef <- rbind(px_df_bef, px_alla)
  
  # Skapa en numerisk åldersvariabel 
  progn_bef$åldernum <- suppressWarnings(parse_number(progn_bef$ålder))
  
  # Lägg ihop i åldersgrupper
  progn_bef$aldergrp <- ifelse(progn_bef$åldernum < 20, "0-19 år", NA)
  progn_bef$aldergrp <- ifelse(progn_bef$åldernum > 19 & progn_bef$åldernum < 65 , "20-64 år", progn_bef$aldergrp)
  progn_bef$aldergrp <- ifelse(progn_bef$åldernum > 64 & progn_bef$åldernum < 80 , "65-79 år", progn_bef$aldergrp)
  progn_bef$aldergrp <- ifelse(progn_bef$åldernum > 79, "80+ år", progn_bef$aldergrp)
  progn_bef$aldergrp <- ifelse(progn_bef$ålder == "totalt ålder", "totalt", progn_bef$aldergrp)
  
  # Skapa df för startår 
  dfstartar <- progn_bef %>%
    filter(år %in% startar) %>% 
    group_by(år, regionkod, region, aldergrp) %>% 
    summarize(antal = sum(Folkmängd))
  
  # Skapa df för målår
  dfmalar <- progn_bef %>% 
    filter(år %in% malar) %>% 
    group_by(år, regionkod, region, aldergrp) %>% 
    summarize(antal = sum(Folkmängd))
  
  # Här skapar vi en rad med total folkmängd i dfmalar ==========================================
  total_df <- dfmalar %>% 
    group_by(år, region) %>% 
    summarize(regionkod = first(regionkod),
              region = first(region),
              antal = sum(antal))
  # Lägg på total_df som rad på dfmalar
  dfmalar <- rbind(dfmalar, total_df) %>% 
    arrange(år)
  
  # döp åldersgruppen för totalbef till "totalt"
  dfmalar$aldergrp[is.na(dfmalar$aldergrp)] <- "totalt"
  
  # se till att sorteringen blir korrekt
  dfmalar <- dfmalar %>% arrange(år, regionkod, aldergrp)
  
  # ====================================== Skapa diff-df ==================================================
  dfdiff <- dfmalar
  dfdiff$år <- paste0("Förändring ", dfstartar$år,"-", dfmalar$år, " (prognos våren ", 
                      as.numeric(dfstartar$år) +1, ")")
  dfdiff$antal <- dfmalar$antal - dfstartar$antal
  
  # Gör om aldergrp till factor som vi lägger i den ordning vi vill 
  # plotta diagrammet
  dfdiff$aldergrp <- factor(dfdiff$aldergrp, levels = 
                              c("totalt", "0-19 år",
                                "20-64 år", "65-79 år", "80+ år"))
  
  AktuellRegion <- ifelse(length(unique(dfdiff$region)) == 1, dfdiff$region[1], AktuellRegion)
  # ===================================== Gör diagram =====================================================
  
  # Om det bara är en befolkningsprognos som plottas så används inte legend (se längre ner) och information 
  # om vilka år som avses + när prognosen är ifrån läggs i diagramtiteln, är det fler prognoser som plottas
  # läggs inte den informationen i diagramtiteln utan istället i legenden. 
  if(JmfrFleraPrognoser == FALSE) {
    diagramtitel <- paste0("Befolkningsförändring i ", AktuellRegion, " ",
                           dfstartar$år[1],"-", dfmalar$år[1], "\n(enligt befolkningsprognos våren ", 
                           as.numeric(dfstartar$år[1]) +1, ")")
  } else {
    diagramtitel <- paste0("Befolkningsförändring i ", AktuellRegion, " på ", jmfrtid, 
                           " års sikt")
  }
  
  # Om det bara är en prognos som används, kör mörkgrönt, annars både ljus- och mörkgrönt
  if (length(unique(dfdiff$år)) == 1){
    stapelfarger <- c(rgb(79,98,40, maxColorValue = 255))
  } else {
    stapelfarger <- c(rgb(155,187,89, maxColorValue = 255), 
                      rgb(79,98,40, maxColorValue = 255))  
  }
  
  # Här skapar vi själva diagrammet i ggplot
  p<-dfdiff %>% 
    ggplot(aes(x=aldergrp, y=antal, fill=år)) +
    geom_bar(position = "dodge", stat="identity")+
    #theme_minimal() + 
    {if(length(unique(dfdiff$region)) == 1){
      geom_text(aes(y=antal+sign(antal),label=antal,
                    vjust = ifelse(antal >= 0, -0.5, 1)),
                color = "#464d48", 
                size=2.3, 
                position = position_dodge(width = 0.9))
    }} +
    theme(axis.text.x = element_text(size = 8),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.ticks = element_blank(), 
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(face = "italic", size = 5,
                                      hjust = 0, vjust = 0),
          plot.caption.position = "plot",
          panel.grid.major = element_line(size=0.5, colour = "lightgrey"),
          panel.grid.minor = element_line(size=0.08, colour = "lightgrey"),
          panel.background = element_rect(fill = "white")) +
    {if(JmfrFleraPrognoser == FALSE) theme(legend.position = "none")}+    # om man inte jämför flera prognoser, ta bort legend
    scale_fill_manual(values=stapelfarger) +
    labs(title = diagramtitel, 
         x = element_blank(),
         y = element_blank(),
         caption = diagram_capt) +
    facet_wrap(~ region, scales = "free") +
    if(length(unique(dfdiff$region)) == 1){
      theme(strip.text = element_blank())
    } else {  
      theme(strip.text = element_text(color = "black"),
            strip.background = element_blank())
    }

  # Ändra höjd och bredd på den sparade png-filen utifrån hur många regioner
  # som är med i diagrammet, + ange mapp och filnamn
  bredd <- ifelse(length(unique(dfdiff$region)) == 1, 7, 13)
  hojd <- ifelse(length(unique(dfdiff$region)) == 1, 4, 8)
  # och om det är fler än 20 kommuner
  bredd <- ifelse(length(unique(dfdiff$region)) > 20 , 19, bredd)
  hojd <- ifelse(length(unique(dfdiff$region)) > 20, 12, hojd)
  fold <- output_fold
  jmfr_pre <- ifelse(JmfrFleraPrognoser == TRUE, "_jmfr", "")   # lägger till ett "_jmfr" efter namnet om flera prognoser jämförs så att man kan skilja dessa prognoser från de med en prognos i filnamnet
  filnamn_pre <- paste0("Befolkningsförändring i ", AktuellRegion, " på ", jmfrtid, " års sikt", jmfr_pre)
  filnamn <- paste0(filnamn_pre, ".png")
  ggsave(paste0(fold,filnamn), width = bredd, height = hojd)
  
  # Lägg till logga till diagrammet =======================================
  if (!is.null(logga_path)){ 
    add_logo(
    plot_path = paste0(fold, filnamn), # url or local file for the plot
    logo_path = logga_path, # url or local file for the logo
    logo_position = "bottom right", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = logga_storlek,
    #10 as default, but can change to manually make logo bigger (lägre tal = större logga)
    replace = TRUE
  )
  }
# slut på funktionen
}

SkapaBefPrognosDiagram_befforandr <- function(aktlan = "20", bara_lan = TRUE, AktuellRegion = NULL,
                                   jmfrtid = 10, JmfrFleraPrognoser = FALSE,
                                   url_tabeller,
                                   output_fold,
                                   logga_path,
                                   logga_storlek,
                                   diagram_capt = "") {
  
  # Initierar variabler
  malar <- 0
  startar <- 0
  px_alla <- NULL
  
  # ================================================================================================================
  
  # Fyll regiontabell
  regdf <- hamtaregtab()
  
  # Här väljer vi om vi ska ta med riket och länet också
  # Om vi bara vill ha län och inte kommuner, lägg in en vektor här nedan
  if (bara_lan){
    location_aktRegion <- aktlan
  } else {
    location_aktRegion <- hamtakommuner(aktlan, tamedriket = FALSE, tamedlan = FALSE) 
  }
  
  # plocka ut vanligaste län i vektorn med alla regioner i 
  vanlan <- names(sort(table(substr(location_aktRegion,1,2)),decreasing=TRUE)[1])
  if (is.null(AktuellRegion)) AktuellRegion <- ifelse(length(location_aktRegion) 
                                                      > 1, paste0(regdf$region[regdf$regionkod
                                                                               == vanlan], "s kommuner"), regdf$region[regdf$regionkod
                                                                                                                       == aktlan])
  
  # =============== jämförelse x år framåt från senaste tillgängliga år ==========================
  
  # ========== Hämta befolkningsprognos för tabell(er) i vektor url_tabeller  ====================
  # om vi valt att jämföra flera prognoser loopas hela vektorn med url:er till tabellerna
  # annars kör vi bara första tabellen
  slutloop <- ifelse(JmfrFleraPrognoser, length(url_tabeller), 1) 
  # först hämtar vi startåret för alla tabeller i vektorn, och skapar samtidigt målåret också
  for (url_progn in 1:slutloop) {
    # först plockar vi ut ett pyttelitet api-uttag för att få tidigaste år i prognos att skapa startår från
    px_small <- pxweb_get(url = url_tabeller[url_progn],query = list(
      Region = "20",ContentsCode = "*", Tid = "*")) 
    px_df_small <- as.data.frame(px_small, column.name.type = "text", variable.value.type = "text")
    startar[url_progn] <- as.character(as.numeric(min(px_df_small$år)) - 1)     # startår är alltid ett år innan första året i prognosen
    malar[url_progn] <- as.character(as.numeric(startar[url_progn]) + jmfrtid)             # målår skapas genom att addera jämförelsetid till startåret
    
    tid_period <- as.character((as.numeric(startar[url_progn])+1):as.numeric(malar[url_progn]))
    
    # API-uttag för att ta ut målåret som är det vi behöver från prognosen
    px_uttag <- pxweb_get(url = url_tabeller[url_progn],
                          query = list(
                            Region = location_aktRegion,
                            #Alder = '*',
                            ContentsCode = '*',
                            Tid = tid_period)) 
    
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    px_df <- as.data.frame(px_uttag) %>% 
      cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
              select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
    
    px_df_grp <- px_df %>% group_by(region) %>% 
      summarize(`Naturlig befolkningstillväxt` = sum(Födda) - sum(Döda),
                `Flyttnetto, inrikes` = sum(`Inrikes inflyttning`) - sum(`Inrikes utflyttning`),
                `Flyttnetto, utrikes` = sum(Invandring) - sum(Utvandring)) %>% 
      mutate(år = paste0("Befolkningsprognos år ", (as.numeric(min(px_df$år)))-1, " (", min(px_df$år), "-", max(px_df$år), ")")) %>% 
      relocate(år, .after = region)
    
    # bind ihop med tidigare
    px_alla <- rbind(px_df_grp, px_alla)
    # slut på loop med alla url:er
  }
  # strukturera om data så att vi kan göra diagram på det
  dfdiff <- px_alla %>% pivot_longer(!c(region, år), names_to = "befforandr", values_to = "antal")

  if (length(unique(dfdiff$region)) > 1) {
    dfdiff$befforandr[dfdiff$befforandr == "Naturlig befolkningstillväxt"] <- "Nat bef.tillv."
    dfdiff$befforandr <- factor(dfdiff$befforandr, levels = c("Nat bef.tillv.",
                                                              "Flyttnetto, inrikes", "Flyttnetto, utrikes"))
  } else {
      # gör om befforandr till faktor som blir rätt sorterad
    dfdiff$befforandr <- factor(dfdiff$befforandr, levels = c("Naturlig befolkningstillväxt",
                                "Flyttnetto, inrikes", "Flyttnetto, utrikes"))
  }
  # ===================================== Gör diagram =====================================================

  # Om det bara är en befolkningsprognos som plottas så används inte legend (se längre ner) och information 
  # om vilka år som avses + när prognosen är ifrån läggs i diagramtiteln, är det fler prognoser som plottas
  # läggs inte den informationen i diagramtiteln utan istället i legenden. 
  if(JmfrFleraPrognoser == FALSE) {
    diagramtitel <- paste0("Befolkningsförändring i ", AktuellRegion) #, " ",
                           #dfstartar$år[1],"-", dfmalar$år[1], "\n(enligt befolkningsprognos våren ", 
                           #as.numeric(subst(dfdiff$år[1]) +1, ")")
  } else {
    diagramtitel <- paste0("Befolkningsförändring i ", AktuellRegion, " på ", jmfrtid, 
                           " års sikt")
  }
  
  # Om det bara är en prognos som används, kör mörkgrönt, annars både ljus- och mörkgrönt
  if (length(unique(dfdiff$år)) == 1){
    stapelfarger <- c(rgb(79,98,40, maxColorValue = 255))
  } else {
    stapelfarger <- c(rgb(155,187,89, maxColorValue = 255), 
                      rgb(79,98,40, maxColorValue = 255))  
  }
  
  # Här skapar vi själva diagrammet i ggplot
  p<-dfdiff %>% 
    ggplot(aes(x=befforandr, y=antal, fill=år)) +
    geom_bar(position = "dodge", stat="identity")+
    #theme_minimal() + 
    {if(length(unique(dfdiff$region)) == 1){
      geom_text(aes(y=antal+sign(antal),label=antal,
                    vjust = ifelse(antal >= 0, -0.5, 1)),
                color = "#464d48", 
                size=2.3, 
                position = position_dodge(width = 0.9))
    }} +
    theme(axis.text.x = element_text(size = 8),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.ticks = element_blank(), 
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(face = "italic", size = 5,
                                      hjust = 0, vjust = 0),
          plot.caption.position = "plot",
          panel.grid.major = element_line(size=0.5, colour = "lightgrey"),
          panel.grid.minor = element_line(size=0.08, colour = "lightgrey"),
          panel.background = element_rect(fill = "white")) +
    {if(JmfrFleraPrognoser == FALSE) theme(legend.position = "none")}+    # om man inte jämför flera prognoser, ta bort legend
    scale_fill_manual(values=stapelfarger) +
    labs(title = diagramtitel, 
         x = element_blank(),
         y = element_blank(),
         caption = diagram_capt) +
    facet_wrap(~ region, scales = "free") +
    if(length(unique(dfdiff$region)) == 1){
      theme(strip.text = element_blank())
    } else {  
      theme(strip.text = element_text(color = "black"),
            strip.background = element_blank())
    }
  
  # Ändra höjd och bredd på den sparade png-filen utifrån hur många regioner
  # som är med i diagrammet, + ange mapp och filnamn
  bredd <- ifelse(length(unique(dfdiff$region)) == 1, 7, 13)
  hojd <- ifelse(length(unique(dfdiff$region)) == 1, 4, 8)
  # och om det är fler än 20 kommuner
  bredd <- ifelse(length(unique(dfdiff$region)) > 20 , 19, bredd)
  hojd <- ifelse(length(unique(dfdiff$region)) > 20, 12, hojd)
  fold <- output_fold
  jmfr_pre <- ifelse(JmfrFleraPrognoser == TRUE, "_jmfr", "")   # lägger till ett "_jmfr" efter namnet om flera prognoser jämförs så att man kan skilja dessa prognoser från de med en prognos i filnamnet
  filnamn_pre <- paste0("Befolkningsförändring i ", AktuellRegion, " per befForandrtyp på ", jmfrtid, " års sikt", jmfr_pre)
  filnamn <- paste0(filnamn_pre, ".png")
  ggsave(paste0(fold,filnamn), width = bredd, height = hojd)
  
  # Lägg till logga till diagrammet =======================================
  if (!is.null(logga_path)){ 
    add_logo(
      plot_path = paste0(fold, filnamn), # url or local file for the plot
      logo_path = logga_path, # url or local file for the logo 
      logo_position = "bottom right", # choose a corner
      # 'top left', 'top right', 'bottom left' or 'bottom right'
      logo_scale = logga_storlek,
      #10 as default, but can change to manually make logo bigger (lägre tal = större logga)
      replace = TRUE
    )
  }
  # slut på funktionen
}

SkapaBefPrognosDiagram_InrUtrFodda <- function(aktlan = "20", bara_lan = TRUE, AktuellRegion = NULL,
                                   jmfrtid = 10,
                                   url_tabeller,
                                   output_fold,
                                   logga_path,
                                   logga_storlek,
                                   diagram_capt = "") {
  
  # Initierar variabler
  malar <- 0
  startar <- 0
  px_alla <- NULL
  
  # ================================================================================================================
  
  # Fyll regiontabell
  regdf <- hamtaregtab()
  
  # Här väljer vi om vi ska ta med riket och länet också
  # Om vi bara vill ha län och inte kommuner, lägg in en vektor här nedan
  if (bara_lan){
    location_aktRegion <- aktlan
  } else {
    location_aktRegion <- hamtakommuner(aktlan, tamedriket = FALSE, tamedlan = FALSE) 
  }
  
  # plocka ut vanligaste län i vektorn med alla regioner i 
  vanlan <- names(sort(table(substr(location_aktRegion,1,2)),decreasing=TRUE)[1])
  if (is.null(AktuellRegion)) AktuellRegion <- ifelse(length(location_aktRegion) 
                                                      > 1, paste0(regdf$region[regdf$regionkod
                                                                               == vanlan], "s kommuner"), regdf$region[regdf$regionkod
                                                                                                                       == aktlan])
  
  # =============== jämförelse x år framåt från senaste tillgängliga år ==========================
  
  # ========== Hämta befolkningsprognos för tabell(er) i vektor url_tabeller  ====================
  # om vi valt att jämföra flera prognoser loopas hela vektorn med url:er till tabellerna
  # annars kör vi bara första tabellen
  slutloop <- 1   # vi kör bara en tabell i denna   #ifelse(JmfrFleraPrognoser, length(url_tabeller), 1) 
  # först hämtar vi startåret för alla tabeller i vektorn, och skapar samtidigt målåret också
  for (url_progn in 1:slutloop) {
    # först plockar vi ut ett pyttelitet api-uttag för att få tidigaste år i prognos att skapa startår från
    px_small <- pxweb_get(url = url_tabeller[url_progn],query = list(
      Region = "20",ContentsCode = "*", Tid = "*")) 
    px_df_small <- as.data.frame(px_small, column.name.type = "text", variable.value.type = "text")
    startar[url_progn] <- as.character(as.numeric(min(px_df_small$år)) - 1)     # startår är alltid ett år innan första året i prognosen
    malar[url_progn] <- as.character(as.numeric(startar[url_progn]) + jmfrtid)             # målår skapas genom att addera jämförelsetid till startåret
    
    
    # API-uttag för att ta ut målåret som är det vi behöver från prognosen
    px_uttag <- pxweb_get(url = url_tabeller[url_progn],
                          query = list(
                            Region = location_aktRegion,
                            InrikesUtrikes = c("13","23"),
                            Alder = '*',
                            ContentsCode = '000005RC',
                            Tid = malar[url_progn])) 
    
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    px_df <- as.data.frame(px_uttag) %>% 
      cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
              select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
    #px_df <- px_df %>% select(regionkod, region, ålder, år, Folkmängd)
    
    # bind ihop med tidigare
    px_alla <- rbind(px_df, px_alla)
    # slut på loop med alla url:er
  }
  
  # =========================== hämta senaste tillgängliga år i befolkingsstatistiken ============================
  
  url_adress <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101E/InrUtrFoddaRegAlKon"
  
  # Välj variabler
  px_uttag_bef <- pxweb_get(url = url_adress,
                            query = list(
                              Region = location_aktRegion,
                              Alder = '*',
                              Fodelseregion = c("09","11"),
                              ContentsCode = '*',
                              Tid = startar)) 
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  px_df_bef <- as.data.frame(px_uttag_bef) %>% 
    cbind(regionkod = as.data.frame(px_uttag_bef, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
  
  # konvergera begreppen i denna tabell med tabellen för befolkningsprognosen
  px_df_bef$födelseregion[px_df_bef$födelseregion == "Född i Sverige"] <- "inrikes födda"
  px_df_bef$födelseregion[px_df_bef$födelseregion == "Utrikes född"] <- "utrikes födda"
  px_df_bef <- px_df_bef %>% 
    rename(`inrikes/utrikes född` = födelseregion) %>% 
    relocate(`inrikes/utrikes född`, .after = region)

  # skapa en df med både prognosvärden och befolkningssiffror för senaste året innan prognosen startar
  progn_bef <- rbind(px_df_bef, px_alla)
  
  # Skapa en numerisk åldersvariabel 
  progn_bef$åldernum <- suppressWarnings(parse_number(progn_bef$ålder))
  
  # Lägg ihop i åldersgrupper
  progn_bef$aldergrp <- ifelse(progn_bef$åldernum < 20, "0-19 år", NA)
  progn_bef$aldergrp <- ifelse(progn_bef$åldernum > 19 & progn_bef$åldernum < 65 , "20-64 år", progn_bef$aldergrp)
  progn_bef$aldergrp <- ifelse(progn_bef$åldernum > 64 & progn_bef$åldernum < 80 , "65-79 år", progn_bef$aldergrp)
  progn_bef$aldergrp <- ifelse(progn_bef$åldernum > 79, "80+ år", progn_bef$aldergrp)
  progn_bef$aldergrp <- ifelse(progn_bef$ålder == "totalt ålder", "totalt", progn_bef$aldergrp)
  
  # Skapa df för startår 
  dfstartar <- progn_bef %>%
    filter(år %in% startar) %>% 
    group_by(år, regionkod, region, `inrikes/utrikes född`, aldergrp) %>% 
    summarize(antal = sum(Folkmängd))
  
  # Skapa df för målår
  dfmalar <- progn_bef %>% 
    filter(år %in% malar) %>% 
    group_by(år, regionkod, region, `inrikes/utrikes född`, aldergrp) %>% 
    summarize(antal = sum(Folkmängd))
  
  # Här skapar vi en rad med total folkmängd i dfmalar ==========================================
  total_df <- dfmalar %>% 
    group_by(år, region, `inrikes/utrikes född`) %>% 
    summarize(regionkod = first(regionkod),
              region = first(region),
              antal = sum(antal))
  # Lägg på total_df som rad på dfmalar
  dfmalar <- rbind(dfmalar, total_df) %>% 
    arrange(år)
  
  # döp åldersgruppen för totalbef till "totalt"
  dfmalar$aldergrp[is.na(dfmalar$aldergrp)] <- "totalt"
  
  # se till att sorteringen blir korrekt
  dfmalar <- dfmalar %>% arrange(år, regionkod, aldergrp)
  
  # och så skapar vi en rad med total folkmängd i dfstartar =====================================
  total_df_start <- dfstartar %>% 
    group_by(år, region, `inrikes/utrikes född`) %>% 
    summarize(regionkod = first(regionkod),
              region = first(region),
              antal = sum(antal))
  # Lägg på total_df som rad på dfmalar
  dfstartar <- rbind(dfstartar, total_df_start) %>% 
    arrange(år)
  
  # döp åldersgruppen för totalbef till "totalt"
  dfstartar$aldergrp[is.na(dfstartar$aldergrp)] <- "totalt"
  
  # se till att sorteringen blir korrekt
  dfstartar <- dfstartar %>% arrange(år, regionkod, aldergrp)
  
  # ====================================== Skapa diff-df ==================================================
  dfdiff <- dfmalar
  dfdiff$år <- paste0("Förändring ", dfstartar$år,"-", dfmalar$år, " (prognos våren ", 
                      as.numeric(dfstartar$år) +1, ")")
  dfdiff$antal <- dfmalar$antal - dfstartar$antal
  
  # Gör om aldergrp till factor som vi lägger i den ordning vi vill 
  # plotta diagrammet
  dfdiff$aldergrp <- factor(dfdiff$aldergrp, levels = 
                              c("totalt", "0-19 år",
                                "20-64 år", "65-79 år", "80+ år"))
  
  AktuellRegion <- ifelse(length(unique(dfdiff$region)) == 1, dfdiff$region[1], AktuellRegion)
  # ===================================== Gör diagram =====================================================
  
  diagramtitel <- paste0("Befolkningsförändring i ", AktuellRegion, " ",
                           dfstartar$år[1],"-", dfmalar$år[1], "\n(enligt befolkningsprognos våren ", 
                           as.numeric(dfstartar$år[1]) +1, ")")
  
  # Om det bara är en prognos som används, kör mörkgrönt, annars både ljus- och mörkgrönt
  stapelfarger <- c(rgb(155,187,89, maxColorValue = 255), 
                      rgb(79,98,40, maxColorValue = 255))  
  
  # Här skapar vi själva diagrammet i ggplot
  p<-dfdiff %>% 
    ggplot(aes(x=aldergrp, y=antal, fill=`inrikes/utrikes född`)) +
    geom_bar(position = "dodge", stat="identity")+
    #theme_minimal() + 
    {if(length(unique(dfdiff$region)) == 1){
      geom_text(aes(y=antal+sign(antal),label=antal,
                    vjust = ifelse(antal >= 0, -0.5, 1)),
                color = "#464d48", 
                size=2.3, 
                position = position_dodge(width = 0.9))
    }} +
    theme(axis.text.x = element_text(size = 8),
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.ticks = element_blank(), 
          plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(face = "italic", size = 5,
                                      hjust = 0, vjust = 0),
          plot.caption.position = "plot",
          panel.grid.major = element_line(size=0.5, colour = "lightgrey"),
          panel.grid.minor = element_line(size=0.08, colour = "lightgrey"),
          panel.background = element_rect(fill = "white")) +
    #{if(JmfrFleraPrognoser == FALSE) theme(legend.position = "none")}+    # om man inte jämför flera prognoser, ta bort legend
    scale_fill_manual(values=stapelfarger) +
    labs(title = diagramtitel, 
         x = element_blank(),
         y = element_blank(),
         caption = diagram_capt) +
    facet_wrap(~ region, scales = "free") +
    if(length(unique(dfdiff$region)) == 1){
      theme(strip.text = element_blank())
    } else {  
      theme(strip.text = element_text(color = "black"),
            strip.background = element_blank())
    }
  
  # Ändra höjd och bredd på den sparade png-filen utifrån hur många regioner
  # som är med i diagrammet, + ange mapp och filnamn
  bredd <- ifelse(length(unique(dfdiff$region)) == 1, 7, 13)
  hojd <- ifelse(length(unique(dfdiff$region)) == 1, 4, 8)
  # och om det är fler än 20 kommuner
  bredd <- ifelse(length(unique(dfdiff$region)) > 20 , 19, bredd)
  hojd <- ifelse(length(unique(dfdiff$region)) > 20, 12, hojd)
  fold <- output_fold
  filnamn_pre <- paste0("Befolkningsförändring i ", AktuellRegion, " inr_utr födda ", startar[1], " - ", malar[1])
  filnamn <- paste0(filnamn_pre, ".png")
  ggsave(paste0(fold,filnamn), width = bredd, height = hojd)
  
  # Lägg till logga till diagrammet =======================================
  if (!is.null(logga_path)){ 
    add_logo(
      plot_path = paste0(fold, filnamn), # url or local file for the plot
      logo_path = logga_path, # url or local file for the logo
      logo_position = "bottom right", # choose a corner
      # 'top left', 'top right', 'bottom left' or 'bottom right'
      logo_scale = logga_storlek,
      #10 as default, but can change to manually make logo bigger (lägre tal = större logga)
      replace = TRUE
    )
  }
  # slut på funktionen
}
