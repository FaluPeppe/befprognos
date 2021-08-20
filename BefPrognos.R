library(pxweb)
library(httr)             # för att komma förbi brandväggen
library(writexl)
library(dplyr)
library(readr)            # för att använda parse_number
library(ggplot2)
#library(grid)
library(png)
#library(ggrepel)

# Initierar variabler
malar_jmfr <- 0
startar_jmfr <- 0

# ================================ Här gör vi inställningar för körningen ========================================
aktlan <- "20"    # c("17", "20", "21")         # Välj län att göra prognoser på
bara_lan <- FALSE                                # TRUE om bara län ska visas, FALSE för att visa länets kommuner
AktuellRegion <- NULL                           # Till diagramrubriken - sätt som tom om namnet ska hämtas automatiskt 

jmfrtid <- 15        # antal år i jämförelsen, alltså hur många års sikt vi vill titta på

JmfrFleraPrognoser <- FALSE    # TRUE om vi vill jämföra med äldre prognoser, FALSE om vi bara vill se den senaste prognosen

# ================================================================================================================

# För att komma förbi proxyn
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
set_config(config(ssl_verifypeer = 0L))

# Ladda skript som sköter regionuttaget och kommuner per region
source("G:/Samhällsanalys/Automatisering och R/Skript/API_func.R", encoding = "utf-8", echo = FALSE)
source("G:/Samhällsanalys/Automatisering och R/Skript/func_logga_i_diagram.R", encoding = "utf-8", echo = FALSE)

# sökväg till logga för att kunna lägga in den i diagrammen
logga_path <- "G:/Samhällsanalys/MallarLoggor/logo_liggande_fri_svart.png"

# Kör bara om vi ska jämföra flera prognosår med varandra - vill bara se en prognos, hoppa över 
if (JmfrFleraPrognoser) {
  # url till prognosen som alltså ligger i en egen tabell Under Befolkning - Befolkningsframskrivningar - Äldre framskrivningar
  url_adress2 <- c("/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401B/BefProgOsiktRegN20",
                   "/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401B/BefProgOsiktRegN20")
}

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

# =========================== jämförelse x år framåt från senaste tillgängliga år ==========================


# ================ Hämta befolkningsprognos senaste jmfr-år =============================================
url_adress <- "/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN"
url_adress <- paste0("http://api.scb.se", url_adress)



# API-uttag 
px_uttag <- pxweb_get(url = url_adress,
                      query = list(
                        Region = location_aktRegion,
                        Kon = c('*'),
                        Alder = c('*'),
                        ContentsCode = c('*'),
                        Tid = c('*')
                      )
) 

# Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
# välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
px_df <- as.data.frame(px_uttag) %>% 
  cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
          select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)

# Skapa startår utifrån ett år innan startåret i prognosdataset:et
startar <- as.character(as.numeric(min(px_df$år)) - 1)
# Välj målår (jämförelseår) utifrån startår ovan
malar <- as.character(as.numeric(startar) + jmfrtid)


px_df <- px_df %>% filter(år == malar)


if (JmfrFleraPrognoser) {
    # ================ Hämta befolkningsprognos tidigare jmfr-år =============================================
    
  for (jmfrProgn in 1:length(url_adress2)) {    
    url_adress3 <- paste0("http://api.scb.se", url_adress2[jmfrProgn])
    
    # API-uttag 
    px_uttag2 <- pxweb_get(url = url_adress3,
                           query = list(
                             Region = location_aktRegion,
                             Kon = c('*'),
                             Alder = c('*'),
                             ContentsCode = c('*'),
                             Tid = c('*')
                           )
    ) 
    
    # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
    # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
    px_df2 <- as.data.frame(px_uttag2) %>% 
      cbind(regionkod = as.data.frame(px_uttag2, column.name.type = "code", variable.value.type = "code") %>% 
              select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)
    
    # Skapa startår utifrån ett år innan startåret i prognosdataset:et
    startar_jmfr <- ifelse(startar_jmfr == 0, as.character(as.numeric(min(px_df2$år)) - 1), 
                           c(startar_jmfr, as.character(as.numeric(min(px_df2$år)) - 1)))
    malar_jmfr <- ifelse(malar_jmfr == 0, as.character(as.numeric(startar_jmfr) + jmfrtid),
                         c(malar_jmfr, as.character(as.numeric(startar_jmfr) + jmfrtid)))
    
    px_df2 <- px_df2 %>% filter(år == malar_jmfr[jmfrProgn])
    
    
    # Sätt ihop de båda åren
    px_df <- rbind(px_df2, px_df)
  # slut på for-looop
  }
# slut på test av JmfrFleraPrognoser 
}


# =========================== hämta senaste tillgängliga år i befolkingsstatistiken ============================

url_adress <- "/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
url_adress <- paste0("http://api.scb.se", url_adress)

# Välj variabler
px_uttag <- pxweb_get(url = url_adress,
                      query = list(
                        Region = location_aktRegion,
                        Alder = c('*'),
                        ContentsCode = c('BE0101N1'),
                        Tid = c('*')
                      )
) 

# Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
# välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
px_df3 <- as.data.frame(px_uttag) %>% 
  cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
          select(Region)) %>% rename(regionkod = Region) %>% relocate(regionkod, .before = region)

# Skapa en numerisk åldersvariabel 
px_df3$åldernum <- parse_number(px_df3$ålder)

# Välj endast ut startåret
px_df3 <- px_df3 %>% filter(år == startar | år %in% startar_jmfr) 

# Lägg ihop i åldersgrupper
px_df3$aldergrp <- NA
px_df3$aldergrp <- ifelse(px_df3$åldernum < 20, "0-19 år", px_df3$aldergrp)
px_df3$aldergrp <- ifelse(px_df3$åldernum > 19 & px_df3$åldernum < 65 , "20-64 år", px_df3$aldergrp)
px_df3$aldergrp <- ifelse(px_df3$åldernum > 64 & px_df3$åldernum < 80 , "65-79 år", px_df3$aldergrp)
px_df3$aldergrp <- ifelse(px_df3$åldernum > 79, "80+ år", px_df3$aldergrp)
px_df3$aldergrp <- ifelse(px_df3$ålder == "totalt ålder", "totalt", px_df3$aldergrp)

# Gruppera för aktuellt år
dfstartar <- px_df3 %>% 
  group_by(år, regionkod, region, aldergrp) %>% 
  summarize(antal = sum(Folkmängd))

AktuellRegion <- ifelse(length(location_aktRegion) == 1, 
                        px_df3$region[1], AktuellRegion)

# Behövs en "px_df <- rbind(px_df3, px_df)" här? Nej, väl?
#px_df <- rbind(px_df3, px_df)

# ==============================



# ==================== Här fortsätter vi bearbetningen av prognosdata ==================================
# ==================== dela upp på åldersgrupper samt gruppera på år ===================================

# Skapa en numerisk åldersvariabel och därefter en variabel för alla som 
# är 25-64 år
px_df$åldernum <- parse_number(px_df$ålder)


# Lägg ihop i åldersgrupper
px_df$aldergrp <- NA
px_df$aldergrp <- ifelse(px_df$åldernum < 20, "0-19 år", px_df$aldergrp)
px_df$aldergrp <- ifelse(px_df$åldernum > 19 & px_df$åldernum < 65 , "20-64 år", px_df$aldergrp)
px_df$aldergrp <- ifelse(px_df$åldernum > 64 & px_df$åldernum < 80 , "65-79 år", px_df$aldergrp)
px_df$aldergrp <- ifelse(px_df$åldernum > 79, "80+ år", px_df$aldergrp)
px_df$aldergrp <- ifelse(px_df$ålder == "totalt ålder", "totalt", px_df$aldergrp)

# Gruppera för aktuellt år
dfmalar <- px_df %>% 
  filter(år == malar | år %in% malar_jmfr) %>% 
  group_by(år, regionkod, region, aldergrp) %>% 
  summarize(antal = sum(Folkmängd)) 

#nyrad <- dfmalar[1,] 
#nyrad$aldergrp <- "totalt"
#nyrad$antal <- sum(dfmalar$antal)
#dfmalar <- rbind(dfmalar, nyrad)

total_df <- dfmalar %>% 
  group_by(år, region) %>% 
  summarize(regionkod = first(regionkod),
            # region = first(region),
    antal = sum(antal))

dfmalar <- rbind(dfmalar, total_df) %>% 
  arrange(år)

dfmalar$aldergrp[is.na(dfmalar$aldergrp)] <- "totalt"

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
AktuellRegion <- ifelse(length(unique(dfdiff$region)) == 1, dfdiff$region[1])
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
       caption = "Källa: SCB:s befolkningsprognos\nBearbetning: Peter Möller, Region Dalarna") +
       facet_wrap(~ region, scales = "free") +
  if(length(unique(dfdiff$region)) == 1){
    theme(strip.text = element_blank())
  } else {  
    theme(strip.text = element_text(color = "black"),
          strip.background = element_blank())
  }
  
#p       # Kör p om du vill rita ut plotten i Plots-fönstret nere till höger


# Ändra höjd och bredd på den sparade png-filen utifrån hur många regioner
# som är med i diagrammet, + ange mapp och filnamn
bredd <- ifelse(length(unique(dfdiff$region)) == 1, 7, 13)
hojd <- ifelse(length(unique(dfdiff$region)) == 1, 4, 8)
# och om det är fler än 20 kommuner
bredd <- ifelse(length(unique(dfdiff$region)) > 20 , 19, bredd)
hojd <- ifelse(length(unique(dfdiff$region)) > 20, 12, hojd)
fold <- "G:\\Samhällsanalys\\API\\Fran_R\\Utskrift\\"
jmfr_pre <- ifelse(JmfrFleraPrognoser == TRUE, "_jmfr", "")   # lägger till ett "_jmfr" efter namnet om flera prognoser jämförs så att man kan skilja dessa prognoser från de med en prognos i filnamnet
filnamn_pre <- paste0("Befolkningsförändring i ", AktuellRegion, " på ", jmfrtid, " års sikt", jmfr_pre)
filnamn <- paste0(filnamn_pre, ".png")
ggsave(paste0(fold,filnamn), width = bredd, height = hojd)

# Lägg till logga till diagrammet =======================================

# beskär så vi tar bort det vita fältet under legend och caption
#image_write(image_crop(image_read(fullpath),geometry = "3600x2000"), 
#            fullpath)

#plot_with_logo <- 
add_logo(
  plot_path = paste0(fold, filnamn), # url or local file for the plot
  logo_path = logga_path, # url or local file for the logo
  logo_position = "bottom right", # choose a corner
  # 'top left', 'top right', 'bottom left' or 'bottom right'
  logo_scale = 15,
  #10 as default, but can change to manually make logo bigger (lägre tal = större logga)
  replace = TRUE
)
