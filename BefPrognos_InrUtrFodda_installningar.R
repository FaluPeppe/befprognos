# Skript som anropar func_BefPrognos_syss.R för att skapa diagram och spara dem som .png-filer 

#library(httr)             # för att komma förbi brandväggen

source("G:/Samhällsanalys/Automatisering och R/Skript/befprognos/func_BefPrognos.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

# ================================ Här gör vi inställningar för körningen ========================================
aktlan <- "20"    #c("17", "20", "21")    # Välj län att göra prognoser på
bara_lan <- FALSE                         # TRUE om bara län ska visas, FALSE för att visa länets kommuner
AktuellRegion <- NULL                     # Till diagramrubriken - NULL om namnet ska hämtas automatiskt 
jmfrtid <- 10                             # antal år i jämförelsen, alltså hur många års sikt vi vill titta på

output_fold <- "G:/Samhällsanalys/API/Fran_R/Utskrift/"     # mapp på datorn som diagrammet skrivs till
logga_path <- "G:/Samhällsanalys/MallarLoggor/logo_liggande_fri_svart.png"       # om vi vill ha logga med, annars kan vi ta bort denna rad eller ge variabeln värdet NULL
diagram_capt <- "Källa: SCB:s befolkningsprognos\nBearbetning: Peter Möller, Region Dalarna"
logga_storlek <- 15  

# Här skrivs url:erna till befolkningsprognostabellerna, om flera prognoser ska jämföras så läggs de 
# som en vektor som kommer att loopas i funktionen, lägg den senaste url:en först i vektorn
# det är bara den som används om man inte jämför prognoser med varandra

url_tabeller <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgRegFakN"

# Senaste årets prognos:
#url_tabeller <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgRegFakN"

# 2020 års befolkningsprognos:


# ================================== här körs funktionen =======================================

# Och så kör vi själva funktionen
SkapaBefPrognosDiagram_InrUtrFodda(
                       aktlan = aktlan, 
                       bara_lan = bara_lan, 
                       AktuellRegion = AktuellRegion,
                       jmfrtid = jmfrtid, 
                       url_tabeller = url_tabeller,
                       output_fold = output_fold,
                       logga_path = logga_path,
                       logga_storlek = logga_storlek,
                       diagram_capt = diagram_capt)