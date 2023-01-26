# Skript som anropar func_BefPrognos_syss.R för att skapa diagram och spara dem som .png-filer 

#library(httr)             # för att komma förbi brandväggen

source("G:/skript/peter/befprognos/func_BefPrognos.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

#diag_befolkningsprognos_inr_utr_fodda(region_vekt = "2062", bara_lan = TRUE, jmfrtid = 10, dataetiketter = FALSE)

diag_befolkningsprognos_inr_utr_fodda <- function(
  region_vekt = "20",                        # Välj län att göra prognoser på
  bara_lan = TRUE,                         # TRUE om bara län ska visas, FALSE för att visa länets kommuner
  AktuellRegion = NULL,                    # Till diagramrubriken - NULL om namnet ska hämtas automatiskt 
  jmfrtid = 10,                             # antal år i jämförelsen, alltså hur många års sikt vi vill titta på
  dataetiketter = TRUE,                   # om man vill ha dataetiketter vid staplarna
  utan_diagramtitel = FALSE,
  ta_med_logga = TRUE,
  skapa_fil = TRUE,                       # om man vill skriva diagrammet till en fil = TRUE, annars FALSE
  facet_scale = "free",
  output_fold = "G:/Samhällsanalys/API/Fran_R/Utskrift/",     # mapp på datorn som diagrammet skrivs till
  logga_path = "G:/Samhällsanalys/MallarLoggor/logo_liggande_fri_svart.png",       # om vi vill ha logga med, annars kan vi ta bort denna rad eller ge variabeln värdet NULL
  diagram_capt = "Källa: SCB:s befolkningsprognos\nBearbetning: Peter Möller, Region Dalarna",
  logga_storlek = 15,  
  url_tabeller = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgRegFakN"
) {
  # Här skrivs url:erna till befolkningsprognostabellerna, om flera prognoser ska jämföras så läggs de 
# som en vektor som kommer att loopas i funktionen, lägg den senaste url:en först i vektorn
# det är bara den som används om man inte jämför prognoser med varandra



# Senaste årets prognos:
#url_tabeller <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgRegFakN"

# 2020 års befolkningsprognos:


# ================================== här körs funktionen =======================================

# Och så kör vi själva funktionen
SkapaBefPrognosDiagram_InrUtrFodda(
                       aktlan = region_vekt, 
                       bara_lan = bara_lan, 
                       AktuellRegion = AktuellRegion,
                       jmfrtid = jmfrtid, 
                       url_tabeller = url_tabeller,
                       output_fold = output_fold,
                       logga_path = logga_path,
                       logga_storlek = logga_storlek,
                       dataetiketter = dataetiketter,
                       utan_diagramtitel = utan_diagramtitel,
                       facet_scale = facet_scale,
                       skapa_fil = skapa_fil,
                       ta_med_logga = ta_med_logga,
                       diagram_capt = diagram_capt)
}
