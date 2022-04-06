# Skript som anropar func_BefPrognos_syss.R för att skapa diagram och spara dem som .png-filer 

library(httr)             # för att komma förbi brandväggen

source("G:/skript/peter/befprognos/func_BefPrognos.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

#test_list=diag_befolkningsprognos(region_vekt = "20")

diag_befolkningsprognos <-function(region_vekt = "20", 
                                   output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/",       # mapp på datorn som diagrammet skrivs till
                                   skapa_fil = TRUE,
                                   bara_lan = TRUE,                          # TRUE om bara län ska visas, FALSE för att visa länets kommuner
                                   AktuellRegion = NULL,                     # Till diagramrubriken - NULL om namnet ska hämtas automatiskt 
                                   jmfrtid = 10,                             # antal år i jämförelsen, alltså hur många års sikt vi vill titta på
                                   JmfrFleraPrognoser = FALSE,               # TRUE om vi vill jämföra med äldre prognoser, FALSE om vi bara vill se den senaste prognosen
                                   gruppera = FALSE,                         # TRUE om vi vill lägga ihop flera regioner, FALSE om vi vill skriva ett diagram per region  
                                   anvand_senaste_befar = TRUE,              # TRUE om vi vill använda senaste tillgängliga år för befolkningsstatistik, annars används första tillgängliga befolkningsprognosår
                                   ta_bort_diagramtitel = FALSE,             # TRUE om vi vill ha diagram utan diagramtitel, annars FALSE (vilket vi brukar vilja ha)
                                   ta_med_logga = FALSE,                     # TRUE om vi vill ha med logga, annars FALSE
                                   logga_path = "G:/Samhällsanalys/MallarLoggor/logo_liggande_fri_svart.png",       # om vi vill ha logga med, annars kan vi ta bort denna rad eller ge variabeln värdet NULL
                                   diagram_capt = "Källa: SCB:s befolkningsprognos\nBearbetning: Peter Möller, Region Dalarna", 
                                   logga_storlek = 15,  
                                   dataetiketter = FALSE,                    # TRUE om vi vill skriva ut värdena i diagrammet
                                   skala_facet = "free"                     # för att välja om man vill ha "free" alla facets har egna skalor, eller "fixed" alla facets har samma skala                    
                                   ){


# Här skrivs url:erna till befolkningsprognostabellerna, om flera prognoser ska jämföras så läggs de 
# som en vektor som kommer att loopas i funktionen, lägg den senaste url:en först i vektorn
# det är bara den som används om man inte jämför prognoser med varandra

url_tabeller <- c("http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN",
                  "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401B/BefProgOsiktRegN20")

# ================================== här körs funktionen =======================================

# För att komma förbi brandvägg - om man har en sådan, annars kan man ta bort dessa två rader
set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090))
set_config(config(ssl_verifypeer = 0L))

gg_list <- list()
i <- 1

# Och så kör vi själva funktionen
gg_obj <- SkapaBefPrognosDiagram(aktlan = region_vekt, 
                       bara_lan = bara_lan, 
                       AktuellRegion = AktuellRegion,
                       jmfrtid = jmfrtid,
                       gruppera_ihop = gruppera,
                       JmfrFleraPrognoser = JmfrFleraPrognoser,
                       url_tabeller = url_tabeller,
                       facet_scale = skala_facet,
                       output_fold = output_mapp,
                       logga_path = logga_path,
                       logga_storlek = logga_storlek,
                       ta_med_logga = ta_med_logga,
                       anvand_senaste_befar = anvand_senaste_befar,
                       dataetiketter = dataetiketter,
                       utan_diagramtitel = ta_bort_diagramtitel)

gg_list[[i]] <-gg_obj
names(gg_list) <- "Befolkningsprognos"

return(gg_list)
}
