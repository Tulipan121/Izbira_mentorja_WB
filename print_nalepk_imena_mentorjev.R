Sys.setlocale("LC_ALL", "English_Slovenia.1250")

library(dplyr)
library(sharepointr)
library(data.table)
library(stringr)

sp_con = sharepointr::sp_connection("https://ztslo.sharepoint.com/sites/woodbadgeteam/",
                                    "blaz.zupancic@taborniki.si", "Caf46557", Office365 = T)

mentorji <- sp_list(con = sp_con, listName = "Ekipa v prihodnje") %>% 
  sp_collect(table = .) %>% data.table() %>%
  .[, .(ID, Title, Upokojen, `Kapaciteta za mentoriranje`)] %>%
  ##.[, Brez_prijave := 1] %>%
  data.table::setnames(., "Title", "Mentorji") %>%
  .[Mentorji == "DAVOR KRŽIŠNIK - JOLBE", Mentorji := "DAVOR KRŽIŠNIK"] %>%
  .[Mentorji == "MIHA LOTRIC", Mentorji := "MIHA LOTRIČ"] %>% 
  .[Upokojen != TRUE, ]

print_imen <- mentorji[, Mentorji]

write.csv2(print_imen, 
           paste0("print_imen_mentorjev_", year, ".csv"), 
           row.names = FALSE, fileEncoding = "UTF-8")
