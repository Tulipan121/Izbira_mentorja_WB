Sys.setlocale("LC_ALL", "English_Slovenia.1250")

##devtools::install_github("lukask13/sharepointr")

## Knjižnice

library(dplyr)
library(sharepointr)
library(data.table)
library(stringi)
library(stringr)

"%!in%" <- Negate("%in%")


# Uvoz podatkov

sp_con = sharepointr::sp_connection("https://ztslo.sharepoint.com/sites/woodbadgeteam/",
                                    "blaz.zupancic@taborniki.si", "Caf46557", Office365 = T)

tabela_izbir <- sp_list(con = sp_con, listName = "Izbira_mentorjev_za_osebne_projekte") %>% 
  sp_collect(table = .) %>% data.table() %>%
  .[, .(Title, Prva_izbira, Druga_izbira, Tretja_izbira)] %>%
  data.table::setnames(., "Title", "udeleženci")

tabela_delavnic <- sp_list(con = sp_con, listName = "Ekipa v prihodnje") %>% 
  sp_collect(table = .) %>% data.table() %>%
  .[, .(ID, Title, Upokojen, `Kapaciteta za mentoriranje`)] %>%
  ##.[, Brez_prijave := 1] %>%
  data.table::setnames(., "Title", "Mentorji") %>%
  .[Mentorji == "DAVOR KRŽIŠNIK - JOLBE", Mentorji := "DAVOR KRŽIŠNIK"] %>%
  .[Mentorji == "MIHA LOTRIC", Mentorji := "MIHA LOTRIČ"] %>% 
  .[Upokojen != TRUE, ]


### Test

tabela_izbir <- sp_list(con = sp_con, listName = "Udelezenci") %>% 
  sp_collect(table = .) %>% data.table() %>%
  .[, .(`Ime in priimek`)] %>%
  data.table::setnames(., "Ime in priimek", "udeleženci") %>% 
  .[, Prva_izbira := c()] %>% 
  .[, Druga_izbira := c()] %>% 
  .[, Tretja_izbira := c()] %>% 
  .[sample(1:.N, 57)]

random_izbira <- function() {
  
  sample(tabela_delavnic$Mentorji, 3, replace = FALSE)
  
}

for (i in 1:nrow(tabela_izbir)) {
  
  a <- random_izbira()
  
  tabela_izbir$Prva_izbira[i] <- a[1]
  tabela_izbir$Druga_izbira[i] <- a[2]
  tabela_izbir$Tretja_izbira[i] <- a[3]
  
}

### KONEC TESTA


# Preveriš, če je kak mentor napačno zapisan med izbirami.

if ({
  
  a <- c(tabela_izbir$Prva_izbira, 
         tabela_izbir$Druga_izbira,
         tabela_izbir$Tretja_izbira)
  
  which(a %!in% tabela_delavnic$Mentorji) %>% 
    length(.) > 0
}){
  
  a[which(a %!in% tabela_delavnic$Mentorji)] %>% 
    print()
  
  stop()
  
}





## Funkcije

{
  
  razlika_v_rezultatu <- function(x, b, d) {
    
    rezultat <<- 0
    
    if(x %in% tabela_izbir$udeleženci[which(tabela_izbir$Prva_izbira == b)]) {
      
      rezultat <<- rezultat - 5
      
    } else if (x %in% tabela_izbir$udeleženci[which(tabela_izbir$Druga_izbira == b)]) {
      
      rezultat <<- rezultat - 3
      
    } else if (x %in% tabela_izbir$udeleženci[which(tabela_izbir$Tretja_izbira == b)]) {
      
      rezultat <<- rezultat - 1
      
    } else {
      
      rezultat <<- rezultat + 10
      
    }
    
    if(x %in% tabela_izbir$udeleženci[which(tabela_izbir$Prva_izbira == d)]) {
      
      rezultat <<- rezultat + 5
      
    } else if (x %in% tabela_izbir$udeleženci[which(tabela_izbir$Druga_izbira == d)]) {
      
      rezultat <<- rezultat + 3
      
    } else if (x %in% tabela_izbir$udeleženci[which(tabela_izbir$Tretja_izbira == d)]) {
      
      rezultat <<- rezultat + 1
      
    } else {
      
      rezultat <<- rezultat - 10
      
    }
    
    rezultat
    
  }
  
  
  rezultat_izbire <- function(x, d) {
    
    rezultat <<- 0
    
    if(x %in% tabela_izbir$udeleženci[which(tabela_izbir$Prva_izbira == d)]) {
      
      rezultat <<- rezultat + 5
      
    } else if (x %in% tabela_izbir$udeleženci[which(tabela_izbir$Druga_izbira == d)]) {
      
      rezultat <<- rezultat + 3
      
    } else if (x %in% tabela_izbir$udeleženci[which(tabela_izbir$Tretja_izbira == d)]) {
      
      rezultat <<- rezultat + 1
      
    } else {
      
      rezultat <<- rezultat - 10
      
    }
    
    rezultat
    
  }
  
  
  mentor <- function(x) {
    
    najboljsi_seznam[udeleženec == x, .(mentor)] %>% unlist
    
  }
  
  mentoriranci <- function(x) {
    
    najboljsi_seznam[mentor == x, .(udeleženec)] %>% unlist
    
  }
  
  moznost_premika <- function(x) {
    
    tabela_izbir[udeleženci == x, 2:4] %>% unlist()
    
  }
  
  zamenjava_v_seznamu <- function(x, y) {
    
    najboljsi_seznam <<- najboljsi_seznam[- which(udeleženec == x),] %>%
      rbind(., c(x, y) %>% matrix(., 1, 2), use.names = FALSE)
    
  }

}

## Razpored se naredi tako, da poskusiš najprej razporediti vse prve izbire. Če je delavnica / mentor že zaseden, gre udeleženec v drugi krog,
## kjer se razporejajo druge izbire in tako naprej, do števila izbir, ki so jih udeleženci imeli (npr. 3 ali 5 rangiranih izbir). Naredi se več
## krogov teh poskušanj in zraven beleži rezultat. Med vsemi poskusi se na koncu izbere tistega z najboljšim rezultatom.


zacasni_seznam <- matrix(NA, 1, 2) %>% data.table()
colnames(zacasni_seznam) <- c("udeleženec", "mentor")

najboljsi_rezultat <- 0

for (j in c(1:1000)){
  
  seznam <- as.list(tabela_delavnic$Mentorji)
  names(seznam) <- tabela_delavnic$Mentorji
  
  for (i in 1:length(seznam)){
    seznam[[i]] <- seznam[[i]][-1]
  }
  
  prvi_krog <- c(sample(1:length(tabela_izbir$udeleženci), length(tabela_izbir$udeleženci)))
  
  drugi_krog <- c()
  tretji_krog <- c()
  ostanek <- c()
  
  
  
  for (i in prvi_krog){
    
    ## i <- prvi_krog[1]
    
    if (is.na(tabela_izbir$Prva_izbira[i])) {
      drugi_krog <- c(drugi_krog, i)
    } else {
      
      # Preveriš, če je kak mentor napačno zapisan.
      
      #tabela_izbir$Prva_izbira[which(tabela_izbir$Prva_izbira %!in% tabela_delavnic$Mentorji)]
      
      a <- which(tabela_delavnic$Mentorji == tabela_izbir$Prva_izbira[i])
      
      if (length(seznam[[a]]) < tabela_delavnic$`Kapaciteta za mentoriranje`[a]) {
        seznam[[a]] <- c(seznam[[a]], tabela_izbir$udeleženci[i])
      } else{
        
        drugi_krog <- c(drugi_krog, i)
      }
    }
  }
  
  drugi_krog <- sample(drugi_krog,length(drugi_krog))
  
  
  
  for (i in drugi_krog){
    
    if (is.na(tabela_izbir$Druga_izbira[i])) {
      tretji_krog <- c(tretji_krog, i)
    }  else {
      
      a <- which(tabela_delavnic$Mentorji == tabela_izbir$Druga_izbira[i])
      
      if (length(seznam[[a]]) < tabela_delavnic$`Kapaciteta za mentoriranje`[a]) {
        seznam[[a]] <- c(seznam[[a]], tabela_izbir$udeleženci[i])
      } else{
        
        tretji_krog <- c(tretji_krog, i)
      }
    }
  }
  
  
  tretji_krog <- sample(tretji_krog,length(tretji_krog))
  
  for (i in tretji_krog){
    
    if (is.na(tabela_izbir$Tretja_izbira[i])) {
      ostanek <- c(ostanek, i)
    } else {
      
      a <- which(tabela_delavnic$Mentorji == tabela_izbir$Tretja_izbira[i])
      
      if (length(seznam[[a]]) < tabela_delavnic$`Kapaciteta za mentoriranje`[a]) {
        seznam[[a]] <- c(seznam[[a]], tabela_izbir$udeleženci[i])
      } else {
        
        ostanek <- c(ostanek, i)
      }
    }
  }
  
  ## Seznam_2 je pretvorba iz seznama seznamov, kjer so bili udeleženci razporejeni po mentorju, 
  ## v tabelo parov mentoriranec - mentor.
  
  seznam_2 <- matrix(NA, 0, 2) %>% data.table()
  colnames(seznam_2) <- c("udeleženec", "mentor")
  
  for (i in seq_along(seznam)) {
    for (j in seq_along(seznam[[i]])) {
      
      zacasni_seznam$udeleženec <- seznam[[i]][j]
      zacasni_seznam$mentor <- names(seznam[i])
      
      seznam_2 <- rbind(seznam_2, zacasni_seznam)
      
    }
  }
  
  for(i in ostanek) {
    
    zacasni_seznam$udeleženec <- tabela_izbir$udeleženci[i]
    zacasni_seznam$mentor <- -1
    
    seznam_2 <- rbind(seznam_2, zacasni_seznam)
    
  }
  
  rezultat <- 0
  
  seznam_2[, rezultat := mapply(rezultat_izbire, seznam_2$udeleženec, seznam_2$mentor)]
  
  rezultat <- sum(seznam_2$rezultat)
  
  
  if (rezultat > najboljsi_rezultat) {
    
    najboljsi_rezultat <- rezultat
    najboljsi_seznam <- seznam_2[, 1:2]
    ostanki <- seznam_2[mentor == "-1", .(udeleženec)] %>% unlist
    
  }
}

cat(ostanki, sep = ", ")

## Razporejanje ostankov, ki niso dobili izbire. 
## Iskanje po drevesni strukturi - pogledaš vse izbire ostankov,
## nato vse udeležence, ki so nato kdo je tam,
## ali bi oni lahko šli na prazne delavnice... Trenutno pripravljeno, da se pogleda 3 nivoje globoko - oz. se naredi 4 premike
## mentor - mentoriranec, da se umesti enega od ostankov. Pri tem se izbira premika še vedno naredi na podlagi rezultata te izbire.


system.time({
  
  for (i in ostanki) {
    
    ## Zasedenost mentorjev
    tabela_delavnic <- tabela_delavnic[, zasedenost := Mentorji %>% 
                                         lapply(., function(x) sum(najboljsi_seznam$mentor == x)) %>% 
                                         unlist()
    ][, preostala_mesta := `Kapaciteta za mentoriranje` - zasedenost]
    
    
    ## Še prazne delavnice
    se_prazne <- tabela_delavnic %>% 
      .[preostala_mesta > 0, .(Mentorji)]
    
    moznosti <- list()
    
    # i <- ostanki[3]
    
    for(j in moznost_premika(i)) {
      
      # j <- j[1] 
      
      for(k in mentoriranci(j)) {
        
        for(l in moznost_premika(k)){
          
          if(l %in% j) next() else {
            
            for(m in mentoriranci(l)){
              
              if(m %in% k) next() else {
              
                for(n in moznost_premika(m)){
                  
                  if(n %in% c(l,j)) next() else {
                  
                    for(o in mentoriranci(n)){
                      
                      if(o %in% c(m,k)) next() else {
                      
                        for(p in moznost_premika(o)){
                          
                          if(p %in% c(n,l,j)) next() else {
                          
                            for(r in mentoriranci(p)){
                              
                              if(r %in% c(o, m, k)) next() else {
                              
                                for(s in moznost_premika(r)){
                                
                                  if(s %in% c(p, n, l, j)) next() else {
                                
                                    b <- c(i,j,k,l,m,n,o,p,r,s)
                                
                                    moznosti[[length(moznosti)+1]] <- b
                                
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    
      
    ## Filtriraš samo tiste, ki vsebujejo še proste mentorje
    
    moznosti_2 <- lapply(moznosti, 
                         function(y) lapply(se_prazne, function(x) x %in% y) %>% 
                           unlist %>% 
                           any) %>% 
      unlist %>% 
      which(.) %>%
      moznosti[.]
    
    if(length(moznosti_2) == 0) {
      
      print(paste0(i, " ni možno razporediti."))
      
      next()
      
    } 
    
    ## Poiščeš element v vektorju, ki predstavlja prostega mentorja.
    ## Pobrišeš vse naslednje elemente za njim.
    
    moznosti_3 <- lapply(moznosti_2, function(y) {
      
      element <- lapply(y, function(x) x %in% c(se_prazne$Mentorji)) %>% 
        unlist %>% 
        which(.)
      
      y[1:element[length(element)]]
      
    })
    
    ## Ko pobrišeš, jih je nekaj identičnih.
    ## Izračunaš spremembno v rezultatu po premikih.
    
    moznosti_4 <- unique(moznosti_3)
    
    a <- lapply(moznosti_4, function(x) {
      
      rezultat_ostanka <<- 10
      
      if(length(x) == 2) rezultat_ostanka + rezultat_izbire(x[1], x[2])
      if(length(x) == 4) rezultat_ostanka + razlika_v_rezultatu(x[3], x[2], x[4])
      if(length(x) == 6) rezultat_ostanka + razlika_v_rezultatu(x[5], x[4], x[6])
      if(length(x) == 8) rezultat_ostanka + razlika_v_rezultatu(x[7], x[6], x[8])
      if(length(x) == 10) rezultat_ostanka + razlika_v_rezultatu(x[9], x[8], x[10])
      
    }) %>%
      unlist
    
    ## Poiščeš najboljši rezultat po menjavah.
    
    a %>% 
      max(.) %>%
      paste0("Razlika v rezultatu po zamenjavi == ", .) %>% 
      print(.)
    
    najboljsi_rezultat <<- najboljsi_rezultat + max(a)
    
    ## Izbereš samo tiste, ki imajo najboljši rezultat.
    
    moznosti_5 <- which(a == max(a)) %>% 
      moznosti_4[.]
    
    ## Če je več najboljših rezultatov, vzemaš prvega z najmanj premiki
    
    b <- moznosti_5 %>% lapply(., length) %>% unlist
    
    b <- which(b == min(b))
    
    moznost_6 <- moznosti_5[b] 
    
    moznost_7 <- moznost_6[[1]] %>% 
      print
    
    ## Premakneš glede na rezultat
    
    st_premikov <- length(moznost_7) / 2
    
    while (st_premikov > 0) {
      
      najboljsi_seznam <<- najboljsi_seznam[udeleženec == moznost_7[st_premikov * 2 - 1],
                       mentor := moznost_7[st_premikov*2]]
      
      st_premikov <- st_premikov - 1
      
    }

  }
})

## Zasedenost mentorjev
tabela_delavnic <- tabela_delavnic[, zasedenost := Mentorji %>% 
                                     lapply(., function(x) sum(najboljsi_seznam$mentor == x)) %>% 
                                     unlist()
][, preostala_mesta := `Kapaciteta za mentoriranje` - zasedenost] %>% 
  print


## Še prazne delavnice
se_prazne <- tabela_delavnic %>% 
  .[preostala_mesta > 0, .(Mentorji)] %>% 
  print


## Samo za evidenco in merjenje tičev.

# frekvenca_izbir <- tabela_izbir %>%
#   .[, .(Prva_izbira, Druga_izbira, Tretja_izbira)] %>%
#   melt.data.table(., measure.vars = c("Prva_izbira", "Druga_izbira", "Tretja_izbira")) %>%
#   setcolorder(., c("value", "variable")) %>%
#   table(.) %>% print()


## Pripis rezultata izbir mentorjev

# najboljsi_seznam[, rezultat := mapply(rezultat_izbire, 
#                                       najboljsi_seznam$udeleženec, 
#                                       najboljsi_seznam$mentor)] %>%
#   setorder(., mentor) %>% View


setwd("D:/Zveza tabornikov Slovenije/Wood Badge Team - Documents/General/Prijave")

year <- year(Sys.Date())

write.csv2(najboljsi_seznam, 
           paste0("najboljsi_seznam_WB_", year, ".csv"), 
           row.names = FALSE, fileEncoding = "UTF-8")
