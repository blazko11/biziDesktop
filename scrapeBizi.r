require(rvest)
require(dplyr)
library(RSelenium)
require(XML)

html <- read_html("http://www.bizi.si/TSMEDIA/")
dejavnosti <- html_nodes(html, "#ctl00_cphMain_divActList a")

# stevilo dejavnosti
stDejavnosti <- length(dejavnosti)

# imena dejavnosti 
imenaDejavnosti <- html_text(dejavnosti)

# linki do dejavnosti
linki <- dejavnosti %>% html_attr("href")


# nastavimo server
# 1. postavimo se v konzoli v mapo kjer se nahaja selenium-server-standalone.jar -> funkcija cd potem pa ime poti do mape
# mapa se nhahaja na C:\Users\Blaz\Documents\R\win-library\3.3\RSelenium\bin
# 2. potem pa v command promtu poženemo ukaz -> java -jar selenium-server-standalone.jar, včasih
# je potrebno napisati  java -jar selenium-server-standalone.jar -role hub -port 4445 
# 3. in na koncu še -> java -Dwebdriver.chrome.driver=chromedriver.exe
# http://stackoverflow.com/questions/6376925/using-chrome-driver-with-selenium-2
# opomba: včasih ne dela -> zakaj?

# to je kul koda -> navedemo ime bowserja chrome, ker sem zloadal chromedrive.exe, ki se nahaja na C:\Users\Blaz\Documents
#startServer()
remDr <- remoteDriver(browserName="chrome")
remDr$open()

scrapeBizi <- function(urlBizi){
#urlBizi <- "http://www.bizi.si/TSMEDIA/A/avtobusni-prevozi-386/"
#urlBizi <- "http://www.bizi.si/TSMEDIA/K/kampi-7228/"
  
  remDr$navigate(urlBizi)
  #setwd("C:/Users/Blaz/Documents")
  #tabelaSkupna <- tabela %>%  dplyr::filter(Naziv=="00000")
  #saveRDS(tabelaSkupna, file="tabelaSkupna.RDS")
  #tabelaSkupna <- readRDS("tabelaSkupna.RDS") %>% mutate(`Matična številka` = as.character(`Matična številka`))
  
  # to nam pove koliko podstrani je. Premisli: kaj pa če jih je manj (naredi neko funkcijo, ki bo preverila)
  zadnji <- c()
  for (i in 1:5){
    zadnjaPodstran <- html_nodes(urlBizi %>% read_html, paste0("#ctl00_cphMain_ResultsPager_repPager_ctl0",i,"_btnPage")) %>%  html_text() 
    zadnji <- c(zadnji, zadnjaPodstran)
  }
  # tu pogledamo katera je zadnja stran, da vemo do kam moramo it
  zadnjaPodstran <- max(zadnji %>%  as.numeric, na.rm=TRUE)
  
  if (zadnjaPodstran == 1){
    tabelaSkupna <- read_html(urlBizi) %>%
    html_nodes("#divResults") %>%
    html_table() %>% .[[1]] %>% .[2:9] %>% as.tbl %>% .[-c(4,8),] %>% mutate_each(funs(as.character))
    print(paste0("Končal s korakom št. 1 od 1."))
    return(tabelaSkupna)
    }
  
  tabelaSkupna <- read_html(urlBizi) %>%
    html_nodes("#divResults") %>%
    html_table() %>% .[[1]] %>% .[2:9] %>% as.tbl %>% .[-c(4,8),] %>% mutate_each(funs(as.character))
  
  
  # eno funkcijo napisi, max gre do 5, najmanj je pa ena, naj se sprehodi med 1 in 5 in poglejda, če obstaja max,
  
  
  
  if (zadnjaPodstran == 4)
    x <- c(2,4,5)
  if (zadnjaPodstran == 3)
    x <- c(2,4)
  if (zadnjaPodstran == 2)
    x <- 2
  if (zadnjaPodstran >=5)
    x <- c(2,4,5,6, rep(7,zadnjaPodstran-5))
  
  
  korak <- 1
  for (i in x){
    korak <- korak+1
    webElem <- remDr$findElement(using = 'css', paste0("#ctl00_cphMain_ResultsPager_repPager_ctl0",i,"_btnPage"))
    Sys.sleep(2) # naj R malo zadrema, da se stran naloži
    webElem$sendKeysToElement(list(urlBizi, key = "enter"))
    
    page_source <- remDr$getPageSource()
    #print(i)
    
    
    tabela <- 
      read_html(page_source[[1]]) %>%
      html_nodes("#divResults") %>%
      html_table() %>% .[[1]] %>% .[2:9] %>% as.tbl  %>%
      mutate_each(funs(as.character))
  #print(tabela)  
    tabelaSkupna <- bind_rows(tabelaSkupna,tabela)
    print(paste0("Končal s korakom št. ",korak," od ", zadnjaPodstran, "."))
  }
  return(tabelaSkupna)
}

# nastavimo kam naj shranjuje
setwd("C:\\Users\\Blaz\\Documents\\GitHub\\BIZI\\podatki")
for (i in 129:length(linki)){
  print(paste0("Delam dejavnost: ", imenaDejavnosti[i], ", (",i, " od ", length(linki),")."))
  tabelaDejavnost <- try(scrapeBizi(linki[i])) 
  if(inherits(tabelaDejavnost, "try-error")) next
  saveRDS(tabelaDejavnost, file=paste0(imenaDejavnosti[i],".RDS"))
}


# pogledamo, ce kak link ni bil obdelan
temp <- list.files(pattern="*.RDS")
imenaZeUvozena <- sapply(strsplit(temp,".", fixed=TRUE), FUN = function(x) x[[1]])
idx <- which(!(imenaDejavnosti %in% imenaZeUvozena))

# scrapamo vse tiste, ki jih prej ni
if (length(idx)!=0){
  k=0
  for (i in idx){
    k <- k + 1
    print(paste0("Delam dejavnost: ", imenaDejavnosti[i], ", (",k, " od ", length(idx),")."))
    tabelaDejavnost <- try(scrapeBizi(linki[i])) 
    if(inherits(tabelaDejavnost, "try-error")) next
    saveRDS(tabelaDejavnost, file=paste0(imenaDejavnosti[i],".RDS"))
  }
}


setwd("C:\\Users\\Blaz\\Documents\\GitHub\\BIZI\\podatki")
# uvozimo vse tabele
temp <- list.files(pattern="*.RDS")

for (i in 1:length(temp)) assign(paste0("tabela",i), readRDS(temp[i]))

# zdruzimo vse tabele
tabelaSkupna <- tabela1
for (i in 2:length(temp)){
  tabelaSkupna <- tabelaSkupna %>% bind_rows(paste0("tabela", i) %>%  get)
}

# shranimo tabelo
saveRDS(tabelaSkupna, file="../tabelaSkupna.RDS")
tabelaSkupna <- readRDS("../tabelaSkupna.RDS")


velikostTab <- function(urlLink){
  # funkcija vzame link in pogleda na tem linku, koliko naj bi bilo podjetij pri posamezni dejavnosti
  urlLink %>% read_html %>%  html_nodes(.,"b") %>%  html_text() %>% .[2] %>%  as.numeric
}

# tole dela precej dolgo
vrsticeTab <- sapply(X = linki, FUN = velikostTab) %>%  as.numeric
# pogledamo skupno stevilo vrstic, ki bi jih morala imeti tabelaSkupna
vrsticeTab %>%  sum

# sestavimo en data frame za primerjavo
tmp <- temp %>%  as.data.frame
tmp[,2] <- sapply(X=paste0("tabela",1:216),FUN= function(x) get(x) %>%  dim %>%  .[1]) %>% as.numeric
names(tmp) <- c("Naziv", "Dejansko") 
tmp <- tmp %>%  as.tbl
funkcija <- function(x) strsplit(x, ".RDS") %>%  unlist
tmp <- tmp %>%  mutate(Naziv = funkcija(as.character(Naziv)))

tmp2 <- imenaDejavnosti %>%  as.data.frame
tmp2[,2] <- vrsticeTab
names(tmp2) <- c("Naziv", "Realizacija") 
tmp2 <- tmp2 %>%  as.tbl %>%  arrange(.,Naziv)
tmp2 <- tmp2 %>%  mutate(Naziv = as.character(Naziv))
# samo progledamo, če je enako
#dejavnosti %>% html_attr("data-name")  %>%  .[1:10]
#imenaDejavnosti %>%  .[1:10]

# left_join ni ok, ker ima težave s šumniki
#skupaj <- tmp %>%  left_join(tmp2, by="Naziv")
skupaj <- tmp %>%  bind_cols(tmp2 %>%  select(-Naziv)) %>%  mutate(Razlika = Dejansko - Realizacija)
skupaj$Razlika %>%  sum
skupaj %>%  arrange(desc(Razlika))
