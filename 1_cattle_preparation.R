##########################################
#### CATTLE AND FARM DATA PREPARATION ####
##########################################

# code by Raphael Ganzenmueller (LMU, CIAT)
# mail: ganzenmueller.r@posteo.de
# date: Feb 2020

#=====================================================
# libraries
library(readxl)
library(tidyverse)

# set working directory
setwd("~/colombia/publication_analysis")

#======================================================
# define percentage for automatic cattle and farm correction
#======================================================

# cattle and farm correction values (e.g. if <=-25% in one and >=+25% in the following year, value will be corrected)
cerr_decr = -25
cerr_incr = 25
ferr_decr = -25
ferr_incr = 25

#=====================================================
# load data
#=====================================================

# load municipality list
mun_list <- readRDS("output/data/mun_list.rds")

# load fedegan cattle data
data_fedegan <- read.csv("data/fedgan_ganadero_2001-2015.csv", fileEncoding = "ISO-8859-1")

# load cia cattle and farm data
data_cia08 <- read_excel("data/Respuesta Censo Bovinos por Municipio 2008 - 2018.xlsx", skip = 4, sheet = "Bovinos_2008")
data_cia09 <- read_excel("data/Respuesta Censo Bovinos por Municipio 2008 - 2018.xlsx", skip = 4, sheet = "Bovinos_2009")
data_cia10 <- read_excel("data/Respuesta Censo Bovinos por Municipio 2008 - 2018.xlsx", skip = 4, sheet = "Bovinos_2010")
data_cia11 <- read_excel("data/Respuesta Censo Bovinos por Municipio 2008 - 2018.xlsx", skip = 4, sheet = "Bovinos_2011")
data_cia12 <- read_excel("data/Respuesta Censo Bovinos por Municipio 2008 - 2018.xlsx", skip = 4, sheet = "Bovinos_2012")
data_cia13 <- read_excel("data/Respuesta Censo Bovinos por Municipio 2008 - 2018.xlsx", skip = 4, sheet = "Bovinos_2013")
data_cia14 <- read_excel("data/Respuesta Censo Bovinos por Municipio 2008 - 2018.xlsx", skip = 4, sheet = "Bovinos_2014", na = "-")
data_cia15 <- read_excel("data/Respuesta Censo Bovinos por Municipio 2008 - 2018.xlsx", skip = 4, sheet = "Bovinos_2015", na = "-")
data_cia16 <- read_excel("data/Respuesta Censo Bovinos por Municipio 2008 - 2018.xlsx", skip = 4, sheet = "Bovinos_2016", na = "-")
data_cia17 <- read_excel("data/Respuesta Censo Bovinos por Municipio 2008 - 2018.xlsx", skip = 4, sheet = "Bovinos_2017", na = "-")
data_cia18 <- read_excel("data/Respuesta Censo Bovinos por Municipio 2008 - 2018.xlsx", skip = 4, sheet = "Bovinos_2018", na = "-")

#=====================================================
# fedegan cattle data
#=====================================================

# prepare fedgan data
df_fedegan <- data_fedegan %>%
  transmute(dpt = as.character(dpt), other = as.character(`Fecha.Total.Bovinos.`)) %>%
  separate(other, into = c("mun", "year", "cattle", "end") ,sep = "[;]") %>%
  transmute(dpt = iconv(dpt, to='ASCII//TRANSLIT'), 
            mun = iconv(substr(mun, 2, 100), to='ASCII//TRANSLIT'),
            year = paste("fedegan", substr(year, 3,4), sep = ""),
            cattle = as.numeric(cattle)) %>%
  pivot_wider(id_cols = c(dpt, mun), names_from = year, values_from = cattle) 

# municipalities without id before fixing
df_fedegan %>%
  full_join(mun_list, by=c("dpt", "mun")) %>%
  filter(is.na(id))

# fixing dpt and mun names
df_fedegan_namefix <- df_fedegan %>%
  mutate(mun = ifelse(dpt == "ANTIOQUIA" & mun == "CARMEN DE VIBORAL", "EL CARMEN DE VIBORAL", mun),
         mun = ifelse(dpt == "ANTIOQUIA" & mun == "DON MATIAS", "DONMATIAS", mun),
         mun = ifelse(dpt == "ANTIOQUIA" & mun == "EL PENOL", "PENOL", mun),
         mun = ifelse(dpt == "ANTIOQUIA" & mun == "EL RETIRO", "RETIRO", mun),
         mun = ifelse(dpt == "ANTIOQUIA" & mun == "PUEBLO RICO", "PUEBLORRICO", mun),
         mun = ifelse(dpt == "ANTIOQUIA" & mun == "SAN ANDRES", "SAN ANDRES DE CUERQUIA", mun),
         mun = ifelse(dpt == "ANTIOQUIA" & mun == "SAN VICENTE", "SAN VICENTE FERRER", mun),
         mun = ifelse(dpt == "ANTIOQUIA" & mun == "SANTAFE DE ANTIOQUIA", "SANTA FE DE ANTIOQUIA", mun),
         mun = ifelse(dpt == "ANTIOQUIA" & mun == "SANTUARIO", "EL SANTUARIO", mun),
         mun = ifelse(dpt == "ARAUCA" & mun == "CRAVONORTE", "CRAVO NORTE", mun),
         dpt = ifelse(dpt == "BOGOTA DC", "BOGOTA, D.C.", dpt),
         mun = ifelse(mun == "BOGOTA DC", "BOGOTA, D.C.", mun),
         mun = ifelse(dpt == "BOLIVAR" & mun == "ARROYO HONDO", "ARROYOHONDO", mun),
         mun = ifelse(dpt == "BOLIVAR" & mun == "CARMEN DE BOLIVAR", "EL CARMEN DE BOLIVAR", mun),
         mun = ifelse(dpt == "BOLIVAR" & mun == "CARTAGENA", "CARTAGENA DE INDIAS", mun),
         mun = ifelse(dpt == "BOLIVAR" & mun == "MOMPOX", "MOMPOS", mun),
         dpt = ifelse(dpt == "ARCHIPIELAGO DE SAN ANDRES", "ARCHIPIELAGO DE SAN ANDRES, PROVIDENCIA Y SANTA CATALINA", dpt),
         mun = ifelse(dpt == "BOYACA" & mun == "PAZ DEL RIO", "PAZ DE RIO", mun),
         mun = ifelse(dpt == "BOYACA" & mun == "SANTAMARIA", "SANTA MARIA", mun),
         mun = ifelse(dpt == "BOYACA" & mun == "SATIVA NORTE", "SATIVANORTE", mun),
         mun = ifelse(dpt == "BOYACA" & mun == "SATIVA SUR", "SATIVASUR", mun),
         mun = ifelse(dpt == "BOYACA" & mun == "GUICAN", "GUICAN DE LA SIERRA", mun),
         mun = ifelse(dpt == "CAQUETA" & mun == "SAN JOSE DE FRAGUA", "SAN JOSE DEL FRAGUA", mun),
         mun = ifelse(dpt == "CASANARE" & mun == "SANLUIS DE PALENQUE", "SAN LUIS DE PALENQUE", mun),
         mun = ifelse(dpt == "CAUCA" & mun == "PIENDAMO", "PIENDAMO - TUNIA", mun),
         mun = ifelse(dpt == "CAUCA" & mun == "SOTARA", "SOTARA - PAISPAMBA", mun),
         mun = ifelse(dpt == "CAUCA" & mun == "VILLARICA", "VILLA RICA", mun),
         mun = ifelse(dpt == "CESAR" & mun == "MANAURE", "MANAURE BALCON DEL CESAR", mun),
         mun = ifelse(dpt == "CORDOBA" & mun == "PURISIMA", "PURISIMA DE LA CONCEPCION", mun),
         mun = ifelse(dpt == "CHOCO" & mun == "EL CANTON DE SAN PABLO", "EL CANTON DEL SAN PABLO", mun),
         mun = ifelse(dpt == "CHOCO" & mun == "EL LITORAL DE SAN JUAN", "EL LITORAL DEL SAN JUAN", mun),
         mun = ifelse(dpt == "CHOCO" & mun == "ITSMINA", "ISTMINA", mun),
         mun = ifelse(dpt == "CHOCO" & mun == "UNGIA", "UNGUIA", mun),
         mun = ifelse(dpt == "CUNDINAMARCA" & mun == "QUEBRADA NEGRA", "QUEBRADANEGRA", mun),
         mun = ifelse(dpt == "CUNDINAMARCA" & mun == "SAN ANTONIO DE TEQUENDAMA", "SAN ANTONIO DEL TEQUENDAMA", mun),
         mun = ifelse(dpt == "CUNDINAMARCA" & mun == "UBATE", "VILLA DE SAN DIEGO DE UBATE", mun),
         mun = ifelse(dpt == "GUAVIARE" & mun == "MIRAFLOREZ", "MIRAFLORES", mun),
         mun = ifelse(dpt == "MAGDALENA" & mun == "PUEBLO VIEJO", "PUEBLOVIEJO", mun),
         mun = ifelse(dpt == "META" & mun == "VISTA HERMOSA", "VISTAHERMOSA", mun),
         mun = ifelse(dpt == "NARINO" & mun == "CUASPUD", "CUASPUD CARLOSAMA", mun),
         mun = ifelse(dpt == "NARINO" & mun == "TUMACO", "SAN ANDRES DE TUMACO", mun),
         mun = ifelse(dpt == "NORTE DE SANTANDER" & mun == "CUCUTA", "SAN JOSE DE CUCUTA", mun),
         mun = ifelse(dpt == "NORTE DE SANTANDER" & mun == "VILLACARO", "VILLA CARO", mun),
         mun = ifelse(dpt == "SANTANDER" & mun == "GUACAMAYO", "EL GUACAMAYO", mun),
         mun = ifelse(dpt == "SANTANDER" & mun == "GUACAMAYO", "EL GUACAMAYO", mun),
         mun = ifelse(dpt == "SANTANDER" & mun == "PLAYON", "EL PLAYON", mun),
         mun = ifelse(dpt == "SUCRE" & mun == "SINCE", "SAN LUIS DE SINCE", mun),
         mun = ifelse(dpt == "SUCRE" & mun == "TOLUVIEJO", "SAN JOSE DE TOLUVIEJO", mun),
         mun = ifelse(dpt == "TOLIMA" & mun == "ARMERO-GUAYABAL", "ARMERO", mun),
         mun = ifelse(dpt == "TOLIMA" & mun == "MARIQUITA", "SAN SEBASTIAN DE MARIQUITA", mun),
         mun = ifelse(dpt == "VALLE DEL CAUCA" & mun == "BUGA", "GUADALAJARA DE BUGA", mun)
  )

# municipalities without id after fixing
mun_list %>%
  right_join(df_fedegan_namefix, by=c("dpt", "mun")) %>%
  filter(is.na(id)) %>%
  select(dpt, mun, id) # one fedegan mun entry can#t be attributed (CORREGIMIENTOS)

# municipalities without fedegan data
anti_join(mun_list, df_fedegan_namefix, by=c("dpt", "mun"))

# fedegan data with ids
df_fedegan_namefixed <- mun_list %>%
  left_join(df_fedegan_namefix, by=c("dpt", "mun")) %>%
  select(id, dpt, mun, fedegan01, fedegan02, fedegan03, fedegan04, fedegan05, fedegan06, fedegan07, 
         fedegan08, fedegan09, fedegan10, fedegan11, fedegan12, fedegan13, fedegan14, fedegan15)

# df with original fedegan data
df_fedegan_orig <- df_fedegan_namefixed %>%
  pivot_longer(cols=-c(id, dpt, mun)) %>%
  transmute(id, year = as.numeric(paste("20", substr(name, 8, 9), sep = "")), fedegan_orig = value)

# correct fedegan data
df_fedegan_cor <- df_fedegan_namefixed %>%
  select(id, starts_with("fedegan")) %>%
  mutate(diff0102 = (fedegan02-fedegan01)/fedegan01*100,
         diff0203 = (fedegan03-fedegan02)/fedegan02*100,
         diff0304 = (fedegan04-fedegan03)/fedegan03*100,
         diff0405 = (fedegan05-fedegan04)/fedegan04*100,
         diff0506 = (fedegan06-fedegan05)/fedegan05*100,
         diff0607 = (fedegan07-fedegan06)/fedegan06*100,
         diff0708 = (fedegan08-fedegan07)/fedegan07*100,
         diff0809 = (fedegan09-fedegan08)/fedegan08*100,
         diff0910 = (fedegan10-fedegan09)/fedegan09*100,
         diff1011 = (fedegan11-fedegan10)/fedegan10*100,
#         diff1112 = (fedegan12-fedegan11)/fedegan11*100,
#         diff1213 = (fedegan13-fedegan12)/fedegan12*100,
#         diff1314 = (fedegan14-fedegan13)/fedegan13*100,
#         diff1415 = (fedegan15-fedegan14)/fedegan14*100,
         cor01 = fedegan01,
         cor02 = ifelse((diff0102 <= cerr_decr & diff0203 >= cerr_incr) | (diff0102 >= cerr_incr & diff0203 <= cerr_decr), (fedegan01+fedegan03)/2, fedegan02), 
         cor03 = ifelse((diff0203 <= cerr_decr & diff0304 >= cerr_incr) | (diff0203 >= cerr_incr & diff0304 <= cerr_decr), (fedegan02+fedegan04)/2, fedegan03), 
         cor04 = ifelse((diff0304 <= cerr_decr & diff0405 >= cerr_incr) | (diff0304 >= cerr_incr & diff0405 <= cerr_decr), (fedegan03+fedegan05)/2, fedegan04), 
         cor05 = ifelse((diff0405 <= cerr_decr & diff0506 >= cerr_incr) | (diff0405 >= cerr_incr & diff0506 <= cerr_decr), (fedegan04+fedegan06)/2, fedegan05), 
         cor06 = ifelse((diff0506 <= cerr_decr & diff0607 >= cerr_incr) | (diff0506 >= cerr_incr & diff0607 <= cerr_decr), (fedegan05+fedegan07)/2, fedegan06), 
         cor07 = ifelse((diff0607 <= cerr_decr & diff0708 >= cerr_incr) | (diff0607 >= cerr_incr & diff0708 <= cerr_decr), (fedegan06+fedegan08)/2, fedegan07), 
         cor08 = ifelse((diff0708 <= cerr_decr & diff0809 >= cerr_incr) | (diff0708 >= cerr_incr & diff0809 <= cerr_decr), (fedegan07+fedegan09)/2, fedegan08), 
         cor09 = ifelse((diff0809 <= cerr_decr & diff0910 >= cerr_incr) | (diff0809 >= cerr_incr & diff0910 <= cerr_decr), (fedegan08+fedegan10)/2, fedegan09), 
         cor10 = ifelse((diff0910 <= cerr_decr & diff1011 >= cerr_incr) | (diff0910 >= cerr_incr & diff1011 <= cerr_decr), (fedegan09+fedegan11)/2, fedegan10),
         cor11 = fedegan11) %>%
#         cor11 = ifelse((diff1011 <= cerr_decr & diff1112 >= cerr_incr) | (diff1011 >= cerr_incr & diff1112 <= cerr_decr), (fedegan10+fedegan12)/2, fedegan11),
#         cor12 = ifelse((diff1112 <= cerr_decr & diff1213 >= cerr_incr) | (diff1112 >= cerr_incr & diff1213 <= cerr_decr), (fedegan11+fedegan13)/2, fedegan12),
#         cor13 = ifelse((diff1213 <= cerr_decr & diff1314 >= cerr_incr) | (diff1213 >= cerr_incr & diff1314 <= cerr_decr), (fedegan12+fedegan14)/2, fedegan13),
#         cor14 = ifelse((diff1314 <= cerr_decr & diff1415 >= cerr_incr) | (diff1314 >= cerr_incr & diff1415 <= cerr_decr), (fedegan13+fedegan15)/2, fedegan14),
#         cor15 = fedegan15) %>%
  select(id, starts_with("cor")) %>%
  pivot_longer(-id, names_to="cor_year", values_to = "fedegan") %>%
  transmute(id, year=as.numeric(paste(20, substr(cor_year, 4, 5), sep="")), fedegan)

#=====================================================
# merge cia files
#=====================================================

# merge cia cattle and farm data
df_cia <- mun_list %>%
  left_join(data_cia08 %>% select(id=CODIGO_MUNICIPIO, cttl08='TOTAL BOVINOS', frms08='TOTAL PREDIOS'), by="id") %>%
  left_join(data_cia09 %>% select(id=CODIGO_MUNICIPIO, cttl09='TOTAL BOVINOS', frms09='TOTAL FINCAS CON BOVINOS'), by="id") %>%
  left_join(data_cia10 %>% select(id=CODIGO_MUNICIPIO, cttl10='TOTAL BOVINOS', frms10='TOTAL FINCAS CON BOVINOS'), by="id") %>%
  left_join(data_cia11 %>% select(id=CODIGO_MUNICIPIO, cttl11='TOTAL BOVINOS', frms11='TOTAL FINCAS CON BOVINOS'), by="id") %>%
  left_join(data_cia12 %>% select(id=CODIGO_MUNICIPIO, cttl12='TOTAL BOVINOS', frms12='TOTAL FINCAS CON BOVINOS'), by="id") %>%
  left_join(data_cia13 %>% select(id=CODIGO_MUNICIPIO, cttl13='TOTAL BOVINOS', frms13='TOTAL FINCAS CON BOVINOS'), by="id") %>%
  left_join(data_cia14 %>% select(id=CODIGO_MUNICIPIO, cttl14='TOTAL BOVINOS', frms14='TOTAL FINCAS CON BOVINOS'), by="id") %>%
  left_join(data_cia15 %>% select(id=CODIGO_MUNICIPIO, cttl15='TOTAL BOVINOS', frms15='TOTAL FINCAS CON BOVINOS'), by="id") %>%
  left_join(data_cia16 %>% select(id=CODIGO_MUNICIPIO, cttl16='TOTAL BOVINOS', frms16='TOTAL FINCAS CON BOVINOS'), by="id") %>%
  left_join(data_cia17 %>% select(id=CODIGO_MUNICIPIO, cttl17='TOTAL BOVINOS', frms17='TOTAL FINCAS CON BOVINOS'), by="id") %>%
  left_join(data_cia18 %>% select(id=CODIGO_MUNICIPIO, cttl18='TOTAL BOVINOS', frms18='TOTAL FINCAS CON BOVINOS'), by="id")

#=====================================================
# prepare cia cattle data
#=====================================================

# df with original cia cattle data
df_cia_cttl_orig <- df_cia %>%
  pivot_longer(cols=-c(id, dpt, mun)) %>%
  filter(startsWith(name, "cttl")) %>%
  transmute(id, year = as.numeric(paste("20", substr(name, 5, 6), sep = "")), cia_cttl_orig = value)

# correct cia cattle data
df_cia_cttl_cor <- df_cia %>%
  select(id, starts_with("cttl")) %>%
  mutate(diff0809 = (cttl09-cttl08)/cttl08*100,
         diff0910 = (cttl10-cttl09)/cttl09*100,
         diff1011 = (cttl11-cttl10)/cttl10*100,
         diff1112 = (cttl12-cttl11)/cttl11*100,
         diff1213 = (cttl13-cttl12)/cttl12*100,
         diff1314 = (cttl14-cttl13)/cttl13*100,
         diff1415 = (cttl15-cttl14)/cttl14*100,
         diff1516 = (cttl16-cttl15)/cttl15*100,
         diff1617 = (cttl17-cttl16)/cttl16*100,
         diff1718 = (cttl18-cttl17)/cttl17*100,
         cor08 = cttl08,
         cor09 = ifelse((diff0809 <= cerr_decr & diff0910 >= cerr_incr) | (diff0809 >= cerr_incr & diff0910 <= cerr_decr), (cttl08+cttl10)/2, cttl09), 
         cor10 = ifelse((diff0910 <= cerr_decr & diff1011 >= cerr_incr) | (diff0910 >= cerr_incr & diff1011 <= cerr_decr), (cttl09+cttl11)/2, cttl10),
         cor11 = ifelse((diff1011 <= cerr_decr & diff1112 >= cerr_incr) | (diff1011 >= cerr_incr & diff1112 <= cerr_decr), (cttl10+cttl12)/2, cttl11),
         cor12 = ifelse((diff1112 <= cerr_decr & diff1213 >= cerr_incr) | (diff1112 >= cerr_incr & diff1213 <= cerr_decr), (cttl11+cttl13)/2, cttl12), 
         cor13 = ifelse((diff1213 <= cerr_decr & diff1314 >= cerr_incr) | (diff1213 >= cerr_incr & diff1314 <= cerr_decr), (cttl12+cttl14)/2, cttl13), 
         cor14 = ifelse((diff1314 <= cerr_decr & diff1415 >= cerr_incr) | (diff1314 >= cerr_incr & diff1415 <= cerr_decr), (cttl13+cttl15)/2, cttl14), 
         cor15 = ifelse((diff1415 <= cerr_decr & diff1516 >= cerr_incr) | (diff1415 >= cerr_incr & diff1516 <= cerr_decr), (cttl14+cttl16)/2, cttl15),
         cor16 = ifelse((diff1516 <= cerr_decr & diff1617 >= cerr_incr) | (diff1516 >= cerr_incr & diff1617 <= cerr_decr), (cttl15+cttl17)/2, cttl16), 
         cor17 = ifelse((diff1617 <= cerr_decr & diff1718 >= cerr_incr) | (diff1617 >= cerr_incr & diff1718 <= cerr_decr), (cttl16+cttl18)/2, cttl17),
         cor18 = cttl18) %>%
  select(id, starts_with("cor")) %>%
  pivot_longer(-id, names_to="cor_year", values_to = "cia_cttl") %>%
  transmute(id, year=as.numeric(paste(20, substr(cor_year, 4, 5), sep="")), cia_cttl)

#======================================================
# prepare combined and corrected cattle data
#======================================================

# merge fedegan and cia 
df_cttl <- df_fedegan_cor %>%
  full_join(df_cia_cttl_cor, by=c("id", "year")) %>%
  mutate(cttl = case_when(is.na(fedegan) & is.na(cia_cttl) ~ NA_real_,
                          is.na(fedegan) & !is.na(cia_cttl) ~ cia_cttl,
                          !is.na(fedegan) & is.na(cia_cttl) ~ fedegan,
                          !is.na(fedegan) & !is.na(cia_cttl) ~ (cia_cttl+fedegan)/2)) %>%
  pivot_wider(id_cols = id, values_from = cttl, names_from = year, names_prefix = "y") %>%
  mutate(y2002 = ifelse(id == "99773", (y2001 + y2004)/2, y2002),
         y2003 = ifelse(id == "99773", (y2001 + y2004)/2, y2003),
         y2017 = ifelse(id == "99773", (y2016 + y2018)/2, y2017),
         y2014 = ifelse(id == "99773", (y2013 + y2015)/2, y2014),
         y2015 = ifelse(id == "97001", (y2013 + y2016)/2, y2015),
         y2014 = ifelse(id == "97001", (y2013 + y2016)/2, y2014),
         y2012 = ifelse(id == "94001", (y2010 + y2013)/2, y2012),
         y2011 = ifelse(id == "94001", (y2010 + y2013)/2, y2011),
         y2016 = ifelse(id == "91669", (y2015 + y2017)/2, y2016),
         y2012 = ifelse(id == "91263", (y2011 + y2014)/2, y2012),
         y2013 = ifelse(id == "91263", (y2011 + y2014)/2, y2013),
         y2017 = ifelse(id == "73504", (y2015 + y2018)/2, y2017),
         y2017 = ifelse(id == "73873", (y2015 + y2018)/2, y2017),
         y2016 = ifelse(id == "73873", (y2015 + y2018)/2, y2016),
         y2016 = ifelse(id == "73870", filter(df_cia_cttl_orig, id=="73870" & year==2016)$cia_cttl_orig, y2016),
         y2003 = ifelse(id == "73686", filter(df_fedegan_orig, id=="73686" & year==2003)$fedegan_orig, y2003),
         y2002 = ifelse(id == "73624", (y2001 + y2003)/2, y2002),
         y2003 = ifelse(id == "73616", (y2002 + y2004)/2, y2003),
         y2013 = ifelse(id == "73411", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "73411", (y2011 + y2014)/2, y2012),
         y2013 = ifelse(id == "73408", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "73408", (y2011 + y2014)/2, y2012),
         y2017 = ifelse(id == "73283", (y2016 + y2018)/2, y2017),
         y2012 = ifelse(id == "70771", filter(df_cia_cttl_orig, id=="70771" & year==2012)$cia_cttl_orig, y2012),
         y2007 = ifelse(id == "70678", (y2003 + y2008)/2, y2007),
         y2006 = ifelse(id == "70678", (y2003 + y2008)/2, y2006),
         y2005 = ifelse(id == "70678", (y2003 + y2008)/2, y2005),
         y2004 = ifelse(id == "70678", (y2003 + y2008)/2, y2004),
         y2012 = ifelse(id == "68895", (y2010 + y2013)/2, y2012),
         y2011 = ifelse(id == "68895", (y2010 + y2013)/2, y2011),
         y2014 = ifelse(id == "68872", (y2012 + y2015)/2, y2014),
         y2013 = ifelse(id == "68872", (y2012 + y2015)/2, y2013),
         y2002 = ifelse(id == "68867", filter(df_fedegan_orig, id=="68867" & year==2002)$fedegan_orig, y2002),
         y2012 = ifelse(id == "68689", (y2010 + y2013)/2, y2012),
         y2011 = ifelse(id == "68689", (y2010 + y2013)/2, y2011),
         y2014 = ifelse(id == "68682", (y2013 + y2015)/2, y2014),
         y2014 = ifelse(id == "68572", (y2012 + y2015)/2, y2014),
         y2013 = ifelse(id == "68572", (y2012 + y2015)/2, y2013),
         y2014 = ifelse(id == "68418", (y2012 + y2015)/2, y2014),
         y2013 = ifelse(id == "68418", (y2012 + y2015)/2, y2013),
         y2006 = ifelse(id == "68234", (y2005 + y2007)/2, y2006),
         y2007 = ifelse(id == "68160", (y2006 + y2008)/2, y2007),
         y2009 = ifelse(id == "68152", (y2007 + y2010)/2, y2009),
         y2008 = ifelse(id == "68152", (y2007 + y2010)/2, y2008),
         y2009 = ifelse(id == "68081", (y2008 + y2010)/2, y2009),
         y2014 = ifelse(id == "68079", (y2012 + y2015)/2, y2014),
         y2013 = ifelse(id == "68079", (y2012 + y2015)/2, y2013),
         y2014 = ifelse(id == "68013", (y2012 + y2015)/2, y2014),
         y2013 = ifelse(id == "68013", (y2012 + y2015)/2, y2013),
         y2017 = ifelse(id == "68013", filter(df_cia_cttl_orig, id=="68013" & year==2017)$cia_cttl_orig, y2017),
         y2017 = ifelse(id == "63302", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "63272", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "54820", (y2016 + y2018)/2, y2017),
         y2002 = ifelse(id == "54820", filter(df_fedegan_orig, id=="54820" & year==2002)$fedegan_orig, y2002),
         y2002 = ifelse(id == "54800", filter(df_fedegan_orig, id=="54800" & year==2002)$fedegan_orig, y2002),
         y2016 = ifelse(id == "54480", (y2015 + y2017)/2, y2016),
         y2008 = ifelse(id == "54261", (y2007 + y2009)/2, y2008),
         y2011 = ifelse(id == "54250", (y2010 + y2012)/2, y2011),
         y2002 = ifelse(id == "54206", filter(df_fedegan_orig, id=="54206" & year==2002)$fedegan_orig, y2002),
         y2016 = ifelse(id == "54128", (y2015 + y2017)/2, y2016),
         y2006 = ifelse(id == "52693", (y2004 + y2007)/2, y2006),
         y2005 = ifelse(id == "52693", (y2004 + y2007)/2, y2005),
         y2017 = ifelse(id == "52687", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "52612", (y2016 + y2018)/2, y2017),
         y2013 = ifelse(id == "52490", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "52490", (y2011 + y2014)/2, y2012),
         y2013 = ifelse(id == "52473", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "52473", (y2011 + y2014)/2, y2012),
         y2002 = ifelse(id == "52378", (y2001 + y2003)/2, y2002),
         y2017 = ifelse(id == "52320", (y2016 + y2018)/2, y2017),
         y2013 = ifelse(id == "52250", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "52250", (y2011 + y2014)/2, y2012),
         y2016 = ifelse(id == "52083", (y2015 + y2017)/2, y2016),
         y2017 = ifelse(id == "52079", (y2015 + y2018)/2, y2017),
         y2016 = ifelse(id == "52079", (y2015 + y2018)/2, y2016),
         y2013 = ifelse(id == "52079", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "52079", (y2011 + y2014)/2, y2012),
         y2017 = ifelse(id == "50683", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "50226", (y2016 + y2018)/2, y2017),
         y2016 = ifelse(id == "47745", (y2015 + y2017)/2, y2016),
         y2016 = ifelse(id == "47570", filter(df_cia_cttl_orig, id=="47570" & year==2016)$cia_cttl_orig, y2016),
         y2016 = ifelse(id == "47161", filter(df_cia_cttl_orig, id=="47161" & year==2016)$cia_cttl_orig, y2016),
         y2017 = ifelse(id == "41885", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "41801", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "41020", (y2016 + y2018)/2, y2017),
         y2008 = ifelse(id == "27250", (y2007 + y2009)/2, y2008),
         y2012 = ifelse(id == "27205", (y2011 + y2013)/2, y2012),
         y2011 = ifelse(id == "27160", (y2010 + y2014)/2, y2011),
         y2012 = ifelse(id == "27160", (y2010 + y2014)/2, y2012),
         y2013 = ifelse(id == "27160", (y2010 + y2014)/2, y2013),
         y2014 = ifelse(id == "27160", filter(df_cia_cttl_orig, id=="27160" & year==2014)$cia_cttl_orig, y2014),
         y2012 = ifelse(id == "27001", (y2011 + y2013)/2, y2012),
         y2007 = ifelse(id == "25845", (y2006 + y2008)/2, y2007),
         y2017 = ifelse(id == "25843", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "25839", (y2015 + y2018)/2, y2017),
         y2016 = ifelse(id == "25839", (y2015 + y2018)/2, y2016),
         y2013 = ifelse(id == "25839", (y2007 + y2014)/2, y2013),
         y2012 = ifelse(id == "25839", (y2007 + y2014)/2, y2012),
         y2011 = ifelse(id == "25839", (y2007 + y2014)/2, y2011),
         y2010 = ifelse(id == "25839", (y2007 + y2014)/2, y2010),
         y2009 = ifelse(id == "25839", (y2007 + y2014)/2, y2009),
         y2008 = ifelse(id == "25839", (y2007 + y2014)/2, y2008),
         y2017 = ifelse(id == "25815", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "25781", (y2015 + y2018)/2, y2017),
         y2016 = ifelse(id == "25781", (y2015 + y2018)/2, y2016),
         y2017 = ifelse(id == "25779", (y2015 + y2018)/2, y2017),
         y2016 = ifelse(id == "25779", (y2015 + y2018)/2, y2016),
         y2013 = ifelse(id == "25535", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "25535", (y2011 + y2014)/2, y2012),
         y2012 = ifelse(id == "25473", (y2011 + y2013)/2, y2012),
         y2017 = ifelse(id == "25407", filter(df_cia_cttl_orig, id=="25407" & year==2017)$cia_cttl_orig, y2017),
         y2009 = ifelse(id == "25372", (y2007 + y2010)/2, y2009),
         y2008 = ifelse(id == "25372", (y2007 + y2010)/2, y2008),
         y2017 = ifelse(id == "25368", (y2016 + y2018)/2, y2017),
         y2007 = ifelse(id == "25339", (y2006 + y2008)/2, y2007),
         y2013 = ifelse(id == "25297", (y2010 + y2014)/2, y2013),
         y2012 = ifelse(id == "25297", (y2010 + y2014)/2, y2012),
         y2011 = ifelse(id == "25297", (y2010 + y2014)/2, y2011),
         y2013 = ifelse(id == "25295", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "25295", (y2011 + y2014)/2, y2012),
         y2013 = ifelse(id == "25290", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "25290", (y2011 + y2014)/2, y2012),
         y2016 = ifelse(id == "25288", (y2015 + y2017)/2, y2016),
         y2017 = ifelse(id == "25224", (y2015 + y2018)/2, y2017),
         y2016 = ifelse(id == "25224", (y2015 + y2018)/2, y2016),
         y2012 = ifelse(id == "25214", (y2011 + y2013)/2, y2012),
         y2013 = ifelse(id == "25151", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "25151", (y2011 + y2014)/2, y2012),
         y2016 = ifelse(id == "23815", filter(df_cia_cttl_orig, id=="23815" & year==2016)$cia_cttl_orig, y2016),
         y2016 = ifelse(id == "23682", filter(df_cia_cttl_orig, id=="23682" & year==2016)$cia_cttl_orig, y2016),
         y2017 = ifelse(id == "23682", (y2016 + y2018)/2, y2017),
         y2013 = ifelse(id == "19809", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "19809", (y2011 + y2014)/2, y2012),
         y2017 = ifelse(id == "19693", (y2016 + y2018)/2, y2017),
         y2013 = ifelse(id == "19418", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "19418", (y2011 + y2014)/2, y2012),
         y2013 = ifelse(id == "19318", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "19318", (y2011 + y2014)/2, y2012),
         y2015 = ifelse(id == "19022", (y2014 + y2016)/2, y2015),
         y2008 = ifelse(id == "18479", (y2007 + y2009)/2, y2008),
         y2008 = ifelse(id == "18256", (y2007 + y2009)/2, y2008),
         y2017 = ifelse(id == "17388", (y2016 + y2018)/2, y2017),
         y2014 = ifelse(id == "15599", filter(df_cia_cttl_orig, id=="15599" & year==2014)$cia_cttl_orig, y2014),
         y2017 = ifelse(id == "15696", (y2015 + y2018)/2, y2017),
         y2016 = ifelse(id == "15696", (y2015 + y2018)/2, y2016),
         y2017 = ifelse(id == "15693", filter(df_cia_cttl_orig, id=="15693" & year==2017)$cia_cttl_orig, y2017),
         y2016 = ifelse(id == "15686", filter(df_cia_cttl_orig, id=="15686" & year==2016)$cia_cttl_orig, y2016),
         y2012 = ifelse(id == "15676", (y2011 + y2013)/2, y2012),
         y2009 = ifelse(id == "15667", (y2007 + y2010)/2, y2009),
         y2008 = ifelse(id == "15667", (y2007 + y2010)/2, y2008),
         y2009 = ifelse(id == "15518", (y2007 + y2010)/2, y2009),
         y2008 = ifelse(id == "15518", (y2007 + y2010)/2, y2008),
         y2016 = ifelse(id == "15218", filter(df_cia_cttl_orig, id=="15218" & year==2016)$cia_cttl_orig, y2016),
         y2012 = ifelse(id == "15187", (y2011 + y2013)/2, y2012),
         y2003 = ifelse(id == "13894", (y2002 + y2004)/2, y2003),
         y2011 = ifelse(id == "13894", (y2010 + y2012)/2, y2011),
         y2017 = ifelse(id == "13654", (y2016 + y2018)/2, y2017),
         y2016 = ifelse(id == "13650", filter(df_cia_cttl_orig, id=="13650" & year==2016)$cia_cttl_orig, y2016),
         y2017 = ifelse(id == "13600", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "13549", (y2016 + y2018)/2, y2017),
         y2012 = ifelse(id == "13473", (y2011 + y2013)/2, y2012),
         y2012 = ifelse(id == "13188", (y2011 + y2013)/2, y2012),
         y2005 = ifelse(id == "13188", filter(df_fedegan_orig, id=="13188" & year==2005)$fedegan_orig, y2005),
         y2003 = ifelse(id == "13188", filter(df_fedegan_orig, id=="13188" & year==2003)$fedegan_orig, y2003),
         y2017 = ifelse(id == "13074", (y2016 + y2018)/2, y2017),
         y2016 = ifelse(id == "08770", filter(df_cia_cttl_orig, id=="08770" & year==2016)$cia_cttl_orig, y2016),
         y2012 = ifelse(id == "08770", (y2011 + y2013)/2, y2012),
         y2012 = ifelse(id == "08758", (y2011 + y2013)/2, y2012),
         y2010 = ifelse(id == "08758", (y2009 + y2011)/2, y2010),
         y2012 = ifelse(id == "08675", (y2011 + y2013)/2, y2012),
         y2013 = ifelse(id == "08606", (y2012 + y2014)/2, y2013),
         y2016 = ifelse(id == "08573", (y2015 + y2017)/2, y2016),
         y2005 = ifelse(id == "08573", filter(df_fedegan_orig, id=="08573" & year==2005)$fedegan_orig, y2005),
         y2012 = ifelse(id == "08436", (y2011 + y2013)/2, y2012),
         y2012 = ifelse(id == "08137", filter(df_cia_cttl_orig, id=="08137" & year==2012)$cia_cttl_orig, y2012),
         y2016 = ifelse(id == "08001", (y2015 + y2017)/2, y2016),
         y2008 = ifelse(id == "05873", (y2007 + y2009)/2, y2008),
         y2006 = ifelse(id == "05792", (y2005 + y2007)/2, y2006),
         y2003 = ifelse(id == "05736", (y2002 + y2004)/2, y2003),
         y2005 = ifelse(id == "05145", (y2004 + y2006)/2, y2005),
         y2003 = ifelse(id == "05101", (y2002 + y2004)/2, y2003)) %>%
  pivot_longer(-id, names_to = "year", values_to = "cttl") %>%
  mutate(cttl = ifelse(id == "91405" | id == "91536", NA, cttl),
         year = as.numeric(substr(year, 2, 5))) %>%
  select(id, year, cttl)

#======================================================
# prepare cia farms data
#======================================================

df_cia_frms_orig <- df_cia %>%
  pivot_longer(-c(id, dpt, mun)) %>%
  filter(startsWith(name, "frms")) %>%
  transmute(id, year = as.numeric(paste("20", substr(name, 5, 6), sep = "")), cia_frms_orig = value)

df_frms <- df_cia %>%
  select(id, starts_with("frms")) %>%
  mutate(diff0809 = (frms09-frms08)/frms08*100,
         diff0910 = (frms10-frms09)/frms09*100,
         diff1011 = (frms11-frms10)/frms10*100,
         diff1112 = (frms12-frms11)/frms11*100,
         diff1213 = (frms13-frms12)/frms12*100,
         diff1314 = (frms14-frms13)/frms13*100,
         diff1415 = (frms15-frms14)/frms14*100,
         diff1516 = (frms16-frms15)/frms15*100,
         diff1617 = (frms17-frms16)/frms16*100,
         diff1718 = (frms18-frms17)/frms17*100,
         cor08 = frms08,
         cor09 = ifelse((diff0809 <= cerr_decr & diff0910 >= cerr_incr) | (diff0809 >= cerr_incr & diff0910 <= cerr_decr), (frms08+frms10)/2, frms09), 
         cor10 = ifelse((diff0910 <= cerr_decr & diff1011 >= cerr_incr) | (diff0910 >= cerr_incr & diff1011 <= cerr_decr), (frms09+frms11)/2, frms10),
         cor11 = ifelse((diff1011 <= cerr_decr & diff1112 >= cerr_incr) | (diff1011 >= cerr_incr & diff1112 <= cerr_decr), (frms10+frms12)/2, frms11),
         cor12 = ifelse((diff1112 <= cerr_decr & diff1213 >= cerr_incr) | (diff1112 >= cerr_incr & diff1213 <= cerr_decr), (frms11+frms13)/2, frms12), 
         cor13 = ifelse((diff1213 <= cerr_decr & diff1314 >= cerr_incr) | (diff1213 >= cerr_incr & diff1314 <= cerr_decr), (frms12+frms14)/2, frms13), 
         cor14 = ifelse((diff1314 <= cerr_decr & diff1415 >= cerr_incr) | (diff1314 >= cerr_incr & diff1415 <= cerr_decr), (frms13+frms15)/2, frms14), 
         cor15 = ifelse((diff1415 <= cerr_decr & diff1516 >= cerr_incr) | (diff1415 >= cerr_incr & diff1516 <= cerr_decr), (frms14+frms16)/2, frms15),
         cor16 = ifelse((diff1516 <= cerr_decr & diff1617 >= cerr_incr) | (diff1516 >= cerr_incr & diff1617 <= cerr_decr), (frms15+frms17)/2, frms16), 
         cor17 = ifelse((diff1617 <= ferr_decr & diff1718 >= ferr_incr) | (diff1617 >= ferr_incr & diff1718 <= ferr_decr), (frms16+frms18)/2, frms17),
         cor18 = frms18) %>%
  select(id, starts_with("cor")) %>%
  pivot_longer(-id, names_to="cor_year", values_to = "frms") %>%
  transmute(id, year=as.numeric(paste(20, substr(cor_year, 4, 5), sep="")), frms) %>%
  pivot_wider(id_cols = id, values_from = frms, names_from = year, names_prefix = "y") %>%
  mutate(y2014 = ifelse(id == "99773", (y2013 + y2015)/2, y2014),
         y2017 = ifelse(id == "99773", (y2016 + y2018)/2, y2017),
         y2013 = ifelse(id == "94001", filter(df_cia_frms_orig, id=="94001" & year==2013)$cia_frms_orig, y2013),
         y2016 = ifelse(id == "91669", filter(df_cia_frms_orig, id=="91669" & year==2016)$cia_frms_orig, y2016),
         y2017 = ifelse(id == "73870", (y2016 + y2018)/2, y2017),
         y2015 = ifelse(id == "73870", (y2014 + y2016)/2, y2015),
         y2017 = ifelse(id == "73686", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "73563", (y2016 + y2018)/2, y2017),
         y2016 = ifelse(id == "73411", (y2015 + y2017)/2, y2016),
         y2013 = ifelse(id == "73411", (y2012 + y2014)/2, y2013),
         y2017 = ifelse(id == "73352", (y2016 + y2018)/2, y2017),
         y2016 = ifelse(id == "70713", (y2015 + y2017)/2, y2016),
         y2012 = ifelse(id == "68895", (y2010 + y2013)/2, y2012),
         y2011 = ifelse(id == "68895", (y2010 + y2013)/2, y2011),
         y2014 = ifelse(id == "68872", (y2012 + y2015)/2, y2014),
         y2013 = ifelse(id == "68872", (y2012 + y2015)/2, y2013),
         y2017 = ifelse(id == "68867", (y2015 + y2018)/2, y2017),
         y2016 = ifelse(id == "68867", filter(df_cia_frms_orig, id=="68867" & year==2016)$cia_frms_orig, y2016),
         y2010 = ifelse(id == "68673", (y2009 + y2011)/2, y2010),
         y2010 = ifelse(id == "68318", (y2009 + y2011)/2, y2010),
         y2017 = ifelse(id == "68250", (y2016 + y2018)/2, y2017),
         y2009 = ifelse(id == "66682", (y2008 + y2010)/2, y2009),
         y2017 = ifelse(id == "66088", (y2016 + y2018)/2, y2017),
         y2012 = ifelse(id == "54820", (y2011 + y2013)/2, y2012),
         y2012 = ifelse(id == "54800", (y2010 + y2013)/2, y2012),
         y2011 = ifelse(id == "54800", (y2010 + y2013)/2, y2011),
         y2015 = ifelse(id == "54250", filter(df_cia_frms_orig, id=="54250" & year==2015)$cia_frms_orig, y2015),
         y2014 = ifelse(id == "54250", filter(df_cia_frms_orig, id=="54250" & year==2014)$cia_frms_orig, y2014),
         y2017 = ifelse(id == "52693", (y2016 + y2018)/2, y2017),
         y2014 = ifelse(id == "52683", (y2013 + y2015)/2, y2014),
         y2013 = ifelse(id == "52520", (y2010 + y2014)/2, y2013),
         y2012 = ifelse(id == "52520", (y2010 + y2014)/2, y2012),
         y2013 = ifelse(id == "52490", (y2010 + y2014)/2, y2013),
         y2012 = ifelse(id == "52490", (y2010 + y2014)/2, y2012),
         y2013 = ifelse(id == "52473", (y2010 + y2014)/2, y2013),
         y2012 = ifelse(id == "52473", (y2010 + y2014)/2, y2012),
         y2013 = ifelse(id == "52250", (y2010 + y2014)/2, y2013),
         y2012 = ifelse(id == "52250", (y2010 + y2014)/2, y2012),
         y2013 = ifelse(id == "52079", (y2010 + y2014)/2, y2013),
         y2012 = ifelse(id == "52079", (y2010 + y2014)/2, y2012),
         y2011 = ifelse(id == "50686", filter(df_cia_frms_orig, id=="50686" & year==2011)$cia_frms_orig, y2011),
         y2017 = ifelse(id == "50683", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "50150", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "50124", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "50001", (y2016 + y2018)/2, y2017),
         y2016 = ifelse(id == "47745", filter(df_cia_frms_orig, id=="47745" & year==2016)$cia_frms_orig, y2016),
         y2014 = ifelse(id == "47707", (y2012 + y2015)/2, y2014),
         y2013 = ifelse(id == "47707", (y2012 + y2015)/2, y2013),
         y2014 = ifelse(id == "47703", filter(df_cia_frms_orig, id=="47703" & year==2014)$cia_frms_orig, y2014),
         y2016 = ifelse(id == "47570", filter(df_cia_frms_orig, id=="47570" & year==2016)$cia_frms_orig, y2016),
         y2013 = ifelse(id == "47541", filter(df_cia_frms_orig, id=="47541" & year==2013)$cia_frms_orig, y2013),
         y2016 = ifelse(id == "47161", filter(df_cia_frms_orig, id=="47161" & year==2016)$cia_frms_orig, y2016),
         y2015 = ifelse(id == "47161", filter(df_cia_frms_orig, id=="47161" & year==2015)$cia_frms_orig, y2015),
         y2014 = ifelse(id == "47161", filter(df_cia_frms_orig, id=="47161" & year==2014)$cia_frms_orig, y2014),
         y2013 = ifelse(id == "47161", filter(df_cia_frms_orig, id=="47161" & year==2013)$cia_frms_orig, y2013),
         y2017 = ifelse(id == "47030", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "41791", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "41676", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "41524", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "41518", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "41020", (y2016 + y2018)/2, y2017),
         y2017 = ifelse(id == "41026", (y2016 + y2018)/2, y2017),
         y2013 = ifelse(id == "27580", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "27580", (y2011 + y2014)/2, y2012),
         y2014 = ifelse(id == "27413", filter(df_cia_frms_orig, id=="27413" & year==2014)$cia_frms_orig, y2014),
         y2010 = ifelse(id == "27413", filter(df_cia_frms_orig, id=="27413" & year==2010)$cia_frms_orig, y2010),
         y2010 = ifelse(id == "27372", filter(df_cia_frms_orig, id=="27372" & year==2010)$cia_frms_orig, y2010),
         y2010 = ifelse(id == "27250", (y2009 + y2011)/2, y2010),
         y2009 = ifelse(id == "27250", (y2009 + y2011)/2, y2009),
         y2012 = ifelse(id == "27205", filter(df_cia_frms_orig, id=="27205" & year==2012)$cia_frms_orig, y2012),
         y2009 = ifelse(id == "27160", filter(df_cia_frms_orig, id=="27160" & year==2009)$cia_frms_orig, y2009),
         y2010 = ifelse(id == "27160", filter(df_cia_frms_orig, id=="27160" & year==2010)$cia_frms_orig, y2010),
         y2012 = ifelse(id == "27160", filter(df_cia_frms_orig, id=="27160" & year==2012)$cia_frms_orig, y2012),
         y2013 = ifelse(id == "27160", filter(df_cia_frms_orig, id=="27160" & year==2013)$cia_frms_orig, y2013),
         y2011 = ifelse(id == "27150", filter(df_cia_frms_orig, id=="27150" & year==2011)$cia_frms_orig, y2011),
         y2010 = ifelse(id == "27050", (y2008 + y2011)/2, y2010),
         y2009 = ifelse(id == "27050", (y2008 + y2011)/2, y2009),
         y2008 = ifelse(id == "25898", NA, y2008),
         y2016 = ifelse(id == "25885", (y2014 + y2017)/2, y2016),
         y2015 = ifelse(id == "25885", (y2014 + y2017)/2, y2015),
         y2009 = ifelse(id == "25885", filter(df_cia_frms_orig, id=="25885" & year==2009)$cia_frms_orig, y2009),
         y2009 = ifelse(id == "25878", filter(df_cia_frms_orig, id=="25878" & year==2009)$cia_frms_orig, y2009),
         y2008 = ifelse(id == "25878", NA, y2008),
         y2009 = ifelse(id == "25873", filter(df_cia_frms_orig, id=="25873" & year==2009)$cia_frms_orig, y2009),
         y2010 = ifelse(id == "25871", filter(df_cia_frms_orig, id=="25871" & year==2010)$cia_frms_orig, y2010),
         y2010 = ifelse(id == "25862", filter(df_cia_frms_orig, id=="25862" & year==2010)$cia_frms_orig, y2010),
         y2017 = ifelse(id == "25851", filter(df_cia_frms_orig, id=="25851" & year==2017)$cia_frms_orig, y2017),
         y2017 = ifelse(id == "25845", filter(df_cia_frms_orig, id=="25845" & year==2017)$cia_frms_orig, y2017),
         y2013 = ifelse(id == "25839", (y2009 + y2014)/2, y2013),
         y2012 = ifelse(id == "25839", (y2009 + y2014)/2, y2012),
         y2011 = ifelse(id == "25839", (y2009 + y2014)/2, y2011),
         y2010 = ifelse(id == "25839", filter(df_cia_frms_orig, id=="25839" & year==2010)$cia_frms_orig, y2010),
         y2013 = ifelse(id == "25779", (y2010 + y2014)/2, y2013),
         y2012 = ifelse(id == "25779", (y2010 + y2014)/2, y2012),
         y2016 = ifelse(id == "25572", filter(df_cia_frms_orig, id=="25572" & year==2016)$cia_frms_orig, y2016),
         y2013 = ifelse(id == "25535", (y2010 + y2014)/2, y2013),
         y2012 = ifelse(id == "25535", (y2010 + y2014)/2, y2012),
         y2016 = ifelse(id == "25530", (y2014 + y2017)/2, y2016),
         y2015 = ifelse(id == "25530", (y2014 + y2017)/2, y2015),
         y2010 = ifelse(id == "25486", (y2009 + y2011)/2, y2010),
         y2013 = ifelse(id == "25438", (y2010 + y2014)/2, y2013),
         y2012 = ifelse(id == "25438", (y2010 + y2014)/2, y2012),
         y2010 = ifelse(id == "25339", (y2008 + y2011)/2, y2010),
         y2009 = ifelse(id == "25339", (y2008 + y2011)/2, y2009),
         y2013 = ifelse(id == "25297", (y2010 + y2014)/2, y2013),
         y2012 = ifelse(id == "25297", (y2010 + y2014)/2, y2012),
         y2011 = ifelse(id == "25297", (y2010 + y2014)/2, y2011),
         y2017 = ifelse(id == "25295", filter(df_cia_frms_orig, id=="25295" & year==2017)$cia_frms_orig, y2017),
         y2013 = ifelse(id == "25295", (y2010 + y2014)/2, y2013),
         y2012 = ifelse(id == "25295", (y2010 + y2014)/2, y2012),
         y2010 = ifelse(id == "25214", (y2009 + y2011)/2, y2010),
         y2013 = ifelse(id == "25154", (y2010 + y2014)/2, y2013),
         y2012 = ifelse(id == "25154", (y2010 + y2014)/2, y2012),
         y2011 = ifelse(id == "25154", filter(df_cia_frms_orig, id=="25154" & year==2011)$cia_frms_orig, y2011),
         y2013 = ifelse(id == "25151", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "25151", (y2011 + y2014)/2, y2012),
         y2015 = ifelse(id == "25148", filter(df_cia_frms_orig, id=="25148" & year==2015)$cia_frms_orig, y2015),
         y2014 = ifelse(id == "25099", (y2013 + y2015)/2, y2014),
         y2010 = ifelse(id == "25099", (y2009 + y2011)/2, y2010),
         y2010 = ifelse(id == "25001", filter(df_cia_frms_orig, id=="25001" & year==2010)$cia_frms_orig, y2010),
         y2016 = ifelse(id == "23815", filter(df_cia_frms_orig, id=="23815" & year==2016)$cia_frms_orig, y2016),
         y2014 = ifelse(id == "23815", filter(df_cia_frms_orig, id=="23815" & year==2014)$cia_frms_orig, y2014),
         y2012 = ifelse(id == "23815", filter(df_cia_frms_orig, id=="23815" & year==2012)$cia_frms_orig, y2012),
         y2012 = ifelse(id == "23686", (y2011 + y2013)/2, y2012),
         y2012 = ifelse(id == "23670", (y2011 + y2013)/2, y2012),
         y2012 = ifelse(id == "23555", (y2011 + y2013)/2, y2012),
         y2012 = ifelse(id == "23419", (y2011 + y2013)/2, y2012),
         y2012 = ifelse(id == "23300", (y2011 + y2013)/2, y2012),
         y2012 = ifelse(id == "23189", (y2011 + y2013)/2, y2012),
         y2013 = ifelse(id == "19701", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "19701", (y2011 + y2014)/2, y2012),
         y2013 = ifelse(id == "19698", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "19698", (y2011 + y2014)/2, y2012),
         y2015 = ifelse(id == "19548", (y2014 + y2016)/2, y2015),
         y2015 = ifelse(id == "19022", (y2014 + y2015)/2, y2015),
         y2017 = ifelse(id == "18756", (y2016 + y2018)/2, y2017),
         y2013 = ifelse(id == "17524", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "17524", (y2011 + y2014)/2, y2012),
         y2013 = ifelse(id == "17513", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "17513", (y2011 + y2014)/2, y2012),
         y2010 = ifelse(id == "15776", (y2009 + y2011)/2, y2010),
         y2010 = ifelse(id == "15774", (y2009 + y2011)/2, y2010),
         y2017 = ifelse(id == "15693", filter(df_cia_frms_orig, id=="15693" & year==2017)$cia_frms_orig, y2017),
         y2011 = ifelse(id == "15690", filter(df_cia_frms_orig, id=="15690" & year==2011)$cia_frms_orig, y2011),
         y2016 = ifelse(id == "15686", filter(df_cia_frms_orig, id=="15686" & year==2016)$cia_frms_orig, y2016),
         y2011 = ifelse(id == "15686", filter(df_cia_frms_orig, id=="15686" & year==2011)$cia_frms_orig, y2011),
         y2013 = ifelse(id == "15518", (y2011 + y2014)/2, y2013),
         y2012 = ifelse(id == "15518", (y2011 + y2014)/2, y2012),
         y2016 = ifelse(id == "15403", filter(df_cia_frms_orig, id=="15403" & year==2016)$cia_frms_orig, y2016),
         y2016 = ifelse(id == "15401", filter(df_cia_frms_orig, id=="15401" & year==2016)$cia_frms_orig, y2016),
         y2016 = ifelse(id == "15380", filter(df_cia_frms_orig, id=="15380" & year==2016)$cia_frms_orig, y2016),
         y2014 = ifelse(id == "15380", filter(df_cia_frms_orig, id=="15380" & year==2014)$cia_frms_orig, y2014),
         y2012 = ifelse(id == "15380", filter(df_cia_frms_orig, id=="15380" & year==2012)$cia_frms_orig, y2012),
         y2011 = ifelse(id == "15368", (y2009 + y2012)/2, y2011),
         y2010 = ifelse(id == "15368", (y2009 + y2012)/2, y2010),
         y2010 = ifelse(id == "15322", filter(df_cia_frms_orig, id=="15322" & year==2010)$cia_frms_orig, y2010),
         y2012 = ifelse(id == "15244", filter(df_cia_frms_orig, id=="15244" & year==2012)$cia_frms_orig, y2012),
         y2013 = ifelse(id == "15238", filter(df_cia_frms_orig, id=="15238" & year==2013)$cia_frms_orig, y2013),
         y2011 = ifelse(id == "15238", filter(df_cia_frms_orig, id=="15238" & year==2011)$cia_frms_orig, y2011),
         y2012 = ifelse(id == "15236", filter(df_cia_frms_orig, id=="15236" & year==2012)$cia_frms_orig, y2012),
         y2012 = ifelse(id == "15232", filter(df_cia_frms_orig, id=="15232" & year==2012)$cia_frms_orig, y2012),
         y2012 = ifelse(id == "15226", (y2009 + y2013)/2, y2012),
         y2011 = ifelse(id == "15226", (y2009 + y2013)/2, y2011),
         y2010 = ifelse(id == "15226", (y2009 + y2013)/2, y2010),
         y2012 = ifelse(id == "15224", filter(df_cia_frms_orig, id=="15224" & year==2012)$cia_frms_orig, y2012),
         y2014 = ifelse(id == "15215", filter(df_cia_frms_orig, id=="15215" & year==2014)$cia_frms_orig, y2014),
         y2012 = ifelse(id == "15215", filter(df_cia_frms_orig, id=="15215" & year==2012)$cia_frms_orig, y2012),
         y2013 = ifelse(id == "15212", filter(df_cia_frms_orig, id=="15212" & year==2013)$cia_frms_orig, y2013),
         y2011 = ifelse(id == "15187", filter(df_cia_frms_orig, id=="15187" & year==2011)$cia_frms_orig, y2011),
         y2016 = ifelse(id == "13760", (y2015 + y2017)/2, y2016),
         y2017 = ifelse(id == "13600", (y2016 + y2018)/2, y2017),
         y2016 = ifelse(id == "13248", filter(df_cia_frms_orig, id=="13248" & year==2016)$cia_frms_orig, y2016),
         y2012 = ifelse(id == "13160", filter(df_cia_frms_orig, id=="13160" & year==2012)$cia_frms_orig, y2012),
         y2016 = ifelse(id == "08675", filter(df_cia_frms_orig, id=="08675" & year==2016)$cia_frms_orig, y2016),
         y2012 = ifelse(id == "08675", filter(df_cia_frms_orig, id=="08675" & year==2012)$cia_frms_orig, y2012),
         y2012 = ifelse(id == "08560", filter(df_cia_frms_orig, id=="08560" & year==2012)$cia_frms_orig, y2012),
         y2017 = ifelse(id == "08558", (y2016 + y2018)/2, y2017),
         y2012 = ifelse(id == "08436", filter(df_cia_frms_orig, id=="08436" & year==2012)$cia_frms_orig, y2012),
         y2012 = ifelse(id == "08137", filter(df_cia_frms_orig, id=="08137" & year==2012)$cia_frms_orig, y2012),
         y2009 = ifelse(id == "05873", (y2008 + y2010)/2, y2009),
         y2013 = ifelse(id == "05667", filter(df_cia_frms_orig, id=="05667" & year==2013)$cia_frms_orig, y2013),
         y2016 = ifelse(id == "05660", filter(df_cia_frms_orig, id=="05660" & year==2016)$cia_frms_orig, y2016),
         y2015 = ifelse(id == "05313", filter(df_cia_frms_orig, id=="05313" & year==2015)$cia_frms_orig, y2015),
         y2012 = ifelse(id == "05036", (y2011 + y2013)/2, y2012)) %>%
  pivot_longer(-id, names_to = "year", values_to = "frms") %>%
  mutate(year = as.numeric(substr(year, 2, 5))) %>%
  select(id, year, frms)

#======================================================
# merge corrected and orignal data
#======================================================

df_cttl_frms <- mun_list %>%
  left_join(df_cttl, by="id") %>%
  left_join(df_frms, by=c("id", "year")) %>%
  left_join(df_fedegan_orig, by=c("id", "year")) %>%
  left_join(df_cia_cttl_orig, by=c("id", "year")) %>%
  left_join(df_cia_frms_orig, by=c("id", "year")) %>%
  tbl_df()

#======================================================
# export data
#======================================================

# save as rds
saveRDS(df_cttl_frms, "output/data/data_cia_fedegan_merged_corrected.rds")

#======================================================
#### END ####