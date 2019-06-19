###############################################################################
# Reikalingų paketų instaliavimas ir užkrovimas.
# BŪTINA nusistatyti working directory į folder'į Duomenų analizės rašto
# darbas. (Session -> Set Working Directory -> Choose Directory...)
# Kitaip neveiks png komanda ir grafikų neišsaugos.
###############################################################################

if(!require(eurostat)) install.packages("eurostat"); require(eurostat)
if(!require(tidyverse)) install.packages("tidyverse"); require(tidyverse)
if(!require(rsdmx)) install.packages("rsdmx"); require(rsdmx)

###############################################################################
# LSD meta duomenų failo atsiuntimas ir išsaugojimas kaip data frame, rankinis
# reikiamų lentelių identifikavimas naudojant filtrą data frame. 
# Informacijai apie SDMX ir meta: https://osp.stat.gov.lt/rdb-rest
# URL yra standartinis LSD meta duomenims rasti
###############################################################################

URL <- "https://osp-rs.stat.gov.lt/rest_xml/dataflow/"
meta <- readSDMX(URL)
meta <- as.data.frame(meta)

###############################################################################
# Duomenų iš LSD parsiuntimas naudojant paketą "RSDMX" ir duomenų iš Eurostat
# parsiuntimas naudojant paketą "Eurostat" 
###############################################################################

###############################################################################
# LSD
# S4R001_M9020211
# S4R001 - Asmenys, pirkę ar užsakę prekių ar paslaugų internetu 
# Amžius | Prekės / paslaugos (2004 - 2018)
###############################################################################

S4R001_M9020211<- readSDMX(providerId = "LSD",
                           resource = "data",
                           flowRef = "S4R001_M9020211",
                           dsd = TRUE)
S4R001_M9020211<- as.data.frame(S4R001_M9020211, label=TRUE)

###############################################################################
# LSD
# S4R001_M9020205
# S4R001 - Asmenys, pirkę ar užsakę prekių ar paslaugų internetu
# Amžius
###############################################################################

S4R001_M9020205<- readSDMX(providerId = "LSD",
                           resource = "data",
                           flowRef = "S4R001_M9020205",
                           dsd = TRUE)
S4R001_M9020205<- as.data.frame(S4R001_M9020205, label=TRUE)

###############################################################################
# LSD
# S4R001_M9020217
# S4R001 - Asmenys, pirkę ar užsakę prekių ar paslaugų internetu
# Amžius | Pirkta kartų 
###############################################################################

S4R001_M9020217<- readSDMX(providerId = "LSD",
                                          resource = "data",
                                          flowRef = "S4R001_M9020217",
                                          dsd = TRUE)
S4R001_M9020217<- as.data.frame(S4R001_M9020217, label=TRUE)

###############################################################################
# LSD
# S4R001_M9020218
# S4R001 - Asmenys, pirkę ar užsakę prekių ar paslaugų internetu
# Amžius | Išleista suma
###############################################################################

S4R001_M9020218<- readSDMX(providerId = "LSD",
                          resource = "data",
                           flowRef = "S4R001_M9020218",
                           dsd = TRUE)
S4R001_M9020218<- as.data.frame(S4R001_M9020218, label=TRUE)                      

###############################################################################
# LSD
# S4R029_M9020101
# S4R029 - Namų ūkiai, turintys asmeninį kompiuterį, interneto prieigą
# Gyvenamoji vietovė | Informacinės technologijos 
###############################################################################

S4R029_M9020101<- readSDMX(providerId = "LSD",
                           resource = "data",
                           flowRef = "S4R029_M9020101",
                           dsd = TRUE)
S4R029_M9020101<- as.data.frame(S4R029_M9020101, label=TRUE)                      

###############################################################################
# Eurostat:
# Internet purchases by individuals: isoc_ec_ibuy 
###############################################################################

isoc_ec_ibuy <- get_eurostat(id="isoc_ec_ibuy", stringsAsFactors = FALSE,
                             filters = list(geo=c("EU28", "LT"),
                                            indic_is="I_BLT12",
                                            ind_type="IND_TOTAL",
                                            time=c("2018","2017","2016","2015","2014"),
                                            unit="PC_IND_ILT12"))

###############################################################################
# Eurostat:
# Households - level of internet access: isoc_ci_in_h
###############################################################################

isoc_ci_in_h <- get_eurostat(id="isoc_ci_in_h", stringsAsFactors = FALSE,
                             filters = list(geo=c("EU28", "LT"),
                                            hhtyp="TOTAL",
                                            time=c("2018","2017","2016","2015","2014"),
                                            unit="PC_HH"))

###############################################################################
# Eurostat: 
# Problems encountered by individuals when buying/ordering over the
# internet: isoc_ec_iprb
###############################################################################

isoc_ec_iprb <- get_eurostat(id="isoc_ec_iprb", stringsAsFactors = FALSE)

###############################################################################
# Eurostat:
# Perceived barriers to buying/ordering over the internet:isoc_ec_inb
###############################################################################

isoc_ec_inb <- get_eurostat(id="isoc_ec_inb", stringsAsFactors = FALSE)

###############################################################################
# Duomenų iš Eurostat ir LSD (Lietuvos Statistikos departamento)apdorojimas ir
# paruošimas grafikams bei lentelėms.
###############################################################################

###############################################################################
# 2 grafiko duomenys.
###############################################################################
df <- S4R001_M9020211 %>%
  select(c(3,6,11,12))%>%
  filter(amziusM9030201_label.lt=="Visi 16–74 metų amžiaus asmenys",
         IT_prekesM9020211_label.lt %in% c("Apgyvendinimo paslaugas atostogoms",
                                            "Knygas, žurnalus, laikraščius",
                                           "Drabužius, avalynę, sporto prekes",
                                           "Elektroninius prietaisus",
                                           "Filmus, muziką",
                                           "Maisto ir kasdienio naudojimo prekes",
                                           "Namų ūkio reikmenis",
                                           "Bilietus į teatrą, kiną, koncertą ir pan.",
                                           "Turistines keliones",
                                           "Telekomunikacijų paslaugas",
                                           "Prekes ar paslaugas - iš viso"),
         LAIKOTARPIS %in% c("2014","2018"))%>%
  mutate(IT_prekesM9020211_label.lt=factor(IT_prekesM9020211_label.lt,
                                           levels = list("Apgyvendinimo paslaugas atostogoms",
                                                        "Knygas, žurnalus, laikraščius",
                                                        "Drabužius, avalynę, sporto prekes",
                                                        "Elektroninius prietaisus",
                                                        "Filmus, muziką",
                                                        "Maisto ir kasdienio naudojimo prekes",
                                                        "Namų ūkio reikmenis",
                                                        "Bilietus į teatrą, kiną, koncertą ir pan.",
                                                        "Turistines keliones",
                                                        "Telekomunikacijų paslaugas",
                                                        "Prekes ar paslaugas - iš viso")))

###############################################################################
# 3 grafiko duomenys.
###############################################################################

df1 <- S4R001_M9020205 %>%
  select(c(3,8,9))%>%
  filter(LAIKOTARPIS %in% c("2014","2016","2018"),
         amziusM9030201_label.lt %in% c("16–24","16–29","25–34","35–44","45–54","55–64","65–74"))%>%
  mutate(amziusM9030201_label.lt=factor(amziusM9030201_label.lt,levels = list("16–24","16–29","25–34","35–44","45–54","55–64","65–74")))

###############################################################################
# 1 Lentelė sudaryta iš LSD duomenų, parodanti kaip kito dažnumas tarp 16-74 metų
# asmenų 2014-2018 metais ir bus panaudota Latex dokumente.
# Lentelė sudaroma per http://www.tablesgenerator.com.
###############################################################################

df2 <- S4R001_M9020217%>%
  select(c(3,6,9,11,12))%>%
  filter(LAIKOTARPIS %in% c("2015","2018"),
         amziusM9030201_label.lt=="Visi 16–74 metų amžiaus asmenys")

###############################################################################
# 1 grafiko duomenys.
###############################################################################

df3 <- S4R029_M9020101 %>%
  filter(LAIKOTARPIS %in% c("2014","2015","2016","2017","2018"),
         vietoveM2040101_label.lt=="Miestas ir kaimas",
         ITM9020101_label.lt=="Interneto prieiga")%>%
  select(c(3,6,8,9))

###############################################################################
# 4 grafiko duomenys
###############################################################################

df5 <- isoc_ci_in_h

###############################################################################
# 2 Lentelė, kuri bus Latex idėta, nufiltravimas.
# Lentelė sudaroma per http://www.tablesgenerator.com ,pakeičiant tik stulpelių
# pavadinimus.
###############################################################################

df4 <- isoc_ec_ibuy %>%
  select(c(4,5,6))

###############################################################################
# 6 Grafiko duomenys
###############################################################################

df6 <- S4R001_M9020218 %>%
  select(c(3,6,8,10,11,12))%>%
  filter(LAIKOTARPIS %in% c("2018","2015"),
         amziusM9030201_label.lt %in% c("Visi 16–74 metų amžiaus asmenys"))%>%
  mutate(islaidosM9020218_label.lt=factor(islaidosM9020218_label.lt,
                                           levels = list("mažiau kaip 50 EUR",
                                                         "50–99 EUR","100–499 EUR",
                                                         "500–999 EUR","1000 EUR ir daugiau")))

###############################################################################
# 3 lentelės duomenys 
# Išverčiami bus pirmo stulpelio pavadinimai iš anglų kalbos ir lygiai
# tokia pačia eilės tvarka lentelėje pateikiami.
# Lentelė sudaroma per http://www.tablesgenerator.com
###############################################################################

df7 <- isoc_ec_iprb %>%
  filter(geo=="EU28",
         time=="2017-01-01",
         indic_is %in% c("I_BDNS","I_BFRA","I_BPRC","I_BCOM","I_BGUA","I_BWD","I_BTFW",
         "I_BSPD","I_BARRX","I_BARR1X"),
         ind_type=="IND_TOTAL",
         unit=="PC_IND_BLT12")

###############################################################################
# 4 lentelės duomenys
# Bus išverčiami angliški duomenys į lietuvių kalbą ir pateikiami tokia pačia 
# tvarka kaip ir data frame išdėstyti.
# Lentelė sudaroma per http://www.tablesgenerator.com
###############################################################################

df8 <- isoc_ec_inb %>%
  filter(geo=="EU28",indic_is %in% c("I_NBSHAB",
                                     "I_NBSKL",
                                     "I_NBSR",
                                     "I_NBPSC",
                                     "I_NBTRCM",
                                     "I_NBCARD"),
         ind_type=="IND_TOTAL", time=="2017-01-01",unit=="PC_IND_BMT12X")

###############################################################################
# Grafikų braižymas
###############################################################################

# 1 Grafikas
png("./Grafikai/1.png")

ggplot(df3, aes(x=LAIKOTARPIS,y=obsValue))+
  geom_bar(stat = "identity",colour="black",fill="grey", width = 0.5)+
  theme_minimal()+
  geom_text(aes(label=obsValue),size=4, vjust=-0.25)+
  theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = 1.25, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"))+  
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100), limits = c(0,100))+
  labs(x="Laikotarpis", 
y="Namų ūkiai (miestas ir kaimas), turintys 
interneto prieigą, proc", 
       title= "Namų ūkiai Lietuvoje, turintys interneto prieigą (2014-2018 metais)",
     subtitle = "Šaltinis: Lietuvos statistikos departamentas (S4R029_M9020101)")

dev.off()

# 2 Grafikas
png("./Grafikai/2.png")

ggplot(df, aes(IT_prekesM9020211_label.lt,obsValue, fill=LAIKOTARPIS))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_minimal()+
  coord_flip()+
  geom_text(aes(label=obsValue),size=4, vjust=0.5, hjust=-0.25, 
            position = position_dodge(width=0.9))+
  scale_fill_manual(values = c("gold", "lightgreen"),name = "Laikotarpis")+
  theme(axis.text.x = element_text(angle = 0,hjust = 1, size = 9, vjust = 0))+
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50), limits = c(0,50))+
  labs(x="Prekės ir paslaugos pirktos ar užsakytos internetu", 
       y="proc. visų 16–74 metų amžiaus asmenų grupės", 
            title= "Asmenys, pirkę ar užsakę prekių ar paslaugų
internetu (2014,2018 metais)",
            subtitle = "Šaltinis: Lietuvos statistikos departamentas (S4R001_M9020211)")
  
dev.off()

# 3 Grafikas
png("./Grafikai/3.png")

ggplot(df1, aes(amziusM9030201_label.lt,obsValue, fill=LAIKOTARPIS))+
  geom_bar(stat = "identity", position = "dodge")+
  theme_minimal()+
  scale_fill_brewer(palette = "Blues", name = "Laikotarpis")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1, size = 9))+
  geom_text(aes(label=obsValue),size=3.2, vjust=-0.25, 
            position = position_dodge(width=0.9))+
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60), 
                     limits = c(0,60))+
  labs(x="Amžiaus grupė", 
       y="proc., nuo visų asmenų atitinkamoje amžiaus grupėje", 
       title= "Asmenys, pirkę ar užsakę prekių ar paslaugų internetu 
per paskutinius 3 mėnesius amžius (2014,2016,2018 metais)",
       subtitle = "Šaltinis: Lietuvos statistikos departamentas (S4R001_M9020205)")

dev.off()

# 4 Grafikas
png("./Grafikai/4.png")

ggplot(df5, aes(x=time, y=values, group=geo, color=geo))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1, size = 10),
        axis.text.y = element_text(size = 10, angle = 0, hjust = 1))+
  scale_y_continuous(breaks = c(60,70,80,90), 
                     limits = c(60,90))+
  geom_line(size=1.5)+
  geom_point(color="red", size=2)+
  scale_color_manual(values=c("darkblue", "darkgreen"))+
  labs(x="Laikotarpis", 
       y="proc", 
       title= "Namų ūkių interneto prieiga EU28 ir Lietuvoje
(2014-2018 metais)",
       subtitle = "Šaltinis: Eurostat (isoc_ci_in_h)", color="Europos šalys")

dev.off()

# 6 Grafikas
png("./Grafikai/6.png")

ggplot(df6, aes(LAIKOTARPIS,obsValue, fill=islaidosM9020218_label.lt))+
  geom_bar(stat = "identity", position = "dodge", colour="darkgrey")+
  theme_minimal()+
  scale_fill_manual(values = c("blue","red","darkgreen","yellow","orange"),name="Išleista suma")+
  geom_text(aes(label=obsValue),size=4, vjust=-0.25, 
            position = position_dodge(width=0.9))+
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40,45,45), 
                     limits = c(0,45))+
  labs(x="Laikotarpis", 
       y="procentai", 
       title= "Asmenys, pirkę ar užsakę prekių ar paslaugų internetu | proc., nuo per 
paskutinius 3 mėn. internetu pirkusių asmenų 16-74 metų amžiaus grupėje 
per paskutinius 3 mėnesius (2015,2018 metais)",
       subtitle = "Šaltinis: Lietuvos statistikos departamentas (S4R001_M9020218)")

dev.off()
