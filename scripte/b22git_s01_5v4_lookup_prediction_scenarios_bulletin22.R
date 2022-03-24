rm(list = ls())
require(toolboxH)
require(ggplot2)
require(ggthemes)
require(stringr)
require(here)
require(lubridate)
require(plotly)
require(patchwork)
initializeSkript()

# datum = as_date("2022-01-31") #\today())-3
datum = today()
datum

marcin_dead = fread(here("../data/marcin/Bulletin Mar 2022v4/mitFDApr3/deaths/scenario_SAX-cqb-deaths-cumulative-Total_False_2022_03_21_year_end_EN.csv"))
marcin_dead[, DateRep := as_date(dmy(dates))]
showClassDF(marcin_dead)
marcin_dead[ ,variable := "Verstorbene"]
marcin_dead[ ,scenario2 := "Freedom Day 3.4.22"]

marcin_testpos = fread(here("../data/marcin/Bulletin Mar 2022v4/mitFDApr3/detections/scenario_SAX-detections-Total_False_2022_03_21_year_end_EN.csv"))
marcin_testpos[, DateRep := as_date(dmy(dates))]
showClassDF(marcin_testpos)
marcin_testpos[ ,variable := "Testpositive"]
marcin_testpos[ ,scenario2 := "Freedom Day 3.4.22"]

marcin_hosp = fread(here("../data/marcin/Bulletin Mar 2022v4/mitFDApr3/hospitalizations/scenario_SAX-hospitalized-Total_False_2022_03_21_year_end_EN.csv"))
marcin_hosp[, DateRep := as_date(dmy(dates))]
showClassDF(marcin_hosp)
marcin_hosp[ ,variable := "Hospitalisierte"]
marcin_hosp[ ,scenario2 := "Freedom Day 3.4.22"]


plotenddate = datum + 4*7
plotenddate
plotenddateTOT = plotenddate +14
plotenddateTOT
## KIT DAten ---

cumdead = fread("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/truth_RKI-Cumulative%20Deaths_Germany.csv", encoding = "UTF-8")
cumdead
inccase = fread("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/truth_RKI-Incident%20Cases_Germany.csv", encoding = "UTF-8")
inccase

datenstand = max(inccase$date)
datenstand

redaktionsschlus_datum = as_date("22-03-20")

# data.table(namex = unique(inccase$location_name), numix = unique(inccase$location)) %>% ccc
setnames(inccase, c('date','location_name', "value"), c('DateRep','CountryExp',"NewConfCases"))
setnames(cumdead, c('date','location_name', "value"), c('DateRep','CountryExp',"AllDeaths"))

inccase[, DateRep := as_date(DateRep)]
cumdead[, DateRep := as_date(DateRep)]

inccase[, id := paste(DateRep,location )]
cumdead[, id := paste(DateRep,location )]

qlist1 = venn2(inccase$id, cumdead$id)

inccase[,AllDeaths := cumdead[match_hk(inccase$id, cumdead$id), AllDeaths]]



ggplot(inccase, aes(DateRep, NewConfCases, col = CountryExp)) + geom_line() + scale_y_log10()
ggplot(inccase, aes(DateRep, AllDeaths, col = CountryExp)) + geom_line()



# add KH data ----

all_icu4 = fread("https://diviexchange.blob.core.windows.net/%24web/zeitreihe-tagesdaten.csv")
all_icu4
all_icu4[,DateRep := as_date(date)]
stopifnot(nrow(all_icu4[is.na(gemeindeschluessel)==T])==0)

kreise = read_excel2(here("data/04-kreise.xlsx"),2, skip = 3)
bl = kreise[str_length(`Schlüssel-nummer`)==2]
bl[,`Schlüssel-nummer`:= as.numeric(`Schlüssel-nummer`)]
all_icu4[,bundesland2 := bl[match_hk(all_icu4$bundesland, bl$`Schlüssel-nummer`),`Regionale Bezeichnung`]]
all_icu4[,.N,bundesland2]

kreise2 = kreise[str_length(`Schlüssel-nummer`)==5]
kreise2[,kreisnr:= as.numeric(`Schlüssel-nummer`)]


all_icu4[,kreis := kreise2[match_hk(all_icu4$gemeindeschluessel, kreise2$kreisnr),`Kreisfreie Stadt`]]
all_icu4[,einwohner := kreise2[match_hk(all_icu4$gemeindeschluessel, kreise2$kreisnr),`Bevölkerung2)`]]
all_icu4[, betten_gesamt := betten_frei+betten_belegt]

#` # plotte`
all_icu4[,.(betten_gesamt=sum(betten_gesamt)), DateRep] %>%
  ggplot( aes(DateRep, betten_gesamt)) + geom_point() + geom_line() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  scale_x_date(labels = date_format("%m-%d"),date_breaks = "3 days" )


all_icu4[,einwohner:= as.numeric(einwohner)]

all_icu4[,frei_typ:= "jetzt frei"]

stopifnot(all_icu4[is.na(betten_gesamt)] %>% nrow(.)==0)# , betten_gesamt:= betten_belegt+betten_frei]

all_icu4_bl = all_icu4[,.(anzahl_standorte=sum(anzahl_standorte), 
                          betten_frei=sum(betten_frei),
                          betten_belegt=sum(betten_belegt), 
                          anzahl_meldebereiche=sum(anzahl_meldebereiche),
                          faelle_covid_aktuell = sum(faelle_covid_aktuell),
                          faelle_covid_aktuell_beatmet = sum(faelle_covid_aktuell_invasiv_beatmet), #update naming 1.4.21
                          einwohner=sum(einwohner),
                          betten_gesamt = sum(betten_gesamt)), .(bundesland, DateRep, bundesland2,frei_typ)]

all_icu4_ger= all_icu4[,.(anzahl_standorte=sum(anzahl_standorte), 
                          betten_frei=sum(betten_frei),
                          betten_belegt=sum(betten_belegt), 
                          anzahl_meldebereiche=sum(anzahl_meldebereiche),
                          faelle_covid_aktuell = sum(faelle_covid_aktuell),
                          faelle_covid_aktuell_beatmet = sum(faelle_covid_aktuell_invasiv_beatmet), # update 1.4.21
                          einwohner=sum(einwohner),
                          betten_gesamt = sum(betten_gesamt)), .( DateRep, frei_typ)]




all_icu4_ger[, level:= "Land"]
all_icu4_bl[, level:= "Bundesland"]
all_icu4[, level:= "Kreis"]

all_icu4_ger[, CountryExp := 'Deutschland']
all_icu4_bl[, CountryExp := bundesland2]
all_icu4[, CountryExp := kreis]


all_icu4_ger[,`COVID-19 aktuell in Behandlung_upscaled` :=  faelle_covid_aktuell] # kein upscaling mehr, weil vollstaendig genug ## ab 18.4.2020 nichtmehr upscaled, weillaut verordnung fast alle melden u

all_icu4_bl[,`COVID-19 aktuell in Behandlung_upscaled` :=  faelle_covid_aktuell] # kein upscaling mehr, weil vollstaendig genug
all_icu4[,`COVID-19 aktuell in Behandlung_upscaled` :=  faelle_covid_aktuell] # kein upscaling mehr, weil vollstaendig genug


(ggplot(all_icu4_bl, aes(DateRep,  `COVID-19 aktuell in Behandlung_upscaled` , col = CountryExp)) + geom_point() + geom_line() + geom_vline(xintercept = as_date('20-04-16'), lty = 2)) %>% ggplotly()
## 
icu_sachsen = all_icu4_bl[CountryExp=="Sachsen"]

# Normastation ----
observed_data2 = inccase[CountryExp =="Free State of Saxony"]
observed_data2[, covid_inICU_upscaled := icu_sachsen[match_hk(observed_data2$DateRep %>% as.character(), icu_sachsen$DateRep%>% as.character()),`COVID-19 aktuell in Behandlung_upscaled`]]

# 
normalstation = read_excel2(here("data/sachsen_kh_normalstation.xlsx"))
normalstation

datenstandnormalstation  = max(normalstation$Datum)
datenstandnormalstation

observed_data2[,belegung_normalstation := normalstation[match_hk(observed_data2$DateRep %>% as.character(), normalstation$Datum %>% as.character()),Hospitalisiert]]
observed_data2[, icu_plus_normal_belegung := belegung_normalstation + covid_inICU_upscaled ]


# 
observed_data2 = observed_data2[DateRep>= min(marcin_testpos$DateRep)-21]
setorder(observed_data2, DateRep)
observed_data2[, NewConfCases7 := frollmean(NewConfCases, 7, na.rm = T, align = "right"), .(CountryExp) ]
observed_data2[, icu_plus_normal_belegung7 := frollmean(icu_plus_normal_belegung, 7, na.rm = T, align = "right"), .(CountryExp) ]

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
# mycolors = c(gg_color_hue(4)[c(4,3,2)], "black")

shadowcolor = c("lightsteelblue3", "lightslategray", "skyblue4")[3]
mycolors = c(shadowcolor, "black")
colorbreaks = c( "Freedom Day 3.4.22", "Berichtet")

colornewdata = "red"

p_posit= ggplot(marcin_testpos[DateRep<=as_date(plotenddate)], aes(DateRep, mean, col = scenario2, fill = scenario2)) +
  theme_minimal(base_size = 16) + 
  ylab("Berichtete tageweise Testpositive") +
  scale_x_date(label = date_format(format ='%d.%b' ))+
  scale_y_continuous(breaks = pretty_breaks(6))+
  geom_ribbon(aes(ymin = p2.5,ymax = p97.5  ), alpha = 0.6,color = NA, fill = shadowcolor)+
  geom_line(lwd = 1, alpha = 0) +
  geom_line(data= observed_data2[DateRep<=as_date(redaktionsschlus_datum),. (DateRep, mean = NewConfCases, scenario2 = "Berichtet")] , lwd = 1, alpha = 0.4) + 
  geom_line(data= observed_data2[DateRep<=as_date(redaktionsschlus_datum),. (DateRep, mean = NewConfCases7 , scenario2 = "Berichtet")] , lwd = 1, alpha = 0.9, lty = 1) +
  ggtitle(unique(marcin_testpos$variable)) +
  guides(color = "none")+
  
  geom_line(data= observed_data2[DateRep>=as_date(redaktionsschlus_datum),. (DateRep, mean = NewConfCases, scenario2 = "Berichtet")] , lwd = 1, alpha = 0.3, col = colornewdata) + 
  geom_line(data= observed_data2[DateRep>=as_date(redaktionsschlus_datum),. (DateRep, mean = NewConfCases7 , scenario2 = "Berichtet")] , lwd = 1, alpha = 0.9, lty = 1, col = colornewdata) +
  
  
  labs(color = "Szenario:", fill = "Szenario:") + 
  scale_color_manual(values = mycolors, breaks  = colorbreaks)+ 
  scale_fill_manual(values = mycolors, breaks  = colorbreaks)+
  xlab("")
p_posit

p_posit + scale_y_log10(breaks = log_breaks(10), labels = label_comma()) 
(p_posit + scale_y_log10(breaks = log_breaks(10), labels = label_comma()) ) %>% ggplotly()

# death ----


p_marcin_dead= ggplot(marcin_dead[DateRep<=as_date(plotenddateTOT)& DateRep>=as_date("2022-03-01")], aes(DateRep, mean, col = scenario2, fill = scenario2)) +
  theme_minimal(base_size = 16) + 
  ylab("Berichtete kumul. Verstorbene") +
  scale_x_date(label = date_format(format ='%d.%b' ))+
  scale_y_continuous(breaks = pretty_breaks(6))+
  geom_ribbon(aes(ymin = p2.5,ymax = p97.5  ), alpha = 0.6,fill  = shadowcolor, colour = NA)+
  geom_line(lwd = 1, alpha = 0) +
  geom_line(data= observed_data2[DateRep >as_date("2021-12-15")& DateRep<=as_date(redaktionsschlus_datum),. (DateRep, mean = AllDeaths , scenario2 = "Berichtet")] , lwd = 1, alpha = 0.8) + 
  
  geom_line(data= observed_data2[DateRep >=as_date(redaktionsschlus_datum),. (DateRep, mean = AllDeaths , scenario2 = "Berichtet")] , lwd = 1, alpha = 1, col = colornewdata) + 
  guides(color = "none")+
  
  ggtitle(unique(marcin_dead$variable)) +
  labs(color = "Szenario:", fill = "Szenario:")+ 
  scale_color_manual(values = mycolors, breaks  = colorbreaks)+ 
  scale_fill_manual(values = mycolors, breaks  = colorbreaks)+
  xlab("")
p_marcin_dead

# p_marcin_dead + scale_y_log10()
# hosp ----



p_marcin_hosp= ggplot(marcin_hosp[DateRep<=as_date(plotenddate)], aes(DateRep, mean, col = scenario2, fill = scenario2)) +  
  theme_minimal(base_size = 16) + 
  scale_x_date(label = date_format(format ='%d.%b' ))+
  geom_ribbon(aes(ymin = p2.5,ymax = p97.5  ), alpha = 0.6, fill = shadowcolor, colour = NA)+
  # geom_line(lwd = 1, alpha = 0.8) +
  scale_y_continuous(breaks = pretty_breaks(6))+
  geom_line(data= observed_data2[DateRep<=as_date(redaktionsschlus_datum),. (DateRep, mean = icu_plus_normal_belegung, scenario2 = "Berichtet")] , lwd = 1, alpha = 0.4) + ggtitle("marcin_hosp") +
  geom_line(data= observed_data2[DateRep<=as_date(redaktionsschlus_datum),. (DateRep, mean = icu_plus_normal_belegung7, scenario2 = "Berichtet")] , lwd = 1, alpha = 0.9) + ggtitle("marcin_hosp") +
  
  geom_line(data= observed_data2[DateRep >=as_date(redaktionsschlus_datum),. (DateRep, mean = icu_plus_normal_belegung, scenario2 = "Berichtet")] , lwd = 1, alpha = 0.3, col = colornewdata) + 
  geom_line(data= observed_data2[DateRep >=as_date(redaktionsschlus_datum),. (DateRep, mean = icu_plus_normal_belegung7, scenario2 = "Berichtet")] , lwd = 1, alpha = 0.9, col = colornewdata) + 
  
  
  ggtitle("Hospitalisierte") +
  guides(color = "none")+
  
  ylab("Hospitalisierte (ITS & Normalstation)")+
  labs(color = "Szenario:", fill = "Szenario:")+ 
  scale_color_manual(values = mycolors, breaks  = colorbreaks)+ 
  scale_fill_manual(values = mycolors, breaks  = colorbreaks) +
  xlab("")
p_marcin_hosp
try(ggplotly(p_marcin_hosp))

p_marcin_hosp 

# Abb 10 haupt bulletin----
p_posit +guides(fill = 'none', col = "none") + p_marcin_hosp + p_marcin_dead + plot_layout(nrow = 1,guides = 'collect') & theme(legend.position = 'top')



jpeg(here("results/b22git_s01_5v4_22-03-21.jpeg"),width = 15,6, res = 150, quality= 100, units = "in")
p_posit +guides(fill = 'none', col = "none") + p_marcin_hosp + p_marcin_dead + plot_layout(nrow = 1,guides = 'collect') + plot_annotation(caption =  paste0("Datenstand Inzidenz:", datenstand, "\n", "Datenstand Hospitalisierte:", datenstandnormalstation) %>% as.character)& theme(legend.position = 'top') 

dev.off()


jpeg(here("results/b22git_s01_5v4_22-03-21_log.jpeg"),width = 15,6, res = 150, quality= 100, units = "in")
p_posit  +scale_y_log10(breaks = log_breaks(15), limits = c(1000, 35000)) +guides(fill = 'none', col = "none") + p_marcin_hosp + p_marcin_dead + plot_layout(nrow = 1,guides = 'collect') + plot_annotation(caption =  paste0("Datenstand Inzidenz:", datenstand, "\n", "Datenstand Hospitalisierte:", datenstandnormalstation) %>% as.character)& theme(legend.position = 'top') 

dev.off()
