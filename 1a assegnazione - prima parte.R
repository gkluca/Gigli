#Inizio------------------------------------------------------------------

number = c(1,2,3)
animal = c("cat","dog","mouse")
df1 = data.frame(number,animal)
df1

df1['animal']
df1$animal


#Loading data (and basic statistics / visualization)--------------------

names =c("State_Code", "County_Code", "Census_Tract_Number", "NUM_ALL", "NUM_FHA", "PCT_NUM_FHA", "AMT_ALL", "AMT_FHA", "PCT_AMT_FHA")
df = read.csv(file='C:/Users/luca.ganugi/Desktop/Master MABIDA/Gigli/Data Reply/fha_by_tract.csv',header=FALSE, col.names=names)
head(df,3)


df$GEOID = df$Census_Tract_Number*100 + 10**6 * df$County_Code + 10**9 * df$State_Code
head(df)

#Per rimuovere una colonna (permanentemente a differenza di Pandas, dove è richiesto il comando inplace=true)
df$GEOID = NULL
head(df)

#non visualizzo la prima riga
head(df[-1,])

#non visualizzo la prima colonna
head(df[,-1])


#Pare non sia possibile introdurre un index con dei valori non univoci
row.names(df) = df$State_Code


#Sembra che tramite la libreria data.table sia possibile fare il multi-index
library(data.table)
datatable <- as.data.table(df)
setkey(datatable, State_Code,County_Code)
head(datatable)



print("Percentage of mortages in each census tract insured by FHA")
summary(df$PCT_AMT_FHA)
summary(df)

hist(df$PCT_AMT_FHA, breaks=50, col = rgb( 0, 0, 1, 0.1))

#Siccome la distribuzione ha una coda estesa sulla destra, trasformiamo la variabile nel suo logaritmo
#Creo una nuova colonna
df$LOG_AMT_ALL = log1p(df$AMT_ALL)
summary(df$LOG_AMT_ALL)
hist(df$LOG_AMT_ALL, breaks = 50, col = rgb(1, 0, 0, 0.5))

#Applico una funzione e plotto direttamente il risultato, senza modificare il dataframe
hist(sapply(df$AMT_ALL, log1p), breaks = 50, col = rgb(1, 0, 0, 0.5))


#Indexing data frames -----------------------------
#Richiamando la funzione head ad una colonna di un dataframe, 
#R restituisce un vettore di valori
head(df$State_Code)

#Per ottenere un sottoinsieme di un dataframe
head(df[,c("State_Code","County_Code")])

#Per recuperare i nomi delle colonne di un dataframe
colnames(df)

#Per selezionare delle righe specifiche
df[1:4,]

#Per indicizzare un particolare elemento del dataframe, riga 4 e colonna 3
df[4,3] 

#Per ottenere i valori compresi dalla riga 1 alla 4, e dalla colonna 4 alla 6, compresi
df[1:4, 4:6]

#Oppure utilizzando i nomi delle colonne
df[1:4, c("State_Code","Census_Tract_Number")]


#Filtering data-------------------------------

head(df$State_Code == 1)
head(df$State_Code == 1 & df$Census_Tract_Number == 9613)

#visualizzo le righe del dataframe solo dove State_Code = 5
head(df[df$State_Code == 5,])


#Joining data-----------------------------------

df_geo = read.csv('C:/Users/luca.ganugi/Desktop/Master MABIDA/Gigli/Data Reply/2013_Gaz_tracts_national.tsv', sep = '\t')
head(df_geo)

df_joined = merge(df,df_geo, by="GEOID", all.x= TRUE) #left join
head(df_joined)




#Aggregating data-----------------

#Filtro solo le righe con valore USPS=AK
head(df_joined[df_joined$USPS=='AK',])

#Ottengo la media per gruppi by USPS di tutte le colonne del df_joined
usps_groups_mean = aggregate(. ~ USPS, df_joined , mean)

# This is the analog of
# SELECT USPS, SUM(AMT_FHA), SUM(AMT_ALL), ... FROM df GROUP BY USPS;

df_by_state = aggregate(cbind(AMT_FHA, AMT_ALL, NUM_FHA, NUM_ALL) ~ USPS, df_joined , sum)
head(df_by_state)

df_by_state$PCT_AMT_FHA = 100.0 * df_by_state$AMT_FHA  / df_by_state$AMT_ALL

# This sure looks different than the census-tract level histogram!
hist(df_by_state$PCT_AMT_FHA, breaks = 20)

#Dataframe con 2 colonne a cui sono state applicate funzioni diverse
head(merge(aggregate(NUM_ALL ~ USPS, df_joined, mean), aggregate(NUM_FHA ~ USPS, df_joined, sum), by="USPS"))

#Creo un dataframe raggruppato per USPS
usps_groups = group_by(df_joined,USPS)

#Utilizzo dplyr per ordinare il dataframe sulla colonna INTPTLAT in senso discendente
arrange(usps_groups,desc(INTPTLAT))[1,]

#creo una funzione in cui per ogni valore di USPS mi restituisce il massimo di INTPTLAT                
farthest_north = function(state_df){
  result = arrange(state_df,desc(INTPTLAT))[1,]
  return (result)
}
#creo un dataframe che mi restituisce, per valore univoco di USPS, il valore massimo trovato nella colonna INTPTLAT
northest = summarise(usps_groups,farthest_north=max(INTPTLAT))
northest




#Sorting by indices and columns----------------------------------
#Sort by index
head(df_by_state[order(row.names(df_by_state), decreasing = TRUE),])

#Sort by valori in una colonna
head(df_by_state[order(df_by_state$AMT_FHA, decreasing = TRUE),])


#Unique values ---------------------------
#valori univoci della colonna State_Code (primi 10 valori)
unique(df$State_Code)[1:10]

#Count della frequenza di State_Code, in senso decrescente
head(sort(table(df$State_Code), decreasing = TRUE))

#verifica della presenza degli elementi di State_Code nei primi 3 valori (restituisce una lista di booleani)
head(is.element(df$State_Code,head(df$State_Code,3)))





#Handling missing and NA data--------------------------
#Primi 10 valori di GEOID
df$GEOID[1:10]
#Ottengo una lista di boleani
is.na(df$GEOID[1:10])

#lunghezza del vettore GEOID
length(df$GEOID)
#Conta dei valori non nulli in GEOID
sum(!is.na(df$GEOID))

library(zoo)
#Sostituisco i valori null di GEOID con 0
df$GEOID0 = replace(df$GEOID,is.na(df$GEOID),0)
#interpolo i valori NA
df$GEOIDinterp = na.approx(df$GEOID, na.rm = FALSE)
#inserisco al posto degli NA il valore medio della colonna
df$GEOIDmean = na.aggregate(df$GEOID,fun=mean)
#fill forward
df$GEOIDforw = na.locf0(df$GEOID)
#fill backward
df$GEOIDback = na.locf0(df$GEOID, fromLast = TRUE)
#riepilogo e confronto
head(df[,c('GEOID','GEOID0','GEOIDinterp','GEOIDmean','GEOIDforw','GEOIDback')],10)


text_series <- as.character(sprintf("%.1f",replace(df$GEOID,is.na(df$GEOID),0)))
head(text_series,10)
text.split = unlist(strsplit(text_series, split = ".", fixed = TRUE))
head(text.split)

#
#for (i in text.split){
#  asd[i]=(cbind(text.split[i],text.split[i]))
#}

#valore medio della colonna GEOID, ottenuto rimuovendo i valori nulli
mean(df$GEOID, na.rm = TRUE)






#Manipulating strings-------------------------------------------------------

#seleziono solo i valori non nulli di USPS
states = df_joined$USPS[!is.na(df_joined$USPS)]
#mi restituisce i valori di states contenenti A
head(states[grepl('A', states)])

#Indices in Pandas--------------------------------------------------------


#non mi riesce di trovare il modo di sommare per index
s1 <- c(1, 2, 3)
s2 <- c(3, 2, 1)
s1 + s2
s3 <- c(3, 2, 1)
s1 + s3


#Function application and mapping---------------------------------------------
#Creo un dataframe di 24 numeri, dallo 0 al 23, in 4 righe e 6 colonne
df1 <- as.data.frame(matrix(0:23, ncol = 6, nrow = 4,byrow = TRUE))
#Applico la funzione seno a tutti gli elementi
head(sin(df1))
#Formatto i numeri del df1 mediante una funzione
apply(df1,1:2, function(x) sprintf("%.2f",x))
#funzione differenza
apply(df1,1, function(x) max(x)-min(x)) #differenze per riga
apply(df1,2, function(x) max(x)-min(x)) #differenze per colonna




#Pandas HTML data import example------------------------------------------------
library(htmltab)
library(stringi)
url <- "http://en.wikipedia.org/wiki/List_of_tallest_buildings_and_structures_in_the_world"
#Importo nell'oggetto tallest i dati dal sito web
tallest <- htmltab(doc = url, which = 3, encoding = "UTF-8", lang="en", parse_dates=False)
#separo i valori delle coordinate per /
strsplit(tallest$Coordinates,"/")
#Creo un oggetto tl contenente le coordinate estratte tramite regulary expression
tl = t(as.data.frame(stri_extract_all(tallest$Coordinates, regex = "-?\\d{1,3}+\\.?\\d{4,6}")))
#inserisco nel dataframe tallest i 2 vettori longitude e latitude
tallest$Longitude = tl[, 4]
tallest$Latitude = tl[, 3]
#Et voila
head(tallest)


#Pandas Timestamps----------------------------------------------------

library(timeDate)
as.POSIXct("July 4, 2016",format='%B %d, %Y')
as.POSIXlt('Monday, July 4, 2016', format = "%A, %B %d, %Y")
as.POSIXlt('Tuesday, July 4th, 2016', format = "%A, %B %dth, %Y")
as.POSIXlt('Monday, July 4th, 2016 05:00 PM', format = "%A, %B %dth, %Y %I:%M %p")
as.POSIXlt('04/07/2016T17:20:13.123456', format = "%d/%m/%YT%H:%M:%OS")
as.Date(as.POSIXlt(1467651600000000000/1000000000, origin="1970-01-01"))

july4 = as.POSIXlt('Monday, July 4th, 2016 05:00 PM', format = "%A, %B %dth, %Y %I:%M %p", tz = "US/Eastern")
labor_day = as.POSIXlt('9/5/2016 12:00', format = "%d/%m/%Y %H:%M", tz = "US/Eastern")
thanksgiving = as.POSIXlt('11/24/2016 16:00', format = "%m/%d/%Y %H:%M") # no timezone

#differenze tra date
labor_day - july4
thanksgiving - july4 #a differenza di pandas anche se le timezones sono differenti il calcolo viene eseguito


library(bizdays)
library(lubridate)

seq.POSIXt( from=july4, by="5 day", length.out=2 )[2]# 5 calendar days later, a Saturday.

#creo una funzione per ricavare l'ultimo giorno del mese
last_day_month <- function(date) {
  ceiling_date(date, "month") - days(1)
}
last_day_month(ymd(20160704)) #last  day of the month.

#Creo un calendario chiamato cal, in cui setto sabato e domenica come non lavorativi
create.calendar(name='test', holidays=holidaysANBIMA, weekdays=c('saturday', 'sunday'))
bizdays.options$set(default.calendar='test')
cal = bizdays.options$get("default.calendar")

#aggiungo a july4
offset(july4, 5, cal) #5 giorni lavorativi
offset(july4, -1, cal) #-1 giorno lavorativo

# l'ultimo giorno lavorativo di luglio
offset(last_day_month(ymd(20160704)), -1, cal) 



#Genero una lista di date lavorative del 2016
business_days = bizseq('2016-01-01', '2016-12-31', "test")
business_days


#Creo una tabella contenente i giorni lavorativi e un indice generato casualmente
tabella = data.frame(business_days, sample(1:length(business_days), replace = FALSE))
row.names(tabella) = seq(1, length(business_days))
colnames(tabella) = c('Giorno lavorativo','N°')
head(tabella)


#Campiono 5 giorni dalla lista business_days
date = sample(business_days, size=5, replace = FALSE) 
#Converto date in un vettore di 'date' 
tempdate = as.POSIXct(date)
cbind(US = format(tempdate, tz="America/New_York"), UK = format(tempdate, tz = "Europe/Rome"))


## Multi-indices, stacking, and pivot tables--------------------------------------------
group1 = group_by(df_joined, State_Code, County_Code)
group2 = summarise(group1, NUM_ALL = sum(NUM_ALL), NUM_FHA = sum(NUM_FHA))
head(grouped1)

#pd.pivot_table(df, index='State_Code', columns='County_Code',
#               values=['NUM_ALL', 'NUM_FHA'], aggfunc=np.sum).head()








