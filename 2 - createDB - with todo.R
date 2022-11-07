library(httr)
library(jsonlite)
library(reshape2)
library(xts)

library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(magrittr)


##########################################################################
#####                    Importation via une API
##########################################################################

# https://opendata.agenceore.fr/explore/dataset/conso-elec-gaz-annuelle-par-secteur-dactivite-agregee-departement

url = "https://opendata.agenceore.fr/api/records/1.0/search/?dataset=conso-elec-gaz-annuelle-par-secteur-dactivite-agregee-departement&q=&rows=10000&facet=operateur&facet=annee&facet=filiere&facet=code_departement&facet=libelle_departement&facet=libelle_region"

elec_cons = httr::GET(url)
elec_cons = fromJSON(content(elec_cons,type="text"))$records$fields

head(elec_cons)
summary(elec_cons)
unique(elec_cons$filiere)
unique(elec_cons$code_departement)
table(elec_cons$annee, elec_cons$code_departement)


elec_cons[(elec_cons$code_departement=="09" & elec_cons$annee == 2011),]

elec_cons = elec_cons[elec_cons$filiere == "Electricité",]

col_keep = c("annee","consototale","code_departement")
elec_cons = elec_cons[,col_keep]


elec_cons = dcast( elec_cons, annee ~ code_departement
                 , function(x) sum(x,na.rm=T)
                 , value.var = "consototale")
melt(elec_cons)



apply(elec_cons[,-1], 1, function(x) mean(x,na.rm=T))
apply(elec_cons[,-1], 2, function(x) mean(x,na.rm=T))

elec_cons$total = apply(elec_cons[,-1], 1, function(x) sum(x,na.rm=T))


##########################################################################
#####                    Importation via un CSV
##########################################################################

# Importation via quantmod
sp500    = Cl(getSymbols( "^GSPC"       , src='yahoo', from="1980-01-01", auto.assign=F, warnings=F, methods="curl" ))
brent    =    getSymbols( "DCOILBRENTEU", src='FRED' , auto.assign=F, methods="curl")
usbond10 =    getSymbols( "DGS10"       , src='FRED' , auto.assign=F, methods="curl")
eurusd   =    getSymbols( "DEXUSEU"     , src='FRED' , auto.assign=F, methods="curl")
uscpi    =    getSymbols( "CPIAUCSL"    , src='FRED' , auto.assign=F, methods="curl")
usgdp    =    getSymbols( "GDPC1"       , src='FRED' , auto.assign=F, methods="curl")

# Importation via des fichier .csv
# https://github.com/dliron/datasets_M2
sp500    = read.table("../data/sp500.csv"   , header = T, sep=",", dec=".") 
brent    = read.table("../data/brent.csv"   , header = T, sep=",", dec=".") 
usbond10 = read.table("../data/usbond10.csv", header = T, sep=",", dec=".") 
eurusd   = read.table("../data/eurusd.csv"  , header = T, sep=",", dec=".") 
uscpi    = read.table("../data/uscpi.csv"   , header = T, sep=",", dec=".") 
usgdp    = read.table("../data/usgdp.csv"   , header = T, sep=",", dec=".") 


# Produire des statistiques desctiptives des variables importées


# Statistiques descriptives
tail(sp500);head(sp500)
class(sp500)
summary(sp500)
plot(sp500$GSPC.Close, type = 'l')
plot(density(sp500$GSPC.Close))




# Alternative
list_ind = c("usgdp","brent","usbond10","eurusd","uscpi","usgdp")
for( ind in list_ind) {
  filename = sprintf( "../data/%s.csv",ind)
  assign(ind, read.table(filename, header = T, sep=",",dec=".") )
}



# Transformation en séries temporelles
usgdp_ts  = ts( usgdp$GDPC1, start = 1947, frequency = 4)
window(usgdp_ts, start = 2020)
aggregate(usgdp_ts, nfrequency = 1, function(x) mean(x,na.rm=T))
lag(usgdp_ts, -1)

usgdp_xts = xts( usgdp$GDPC1, order.by = as.Date(usgdp$date) )
usgdp_xts["2020/"]
usgdp_xts_y = apply.yearly(usgdp_xts, function(x) mean(x,na.rm=T))
index(usgdp_xts_y)
index(usgdp_xts_y) = as.Date( format( index(usgdp_xts_y), "%Y-01-01"))
lag(usgdp_xts, 1)

sp500 = xts( sp500$GSPC.Close, order.by = as.Date(sp500$date) )
rollapply(sp500,252,function(x)mean(x,na.rm=T))
plot(sp500["2015/"])
lines(rollapply(sp500,252,function(x)mean(x,na.rm=T)) ["2015/"] ,col='red')




# TODO : Transformer les variables importées en xts











# Calcul de l'inflation et de la croissance (Year on Year)
# Le faire sur la frequence de la variable, avant concatenation
usinfl = (uscpi/lag(uscpi,12)-1)*100
ustgdp = (usgdp/lag(usgdp,4)-1)*100

# Concatenation de toutes les variables dans une base
base = cbind(sp500, brent, usbond10, eurusd, usinfl, ustgdp)
colnames(base) = c("sp500", "brent", "usbond10", "eurusd", "usinfl", "ustgdp")


# Gestion des NAs

na.fill(base$usinfl,0)["2022-01/"] %>% plot()
na.approx(base$usinfl, na.rm=F)["2022-01/"] %>% plot()
na.locf(base$usinfl, fromLast = T)["2022-01/"] %>% plot()
na.locf(base$usinfl, fromLast = F)["2022-01/"] %>% plot()



# TODO : gerer les NAs des colonnes inflations et pib











# TODO : calculer sur le sp500 
#    1 - le return quotidien ( = croissance quotidienne)
#    2 - Le return calculé précédemment,moyen sur 1 et 3 mois (utiliser rollapply)
#    3 - la volatilité moyenne du mêle return, à 1 et 3 mois
#    4 - Indicateurs chartistes : macd, bande de bollinger up and down

















# Discretisation des returns

q = quantile( ret, seq(0,1,0.1), na.rm=T )

findInterval(ret, q)
table(findInterval(ret, q))

stepq = 0.1
q = quantile( ret, seq(0+stepq,1-stepq,stepq), na.rm=T )

findInterval(ret, q)+1
table(findInterval(ret, q)+1)




# TODO : Calculer un indicateur de choc
# vaut 1 si 5% des pires baisses, 0 sinon














# Definition de la cible
targ  = lag(choc, -21)

# Concatenation des résultats et écritures dans un csv
colnam = colnames(base)
base = cbind(base, macd, bb_up, bb_dn, vol1m, vol3m, ret1m, ret3m, choc, targ)
colnames(base) = c(colnam, "macd", "bb_up", "bb_dn", "vol1m", "vol3m", "ret1m", "ret3m", "choc", "target")


base_df = data.frame( date = index(base)
                    , base             )
write.table(base_df, "data/base.csv", sep=",", dec = ".", row.names = F)




