import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import sklearn
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix, plot_roc_curve

pd.set_option('display.max_rows', 500)
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)

pd.options.mode.chained_assignment = None  # default='warn'

# base = pd.read_csv( "base.csv",index_col=0)

base = pd.read_csv( "base.csv")
base = base[['date', 'sp500', 'brent', 'usbond10', 'eurusd', 'usinfl', 'ustgdp']]

from datetime import datetime


# Transformer la base en serie temporelle










base = base.set_index('date')
base.index = pd.to_datetime(base.index,format="%Y-%m-%d")

# Supression des weekends







base = base[base.index.dayofweek < 5]



# Calculer volatilité et moyenne mobille sp500 sur 21 jours



base.sp500.rolling(window=21).std()
base.sp500.rolling(window=21).mean()



# Faire une fonction taux de croissance (via un lag)


def growth(x,nblags):
    return x / x.shift(nblags) -1
  
def Bollinger_Bands(stock_price, window_size, num_of_std):
    
    rolling_mean      = stock_price.rolling(window=window_size).mean()
    rolling_std       = stock_price.rolling(window=window_size).std()
    upper_band        = rolling_mean + (rolling_std*num_of_std)
    lower_band        = rolling_mean - (rolling_std*num_of_std)
    upper_minus_lower = upper_band - lower_band

    return rolling_mean, upper_band, lower_band, upper_minus_lower

def MACD(stock_price,ma1, ma2):
    ema1 = pd.DataFrame.ewm(stock_price, span=ma1).mean()
    ema2 = pd.DataFrame.ewm(stock_price, span=ma2).mean()
    macd = (ema2 - ema1)

    return macd


sp500 = base[['sp500']].dropna()

sp500['sp500_Mean'],sp500['sp500_Bollinger_High'],sp500['sp500_Bollinger_Low'],sp500['sp500_Bollinger_up_minus_down'] = Bollinger_Bands(stock_price = sp500[['sp500']], window_size = 21, num_of_std = 2)
sp500['MACD'] = MACD(stock_price = sp500[['sp500']],ma1=26,ma2=12)
sp500[['sp500','sp500_Mean','sp500_Bollinger_High','sp500_Bollinger_Low']][-200:].plot()
plt.show()

base = base.join(sp500[['sp500_Bollinger_up_minus_down','MACD']], how='outer')





base = base.dropna()

base["ret1m"] = growth( base[['sp500']] ,21)
base["vol1m"] = base[['sp500']].rolling(21).std()



base["choc"] = (base.ret1m <= base.ret1m.quantile(.05) ).astype('int')


base["target"] =  base.choc.shift(-21).astype('Int64')


base = base.drop(columns=["choc"])

base = base.dropna()



# base.columns
# col_keep = [ 'brent', 'usbond10', 'eurusd', 'usinfl'
#            , 'ustgdp', 'macd', 'rsi', 'bbw', 'vol1m', 'vol3m'
#            , 'ret1m', 'ret3m', 'target']
# 
# base = base[col_keep]

# creer une base x avec toutes les exogenes sauf la cible
# creer une y avec uniquement la cible



x = base.drop(columns=["target"])
y = base["target"].values



# creer x_train, x_test, y_train, y_test

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size = 0.2, random_state = 123, shuffle = False)

rf = sklearn.ensemble.RandomForestClassifier( random_state      = 123
                                            , n_estimators      = 200
                                            , max_depth         = 5
                                            , min_samples_split = 0.001
                                            , max_features      = 0.5
                                            )

rf.fit(x_train,y_train)

# Faire une prediction


pred_train = rf.predict_proba(x_train)
pred_test  = rf.predict_proba(x_test)

rf.predict(x_test)


threshold = 0.5

# Afficher la matrice de confusion train et test

cm_train = confusion_matrix(y_train, (pred_train[:,1] >= threshold).astype('int'))
cm_test  = confusion_matrix(y_test, (pred_test[:,1] >= threshold).astype('int'))


# Faire une fonction qui calcule les performances de la matrice de confusion, pour un seuil donné


def performances_custom(y, y_pred_proba, threshold = 0.5) :
  y_pred = (y_pred_proba[:,1] >= threshold).astype('int')
  
  cm = confusion_matrix(y, y_pred)
  
  TN = cm[0,0]
  TP = cm[1,1]
  FN = cm[1,0]
  FP = cm[0,1]
  e = 1e-6
  NPV  = (TN + e) / (TN + FN + e)
  PPV  = (TP + e) / (TP + FP + e)
  Se   = (TP + e) / (TP + FN + e)
  Sp   = (TN + e) / (TN + FP + e)
  Pr   = (TP + FN) / (TN + TP + FN + FP)
  Acc  = (TN + TP) / (TN + TP + FN + FP)
  
  return pd.DataFrame([{ "Threshold": threshold, "Accuracy": Acc, "NPV": NPV, "PPV": PPV, "Sensitivity": Se, "Specificity": Sp, "Prevalence": Pr }])

performances_custom(y_test, pred_test, threshold = 0.5)


step = 0.05
df_perf = performances_custom(y_test, pred_test, 0)
for i in np.arange(step,1+step,step):
  df_perf = pd.concat([df_perf,performances_custom(y_test, pred_test, i)])

df_perf["best"] = df_perf["Sensitivity"] + df_perf["Specificity"]


plt.scatter(1-df_perf["Specificity"],df_perf["Sensitivity"], marker=None)
plt.show()

plot_roc_curve(rf, x_train, y_train)
plt.show()

plot_roc_curve(rf, x_test, y_test)
plt.show()


