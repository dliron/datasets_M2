

##########################################################################
#####                            LES TYPES
##########################################################################

type(1)
type(1.0)
type("1")
type(True)
type(None)

float(1)
int("1")

isinstance(1,int)
isinstance(1,float)



# Affecter une valeur à une variable
x = 1

x + 1
x = x+1
x += 1

# Différents affichages
print("La valeur de x est ", x, sep = "")
print("La %s valeur %s de x est %s" % (x,x,x))


##########################################################################
#####                            LES STRUCTURES
##########################################################################
import numpy as np

# Les matrices
# Python n'a pas de type Matrice prédéfinie. On peut procéder ainsi:
matrice = [[1, 4, 5, 12] 
          ,[-5, 8, 9, 0]
          ,[-6, 7, 11, 19]]
matrice.shape
matrice[0]
matrice[0][0]

# Sinon il faut passer par Numpy
matrice = np.array([[1, 4, 5, 12] 
                  ,[-5, 8, 9, 0]
                  ,[-6, 7, 11, 19]])
matrice.shape

# Les listes
li = []
type(li)

li.append(2)
li = li + [2,"test"]

len(li)

# Les dictionnaires
d = {}
type(d)

d["pseudo"] = ["Didier","Liron"]
d["mot de passe"] = ["test","test2"]
d.keys()
d.values()

# Les dataframe
import pandas as pd

df = {'col1':["a","b"], 'col2':["c","d"]}
df = pd.DataFrame(df)

df["col3"] = ["e","f"]
df.index = ["row1","row2"]

df.loc["row1"]
df.iloc[0]

df.append(df.iloc[0])



##########################################################################
#####                            LES CONDITIONS
##########################################################################
from random import randint
from random import seed

a = 1
nb_alea = randint(1,10)
if nb_alea == a :
  print("Les deux valeurs sont identiques")
else:
  print("Les deux valeurs sont differentes: a = %s, nb_alea = %s" %( a, nb_alea) )



# Le seed
randint(1,10)

seed(123)
randint(1,10)



##########################################################################
#####                            LES BOUCLES
##########################################################################


# la boucle for
for i in range(0,30) :
  nb_alea = randint(1,10)
  if nb_alea == a :
    print("Les deux valeurs sont identiques")
  else :
    print("Les deux valeurs sont differentes: a = %s, nb_alea = %s" %( a, nb_alea) )

list_valeur = ["ceci","est","une","phrase","!"]
for i in list_valeur:
  print( i )


# la boucle while
nb_alea = randint(1,10)
while nb_alea != a :
  print("Les deux valeurs sont differentes: a = %s, nb_alea = %s" %( a, nb_alea) )
  nb_alea = randint(1,10)

print(nb_alea)



##########################################################################
#####                           LES FONCTIONS
##########################################################################

def is_a_equal_b(a, b, verbose = False):
  res = np.NAN
  if a == b:
    res = True
  else:
    res = False
    
  if (verbose and res) :
    print("yes, a and b are equal !")
  
  if (verbose and not(res)) :
    print("no, a and b are not equal ...")
  
  return(res)


is_a_equal_b(a = 2, b = 1)
is_a_equal_b(b = 1, a = 2)
is_a_equal_b(1,2)

is_a_equal_b(2,1)
is_a_equal_b(1,1,verbose=True)


##########################################################################
#####                       IMPORTER DES DONNEES
##########################################################################

# Via une API
import requests
import json

url = "https://opendata.agenceore.fr/api/records/1.0/search/?dataset=conso-elec-gaz-annuelle-par-secteur-dactivite-agregee-departement&q=&rows=10000&facet=annee&facet=code_departement&refine.filiere=Electricit%C3%A9"
data = requests.get(url)
data = data.json()



data = pd.json_normalize(data['records'])

data_cols = [col for col in data.columns if 'fields.' in col]
data = data[data_cols]
data.columns = data.columns.str.lstrip("fields.")




