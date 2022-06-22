### ETAPE 1 ###
#Nous effectuons creation d un dataset et procedons a la lecture et verification de la nature des donnees.
dataset <- read.csv("data.csv")
View(dataset)
str(dataset)

  

### ETAPE 2 ###
#Nous identifions les variables a supprimer:
#La premiere variable n'apporte aucune information pertinente.
#Les six dernieres variables sont mutuellement exclusives, ie. aucune "entry" en ligne ne repond a plus de deux de ces six variables,
#De plus, leur utilite etant marginale, nous decidons de les supprimer.
cleandataset <- dataset[-c(1,20,26:31)]
View(cleandataset)
dataset <- cleandataset

  
  
### ETAPE 3 ###
#Nous sommes confrontes au probleme de cellules incompletes.
#Certaines sont "N/A" et d'autres simplement vides (" ").
#Création d un dataset de test.
testdataset <- cleandataset

#Nous avons pu observer que certaines lignes avaient des données manquantes.
#Nous procedons a la transformation des cases vides (" ") en "N/A".
testdataset[testdataset==""] <- NA

#Nous procedons maintenant a la suppression de toutes les lignes "entry" comportant au minimum une case "N/A".
testdataset <- na.omit(testdataset)

#Nous procedoncs a une verification des donnees.
View(testdateset)
#Les donnes etant validees nous copions le dataset de test dans le dataset originel.
dataset <- testdataset

  
  
### ETAPE 4 ###
#Nous procedons a nouveau a une verification de la nature des donnes.
str(dataset)
#Nous procedons a la transformation en type "factor" des variables suivantes :
#"year" , "manufacturer" , "model" , "tax_band" , "euro_standard" , "description" , "transmission" , "transmission_type" , "fuel_type".
dataset$year <- factor(dataset$year)
dataset$manufacturer <- factor(dataset$manufacturer)
dataset$model <- factor(dataset$model)
dataset$tax_band <- factor(dataset$tax_band)
dataset$euro_standard <- factor(dataset$euro_standard)
dataset$description <- factor(dataset$description)
dataset$transmission <- factor(dataset$transmission)
dataset$transmission_type <- factor(dataset$transmission_type)
dataset$fuel_type <- factor(dataset$fuel_type)

#Nous procedons a une verification intermediaire des donnees.
str(dataset)

#Nous procedons a la transformation en tyme "numeric" des variables suivantes:
#"particulates_emissions" , "co2" , "engine_capacity"
dataset$particulates_emissions <- as.numeric(dataset$particulates_emissions)
dataset$co2 <- as.numeric(dataset$co2)
dataset$engine_capacity <- as.numeric(dataset$engine_capacity)



### ETAPE 5 ###
#Nous verifions la normalite de la distribution des variables quantitatives.

#Nous utilisons a nouveau un dataset de test.
testdataset <- dataset

#Nous effectuons des boites a moustache des variables quantitatives.
boxplot(testdataset$engine_capacity, main= "Engine capacity", ylab="cm3")
boxplot(testdataset$urban_metric, main= "Consommation urbaine en métric", ylab="L/100")
boxplot(testdataset$extra_urban_metric, main= "Consommation extra urbaine en métric", ylab="L/100")
boxplot(testdataset$combined_metric, main= "Consommation combinée en métric", ylab="L/100")
boxplot(testdataset$urban_imperial, main= "Consommation urbaine en imperial", ylab="MPG")
boxplot(testdataset$extra_urban_imperial, main= "Consommation extra urbaine en imperial", ylab="MPG")
boxplot(testdataset$combined_imperial, main= "Consommation combinée en imperial", ylab="MPG")
boxplot(testdataset$noise_level, main= "Bruit", ylab="dB")
boxplot(testdataset$co2, main= "Emissions de dioxyde de carbone", ylab="g/km")
boxplot(testdataset$co_emissions, main= "Emissions monoxyde de carbone", ylab="g/km")
boxplot(testdataset$nox_emissions, main= "Emissions de nox", ylab="g/km")
boxplot(testdataset$thc_nox_emissions, main= "Emissions de nox et ths", ylab="g/km")
boxplot(testdataset$particulates_emissions, main= "Emissions de particules", ylab="g/km")
boxplot(testdataset$fuel_cost_12000_miles, main= "Fuel cost", ylab="£")


### ETAPE 5 BIS ###
#La normalite de la distribution n etant pas parfaite, nous decidons de normaliser les variables.

#Nous creons un dataset disposant uniquement des variables quantitatives.
quanti.dataset <- dataset[c(9, 11:23)]

#Nous procedons a la normalisation des variables au travers de la methode des z-scores.
quanti.dataset.zscore <- scale(quanti.dataset)



### ETAPE 6 ###
#Nous procedons a l execution de l algorithme de classification hierarchique.

#Nous procedons au calcul de la distance euclidienne entre les donnes.
dataset.distance <- dist(quanti.dataset.zscore, method="euclidean")

#Nous procedons au regroupement des voitures en groupes au travers de la classification hierarchique.
dataset.results <- hclust(dataset.distance, method="ward.D")



### ETAPE 7 ###
#Identification du nombre de groupes

#Visualisation du dendogramme des groupes
plot(dataset.results,  main="Dendrogramme des modeles de voiture",)

#Visualisation du nombre de voitures dans chaque groupe.
rect.hclust(tree= dataset.results, k=5, border="red",)
#Enregistrement d'une solution avec cinq groupes de voitures.
dataset$groupe <- cutree(dataset.results, k=5)

table(dataset$groupe)
prop.table(table(dataset$groupe))



### Etape 8 ###
#Nous verifions de l'existance d'un lien significatif entre les groupes et les emissions de co, co2, nox et particules.
summary(aov(dataset$co_emissions ~ dataset$groupe))
summary(aov(dataset$co2 ~ dataset$groupe))
summary(aov(dataset$nox_emissions ~ dataset$groupe))
summary(aov(dataset$particulates_emissions ~ dataset$groupe))

#Nous procedons a des chisq-tests pour les variables qualitatives.
chisq.test(table(dataset$manufacturer, dataset$groupe), simulate.p.value = TRUE)
chisq.test(table(dataset$tax_band, dataset$groupe), simulate.p.value = TRUE)
chisq.test(table(dataset$transmission, dataset$groupe), simulate.p.value = TRUE)
chisq.test(table(dataset$transmission_type, dataset$groupe))
chisq.test(table(dataset$fuel_type, dataset$groupe), simulate.p.value = TRUE)
chisq.test(table(dataset$year, dataset$groupe))

#Les hypotheses sont les suivantes :
#H0 : Il n existe pas de lien entre le groupe et les variables sus-citées.
#H1 : Il existe un lien entre le groupe et les variables.
#Puisque pour tous les chisq-test les p_values obtenues sont inferieures a 5%, nous rejettons H0.
#Nous pouvons donc en conclure qu il existe un lien entre les variables "year" , "manufacturer", "tax_band" , "transmission" , "transmission_type" et "fuel_type".

#Nous procedons au des moyennes par groupes pour les variables quantitatives.
apply(quanti.dataset, 2, mean)
tapply(dataset$co_emissions, dataset$groupe, mean)
tapply(dataset$co2, dataset$groupe, mean)
tapply(dataset$nox_emissions, dataset$groupe, mean)
tapply(dataset$particulates_emissions, dataset$groupe, mean)

#A l aide du test de Tukey nous comparons les moyennes des groupes 2 a 2.
dataset$groupe <- factor(dataset$groupe)
TukeyHSD(aov(dataset$co_emissions ~ dataset$groupe))
TukeyHSD(aov(dataset$co2 ~ dataset$groupe))
TukeyHSD(aov(dataset$nox_emissions ~ dataset$groupe))
TukeyHSD(aov(dataset$particulates_emissions ~ dataset$groupe))

### Etape 9 ###
#Interpretation des resultats

#Monoxyde de carbone
#Au regard des p_values, il n'y a pas de difference significative dans les emissions des paires de groupes 5-1 (p_value = 69,41%) et 5-4 (p_value=83,75%)

#Dioxyde de carbone
#Au regard des p_values qui etaient toutes nulles, il existe une difference significative dans les emissions moyennes de CO2 de chacun des groupes 2 a 2.

#Emissions de nox
#Au regard des p_values, toutes inferieures a 5% on peut en conclure qu'il existe une difference significative dans les emissions moyennes de Nox de chacun des groupes 2 a 2.

#Emissions de particules
#Au regard des p_values, toutes inferieures a 5%, on peut en conclure qu'il existe une difference significative dans les emissions moyennes de particulues de chacun des groupes 2 a 2.


### Etape 10 ###
#Creation des tableaux de frequence pour les variables qualitatives
prop.table(table(dataset$manufacturer, dataset$groupe), 1)
prop.table(table(dataset$tax_band, dataset$groupe), 1)
prop.table(table(dataset$transmission, dataset$groupe), 1)
prop.table(table(dataset$transmission_type, dataset$groupe), 1)
prop.table(table(dataset$fuel_type, dataset$groupe), 1)
prop.table(table(dataset$year, dataset$groupe), 1)


#Nous cherchons a determiner quels sont les modeles qui ont la contribution marginale la plus elevee pour le groupe 1.
prop.table(table(dataset$model, dataset$groupe), 2)



### CONCLUSION ###
#Au regard des resultats nous pouvons en conclure que les modeles qui ont la contribution marginale au groupe 1 la plus importante.
#sont les modeles BMW Serie 1, 3 et 5 portes des annees 2011 et 2012
#La VW Golf 2012 et Golf 2013
#La Peugeot 308

#On peut en conclure que ce sont ces modeles qui sont a privilegier pour le choix des voitures des experts-comptables.