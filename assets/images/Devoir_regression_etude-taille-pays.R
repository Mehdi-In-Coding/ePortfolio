library(ggplot2)

#Age Fiji
age_values_fiji = seq(min(Data$Age[Data$Pays == "Fiji"]), max(Data$Age[Data$Pays == "Fiji"]), by = 0.1)
#Age Swaziland
age_values_Swaziland = seq(min(Data$Age[Data$Pays == "Swaziland"]), max(Data$Age[Data$Pays == "Swaziland"]), by = 0.1)

#Determine AIC le plus petit ===================================================
#Fiji garcon
for (i in c(1:25)) {
  print(AIC(lm(Taille ~ poly(Age, i), data = subset(Data, Sexe == "Boys" & Pays == "Fiji"))))
}

preticted_height_boys_fiji1 = predict(lm(Taille ~ poly(Age, 1), data = subset(Data, Sexe == "Boys" & Pays == "Fiji")), newdata = data.frame(Age = age_values_fiji))
preticted_height_boys_fiji2 = predict(lm(Taille ~ poly(Age, 2), data = subset(Data, Sexe == "Boys" & Pays == "Fiji")), newdata = data.frame(Age = age_values_fiji))
preticted_height_boys_fiji3 = predict(lm(Taille ~ poly(Age, 4), data = subset(Data, Sexe == "Boys" & Pays == "Fiji")), newdata = data.frame(Age = age_values_fiji))
preticted_height_boys_fiji4 = predict(lm(Taille ~ poly(Age, 6), data = subset(Data, Sexe == "Boys" & Pays == "Fiji")), newdata = data.frame(Age = age_values_fiji))

#Fiji fille
for (i in c(1:25)) {
  print(AIC(lm(Taille ~ poly(Age, i), data = subset(Data, Sexe == "Girls" & Pays == "Fiji"))))
}

preticted_height_girls_fiji1 = predict(lm(Taille ~ poly(Age, 1), data = subset(Data, Sexe == "Girls" & Pays == "Fiji")), newdata = data.frame(Age = age_values_fiji))
preticted_height_girls_fiji2 = predict(lm(Taille ~ poly(Age, 2), data = subset(Data, Sexe == "Girls" & Pays == "Fiji")), newdata = data.frame(Age = age_values_fiji))
preticted_height_girls_fiji3 = predict(lm(Taille ~ poly(Age, 4), data = subset(Data, Sexe == "Girls" & Pays == "Fiji")), newdata = data.frame(Age = age_values_fiji))
preticted_height_girls_fiji4 = predict(lm(Taille ~ poly(Age, 6), data = subset(Data, Sexe == "Girls" & Pays == "Fiji")), newdata = data.frame(Age = age_values_fiji))

#Swaziland garcon
for (i in c(1:25)) {
  print(AIC(lm(Taille ~ poly(Age, i), data = subset(Data, Sexe == "Boys" & Pays == "Swaziland"))))
}

preticted_height_boys_Swaziland1 = predict(lm(Taille ~ poly(Age, 1), data = subset(Data, Sexe == "Boys" & Pays == "Swaziland")), newdata = data.frame(Age = age_values_Swaziland))
preticted_height_boys_Swaziland2 = predict(lm(Taille ~ poly(Age, 2), data = subset(Data, Sexe == "Boys" & Pays == "Swaziland")), newdata = data.frame(Age = age_values_Swaziland))
preticted_height_boys_Swaziland3 = predict(lm(Taille ~ poly(Age, 5), data = subset(Data, Sexe == "Boys" & Pays == "Swaziland")), newdata = data.frame(Age = age_values_Swaziland))
preticted_height_boys_Swaziland4 = predict(lm(Taille ~ poly(Age, 7), data = subset(Data, Sexe == "Boys" & Pays == "Swaziland")), newdata = data.frame(Age = age_values_Swaziland))

#Swaziland fille
for (i in c(1:25)) {
  print(AIC(lm(Taille ~ poly(Age, i), data = subset(Data, Sexe == "Girls" & Pays == "Swaziland"))))
}

preticted_height_girls_Swaziland1 = predict(lm(Taille ~ poly(Age, 1), data = subset(Data, Sexe == "Girls" & Pays == "Swaziland")), newdata = data.frame(Age = age_values_Swaziland))
preticted_height_girls_Swaziland2 = predict(lm(Taille ~ poly(Age, 2), data = subset(Data, Sexe == "Girls" & Pays == "Swaziland")), newdata = data.frame(Age = age_values_Swaziland))
preticted_height_girls_Swaziland3 = predict(lm(Taille ~ poly(Age, 4), data = subset(Data, Sexe == "Girls" & Pays == "Swaziland")), newdata = data.frame(Age = age_values_Swaziland))
preticted_height_girls_Swaziland4 = predict(lm(Taille ~ poly(Age, 5), data = subset(Data, Sexe == "Girls" & Pays == "Swaziland")), newdata = data.frame(Age = age_values_Swaziland))

#Nuage de points des garcon ====================================================
garcon <- Data[Data$Sexe == "Boys",]
#Nuage pays confondus
ggplot(garcon) +
  aes(x = Age, y = Taille) +
  geom_point(shape = "bullet",size = 1.5) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  labs(title = "Repartition de la taille en fonction de l'âge chez les garçons", x = "Age (en années décimales)", y = "Taille (en cm)")
#Couleur
ggplot(garcon) +
  aes(x = Age, y = Taille, colour = Pays) +
  geom_point(shape = "bullet",size = 1.5) +
  scale_color_manual(
    values = c(Fiji = "#5DA5DA",
               Swaziland = "#F15854")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  labs(title = "Repartition de la taille en fonction de l'age et par pays chez les garçons", x = "Age (en années décimales)", y = "Taille (en cm)")
#boite à moustache
p<-ggplot(garcon, aes(y=Taille, fill=Pays)) +
  geom_boxplot() + scale_fill_manual(values=c(Fiji = "#5DA5DA", Swaziland = "#F15854")) +
  ggtitle("Répartion de la taille par pays chez les garçons") + theme_minimal()
p
#Classe d'age
garcon$categ_age <- cut(garcon$Age, breaks = c(5,8,11,14,17,max(garcon$Age)))
Taille_Q1_garcon <- aggregate(garcon$Taille, by = list(garcon$categ_age), FUN = "quantile", probs = 0.25)
Taille_Median_garcon <- aggregate(garcon$Taille, by = list(garcon$categ_age), FUN = "quantile", probs = 0.5)
Taille_Q3_garcon <- aggregate(garcon$Taille, by = list(garcon$categ_age), FUN = "quantile", probs = 0.75)
Taille_Q1_garcon$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)
Taille_Median_garcon$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)
Taille_Q3_garcon$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)

ggplot(garcon) + 
  aes(x = Age, y = Taille) +
  geom_point(shape = 'bullet', size = 1.5, color = "gray52") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  geom_line(data = Taille_Q1_garcon, aes(x = Group.1, y = x), col = "dodgerblue3", lwd = 1) +
  geom_line(data = Taille_Median_garcon, aes(x = Group.1, y = x), col = "firebrick1", lwd = 1) +
  geom_line(data = Taille_Q3_garcon, aes(x = Group.1, y = x), col = "darkgreen", lwd = 1) +
  labs(title = "Observation du 1er, 3eme quartile et de la médiane des tailles chez les garçons", x = "Age (en années décimales)", y = "Taille (en cm)")

p<-ggplot(garcon, aes(y=Taille, fill = Sexe)) +
  geom_boxplot() + scale_fill_manual(values=c(Boys = "cyan")) +
  ggtitle("Répartion de la taille par pays chez les garçons") + theme_minimal()
p
#Classe d'age par pays
garcon_Fiji <- garcon[garcon$Pays == "Fiji",]
garcon_Swaziland <- garcon[garcon$Pays == "Swaziland",]

garcon_Fiji$categ_age <- cut(garcon_Fiji$Age, breaks = c(5,8,11,14,17,max(garcon_Fiji$Age)))
Taille_Q1_garcon_Fiji <- aggregate(garcon_Fiji$Taille, by = list(garcon_Fiji$categ_age), FUN = "quantile", probs = 0.25)
Taille_Median_garcon_Fiji <- aggregate(garcon_Fiji$Taille, by = list(garcon_Fiji$categ_age), FUN = "quantile", probs = 0.5)
Taille_Q3_garcon_Fiji <- aggregate(garcon_Fiji$Taille, by = list(garcon_Fiji$categ_age), FUN = "quantile", probs = 0.75)
Taille_Q1_garcon_Fiji$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)
Taille_Median_garcon_Fiji$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)
Taille_Q3_garcon_Fiji$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)

ggplot(garcon_Fiji) + 
  aes(x = Age, y = Taille) +
  geom_point(shape = 'bullet', size = 1.5, color = "gray52") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  geom_line(data = Taille_Q1_garcon_Fiji, aes(x = Group.1, y = x), col = "dodgerblue3", lwd = 1) +
  geom_line(data = Taille_Median_garcon_Fiji, aes(x = Group.1, y = x), col = "firebrick1", lwd = 1) +
  geom_line(data = Taille_Q3_garcon_Fiji, aes(x = Group.1, y = x), col = "darkgreen", lwd = 1) +
  labs(title = "Observation des écarts de tailles chez les garçons des Fidji (quartile)", x = "Age (en années décimales)", y = "Taille (en cm)")

garcon_Swaziland$categ_age <- cut(garcon_Swaziland$Age, breaks = c(5,8,11,14,17,max(garcon_Swaziland$Age)))
Taille_Q1_garcon_Swaziland <- aggregate(garcon_Swaziland$Taille, by = list(garcon_Swaziland$categ_age), FUN = "quantile", probs = 0.25)
Taille_Median_garcon_Swaziland <- aggregate(garcon_Swaziland$Taille, by = list(garcon_Swaziland$categ_age), FUN = "quantile", probs = 0.5)
Taille_Q3_garcon_Swaziland <- aggregate(garcon_Swaziland$Taille, by = list(garcon_Swaziland$categ_age), FUN = "quantile", probs = 0.75)
Taille_Q1_garcon_Swaziland$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)
Taille_Median_garcon_Swaziland$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)
Taille_Q3_garcon_Swaziland$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)

ggplot(garcon_Swaziland) + 
  aes(x = Age, y = Taille) +
  geom_point(shape = 'bullet', size = 1.5, color = "gray52") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  geom_line(data = Taille_Q1_garcon_Swaziland, aes(x = Group.1, y = x), col = "dodgerblue3", lwd = 1) +
  geom_line(data = Taille_Median_garcon_Swaziland, aes(x = Group.1, y = x), col = "firebrick1", lwd = 1) +
  geom_line(data = Taille_Q3_garcon_Swaziland, aes(x = Group.1, y = x), col = "darkgreen", lwd = 1) +
  labs(title = "Observation des écarts de tailles chez les garçons de l'Eswatini (quartile)", x = "Age (en années décimales)", y = "Taille (en cm)")

ggplot(garcon) +
  aes(x = Age, y = Taille) +
  geom_point(shape = 'bullet', size = 1.5, color = "gray52") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  geom_line(data = Taille_Q1_garcon_Fiji, aes(x = Group.1, y = x), col = "blue", lwd = 1) +
  geom_line(data = Taille_Q3_garcon_Swaziland, aes(x = Group.1, y = x), col = "red", lwd = 1) +
  labs(title = "Comparaison des tailles du 1er quartile chez les fidjiens \net 3ème quartile chez les swazis", x = "Age (en années décimales)", y = "Taille (en cm)")
  
#model predictif
ggplot(garcon_Fiji) +
  aes(x = Age, y = Taille, colour = Pays) +
  geom_point(shape = "bullet", size = 1.5) +
  scale_color_manual(
    values = c(Fiji = "#5DA5DA")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  geom_line(data = data.frame(age_values_fiji, preticted_height_boys_fiji1), aes(x = age_values_fiji, y = preticted_height_boys_fiji1), col = "red", lwd = 1) +
  geom_line(data = data.frame(age_values_fiji, preticted_height_boys_fiji2), aes(x = age_values_fiji, y = preticted_height_boys_fiji2), col = "purple", lwd = 1) +
  geom_line(data = data.frame(age_values_fiji, preticted_height_boys_fiji3), aes(x = age_values_fiji, y = preticted_height_boys_fiji3), col = "orange", lwd = 1) +
  geom_line(data = data.frame(age_values_fiji, preticted_height_boys_fiji4), aes(x = age_values_fiji, y = preticted_height_boys_fiji4), col = "green", lwd = 1) +
  labs(title = "modèles predictifs sur la taille des garçons des Fidji en fonction de l'âge", x = "Age (en années décimales)", y = "Taille (en cm)")

ggplot(garcon_Swaziland) +
  aes(x = Age, y = Taille, colour = Pays) +
  geom_point(shape = "bullet", size = 1.5) +
  scale_color_manual(
    values = c(Swaziland = "#F15854")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  geom_line(data = data.frame(age_values_Swaziland, preticted_height_boys_Swaziland1), aes(x = age_values_Swaziland, y = preticted_height_boys_Swaziland1), col = "blue", lwd = 1) +
  geom_line(data = data.frame(age_values_Swaziland, preticted_height_boys_Swaziland2), aes(x = age_values_Swaziland, y = preticted_height_boys_Swaziland2), col = "purple", lwd = 1) +
  geom_line(data = data.frame(age_values_Swaziland, preticted_height_boys_Swaziland3), aes(x = age_values_Swaziland, y = preticted_height_boys_Swaziland3), col = "black", lwd = 1) +
  geom_line(data = data.frame(age_values_Swaziland, preticted_height_boys_Swaziland4), aes(x = age_values_Swaziland, y = preticted_height_boys_Swaziland4), col = "green", lwd = 1) +
  labs(title = "modèles predictifs sur la taille des garçons de l'Eswatini en fonction de l'âge", x = "Age (en années décimales)", y = "Taille (en cm)")

#Nuage de points filles ========================================================
fille <- Data[Data$Sexe == "Girls",]
#Nuage pays confondus
ggplot(fille) +
  aes(x = Age, y = Taille) +
  geom_point(shape = "bullet",size = 1.5) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  labs(title = "Repartition de la taille en fonction de l'âge chez les filles", x = "Age (en années décimales)", y = "Taille (en cm)")
#Nuages avec pays en couleurs
ggplot(fille) +
  aes(x = Age, y = Taille, colour = Pays) +
  geom_point(shape = "bullet",size = 1.5) +
  scale_color_manual(
    values = c(Fiji = "darkorchid",
               Swaziland = "darkolivegreen3")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  labs(title = "Repartition de la taille en fonction de l'âge et par pays chez les filles", x = "Age (en années décimales)", y = "Taille (en cm)")
#boite à moustache
p<-ggplot(fille, aes(y=Taille, fill=Pays)) +
  geom_boxplot() + scale_fill_manual(values=c(Fiji = "darkorchid", Swaziland = "darkolivegreen3")) +
  ggtitle("Répartion de la taille par pays chez les filles") + theme_minimal()
p
#Classe d'age fille
fille$categ_age <- cut(fille$Age, breaks = c(5,8,11,14,17,max(fille$Age)))
Taille_Q1_fille <- aggregate(fille$Taille, by = list(fille$categ_age), FUN = "quantile", probs = 0.25)
Taille_Median_fille <- aggregate(fille$Taille, by = list(fille$categ_age), FUN = "quantile", probs = 0.5)
Taille_Q3_fille <- aggregate(fille$Taille, by = list(fille$categ_age), FUN = "quantile", probs = 0.75)
Taille_Q1_fille$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)
Taille_Median_fille$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)
Taille_Q3_fille$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)

ggplot(fille) + 
  aes(x = Age, y = Taille) +
  geom_point(shape = 'bullet', size = 1.5, color = "gray52") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  geom_line(data = Taille_Q1_fille, aes(x = Group.1, y = x), col = "dodgerblue3", lwd = 1) +
  geom_line(data = Taille_Median_fille, aes(x = Group.1, y = x), col = "firebrick1", lwd = 1) +
  geom_line(data = Taille_Q3_fille, aes(x = Group.1, y = x), col = "darkgreen", lwd = 1) +
  labs(title = "Observation du 1er, 3eme quartile et de la médiane de tailles chez les filles (quartile)", x = "Age (en années décimales)", y = "Taille (en cm)")
#Classe d'age par pays
fille_Fiji <- fille[fille$Pays == "Fiji",]
fille_Swaziland <- fille[fille$Pays == "Swaziland",]

fille_Fiji$categ_age <- cut(fille_Fiji$Age, breaks = c(5,8,11,14,17,max(fille_Fiji$Age)))
Taille_Q1_fille_Fiji <- aggregate(fille_Fiji$Taille, by = list(fille_Fiji$categ_age), FUN = "quantile", probs = 0.25)
Taille_Median_fille_Fiji <- aggregate(fille_Fiji$Taille, by = list(fille_Fiji$categ_age), FUN = "quantile", probs = 0.5)
Taille_Q3_fille_Fiji <- aggregate(fille_Fiji$Taille, by = list(fille_Fiji$categ_age), FUN = "quantile", probs = 0.75)
Taille_Q1_fille_Fiji$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)
Taille_Median_fille_Fiji$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)
Taille_Q3_fille_Fiji$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)

ggplot(fille_Fiji) + 
  aes(x = Age, y = Taille) +
  geom_point(shape = 'bullet', size = 1.5, color = "gray52") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  geom_line(data = Taille_Q1_fille_Fiji, aes(x = Group.1, y = x), col = "dodgerblue3", lwd = 1) +
  geom_line(data = Taille_Median_fille_Fiji, aes(x = Group.1, y = x), col = "firebrick1", lwd = 1) +
  geom_line(data = Taille_Q3_fille_Fiji, aes(x = Group.1, y = x), col = "darkgreen", lwd = 1) +
  labs(title = "Observation des écarts de tailles chez les filles des Fidji (quartile)", x = "Age (en années décimales)", y = "Taille (en cm)")

fille_Swaziland$categ_age <- cut(fille_Swaziland$Age, breaks = c(5,8,11,14,17,max(fille_Swaziland$Age)))
Taille_Q1_fille_Swaziland <- aggregate(fille_Swaziland$Taille, by = list(fille_Swaziland$categ_age), FUN = "quantile", probs = 0.25)
Taille_Median_fille_Swaziland <- aggregate(fille_Swaziland$Taille, by = list(fille_Swaziland$categ_age), FUN = "quantile", probs = 0.5)
Taille_Q3_fille_Swaziland <- aggregate(fille_Swaziland$Taille, by = list(fille_Swaziland$categ_age), FUN = "quantile", probs = 0.75)
Taille_Q1_fille_Swaziland$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)
Taille_Median_fille_Swaziland$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)
Taille_Q3_fille_Swaziland$Group.1 <- c(6.5, 9.5, 12.5, 15.5,18)

ggplot(fille_Swaziland) + 
  aes(x = Age, y = Taille) +
  geom_point(shape = 'bullet', size = 1.5, color = "gray52") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  geom_line(data = Taille_Q1_fille_Swaziland, aes(x = Group.1, y = x), col = "dodgerblue3", lwd = 1) +
  geom_line(data = Taille_Median_fille_Swaziland, aes(x = Group.1, y = x), col = "firebrick1", lwd = 1) +
  geom_line(data = Taille_Q3_fille_Swaziland, aes(x = Group.1, y = x), col = "darkgreen", lwd = 1) +
  labs(title = "Observation des écarts de tailles chez les filles de l'Eswatini (quartile)", x = "Age (en années décimales)", y = "Taille (en cm)")

ggplot(fille) +
  aes(x = Age, y = Taille) +
  geom_point(shape = 'bullet', size = 1.5, color = "gray52") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  geom_line(data = Taille_Median_fille_Fiji, aes(x = Group.1, y = x), col = "blue", lwd = 1) +
  geom_line(data = Taille_Q3_fille_Swaziland, aes(x = Group.1, y = x), col = "red", lwd = 1) +
  labs(title = "Comparaison des tailles du 1er quartile chez les fidjiennes \net 3ème quartile chez les swazies", x = "Age (en années décimales)", y = "Taille (en cm)")
#model predictif
ggplot(fille_Fiji) +
  aes(x = Age, y = Taille) +
  geom_point(shape = "bullet", size = 1.5, color = "#5DA5DA") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  geom_line(data = data.frame(age_values_fiji, preticted_height_girls_fiji1), aes(x = age_values_fiji, y = preticted_height_girls_fiji1), col = "red", lwd = 1) +
  geom_line(data = data.frame(age_values_fiji, preticted_height_girls_fiji2), aes(x = age_values_fiji, y = preticted_height_girls_fiji2), col = "purple", lwd = 1) +
  geom_line(data = data.frame(age_values_fiji, preticted_height_girls_fiji3), aes(x = age_values_fiji, y = preticted_height_girls_fiji3), col = "orange", lwd = 1) +
  geom_line(data = data.frame(age_values_fiji, preticted_height_girls_fiji4), aes(x = age_values_fiji, y = preticted_height_girls_fiji4), col = "green", lwd = 1) +
  labs(title = "modèles predictifs sur la taille des filles \ndes Fidji en fonction de l'âge", x = "Age (en années décimales)", y = "Taille (en cm)")

ggplot(fille_Swaziland) +
  aes(x = Age, y = Taille) +
  geom_point(shape = "bullet", size = 1.5, color = "#F15854") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20L, hjust = 0.5)) +
  geom_line(data = data.frame(age_values_Swaziland, preticted_height_girls_Swaziland1), aes(x = age_values_Swaziland, y = preticted_height_girls_Swaziland1), col = "blue", lwd = 1) +
  geom_line(data = data.frame(age_values_Swaziland, preticted_height_girls_Swaziland2), aes(x = age_values_Swaziland, y = preticted_height_girls_Swaziland2), col = "purple", lwd = 1) +
  geom_line(data = data.frame(age_values_Swaziland, preticted_height_girls_Swaziland3), aes(x = age_values_Swaziland, y = preticted_height_girls_Swaziland3), col = "black", lwd = 1) +
  geom_line(data = data.frame(age_values_Swaziland, preticted_height_girls_Swaziland4), aes(x = age_values_Swaziland, y = preticted_height_girls_Swaziland4), col = "green", lwd = 1) +
  labs(title = "modèles predictifs sur la taille des filles \nde l'Eswatini en fonction de l'âge", x = "Age (en années décimales)", y = "Taille (en cm)")
