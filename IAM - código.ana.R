#Instalando e carregando o pacote "palmerpenguins"
install.packages("palmerpenguins")
library(palmerpenguins)


#Carregando o banco de dados penguins
db = penguins
db = data.frame(db)


#Drop dos valores faltantes do banco de dados
db = na.omit(db)


#Separando as especies do banco de dados
Y = db$species
X = db[,c(3:6)]
View(cbind(X,Y))


# Univariada (não sei o que é essa parte)
par(mfrow=c(4,3))
for(i in 1:4){
  for(j in 1:3){
    hist(X[Y==levels(Y)[j],i])
  }
}

# Multivariada (não sei o que é essa parte)
pairs(X, col = Y, pch = 19)

# proporção de Y
Y
table(Y)
prop.table(table(Y))


# Classificador de Bayes Normal Multivariada
X1 = X[Y == "Adelie",]
X2 = X[Y == "Chinstrap",]
X3 = X[Y == "Gentoo",]


# 1 - Estimar os vetores medias para cada classe


# vetor de media para adelie
m1 = colMeans(X1)
m1 = apply(X1,2, FUN = mean)

m1 = c()
for(i in 1:ncol(X1)){
  m1[i] = mean(X1[,i])
}


# vetor de media para Chinstrap
m2 = colMeans(X2)
m2 = apply(X2,2, FUN = mean)

m2 = c()
for(i in 1:ncol(X2)){
  m2[i] = mean(X2[,i])
}


# vetor de media para Gentoo
m3 = colMeans(X3)
m3 = apply(X3,2, FUN = mean)

m3 = c()
for(i in 1:ncol(X3)){
  m3[i] = mean(X3[,i])
}


# 2 - Estimar as matrizes de covariancia para cada classe


#matriz de covariancia para adelie
S1 = cov(X1)

S1 = matrix(,ncol(X1),ncol(X1))

for(i in 1:ncol(X1)){
  for(j in 1:ncol(X1)){
    S1[i,j] = (1/(nrow(X1)-1)) * (X1[,i] - m1[i]) %*% (X1[,j] - m1[j])
  } 
}


#matriz de covariancia para Chinstrap
S2 = cov(X2)

S2 = matrix(,ncol(X2),ncol(X2))

for(i in 1:ncol(X2)){
  for(j in 1:ncol(X2)){
    S2[i,j] = (1/(nrow(X2)-1)) * (X2[,i] - m2[i]) %*% (X2[,j] - m2[j])
  } 
}


#matriz de covariancia para gentoo
S3 = cov(X3)

S3 = matrix(,ncol(X3),ncol(X3))

for(i in 1:ncol(X3)){
  for(j in 1:ncol(X3)){
    S3[i,j] = (1/(nrow(X3)-1)) * (X3[,i] - m3[i]) %*% (X3[,j] - m3[j])
  } 
}


# 3 - Calcular as densidades