#Carregando o banco de dados

hemato_data <- read.csv("hemato_data.csv", dec = ",")

View(hemato_data)
h <- as.numeric(unlist(hemato_data$y1))
hemato_data$y1 = h


plot.new()
result <- mvn(data = hemato_data[c(2,3,4,5,6,7)], mvnTest = "royston", univariatePlot = "histogram")

#Transformação Box-Cox

library(MASS)

# Transformação p/ y1
b1 <- boxcox(lm(hemato_data$y1 ~ 1))
lambda_y1 <- b1$x[which.max(b1$y)]

y1 <- (hemato_data$y1 ^ lambda_y1 - 1) / lambda_y1

# Transformação p/ y2
b2 <- boxcox(lm(hemato_data$y2 ~ 1))
lambda_y2 <- b2$x[which.max(b2$y)]

y2 <- (hemato_data$y2 ^ lambda_y2 - 1) / lambda_y2

# Transformação p/ y3

b3 <- boxcox(lm(hemato_data$y3 ~ 1))
lambda_y3 <- b3$x[which.max(b3$y)]

y3 <- (hemato_data$y3 ^ lambda_y3 - 1) / lambda_y3

# Transformação p/ y4

b4 <- boxcox(lm(hemato_data$y4 ~ 1))
lambda_y4 <- b4$x[which.max(b4$y)]

y4 <- (hemato_data$y4 ^ lambda_y4 - 1) / lambda_y4


# Transformação p/ y5

b5 <- boxcox(lm(hemato_data$y5 ~ 1))
lambda_y5 <- b5$x[which.max(b5$y)]

y5 <- (hemato_data$y5 ^ lambda_y5 - 1) / lambda_y5



# Transformação p/ y6

b6 <- boxcox(lm(hemato_data$y6 ~ 1))
lambda_y6 <- b6$x[which.max(b6$y)]

y6 <- (hemato_data$y6 ^ lambda_y6 - 1) / lambda_y6


# Nova tabela

nova_tabela <- data.frame(y1, y2, y3, y4, y5, y6) 

# Teste Multivariado

library(mvnormtest)
mvnormtest::mshapiro.test(t(nova_tabela))

# Teste Multivariado e gráfico

library(MVN)

result <- mvn(nova_tabela, mvnTest = "royston",  
              univariateTest = "SW", desc = TRUE)
result$univariateNormality
result$Descriptives
result <- mvn(data = hemato_data[,2:7], mvnTest = "royston")

#### Teste de Barttlet 




###### Componentes principais ##########

# Matriz de convariâncias estimadas da nova tabela
S <- cov(nova_tabela)
a_e <- eigen(S); a_e


#Variância dos componentes principais
Var_Yi<-a_e$values; Var_Yi # autovalores

#Proporção da variância explicada pelos componentes principais mensurada de 
#forma individual 
porc_expl<-a_e$values/sum(a_e$values)*100;porc_expl

# Estimar os componentes principais usando a matriz de covariâncias

P <- a_e$vectors; P
t(P)

X1 <- nova_tabela[,1]
X2 <- nova_tabela[,2]
X3 <- nova_tabela[,3]
X4 <- nova_tabela[,4]
X5 <- nova_tabela[,5]
X6 <- nova_tabela[,6]

# Estimação dos componentes principais usando a matriz de covariâncias

Y1 <- -1.156940e-04*X1 -0.8986001768*X2 -0.001197076*X3 -9.205702e-02*X4 -4.289921e-01*X5 -0.002744707*X6
Y2 <- -5.741006e-05*X1 -0.4258264423*X2 +0.001053422*X3 -5.262478e-02*X4 +9.032707e-01*X5 -0.001849293*X6
Y3 <- +2.579444e-06*X1 +0.1057582785*X2 -0.004887018*X3 -9.942944e-01*X4 -8.085737e-03*X5 -0.010223326*X6
Y4 <- +1.295156e-03*X1 +0.0021719015*X2 +0.006248029*X3 +1.048491e-02*X4 -4.196277e-04*X5 -0.999922226*X6
Y5 <- +1.545979e-02*X1 -0.0001258680*X2 +0.999847636*X3 -4.979358e-03*X4 -1.501781e-03*X5 +0.006215732*X6
Y6 <- +9.998796e-01*X1 -0.0001295648*X2 -0.015467453*X3 +5.229961e-05*X4 +2.600968e-05*X5 +0.001198709*X6


cbind((Y1-mean(Y1))/sd(Y1), (Y2-mean(Y2))/sd(Y2), (Y3-mean(Y3))/sd(Y3), 
      (Y4-mean(Y4))/sd(Y4), (Y5-mean(Y5))/sd(Y5), (Y6-mean(Y6))/sd(Y6))


#Data frame com os resultados
j <- data.frame(Componentes=c("Y1","Y2","Y3", "Y4", "Y5", "Y6"),
           lambda_i = a_e$values, porc_explicada = porc_expl,
           porc_explicada_acum = cumsum(porc_expl))


# Scree plot
plot.new()
screeplot(j)










