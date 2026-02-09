Datos <- read.csv("C:/Users/Emir/Desktop/Seminario_inclusion_f/Database_final.csv")
View(Database)
str(Database)
attach(Database)
names(Database)
dim(Database)
table(Cuenta_Fintech, TLOC)
table(Nomina_Cuenta, TLOC)
table(Credit_Fintech, TLOC)
table(Microfinanza, TLOC)
table(Familiar, TLOC)
table(Amistad, TLOC)
table(Empenio, TLOC)
table(Credito, TLOC)
table(Seguro, TLOC)
table(Afore, TLOC)
table (Cuenta_Fintech, TLOC)
mean(Datos$Ingreso)   #4317.831 promedio de los sueldos

# Promedio por localidad
library(dplyr)
Datos %>%
  group_by(TLOC) %>% 
  # Agrupa por la columna 'categoria'
  summarise(promedio_valor = mean(Ingreso, na.rm = TRUE)) 
# Calcula la media de 'valor' para cada grupo
# para 0, es decir hasta 15 mil habitantes: 5445 pesos
# para 0, es decir menos de 2500 habitantes: 3653


# importar la base de datos final
Datos <- read.csv("C:/Users/lilbo/Downloads/Seminario_inclusion_f/Database_final.csv")
# Se consideran como modelos a las variables con mayor peso
# es decir 
# tener cuenta nomina, tener afore, tener credito, credito familiar



# ###Logistic regression####
#Ajustar 4 modelos logit
Nomina=glm(Nomina_Cuenta~TLOC+SEXO+EDAD_V+Ser_indig+
             Distancia+(Ingreso)+Edu_fin+Tener_conyuge+
             Primaria+Secundaria+Preparatoria+Carrera,
           data=Datos, family = "binomial")

summary(Nomina)
write.csv(summary(Nomina), "summaryN.csv")

#compute R squared
R2= 1-(Nomina$deviance/Nomina$null.deviance)
R2
# odd ratio
exp(Nomina$coefficients)
write.csv(exp(Nomina$coefficients), "oddratio1.csv")

# Para AFORE
LG_Afore=glm(Afore~TLOC+SEXO+EDAD_V+Ser_indig+
               Distancia+Ingreso+Edu_fin+Tener_conyuge+
               Primaria+Secundaria+Preparatoria+Carrera,
             data=Datos, family = "binomial")

summary(LG_Afore)
#compute R squared
R2_Afore= 1-(LG_Afore$deviance/LG_Afore$null.deviance)
R2_Afore
write.csv(exp(LG_Afore$coefficients), "afore.csv")

# regresion logistica para familiar
LG_Familiar=glm(Familiar~TLOC+SEXO+EDAD_V+Ser_indig+
                  Distancia+Ingreso+Edu_fin+Tener_conyuge+
                  Primaria+Secundaria+Preparatoria+Carrera,
                data=Datos, family = "binomial")
summary(LG_Familiar)
#compute R squared
R2_fam= 1-(LG_Familiar$deviance/LG_Familiar$null.deviance)
R2_fam
write.csv(exp(LG_Familiar$coefficients), "familiar.csv")



LG_Credito=glm(Credito~TLOC+SEXO+EDAD_V+Ser_indig+
                 Distancia+Ingreso+Edu_fin+Tener_conyuge+
                 Primaria+Secundaria+Preparatoria+Carrera,
               data=Datos, family = "binomial")
summary(LG_Credito)
#compute R squared
R2_Credit= 1-(LG_Credito$deviance/LG_Credito$null.deviance)
R2_Credit
write.csv(exp(LG_Credito$coefficients), "credit.csv")

### estadisticas descriptivas
summary.data.frame(Datos)
round(sapply(Datos, sd, na.rm = TRUE),4)
round(sapply(Datos, mean, na.rm = TRUE),4)


##### eliminar datos faltantes ####

# Identificar y eliminar las filas con NA en la variable 'Clase'
library(dplyr)
dim(Datos)
Datos2 <- Datos %>%
  filter(!is.na(Nomina_Cuenta))
dim(Datos2)
Datos_nom=Datos2[, c("TLOC","SEXO","EDAD_V",          
                    "Ser_indig","Distancia","Ingreso",  "Edu_fin",   
                  "Tener_conyuge", "Primaria", "Secundaria", "Preparatoria", 
                  "Carrera","Nomina_Cuenta")]
dim(Datos_nom)

Datos_Fam <- Datos %>%
  filter(!is.na(Familiar))
dim(Datos_Fam)

Datos_Afo <- Datos %>%
  filter(!is.na(Afore))
dim(Datos_Afo)


####Random Forest Nomina####
library(ranger)

# Fijar semillas para obtener los mismos resultados en otras pruebas
set.seed(1234)
# Separa entrenamiento y prueba
library(caret)

#Indice <- createDataPartition(Datos_nom$Nomina_Cuenta, times=1, p=.8, list=FALSE)

#Conjunto de entrenamiento
#Entrenam <- Datos_nom[Indice, ]

#Conjunto de pruebas
#Prueba <- Datos_nom[-Indice, ]

RF_nom <- ranger(Nomina_Cuenta~TLOC+SEXO+EDAD_V+Ser_indig+
                    Distancia+(Ingreso)+Edu_fin+Tener_conyuge+
                    Primaria+Secundaria+Preparatoria+Carrera,
                  data = Datos_nom,
                  num.trees = 500,
                  seed =42,
                  importance = "impurity",
                 classification=T)


# Extraer el puntaje de importancia de las variables en un dataframe
Importance_N <- data.frame(
  Variable = names(RF_nom$variable.importance),
  Importance = RF_nom$variable.importance
)
library(dplyr)

# Sort the variables by importance score in descending order
Importance_N <- Importance_N %>%
  arrange(Importance) %>%
  mutate(Variable = factor(Variable, levels = Variable))


importance_plotNomina<- ggplot(Importance_N, aes(x = Importance, y = Variable)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Cuenta Nomina",
    ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 10)
  )

# Print the plot
print(importance_plotNomina)



#### Random Forest for AFORE ####
#Datos_Afo
#library(caret)

#Indice_Afo <- createDataPartition(Datos_Afo$Afore, times=1, p=.8, list=FALSE)

#Conjunto de entrenamiento
#Entren_Af <- Datos_Afo[Indice_Afo, ]

#Conjunto de pruebas
#Prueba_AF <- Datos_Afo[-Indice_Afo, ]
# PLot para AFORE
RF_Afore <- ranger(Afore~TLOC+SEXO+EDAD_V+Ser_indig+
                   Distancia+(Ingreso)+Edu_fin+Tener_conyuge+
                   Primaria+Secundaria+Preparatoria+Carrera,
                 data = Datos_Afo,
                 num.trees = 500,
                 seed =42,
                 importance = "impurity",
                 classification=T)

# Extraer el puntaje de importancia de las variables en un dataframe
Importance_Af <- data.frame(
  Variable = names(RF_Afore$variable.importance),
  Importance = RF_Afore$variable.importance
)
library(dplyr)

# Sort the variables by importance score in descending order
Importance_Af <- Importance_Af %>%
  arrange(Importance) %>%
  mutate(Variable = factor(Variable, levels = Variable))


PLot_AFo <- ggplot(Importance_Af, aes(x = Importance, y = Variable)) +
  geom_col(fill = "steelblue") +
  labs(
    title = " AFORE",
     ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 10)
  )

# Print the plot
print(PLot_AFo)

# Random Forest para credito familiar #####
#library(caret)
#Indice_Fam <- createDataPartition(Datos_Fam$Familiar, times=1, p=.8, list=FALSE)

#Conjunto de entrenamiento
#Entren_Fam <- Datos_Fam[Indice_Fam, ]

#Conjunto de pruebas
#Prueba_Fam <- Datos_Fam[-Indice_Fam, ]

# PLot para credito familiar
RF_Fam <- ranger(Familiar~TLOC+SEXO+EDAD_V+Ser_indig+
                     Distancia+(Ingreso)+Edu_fin+Tener_conyuge+
                     Primaria+Secundaria+Preparatoria+Carrera,
                   data = Datos_Fam,
                   num.trees = 500,
                   seed =42,
                   importance = "impurity",
                 classification=T)

# Extraer el puntaje de importancia de las variables en un dataframe
Importance_Fam <- data.frame(
  Variable = names(RF_Fam$variable.importance),
  Importance = RF_Fam$variable.importance)

library(dplyr)

# Sort the variables by importance score in descending order
Importance_Fam <- Importance_Fam %>%
  arrange(Importance) %>%
  mutate(Variable = factor(Variable, levels = Variable))

PLot_Fam <- ggplot(Importance_Fam, aes(x = Importance, y = Variable)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Crédito familiar",
   
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 10)
  )

# Print the plot
print(PLot_Fam)


#### Random forest para credito bancario #####
#library(caret)
# Eliminar datos faltantes
Datos_Cred <- Datos %>%
  filter(!is.na(Credito))
dim(Datos_Cred)
View(Datos_Cred)
sum(is.na(Datos_Cred$Credito))

#Indice_Cred<- createDataPartition(Datos_Cred$Credito, times=1, p=.8, 
 #                                 list=FALSE)

#Conjunto de entrenamiento
#Entren_Cred <- Datos_Cred[Indice_Cred, ]

#Conjunto de pruebas
#Prueba_Cred <- Datos_Cred[-Indice_Cred, ]

# PLot para credito bancario
RF_Credit <- ranger(Credito~TLOC+SEXO+EDAD_V+Ser_indig+
                   Distancia+(Ingreso)+Edu_fin+Tener_conyuge+
                   Primaria+Secundaria+Preparatoria+Carrera,
                 data = Datos_Cred,
                 num.trees = 500,
                 seed =42,
                 importance = "impurity",
                 classification=T)

# Extraer el puntaje de importancia de las variables en un dataframe
Importance_Cred <- data.frame(
  Variable = names(RF_Credit$variable.importance),
  Importance = RF_Credit$variable.importance
)
library(dplyr)

# Sort the variables by importance score in descending order
Importance_Cred <- Importance_Cred %>%
  arrange(Importance) %>%
  mutate(Variable = factor(Variable, levels = Variable))

PLot_Cred <- ggplot(Importance_Cred, aes(x = Importance, y = Variable)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Crédito bancario",
     ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 10)
  )

# Print the plot
print(PLot_Cred)

library(randomForest)
varImpPlot(RF_Afore)


#### Evaluación  ####
# El out of bag error
RF_Afore$prediction.error
#0.2331037
RF_Credit$prediction.error
#0.2228814
RF_nom$prediction.error
#0.1875719
RF_Fam$prediction.error
#0.20361





#### with random forest library ####
library(randomForest)

varImpPlot(RF_Afore)
RF_NOMINA<- randomForest(Nomina_Cuenta~TLOC+SEXO+EDAD_V+Ser_indig+
                   Distancia+(Ingreso)+Edu_fin+Tener_conyuge+
                   Primaria+Secundaria+Preparatoria+Carrera,
                 data = Datos_nom, na.action = na.omit,
                 num.trees = 500,
                 seed =42,
                 importance = T,
                 classification=T)
varImpPlot(RF_NOMINA)
install.packages('randomForestSRC')
library(randomForestSRC)
library(ggRandomForests)



# The default prediction type is the predicted class (factor)
Pred_nom <- predict(RF_nom, data = Prueba)

# Tomar el vector de las predicciones
PRedichas_N <- Pred_nom$predictions

A_N=as.factor(PRedichas_N)
B_N=as.factor(Prueba$Nomina_Cuenta)

levels_N=levels(Prueba$Nomina_Cuenta)
# usar estos niveles para refactorizar

AN_fix <- factor(A_N, levels=levels_N)



# Crear la matriz de confusion (actual vs predichas)
confusion_matrix_caret <- confusionMatrix(
  data = AN_fix, 
  reference = Prueba$Nomina_Cuenta)

(PRedichas_N)
Prueba$Nomina_Cuenta

# Print the comprehensive output
print(confusion_matrix_caret)








tabla_confusion <- table(datos_prueba$Species, predicciones)
print(modelo_rf_tradicional)
cat("\nTabla de Confusión:\n")
print(tabla_confusion)


#### Logistic regression with predict ####
# Entrenar el Modelo de Regresión Logística
LogitModel<- glm(Nomina_Cuenta~TLOC+SEXO+EDAD_V+Ser_indig+
                         Distancia+(Ingreso)+Edu_fin+Tener_conyuge+
                         Primaria+Secundaria+Preparatoria+Carrera,
                       data = Entrenam,
                       family="binomial")

# Realizar Predicciones
# Tipo = "response" genera las probabilidades
prob_pred <- predict(LogitModel, 
                      newdata = Prueba, 
                      type = "response")

# Convertir Probabilidades a Clases (Usando el umbral estándar de 0.5)
Clases_predichas <- factor(ifelse(prob_pred > 0.5, 1, 0),
                           levels = levels(Prueba$Nomina_Cuenta))


MC_nom <- confusionMatrix(Clases_predichas, Prueba$Nomina_Cuenta)
table(prob_pred, Clases_predichas)



##### XG boost #####
# Instalar y cargar las librerías necesarias
library(xgboost)
library(caret)

set.seed(123) # Para reproducibilidad

# Preparar los datos
# Convertir la variable objetivo a numérica (XGBoost requiere etiquetas numéricas)
iris$Species <- as.numeric(as.factor(iris$Species)) - 1 # 0, 1, 2 para las tres clases

# Dividir en conjunto de entrenamiento y prueba
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
train_data <- iris[trainIndex, ]
test_data <- iris[-trainIndex, ]

# Separar características y variable objetivo
X_train <- as.matrix(train_data[, 1:4]) # Características
y_train <- train_data$Species
X_test <- as.matrix(test_data[, 1:4])
y_test <- test_data$Species

# Crear matrices DMatrix para XGBoost
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# Definir parámetros de XGBoost
params <- list(
  objective = "multi:softmax", # Para clasificación multiclase
  num_class = 3,              # Número de clases
  max_depth = 6,              # Profundidad máxima del árbol
  eta = 0.3,                  # Tasa de aprendizaje
  subsample = 0.8,            # Proporción de datos para muestreo
  colsample_bytree = 0.8      # Proporción de columnas para muestreo
)

# Entrenar el modelo
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,              # Número de iteraciones
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10, # Detener si no mejora en 10 iteraciones
  verbose = 1
)

# Realizar predicciones
predictions <- predict(xgb_model, X_test)

# Evaluar el modelo
confusionMatrix(as.factor(predictions), as.factor(y_test))

# Importancia de las variables
importance_matrix <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix)
