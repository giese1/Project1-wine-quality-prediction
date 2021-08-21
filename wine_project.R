# Predict wine quality based on 11 predecessor variables

# Packages
library(knitr)
library(skimr)
library(Hmisc)
library(treemapify)
library(randomForest)
library(caret)
library(naivebayes)
library(e1071)
library(rpart)
library(multiROC)
library(ROCR)
library(RColorBrewer)
library(ggridges)
library(cowplot)
library(ggplot2)
library(corrplot)
library(dplyr)
library(glue)
library(hrbrthemes)
library(ggthemes)

# loading dataset

vinho_vermelho <- read.csv("winequality-red.csv", header = TRUE, sep = ";")

vinho_branco <- read.csv("winequality-white.csv", header = TRUE, sep = ";")

str(vinho_branco)



#### Descriptive analysis ####

c(unique(df_vinhos_final["quality"]))

#### exploratory analysis ####

# analysis of variables density & alcohol with scatter plot white wine & red wine

a <- ggplot(vinho_branco, aes(x = density, y = alcohol)) +
  geom_point(aes(color = quality)) +
  theme_minimal() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Quantity of density and alcohol compared to white wine quality")
a

b <- ggplot(vinho_vermelho, aes(x = density, y = alcohol)) +
  geom_point(aes(color = quality)) +
  theme_minimal() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Quantity of density and alcohol compared to red wine quality")
b

plot_grid(a, b, ncol = 2, nrow = 1)

# analysis of variables density & alcohol with violin plot white wine & red wine

c <- ggplot(vinho_branco, aes(x=density, y=alcohol)) + 
  geom_violin(trim = FALSE) +
  stat_summary(fun.data = mean_sdl, 
               geom="pointrange", color="red") +
  ggtitle("Comparison of the variables density & alcohol white wine")
c

d <- ggplot(vinho_vermelho, aes(x=density, y=alcohol)) + 
  geom_violin(trim = FALSE) +
  stat_summary(fun.data = mean_sdl, 
               geom="pointrange", color="red") +
  ggtitle("Comparison of the variables density & alcohol red wine")
d


plot_grid(c, d, ncol = 2, nrow = 1)

# analysis of variables residual.sugar & density with bubbleplot white wine & red wine

e <- ggplot(vinho_branco, aes(x = residual.sugar, y = density)) + 
  geom_point(aes(color = "cyan", size = quality), alpha = 0.5) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_size(range = c(0.5, 12)) +
  ggtitle("Comparison of the variables residual sugar & density white wine")
e

f <- ggplot(vinho_vermelho, aes(x = residual.sugar, y = density)) + 
  geom_point(aes(color = "cyan", size = quality), alpha = 0.5) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_size(range = c(0.5, 12)) +
  ggtitle("Comparison of the variables residual sugar & density red wine")
f

plot_grid(e, f, ncol = 2, nrow = 1)

# analysis of variables free.sulfur.dioxide & total.sulfur.dioxide with scatterplot white & red wine

g <- ggplot(vinho_branco, aes(x = free.sulfur.dioxide, y = total.sulfur.dioxide)) +
  geom_point(aes(color = quality)) +
  theme_minimal() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Comparison of white wine quality in relation to sulfur dioxide")
   
g

h <- ggplot(vinho_vermelho, aes(x = free.sulfur.dioxide, y = total.sulfur.dioxide)) +
  geom_point(aes(color = quality)) +
  theme_minimal() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  ggtitle("Comparison of red wine quality in relation to sulfur dioxide")

h

plot_grid(g, h, ncol = 2, nrow = 1)

# analysis of variables ph & density with bubbleplot white wine & red wine

i <- ggplot(vinho_branco, aes(x = pH, y = density)) + 
  geom_point(aes(color = "cyan", size = quality), alpha = 0.5) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_size(range = c(0.5, 12)) +
  ggtitle("Comparison pH & density in relation of quality of white wine")

i

j <- ggplot(vinho_vermelho, aes(x = pH, y = density)) + 
  geom_point(aes(color = "cyan", size = quality), alpha = 0.5) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_size(range = c(0.5, 12)) +
  ggtitle("Comparison pH & density in relation of quality of red wine")

j

plot_grid(i, j, ncol = 2, nrow = 1)

#### pre processing ####

# creating new variable for red wine & white wine
?dplyr

color = 0

vinho_branco <- cbind(vinho_branco, color)

color = 1

vinho_vermelho <- cbind(vinho_vermelho, color)

# merging white wine & red wine datasets

df_vinhos <- merge(vinho_branco, vinho_vermelho, all = TRUE)

str(df_vinhos)

# creating new target variable with quality column
quality <- cut(df_vinhos$quality, breaks = c(3), labels = c(0,1,2))

df_vinhos$quality <- NULL

df_vinhos_final <- cbind(df_vinhos, quality)

str(df_vinhos_final)

# frequency of ratings white wine

freq_notas_vinhos<- data.frame(cbind(frequency = table(df_vinhos_final$quality, useNA = NULL),
                                     percent = prop.table(table(df_vinhos_final$quality)) * 100))

freq_notas_vinhos





# Histogram of ratings white wine

tema <- theme(plot.background = element_rect(fill = "#FFFAFA", color = "#FFFAFA"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=14, repr.plot.height=6)
a <- ggplot(data = df_vinhos_final, mapping = aes(x = as.integer(quality))) +
  geom_histogram(fill = "cyan", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("quality") +
  ggtitle("Frequencia da qualidade dos vinhos") +
  tema

a

# analysis of frequency of new variable target

freq_notas_vinho <- data.frame(cbind(frequency = table(df_vinhos_final$quality, useNA = NULL),
                                      percent = prop.table(table(df_vinhos_final$quality)) * 100))

freq_notas_vinho

# correlation of variables white wine
correlations <- cor(df_vinhos_final,method="pearson")

corrplot(correlations, number.cex = .9, method = "circle", type = "full", tl.cex=0.8,tl.col = "black",
         title = "Correlation variables")


# Feature selection 

modelo_vinho <- randomForest(quality ~., data = df_vinhos_final)

importance(modelo_branco)

varImp(modelo_branco)

varImpPlot(modelo_branco)



# spliting data into training and test

indexes <- sample(1:nrow(df_vinhos_final), size = 0.7 * nrow(df_vinhos_final))
train.data.vinho <- df_vinhos_final[indexes,]
test.data.vinho <- df_vinhos_final[-indexes,]
class(train.data.vinho)
class(test.data.vinho)

str(train.data.vinho)

prop.table(table(train.data.vinho$quality)) * 100

# balancing target variable with SMOTE
?SMOTE
train.data.vinho.balanced <- SMOTE(quality ~ ., train.data.vinho, perc.over = 1000, perc.under = 300)

# checking balanced target

train.data.vinho.balanced <- na.omit(train.data.vinho.balanced)

prop.table(table(train.data.vinho.balanced$quality)) * 100



#### Machine learning ####

#svm 66% accuracy with SMOTE 1000 & 300
modelo_ma_branco <- svm(quality ~. ,data = train.data.vinho.balanced)
summary(modelo_ma_branco)
print(modelo_ma_branco)

# naive bayes 49% accuracy

modelo_ma_branco <- naiveBayes(quality~ + alcohol + volatile.acidity + density, train.data.vinho.balanced)


# svm 2 49% accuracy

modelo_grid1 <- tune(svm, 
                     quality ~., 
                     data = train.data.vinho.balanced, 
                     kernel = 'linear',
                     ranges = list(cost = c(0.05, 0.1, 0.5, 1, 2))) 

summary(modelo_grid1)

# RandomForest 75% accuracy
set.seed(2021)

modelo_ma_branco <- randomForest(quality ~., data = train.data.vinho.balanced)

print(modelo_ma_branco)
plot(modelo_ma_branco)




# prevision of quality of wine

modelo_pred_branco <- predict(modelo_ma_branco, test.data.vinho)



table(modelo_pred_branco, test.data.vinho$quality)

confusionMatrix(modelo_pred_branco, test.data.vinho$quality)


# quality of wine predictive data

View(modelo_pred_branco)

modelo_pred_branco_plot <- as.data.frame(modelo_pred_branco)
names(modelo_pred_branco_plot) <- c("quality")

names(modelo_pred_branco_plot)


tema <- theme(plot.background = element_rect(fill = "#FFFAFA", color = "#FFFAFA"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=14, repr.plot.height=6)
a <- ggplot(data = modelo_pred_branco_plot , mapping = aes(x = as.numeric(quality))) +
  geom_histogram(fill = "cyan", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("quality") +
  ggtitle("quality of wines predictive data") +
  tema

a

# quality of wine real data

options(repr.plot.width=14, repr.plot.height=6)
b <- ggplot(data = test.data.vinho , mapping = aes(x = as.numeric(quality))) +
  geom_histogram(fill = "red", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("quality") +
  ggtitle("quality of wines real data") +
  tema

b

plot_grid(a, b, ncol = 2, nrow = 1)