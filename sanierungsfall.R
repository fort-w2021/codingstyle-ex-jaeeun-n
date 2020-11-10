### Befragung zur Reisebereitschaft bzw. -tätigkeit

# Zielgröße:
# Haben Sie dieses Jahr schon eine Urlaubsreise oder mehrere Urlaubsreisen gemacht?

library(foreign) # read spss
library(pROC) # ROC-Kurve, AUC-Wert etc.
library(tidyverse)
library(ggplot2)
library(ggmosaic) # mosaicplots
library(gridExtra) # grid.arrange()
library(DataExplorer) # plot_bar()
library(car) # Anova()
library(caret) # confusionmatrix()
library(gbm)
library(rsample) # initial_split()
library(ROCR) # prediction()
library(xlsx) # write.xlsx()


### Daten einlesen ####
daten <- read.spss("Reisen.sav",
                   to.data.frame = T)
head(daten)
names(daten)

# Relevante Variablen
df2 <- data.frame(befnr = daten$befnr, f1 = daten$f1, daten[,217:236])
head(df2)
names(df2)

#### Datenaufgebreitung ##########################################################

#### Zielvariable binär kodieren
levels(df2$f1)
# Zielgröße:
# Haben Sie dieses Jahr schon eine Urlaubsreise oder mehrere Urlaubsreisen gemacht?

# Gruppe1 (Ja): "Ja, eine Urlaubsreise"
#               "Ja, mehrere Urlaubsreisen"
# Gruppe2 (Nein): "Nein"

df2$f1_2 <- df2$f1 %>%
  fct_collapse(Ja = c("   Ja, eine Urlaubsreise",
                      "   Ja, mehrere Urlaubsreisen"),
               Nein = c(" Nein" ))
levels(df2$f1_2)
df2$f1_2 <- df2$f1_2 %>% relevel(ref = "Nein")
contrasts(df2$f1_2)


#### Nummerisieren von ordinalen Variablen
str(df2)

df2 <- df2 %>%
  mutate_at(c("hh_gr_num" = "hh_gr",
              "kindhh_num" = "kindhh",
              "nek_hh_num" = "nek_hh",
              "orts_gr_num" = "orts_gr"), as.numeric)

#### Binäre Kodierung der Variable trend
levels(df2$trend)
df2$trend_2 <- factor(ifelse(df2$trend == "Neue Trends und Entwicklungen interessieren mich sehr.Ich probiere gerne als einer der Ersten etwas Neues aus.",
                             "interessiert", "nicht interssiert"))

#### Umkodierung der Variable wohnen
levels(df2$wohnen)
df2$miete <- df2$wohnen %>%
  fct_collapse(Miete = c("Zur Miete"),
               Eigentum = c("Im eigenen Haus",
                            "In einer Eigentumswohnung"))

#### Umkodierung der Variable bet_hhv (Berufstätigkeit)
levels(df2$bet_hhv)
df2$bet_hhv_2 <- df2$bet_hhv %>%
  fct_collapse(Rest = levels(df2$bet_hhv)[-c(1,4)])

str(df2)

#### Deskriptive Analyse ##########################################################

### Zielgröße
plot(df2$f1)
plot(df2$f1_2)
barplot(table(df2$f1_2) / nrow(df2))

### Zusammenhang zwischen Zielgröße und Einflussgrößen

# numerische Merkmale
plot_boxplot(df2[,-1], by = "f1_2")

# numerische Variablen, die ursprünglich kategorial sind
mosaicplot(table(df2$orts_gr, df2$f1_2)) # leicht positiv
mosaicplot(table(df2$kindhh, df2$f1_2)) # positiv
mosaicplot(table(df2$nek_hh, df2$f1_2)) # positiv
mosaicplot(table(df2$hh_gr, df2$f1_2)) # leicht positiv, aber nicht eindeutig

# kategoriale Merkmale
mosaicplot(table(df2$geschl, df2$f1_2)) # kein
mosaicplot(table(df2$bdgeb, df2$f1_2)) # kein
mosaicplot(table(df2$sb_hhv, df2$f1_2)) # positiv
mosaicplot(table(df2$trend_2, df2$f1_2)) # leicht positiv
mosaicplot(table(df2$miete, df2$f1_2)) # leicht höher für Eigentümer
mosaicplot(table(df2$bet_hhv_2, df2$f1_2)) # positiv


#### Modellierung GBM ############################################################
df2.mod <- df2 %>%
  select(f1_2,
         geschl, alter, hh_gr_num, orts_gr_num, sb_hhv, nek_hh_num, trend_2,
         bdgeb, kindhh_num, miete, bet_hhv_2, weight)

levels(df2.mod$f1_2)
levels(df2.mod$f1_2) <- c("0", "1")
df2.mod$f1_2 <- as.numeric(df2.mod$f1_2)
df2.mod$f1_2[df2.mod$f1_2 == 1] <- 0
df2.mod$f1_2[df2.mod$f1_2 == 2] <- 1

set.seed(123)
df2.mod.split <- initial_split(df2.mod, prop = 0.7)
df2.mod.train <- training(df2.mod.split)
df2.mod.test <- testing(df2.mod.split)

gbm.df2 <- gbm(f1_2 ~.,
               distribution = "bernoulli",
               data = df2.mod.train %>% select(-weight),
               # weights = df2.mod.train$weight,
               n.trees = 1000,
               interaction.depth = 5, #2: Acc = 0.6086, 3: 0.6136, 4: 0.6136
               shrinkage = 0.01,
               cv.folds = 3)
print(gbm.df2)
par(mar = c(5,6,4,2))
summary(gbm.df2, las = 1)
par(mar = c(5,4,4,2))

ntree.opt.cv.mod.df2 <- gbm.perf(gbm.df2, method = "cv")
print(ntree.opt.cv.mod.df2)

gbm.df2.pred <- predict.gbm(gbm.df2,
                            newdata = df2.mod.test,
                            n.trees = ntree.opt.cv.mod.df2,
                            type = "response")
schwellenwert2 <- mean(gbm.df2.pred)
schwellenwert2
gbm.df2.pred[1:10]

gbm.df2.pred.bin <- as.factor(ifelse(gbm.df2.pred > schwellenwert2, 1, 0))
df2.mod.test$f1_2 <- as.factor(df2.mod.test$f1_2)
confusionMatrix(gbm.df2.pred.bin, df2.mod.test$f1_2) # Acc 0.6103

gbm.df2.pred.testing <- prediction(gbm.df2.pred, df2.mod.test$f1_2)
gbm.df2.roc.testing <- performance(gbm.df2.pred.testing, "tpr", "fpr")
plot(gbm.df2.roc.testing)
auc.gbm.df2.temp <- performance(gbm.df2.pred.testing, "auc")
(auc.gbm.df2.test <- as.numeric(auc.gbm.df2.temp@y.values))


#### Modellierung GLM ###########################################################
glm.df2 <- glm(f1_2 ~.,
               data = df2.mod.train %>% select(-weight),
               # weights = df2.mod.train$weight,
               family = "binomial")
summary(glm.df2)
Anova(glm.df2)

# zur Interpretation
levels(df2$sb_hhv)
levels(df2$miete)
levels(df2$bet_hhv_2)

## Modellgüte
glm.df2.pred <- predict(glm.df2, df2.mod.test, type = c("response"))
df2.mod.test$glm.df2.pred <- glm.df2.pred

# Pseudo R^2
with(summary(glm.df2), 1 - deviance/null.deviance)

# ROC
glm.df2.roc <- roc(f1_2 ~ glm.df2.pred, data = df2.mod.test)
plot(glm.df2.roc, print.auc=T)
auc(glm.df2.roc)

# Konfusionsmatrix
(opt_schwelle2 <- as.numeric(coords(glm.df2.roc, "best", ret = "threshold", transpose = FALSE)))
glm.df2.pred.bin <- ifelse(df2.mod.test$glm.df2.pred > opt_schwelle2, "1", "0") %>%
  as.factor() %>%
  relevel(ref = "0")
df2.mod.test$glm.df2.pred.bin <- glm.df2.pred.bin

confusionMatrix(df2.mod.test$f1_2, df2.mod.test$glm.df2.pred.bin, positive = "1") # Acc 0.6103

