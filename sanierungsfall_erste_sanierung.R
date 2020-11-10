### Befragung zur Reisebereitschaft bzw. -tätigkeit

# Zielgröße:
# Haben Sie dieses Jahr schon eine Urlaubsreise
# oder mehrere Urlaubsreisen gemacht?

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
rohdaten <- read.spss("Reisen.sav",
  to.data.frame = TRUE
)
head(rohdaten)
names(rohdaten)

# Relevante Variablen
zweite_umfrage <- data.frame(
  befnr = rohdaten[, "befnr"],
  f1 = rohdaten[, "f1"],
  rohdaten[, 217:236]
)
head(zweite_umfrage)
names(zweite_umfrage)
names(zweite_umfrage) <- c(
  "befragungsnummer", "reisebereitschaft", "geschlecht",
  "alter", "haushaltsgroesse", "kinder_im_haushalt",
  "ist_haushaltsvorstand", "ist_haushaltsfuehrer",
  "familienstand", "schulbildung", "berufstaetigkeit",
  "beruf", "schulbildung_haushaltsvorstand",
  "berufstaetigkeit_haushaltsvorstand", "wohnverhaeltnis",
  "nettoeinkommen_haushalt", "ortsgroesse", "bundesland",
  "bundesgebiet", "interesse_trend", "nielsengebiet",
  "stichprobengewichtung"
)

#### Datenaufgebreitung #######################################################

#### Zielvariable binär kodieren
levels(zweite_umfrage[, "reisebereitschaft"])
# Zielgröße:
# Haben Sie dieses Jahr schon eine Urlaubsreise oder
#   mehrere Urlaubsreisen gemacht?
# -> nehme binäre Kodierung vor
# Gruppe1 (Ja): "Ja, eine Urlaubsreise"
#               "Ja, mehrere Urlaubsreisen"
# Gruppe2 (Nein): "Nein"

zweite_umfrage[, "reisebereitschaft_binaer"] <-
  zweite_umfrage[, "reisebereitschaft"] %>%
  fct_collapse(
    Ja = c(
      "   Ja, eine Urlaubsreise",
      "   Ja, mehrere Urlaubsreisen"
    ),
    Nein = " Nein"
  ) %>%
  relevel(ref = "Nein")
levels(zweite_umfrage[, "reisebereitschaft_binaer"])
contrasts(zweite_umfrage[, "reisebereitschaft_binaer"])


#### Nummerisieren von ordinalen Variablen
str(zweite_umfrage)

zweite_umfrage <- zweite_umfrage %>%
  mutate_at(
    c(
      "haushaltsgroesse_numerisch" = "haushaltsgroesse",
      "kinder_im_haushalt_numerisch" = "kinder_im_haushalt",
      "nettoeinkommen_haushalt_numerisch" = "nettoeinkommen_haushalt",
      "ortsgroesse_numerisch" = "ortsgroesse"
    ),
    as.numeric
  )

#### Binäre Kodierung der Variable interesse_trend
levels(zweite_umfrage[, "interesse_trend"])
zweite_umfrage[, "interesse_trend_binaer"] <- factor(ifelse(
  zweite_umfrage[, "interesse_trend"] == "Neue Trends und Entwicklungen interessieren mich sehr.Ich probiere gerne als einer der Ersten etwas Neues aus.",
  "interessiert", "nicht interssiert"
))

#### Umkodierung der Variable wohnverhältnis
levels(zweite_umfrage[, "wohnverhaeltnis"])
zweite_umfrage[, "eigentum_oder_miete"] <-
  zweite_umfrage[, "wohnverhaeltnis"] %>%
  fct_collapse(
    Miete = "Zur Miete",
    Eigentum = c(
      "Im eigenen Haus",
      "In einer Eigentumswohnung"
    )
  )

#### Umkodierung der Variable berufstätigkeit_haushaltsvorstand
levels(zweite_umfrage[, "berufstaetigkeit_haushaltsvorstand"])
zweite_umfrage[, "berufstaetigkeit_haushaltsvorstand_mit_rest"] <-
  zweite_umfrage[, "berufstaetigkeit_haushaltsvorstand"] %>%
  fct_collapse(
    Rest = levels(zweite_umfrage[, "berufstaetigkeit_haushaltsvorstand"])[-c(1, 4)]
  )

str(zweite_umfrage)

#### Deskriptive Analyse #######################################################

### Zielgröße
plot(zweite_umfrage[, "reisebereitschaft"])
plot(zweite_umfrage[, "reisebereitschaft_binaer"])
barplot(table(zweite_umfrage[, "reisebereitschaft_binaer"]) / nrow(zweite_umfrage))

### Zusammenhang zwischen Zielgröße und Einflussgrößen

# numerische Merkmale
plot_boxplot(zweite_umfrage[, -1], by = "reisebereitschaft_binaer")

# Funktion: Mosaicplots zeichnen mit der Zielgroesse
mosaicplot_reisebereitschaft <- function(kovariablen) {
  withr::with_par(
    list(mfrow = c(ceiling(length(kovariablen) / 2), 2)),
    for (i in seq_along(kovariablen)) {
      mosaicplot(table(
        zweite_umfrage[, kovariablen[i]],
        zweite_umfrage[, "reisebereitschaft_binaer"]
      ),
      main = kovariablen[i]
      )
    }
  )
}
# numerische Variablen, die ursprünglich kategorial sind
kovariablen <- c(
  "ortsgroesse", "kinder_im_haushalt", "nettoeinkommen_haushalt",
  "haushaltsgroesse"
)
mosaicplot_reisebereitschaft(kovariablen)

# kategoriale Merkmale
kovariablen <- c(
  "geschlecht", "bundesgebiet", "schulbildung_haushaltsvorstand",
  "interesse_trend_binaer", "eigentum_oder_miete",
  "berufstaetigkeit_haushaltsvorstand_mit_rest"
)
mosaicplot_reisebereitschaft(kovariablen)


#### Modellierung GBM ##########################################################
zweite_umfrage_modell <- zweite_umfrage %>%
  select(
    reisebereitschaft_binaer,
    geschlecht, alter, haushaltsgroesse_numerisch, ortsgroesse_numerisch,
    schulbildung_haushaltsvorstand, nettoeinkommen_haushalt_numerisch,
    interesse_trend_binaer, bundesgebiet, kinder_im_haushalt_numerisch,
    eigentum_oder_miete, berufstaetigkeit_haushaltsvorstand_mit_rest,
    stichprobengewichtung
  )

levels(zweite_umfrage_modell[, "reisebereitschaft_binaer"])
levels(zweite_umfrage_modell[, "reisebereitschaft_binaer"]) <- c("0", "1")

zweite_umfrage_modell[, "reisebereitschaft_binaer"] <- as.numeric(
  zweite_umfrage_modell[, "reisebereitschaft_binaer"]
)

zweite_umfrage_modell[, "reisebereitschaft_binaer"] <- ifelse(
  zweite_umfrage_modell[, "reisebereitschaft_binaer"] == 1,
  0, 1
)

set.seed(123)
zweite_umfrage_modell_split <- initial_split(zweite_umfrage_modell, prop = 0.7)
zweite_umfrage_modell_train <- training(zweite_umfrage_modell_split)
zweite_umfrage_modell_test <- testing(zweite_umfrage_modell_split)

gbm_zweite_umfrage <- gbm(
  reisebereitschaft_binaer ~ .,
  distribution = "bernoulli",
  data = zweite_umfrage_modell_train %>% select(-stichprobengewichtung),
  # weights = zweite_umfrage_modell_train$stichprobengewichtung,
  n.trees = 1000,
  interaction.depth = 5, # 2: Acc = 0.6086, 3: 0.6136, 4: 0.6136, 5: 0.6103
  shrinkage = 0.01,
  cv.folds = 3
)
print(gbm_zweite_umfrage)
withr::with_par(
  list(mar = c(5, 6, 4, 2)),
  summary(gbm_zweite_umfrage, las = 1)
)

iterationen_gbm_zweite_umfrage <- gbm.perf(
  gbm_zweite_umfrage,
  method = "cv"
)
print(iterationen_gbm_zweite_umfrage)

gbm_zweite_umfrage_prediction <- predict.gbm(
  gbm_zweite_umfrage,
  newdata = zweite_umfrage_modell_test,
  n.trees = iterationen_gbm_zweite_umfrage,
  type = "response"
)
schellenwert_gbm_zweite_umfrage <- mean(gbm_zweite_umfrage_prediction)
schellenwert_gbm_zweite_umfrage
gbm_zweite_umfrage_prediction[1:10]

gbm_zweite_umfrage_prediction_binaer <- as.factor(ifelse(
  gbm_zweite_umfrage_prediction > schellenwert_gbm_zweite_umfrage,
  1, 0
))
zweite_umfrage_modell_test[, "reisebereitschaft_binaer"] <- as.factor(
  zweite_umfrage_modell_test[, "reisebereitschaft_binaer"]
)
confusionMatrix(
  gbm_zweite_umfrage_prediction_binaer,
  zweite_umfrage_modell_test[, "reisebereitschaft_binaer"]
) # Acc 0.6103

gbm_zweite_umfrage_prediction_roc <- prediction(
  gbm_zweite_umfrage_prediction,
  zweite_umfrage_modell_test[, "reisebereitschaft_binaer"]
)
gbm_zweite_umfrage_roc <- performance(
  gbm_zweite_umfrage_prediction_roc, "tpr", "fpr"
)
plot(gbm_zweite_umfrage_roc)

gbm_zweite_umfrage_auc_temp <- performance(
  gbm_zweite_umfrage_prediction_roc, "auc"
)
(gbm_zweite_umfrage_auc <- as.numeric(gbm_zweite_umfrage_auc_temp@y.values))


#### Modellierung GLM ##########################################################
glm_zweite_umfrage <- glm(
  reisebereitschaft_binaer ~ .,
  data = zweite_umfrage_modell_train %>% select(-stichprobengewichtung),
  # weights = zweite_umfrage_modell_train$stichprobengewichtung,
  family = "binomial"
)
summary(glm_zweite_umfrage)
Anova(glm_zweite_umfrage)

# zur Interpretation
levels(zweite_umfrage[, "schulbildung_haushaltsvorstand"])
levels(zweite_umfrage[, "eigentum_oder_miete"])
levels(zweite_umfrage[, "berufstaetigkeit_haushaltsvorstand_mit_rest"])

## Modellgüte
glm_zweite_umfrage_prediction <- predict(
  glm_zweite_umfrage,
  zweite_umfrage_modell_test,
  type = "response"
)
zweite_umfrage_modell_test[, "glm_zweite_umfrage_prediction"] <-
  glm_zweite_umfrage_prediction

# Pseudo R^2
with(summary(glm_zweite_umfrage), 1 - deviance / null.deviance)

# ROC
glm_zweite_umfrage_roc <- roc(
  reisebereitschaft_binaer ~ glm_zweite_umfrage_prediction,
  data = zweite_umfrage_modell_test
)
plot(glm_zweite_umfrage_roc, print.auc = TRUE)
auc(glm_zweite_umfrage_roc)

# Konfusionsmatrix
schwellenwert_glm_zweite_umfrage <- as.numeric(coords(
  glm_zweite_umfrage_roc, "best",
  ret = "threshold", transpose = FALSE
))
schwellenwert_glm_zweite_umfrage

glm_zweite_umfrage_prediction_binaer <- ifelse(
  zweite_umfrage_modell_test[, "glm_zweite_umfrage_prediction"] > schwellenwert_glm_zweite_umfrage,
  "1", "0"
) %>%
  as.factor() %>%
  relevel(ref = "0")
zweite_umfrage_modell_test[, "glm_zweite_umfrage_prediction_binaer"] <-
  glm_zweite_umfrage_prediction_binaer

confusionMatrix(zweite_umfrage_modell_test[, "reisebereitschaft_binaer"],
  zweite_umfrage_modell_test[, "glm_zweite_umfrage_prediction_binaer"],
  positive = "1"
) # Acc 0.6103
