# 1. Caricate il dataframe downs.bc, contenuto nel package boot. L’incidenza della sindrome di Down dipende fortemente dall’età della madre ed aumenta considerevolmente dopo i 30 anni. 
# I dati contenuti nel dataset riguardano uno studio condotto nel 1960 nella British Columbia. 
# Le madri sono state raggruppate in classi di età: Il data frame downs.bc è composto da 30 righe e 3 colonne:
#  age   valore centrale della classe di età della madre;
#  m     numero di nati vivi per classe di età della madre;
#  r     numero di casi di sindrome di Down per classe di età della madre.

# 2. Calcolate la correlazione e costruite la matrice degli scatterplot per tutte le variabili del dataframe. # Commentate i risultati numerici e grafici.

# 3. Inserite nel dataframe una nuova variabile rm che contenga le frequenze relative dei bambini nati con sindrome di Down: rapporto tra nati con sindrome di Down e nati vivi per ciascuna classe di età della madre. 
# Calcolate la correlazione della variabile creata con l’età della madre e costruite un diagramma di dispersione che metta in relazione le due variabili. Commentate.

# 4. Costruite un nuovo dataframe downs37 che contenga le stesse variabili del precedente selezionando solo le madri con età superiore ai 37 anni. 
# Costruite un diagramma di dispersione come al punto 3, ma estraendo i dati dal nuovo dataframe. 
# Cosa potete notare osservando il diagramma?

# 5. Effettuate lo studio della dipendenza del numero dei bambini nati con sindrome di Down dall’età della madre per il dataframe downs37. 
# Allo scopo considerate due tipi di modelli:
#   a. Un modello per dati di conteggio ottenuto considerando il numero di nati con sindrome di Down (evento raro) in funzione dell’età della madre e del numero di nati vivi. 
# Valutate la bontà del modello stimato e commentate i risultati ottenuti.
#   b. Un modello per dati dicotomici ottenuto considerando le frequenze di nati con e senza sindrome di Down come quelle di successi e insuccessi. 
# In questo caso i dati sono raggruppati per classi di età della madre e non si dispone di una variabile risposta dicotomica. 
# Pertanto è necessario considerare come risposta nella formula che definisce il modello lineare generalizzato, una matrice formata da due colonne rispettivamente corrispondenti alle frequenze assolute dei nati con e senza sindrome di Down per ciascuna classe di età della madre. 
# Valutate (comandi anova(), summary(), etc.) quale funzione link fornisce il risultato migliore. 
# Integrate il diagramma di dispersione del punto 4 con una curva che che metta in relazione l’età della madre con i valori stimati (predicted) dal modello. Commentate tutti i risultati numerici e grafici ottenuti.

# Extra

# 1. Effettuate lo studio della dipendenza del numero dei bambini nati con sindrome di Down dall’età della madre per l’intero dataframe downs.bc. 
# Allo scopo considerate un modello per dati dicotomici ottenuto come al punto 5.b. 
# Integrate il diagramma di dispersione del punto 3 con una curva che metta in relazione l’età della madre con i valori stimati dal modello. 
# Commentate i risultati ottenuti per il modello stimato.

# 2. Dall’analisi grafica del comportamento dei residui di devianza evidenziate che la forma lineare del modello precedente è insufficiente e che potrebbe essere utile inserire un termine quadratico. 
# Stimate un nuovo modello considerando come variabili antecedenti l’età della madre e la stessa età al quadrato. 
# Valutate la performance del modello ottenuto commentando l’output numerico e grafico.

library(boot)
data(downs.bc)

cor(downs.bc)
plot(downs.bc, pch = 16, col = "blue")
#lo scatterplot mostra una forte relazione non lineare tra l'età della madre e il numero di nati vivi, viene confermato dalla forte relazione inversa indicata dal valore -0.89 della correlazione
#Il coefficiente vicino a zero indica una correlazione lineare debole tra l'età della madre e r.

downs.bc$rm <- downs.bc$r / downs.bc$m
cor(downs.bc$age, downs.bc$rm)
plot(downs.bc$age, downs.bc$rm, pch = 16, col = "green",
     xlab = "Età della madre (valore centrale)", 
     ylab = "Frequenza relativa (r / m)",
     main = "Relazione tra età e frequenza sindrome di Down")
#La correlazione (0.73) indica un'associazione lineare positiva.
#Il diagramma di dispersione conferma questa affermazione mostrando che la frequenza relativa aumenta notevolmente superati i 37 anni di età della madre. 

downs37 <- downs.bc[downs.bc$age > 37, ]
plot(downs37$age, downs37$rm, pch = 16, col = "red",
     xlab = "Età della madre (> 37)", 
     ylab = "Frequenza relativa (r / m)",
     main = "Frequenza sindrome di Down per età madre> 37")
#limitando l'analisi alle madri con età superiore ai 37 anni, si nota una crescita ancora più rapida della percentuale di nati con la sindrome di Down.

downs37.glm1 <- glm(r ~ age + m, data = downs37, family = poisson)
summary(downs37.glm1)
1 - pchisq(downs37.glm1$deviance, downs37.glm1$df.residual)
#si rifiuta l'ipotesi nulla: sia age che m hanno un effetto rilevante su r.
#il modello di Poisson si adatta sufficientemente bene ai dati del dataframe (0.139>0.05)

downs37.logit  <- glm(cbind(r, m - r) ~ age, data = downs37, family = binomial(link = "logit"))
downs37.probit <- glm(cbind(r, m - r) ~ age, data = downs37, family = binomial(link = "probit"))
downs37.cloglog <- glm(cbind(r, m - r) ~ age, data = downs37, family = binomial(link = "cloglog"))
summary(downs37.logit)
summary(downs37.probit)
summary(downs37.cloglog)
anova(downs37.logit, downs37.probit, downs37.cloglog)
# Dopo il confronto con l'anova, utilizzo il modello probit per le previsioni, in quanto ha la devianza residua inferiore rispetto alle altre funzioni link.

pred.probit <- predict(downs37.probit, type = "response")
plot(downs37$age, downs37$rm, pch = 16, col = "red",
     xlab = "Età della madre (> 37)", 
     ylab = "Frequenza relativa (r / m)",
     main = "Previsioni del modello Probit")
points(downs37$age, pred.probit, pch = 16, col = "blue")
ordine <- order(downs37$age)
lines(downs37$age[ordine], pred.probit[ordine], col = "blue", lwd = 2)
legend("topleft", legend = c("Dati osservati", "Valori stimati (Probit)"),
       pch = c(16, 16), col = c("red", "blue"))
#Nel grafico la linea blu che unisce i punti stimati dalla funzione probit cattura bene l'andamento descritto dalle osservazioni.
#l'andamento dei valori stimati sintetizza l'aumentare del rischio di sindrome di Down all'aumentare dell'età della madre, a partire dai 37 anni.

#EXTRA
downs.glm1 <- glm(cbind(r, m - r) ~ age, data = downs.bc, family = binomial)
summary(downs.glm1)
plot(downs.bc$age, downs.bc$rm, pch = 16, col = "green",
     xlab = "Età della madre (valore centrale)", ylab = "Frequenza relativa (r / m)",
     main = "Relazione tra età e frequenza sindrome di Down")
pred.downs <- predict(downs.glm1, type = "response")
points(downs.bc$age, pred.downs, pch = 16, col = "purple")
lines(downs.bc$age[ordine], pred.downs[ordine], col = "purple", lwd = 2)
legend("topleft", legend = c("Dati osservati", "Valori stimati"), 
       pch = c(16, 16), col = c("green", "purple"))
#il valore del p-value evidenzia che l'effetto dell'età è estremamente significativo: 
#all'aumentare dell'età della madre aumenta la probabilità di nascere affetti da sindrome di Down.

downs.glm2 <- glm(cbind(r, m - r) ~ age + I(age^2), data = downs.bc, family = binomial)
summary(downs.glm2)
anova(downs.glm1, downs.glm2, test = "Chisq")
#il valore altamente significativo ci fa preferire il modello 2 con i termini quadratici
plot(downs.bc$age, downs.bc$rm, pch = 16, col = "green",
     xlab = "Età della madre", ylab = "Frequenza relativa (r / m)",
     main = "Confronto modelli: Lineare vs Quadratico")
pred.quad <- predict(downs.glm2, type = "response")
ordine <- order(downs.bc$age)
lines(downs.bc$age[ordine], pred.quad[ordine], col = "orange", lwd = 2)
legend("topleft", legend = c("Dati osservati", "Modello quadratico"), 
       pch = c(16, NA), lty = c(NA, 1), col = c("green", "orange"), lwd = c(NA, 2))
#la curva quadratica cattura l'informazione delle osservazioni molto rispetto al modello predittivo precedente.

####
install.packages("ggplot2")
library(ggplot2)

df_plot <- data.frame(
  age = downs.bc$age,
  freq_obs = downs.bc$r / downs.bc$m,
  freq_pred = predict(downs.glm2, type = "response"))

#Costruzione del grafico con ggplot2
ggplot(df_plot, aes(x = age)) +
  geom_point(aes(y = freq_obs, color = "Dati osservati"), size = 2.5, alpha = 0.8) +
  geom_line(aes(y = freq_pred, color = "Modello quadratico"), linewidth = 1) +
  scale_color_manual(values = c("Dati osservati" = "green", "Modello quadratico" = "orange")) +
  labs(
    title = "Previsioni del Modello Quadratico",
    subtitle = "Regressione binomiale sull'intero dataset downs.bc",
    x = "Età della madre",
    y = "Frequenza relativa (r / m)",
    color = "Legenda"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank())
    
    