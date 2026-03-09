# ============================================================================
# Simulationsseminar in R - Session 2
# Erste Simulationsstudie I
# Datum: 05.03.2026
# ============================================================================


# ============================================================================
# === 1. Der Datengenerierende Prozess (DGP) ===
# === Schritt 1: Parameter definieren ===
# Zunächst definieren wir die grundlegenden Parameter unseres Modells:

# set.seed(42) -> Setzt den Startwert des Zufallszahlengenerators, damit alle Zufallsziehungen (rnorm etc.)
#                bei erneutem Ausführen exakt reproduzierbar sind.
set.seed(42)  # für Reproduzierbarkeit

# npers <- 100 -> Speichert die Stichprobengrösse (Anzahl Beobachtungen/Personen) als Variable "npers".
npers <- 100  # Stichprobengrösse

# s_err <- 5 -> Speichert die Standardabweichung des Fehlerterms (σ), also wie stark ε um 0 streut.
s_err <- 5    # Standardabweichung der Fehler

# beta <- c(1.5, 2.5) -> Erstellt einen Vektor der "wahren" Regressionsparameter:
#                        beta[1] = Intercept (β0), beta[2] = Steigung (β1).
beta <- c(1.5, 2.5)  # beta0 = 1.5, beta1 = 2.5


# === Schritt 2: Fehlerterm generieren ===
# Das Regressionsmodell nimmt an, dass die Fehler normalverteilt sind mit Mittelwert 0 und Standardabweichung σ.

# err <- rnorm(n = npers, mean = 0, sd = s_err)
# -> Generiert npers Zufallswerte aus einer Normalverteilung N(mean, sd^2).
#    Hier: ε ~ N(0, 5^2). Ergebnis ist ein Vektor der Länge 100.
err <- rnorm(n = npers, mean = 0, sd = s_err)

# head(err) -> Zeigt die ersten 6 Werte des Fehlervektors an (Quick-Check).
head(err)

# Visualisierung

# hist(err, ...)
# -> Erstellt ein Histogramm, um zu prüfen, ob die Fehler ungefähr normalverteilt aussehen.
#    main = Titel, xlab = Beschriftung der x-Achse.
hist(err, main = "Verteilung der generierten Fehler", xlab = "Fehler")

# Überprüfung der Kennwerte

# mean(err) -> Berechnet den empirischen Mittelwert der generierten Fehler (sollte nahe 0 liegen).
mean(err)

# sd(err) -> Berechnet die empirische Standardabweichung der Fehler (sollte nahe s_err = 5 liegen).
sd(err)

# var(err) -> Berechnet die empirische Varianz (sollte nahe 25 liegen, da 5^2 = 25).
var(err)


# === Schritt 3: X-Werte generieren ===
# X-Werte: zufällige Ziehung aus Standardnormalverteilung

# x <- rnorm(n = npers, mean = 0, sd = 1)
# -> Generiert den Prädiktor x als Standardnormalverteilung N(0,1).
#    Ergebnis: Vektor der Länge 100.
x <- rnorm(n = npers, mean = 0, sd = 1)

# Länge prüfen

# length(x) -> Prüft, ob x wirklich npers Beobachtungen hat (hier: 100).
length(x)


# === Schritt 4: Y-Werte nach dem Modell generieren ===
# Y-Werte generieren nach dem Modell: y_i = beta0 + beta1 * x_i + ε_i

# y <- beta[1] + beta[2] * x + err
# -> Baut die Zielvariable y gemäss linearem Modell zusammen:
#    Intercept + Steigung * x + Fehler.
#    Ergebnis: Vektor der Länge 100.
y <- beta[1] + beta[2] * x + err

# Längen überprüfen

# length(x) -> Länge von x (soll 100 sein).
length(x)

# length(err) -> Länge des Fehlerterms (soll 100 sein).
length(err)

# length(y) -> Länge von y (soll 100 sein).
length(y)


# === Schritt 5: Datensatz zusammenstellen ===
# X- und Y-Werte in einem Datensatz abspeichern

# dat <- data.frame(x = x, y = y)
# -> Baut ein Data Frame (tabellarischer Datensatz) mit zwei Spalten: x und y.
dat <- data.frame(x = x, y = y)

# head(dat) -> Zeigt die ersten 6 Zeilen des Datensatzes.
head(dat)


# === Kontrolle und Visualisierung ===
# Lasst uns prüfen, wie gut wir die wahren Koeffizienten aus dieser einen Stichprobe schätzen können:

# Schätzung eines Regressionsmodells

# model <- lm(y ~ x, data = dat)
# -> Fit eines linearen Regressionsmodells:
#    y wird durch x erklärt (Formel y ~ x) basierend auf dem Datensatz dat.
#    Ergebnis: ein lm-Objekt mit geschätzten Koeffizienten, Residuen, etc.
model <- lm(y ~ x, data = dat)

# summary(model)
# -> Gibt eine ausführliche Modellzusammenfassung aus:
#    Koeffizienten, Standardfehler, t-Werte, p-Werte, R^2, Residual SE usw.
summary(model)

summary(model)

# ----------------------------------------------------------------------------
# NOTIZ ZUM OUTPUT:
# In der Tabelle von summary(lm()) stehen die geschätzten Regressions-
# koeffizienten in der Spalte "Estimate".
#
# (Intercept) = geschätztes β0 (Achsenabschnitt)
# x           = geschätztes β1 (Steigung der Regressionsgerade)
#
# Beispiel Output:
#             Estimate
# (Intercept)  1.6783   -> β̂0 (geschätzter Intercept)
# x            2.6801   -> β̂1 (geschätzte Steigung)
#
# Das geschätzte Modell lautet also:
# ŷ = 1.6783 + 2.6801 * x
#
# Vergleich mit den "wahren" Parametern aus dem DGP:
# beta <- c(1.5, 2.5)
#
# β0 = 1.5  (wahrer Intercept)
# β1 = 2.5  (wahre Steigung)
#
# Die Abweichung entsteht durch den zufälligen Fehlerterm ε.
# ----------------------------------------------------------------------------


# Extrahieren des geschätzten Steigungskoeffizients

# slope_est <- coef(model)[2]
# -> coef(model) liefert (Intercept, Steigung) als Vektor.
#    [2] nimmt das zweite Element: die geschätzte Steigung (β1-hat).
slope_est <- coef(model)[2]

# cat(...) -> Gibt Text + Werte formatiert in der Konsole aus (inkl. Zeilenumbruch \n).
cat("Wahrer Steigungskoeffizient:", beta[2], "\n")
cat("Geschätzter Steigungskoeffizient:", slope_est, "\n")

# abs(beta[2] - slope_est) -> Betrag der Differenz zwischen wahrer und geschätzter Steigung (Fehlergrösse).
cat("Abweichung:", abs(beta[2] - slope_est), "\n")


# Grafische Darstellung:
# Plot: Daten mit wahrer und geschätzter Regressionsgerade

# plot(dat$x, dat$y, ...)
# -> Streudiagramm von x (x-Achse) gegen y (y-Achse).
#    xlim/ylim setzen sichtbaren Bereich; pch=16 bedeutet gefüllte Punkte.
plot(dat$x, dat$y, 
     xlim = c(-2, 2),
     ylim = c(-10, 20),
     main = "Datengenerierender Prozess",
     xlab = "X", ylab = "Y",
     pch = 16)

# Wahre Regressionsgerade (grün)

# abline(beta[1], beta[2], ...)
# -> Zeichnet eine Gerade mit Intercept = beta[1] und Steigung = beta[2].
#    col, lwd, lty bestimmen Farbe, Linienbreite und Linientyp (gestrichelt).
abline(beta[1], beta[2], col = "green", lwd = 2, lty = 2)

# Geschätzte Regressionsgerade (orange)

# abline(model, ...)
# -> Wenn man ein lm-Objekt übergibt, zeichnet R automatisch die geschätzte Regressionsgerade
#    aus dem Modell (Intercept-hat und Steigung-hat).
abline(model, col = "orange", lwd = 2)

# legend(...)
# -> Fügt eine Legende hinzu.
#    "topleft" = Position; legend = Text; col/lwd/lty müssen zur Darstellung passen.
#    paste0(...) klebt Strings ohne Leerzeichen zusammen; round(...) rundet die Steigung.
legend("topleft", 
       legend = c(paste0("Wahre Gerade (Steigungskoeffizient = ", beta[2], ")"), 
                  paste0("Geschätzte Gerade  (Steigungskoeffizient = ", round(coef(model)[2], 3), ")")),
       col = c("green", "orange"),
       lwd = c(2, 2),
       lty = c(2, 1))


# === Übung 1: Die `dgp()`-Funktion ===
# Die Funktion sollte die Parameter `npers`, `beta` und `s_err` haben.

# dgp <- function(){ ... }
# -> Definiert eine eigene Funktion mit dem Namen dgp (data generating process).
#    Sie soll später: x generieren, err generieren, y berechnen und dat zurückgeben.
#    dient der Automatisierung.
# () enthalten die Argumente einer Funktion oder rufen eine Funktion auf.
# {} definieren den Codeblock einer Funktion – alles darin wird beim Aufruf ausgeführt.
# Der letzte Ausdruck innerhalb {} wird automatisch als Ergebnis der Funktion zurückgegeben.

dgp <- function(npers,beta,s_err){x <- rnorm(n = npers, mean = 0, sd = 1) 
err <- rnorm(n = npers, mean = 0, sd = s_err)
y <- beta[1] + beta[2] * x + err
data.frame(x = x, y = y)
}



# Test mit den vorgegebenen Parametern
dat <- dgp(npers = 100, beta = c(1.5, 2.5), s_err = 5)
head(dat)


# Beispiel-Aufruf zum Testen
# dat <- dgp(npers = 100, beta = c(1.5, 2.5), s_err = 5)
# head(dat)
# -> Kommentierter Beispielcode (wird nicht ausgeführt), der zeigt, wie man die Funktion nutzen würde:
#    Datensatz generieren und die ersten Zeilen anschauen.



# ============================================================================
# === 2. Ein Simulations-Durchgang ===
# === Übung 2: Die `one_simulation()`-Funktion ===
# Die Funktion sollte die Parameter `npers`, `beta` und `s_err` enthalten.

# one_simulation <- function(npers, beta, s_err){ ... }
# -> Definiert eine Funktion, die EINEN Simulationsdurchgang macht:
#    1) Daten generieren (via DGP),
#    2) lm(y ~ x) schätzen,
#    3) die geschätzte Steigung zurückgeben.
#    Generiert ein Datensatz, ein Slope 
one_simulation <- function(npers, beta, s_err){
  dat <- dgp(npers = npers, beta = beta, s_err = s_err) #Daten generieren (via DGP) damit man sich im zweiten Schritt darauf beziehen kann (siehe Zeile unten)
  model <- lm(y ~ x, data = dat) #2) lm(y ~ x) schätzen,
  slope_est <- coef(model)[2] #3) die geschätzte Steigung 
  return(slope_est) #3) die geschätzte Steigung zurückgeben.ist im Gesamten Output nur die Spalte Slope Sie zeigt was bestimmtes (bestimmter Outome)
}
one_simulation(npers = 100, beta = c(1.5, 2.5), s_err = 5)

# Beispiel-Aufruf zum Testen
# slope_est <- one_simulation(npers = 100, beta = c(1.5, 2.5), s_err = 5)
# slope_est
# -> Kommentiertes Beispiel: Funktion ausführen und geschätzte Steigung anschauen.
