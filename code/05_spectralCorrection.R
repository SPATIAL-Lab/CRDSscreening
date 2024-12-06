
library(MASS)

## Read data
p = read.csv("out/plants.csv")
s = read.csv("out/soils.csv")
pirms = read.csv("out/pirms.csv")
sirms = read.csv("out/sirms.csv")
ssAve = read.csv("out/sshift.csv")
load("out/ld.rda")

## Add spectral parms
p = p[, !(names(p) %in% names(ssAve))]
s = s[, !(names(s) %in% names(ssAve))]
p = merge(p, ssAve, by.x = "Sample_ID", by.y = "ID")
s = merge(s, ssAve, by.x = "Sample_ID", by.y = "ID")

## Apply screening
p$Good = as.logical(predict(ld, p)$class)
s$Good = as.logical(predict(ld, s)$class)

## Fraction of data affected
1 - sum(p$Good) / nrow(p)
1 - sum(s$Good) / nrow(s)

# Offset correction ----
## Are averages for the screened data different than zero?
shapiro.test(pirms$d18O[pirms$Good] - pirms$d18O.irms[pirms$Good])
shapiro.test(pirms$d2H[pirms$Good] - pirms$d2H.irms[pirms$Good])
t.test(pirms$d18O[pirms$Good] - pirms$d18O.irms[pirms$Good])
t.test(pirms$d2H[pirms$Good] - pirms$d2H.irms[pirms$Good])
plot(density(pirms$d18O[pirms$Good] - pirms$d18O.irms[pirms$Good], 
             na.rm = TRUE))

shapiro.test(sirms$d18O[sirms$Good] - sirms$d18O.irms[sirms$Good])
shapiro.test(sirms$d2H[sirms$Good] - sirms$d2H.irms[sirms$Good])
t.test(sirms$d18O[sirms$Good] - sirms$d18O.irms[sirms$Good])
t.test(sirms$d2H[sirms$Good] - sirms$d2H.irms[sirms$Good])

## d18O offsets
off.p = mean(pirms$d18O[pirms$Good] - pirms$d18O.irms[pirms$Good], na.rm = TRUE)
off.s = mean(sirms$d18O[sirms$Good] - sirms$d18O.irms[sirms$Good], na.rm = TRUE)

## Apply offset
p$d18O.oc = p$d18O - off.p
s$d18O.oc = s$d18O - off.s

## Save
write.csv(p, "out/plants.csv", row.names = FALSE)
write.csv(s, "out/soils.csv", row.names = FALSE)
