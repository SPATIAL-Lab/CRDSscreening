library(MASS)

## Read data
p = read.csv("out/plants.csv")
s = read.csv("out/soils.csv")
ssAve = read.csv("out/sshift.csv")
load("out/lm.rda")

## Add spectral parms
p = p[, !(names(p) %in% names(ssAve))]
s = s[, !(names(s) %in% names(ssAve))]
p = merge(p, ssAve, by.x = "Sample_ID", by.y = "ID")
s = merge(s, ssAve, by.x = "Sample_ID", by.y = "ID")

## Predict
pO.off = predict(lmO, p, interval = "prediction")
p$d18O.off = pO.off[, 1]
p$d18O.off.pw = (pO.off[, 3] - pO.off[, 2]) / 2

pH.off = predict(lmH, p, interval = "prediction")
p$d2H.off = pH.off[, 1]
p$d2H.off.pw = (pH.off[, 3] - pH.off[, 2]) / 2

sO.off = predict(lmO, s, interval = "prediction")
s$d18O.off = sO.off[, 1]
s$d18O.off.pw = (sO.off[, 3] - sO.off[, 2]) / 2

sH.off = predict(lmH, s, interval = "prediction")
s$d2H.off = sH.off[, 1]
s$d2H.off.pw = (sH.off[, 3] - sH.off[, 2]) / 2

## Compare
plot(density(p$d18O.off))
lines(density(s$d18O.off), col = 2)
mean(p$d18O.off)
mean(s$d18O.off)

plot(density(p$d2H.off))
lines(density(s$d2H.off), col = 2)
mean(p$d2H.off)
mean(s$d2H.off)

## Fraction of data w/ large corrections 
sum(abs(p$d18O.off) > 1)
sum(abs(p$d18O.off > 1)) / nrow(p)

sum(abs(s$d18O.off) > 1)
sum(abs(s$d18O.off) > 1) / nrow(s)

sum(abs(p$d2H.off) > 8)
sum(abs(p$d2H.off > 8)) / nrow(p)

sum(abs(s$d2H.off) > 1)
sum(abs(s$d2H.off) > 1) / nrow(s)

## H vs O
plot(p$d18O.off, p$d2H.off)
points(s$d18O.off, s$d2H.off, col = 2)

# Apply correction ----
p$d18O.oc = p$d18O - p$d18O.off
s$d18O.oc = s$d18O - s$d18O.off

p$d2H.oc = p$d2H - p$d2H.off
s$d2H.oc = s$d2H - s$d2H.off

plot(p$d18O, p$d2H)
abline(10, 8, lwd = 2)
arrows(p$d18O.oc - 2 * p$d18O.off.se, p$d2H.oc,
       p$d18O.oc + 2 * p$d18O.off.se, p$d2H.oc,
       angle = 90, length = 0.05, code = 3, col = 2)
arrows(p$d18O.oc, p$d2H.oc - 2 * p$d2H.off.se,
       p$d18O.oc, p$d2H.oc + 2 * p$d2H.off.se,
       angle = 90, length = 0.05, code = 3, col = 2)
points(p$d18O.oc, p$d2H.oc, pch = 21, bg = 2)

plot(s$d18O, s$d2H)
points(s$d18O.oc, s$d2H.oc, pch = 21, bg = 2)
abline(10, 8)

## Save
write.csv(p, "out/plants.csv", row.names = FALSE)
write.csv(s, "out/soils.csv", row.names = FALSE)
