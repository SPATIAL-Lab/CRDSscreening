library(MASS)

# Read data
pirms = read.csv("out/pirms.csv")
sirms = read.csv("out/sirms.csv")

## Combine plant and soil for model fitting
pirms = pirms[, names(pirms) != "Species"]
names(sirms) = names(pirms)
ad = rbind(pirms, sirms)

## Tolerance for 'good' samples
ad$Good = ad$p.diff.o < 3 & ad$p.diff.o > -1

## LDA cross validation
ld.cv = lda(Good ~ BaseShift + SlopeShift + Residuals + BaseCurve + CH4, 
            data = ad, CV = TRUE)
ld.cv$class = as.logical(ld.cv$class)

## Review classifications
plot(ad$d18O.irms, ad$d18O, pch = 1, cex = 0.75)
points(ad$d18O.irms[ad$Good], ad$d18O[ad$Good], pch = 21, bg = "white", 
       cex = 1.5)
points(ad$d18O.irms[ld.cv$class], ad$d18O[ld.cv$class], 
       pch = 21, bg = "blue")

plot(density(ad$p.diff.o[ld.cv$class]))

## Fit full model
ld = lda(Good ~ BaseShift + SlopeShift + Residuals + BaseCurve + CH4, 
         data = ad)
ld

## Apply screening
pirms$Good = as.logical(predict(ld, pirms)$class)
sirms$Good = as.logical(predict(ld, sirms)$class)

## Fraction of data affected
1 - sum(pirms$Good) / nrow(pirms)
1 - sum(sirms$Good) / nrow(sirms)

## Save
write.csv(pirms, "out/pirms.csv", row.names = FALSE)
write.csv(sirms, "out/sirms.csv", row.names = FALSE)
save(ld, file = "out/ld.rda")
