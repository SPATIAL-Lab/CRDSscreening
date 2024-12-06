library(MASS)

# Prep ----
## Read data
pirms = read.csv("out/pirms.csv")
sirms = read.csv("out/sirms.csv")

## Combine plant and soil for model fitting
pirms = pirms[, names(pirms) != "Species"]
names(sirms) = names(pirms)
ad = rbind(pirms, sirms)

# Optimize model ----
## Tolerance for 'good' samples
ad$Good = ad$d18O - ad$d18O.irms < 1.3 & ad$d18O - ad$d18O.irms > -1.3

## LDA cross validation
ld.cv = lda(Good ~ BaseShift + SlopeShift + Residuals + BaseCurve + CH4, 
            data = ad, CV = TRUE, prior = c(0.6, 0.4))
ld.cv$class = as.logical(ld.cv$class)

## Review classifications
plot(ad$d18O.irms, ad$d18O, pch = 1, cex = 0.75)
points(ad$d18O.irms[ad$Good], ad$d18O[ad$Good], pch = 21, bg = "white", 
       cex = 1.5)
points(ad$d18O.irms[ld.cv$class], ad$d18O[ld.cv$class], 
       pch = 21, bg = "blue")

# Test model ----
s1 = s2 = numeric()

for(i in 1:100){
  ind = sample(1:nrow(ad), 50)
  tr = ad[ind,]
  ts = ad[-ind,]
  ld = lda(Good ~ BaseShift + SlopeShift + Residuals + BaseCurve + CH4,
           data = tr, prior = c(0.6, 0.4))
  g = as.logical(predict(ld, ts)$class)
  
  s1 = c(s1, sd(ts$d18O - ts$d18O.irms))
  s2 = c(s2, sd(ts$d18O[g] - ts$d18O.irms[g])) 
}

mean(s1)
mean(s2)

# Full model ----
## Fit model
ld = lda(Good ~ BaseShift + SlopeShift + Residuals + BaseCurve + CH4, 
         data = ad, prior = c(0.6, 0.4))
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
save(ld, ad, file = "out/ld.rda")
