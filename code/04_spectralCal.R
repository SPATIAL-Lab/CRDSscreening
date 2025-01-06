library(MASS)

# Prep ----
## Read data
pirms = read.csv("out/pirms.csv")
sirms = read.csv("out/sirms.csv")

## Combine plant and soil for model fitting
pirms = pirms[, names(pirms) != "Species"]
names(sirms) = names(pirms)
airms = rbind(pirms, sirms)

# Optimize model ----
## Tolerance for 'good' samples
airms$Good = airms$d18O - airms$d18O.irms < 1.3 & airms$d18O - airms$d18O.irms > -1.3

## LDA cross validation
ld.cv = lda(Good ~ BaseShift + SlopeShift + Residuals + BaseCurve + CH4, 
            data = airms, CV = TRUE, prior = c(0.6, 0.4))
ld.cv$class = as.logical(ld.cv$class)

## Review classifications
plot(airms$d18O.irms, airms$d18O, pch = 1, cex = 0.75)
points(airms$d18O.irms[airms$Good], airms$d18O[airms$Good], pch = 21, bg = "white", 
       cex = 1.5)
points(airms$d18O.irms[ld.cv$class], airms$d18O[ld.cv$class], 
       pch = 21, bg = "blue")

## Compare accuracy for two instruments
right = ld.cv$class == airms$Good
sum(right) / length(right)
sum(right[airms$Instrument == "HIDS2046"]) / 
  length(right[airms$Instrument == "HIDS2046"])
sum(right[airms$Instrument == "HIDS2052"]) / 
  length(right[airms$Instrument == "HIDS2052"])

# Test model ----
test = function(ntest, niter){
  s.raw = s.scr = numeric()
  right = numeric()
  for(i in 1:niter){
    ind = sample(1:nrow(airms), ntest)
    tr = airms[-ind,]
    ts = airms[ind,]
    ld = lda(Good ~ BaseShift + SlopeShift + Residuals + BaseCurve + CH4,
             data = tr, prior = c(0.6, 0.4))
    g = as.logical(predict(ld, ts)$class)
    
    s.raw = c(s.raw, sd(ts$d18O - ts$d18O.irms))
    s.scr = c(s.scr, sd(ts$d18O[g] - ts$d18O.irms[g])) 
    right = c(right, sum(g == ts$Good) / ntest)
  }
  return(list(s.raw = s.raw, s.scr = s.scr, right = right))
}

t10 = test(7, 1000)
t20 = test(15, 1000)

lapply(t10, mean, na.rm = TRUE)
lapply(t20, mean, na.rm = TRUE)

# Full model ----
## Fit model
ld = lda(Good ~ BaseShift + SlopeShift + Residuals + BaseCurve + CH4, 
         data = airms, prior = c(0.6, 0.4))
ld

## Apply screening
pirms$Good = as.logical(predict(ld, pirms)$class)
sirms$Good = as.logical(predict(ld, sirms)$class)

## Fraction of data affected
1 - sum(pirms$Good) / nrow(pirms)
1 - sum(sirms$Good) / nrow(sirms)

## By instrument
1 - sum(pirms$Good[pirms$Instrument == "HIDS2046"]) / 
  nrow(pirms[pirms$Instrument == "HIDS2046",])
1 - sum(pirms$Good[pirms$Instrument == "HIDS2052"]) / 
  nrow(pirms[pirms$Instrument == "HIDS2052",])

## Save
write.csv(pirms, "out/pirms.csv", row.names = FALSE)
write.csv(sirms, "out/sirms.csv", row.names = FALSE)
write.csv(airms, "out/airms.csv", row.names = FALSE)
save(ld, airms, file = "out/ld.rda")
