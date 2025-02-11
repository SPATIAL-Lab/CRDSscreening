library(rgbif)

# Read data
p = read.csv("out/plants.csv")
s = read.csv("out/soils.csv")

# Species summaries ----
## Space
specSpec = data.frame("Species" = unique(p$Species), "Count" = rep(0),
                      "d18O.off.m" = rep(0), "d18O.off.sd" = rep(0),
                      "d2H.off.m" = rep(0), "d2H.off.sd" = rep(0),
                      "d18O.off.high" = rep(0), "d2H.off.high" = rep(0))

## Offset stats
for(i in seq_along(specSpec$Species)){
  psub = p[p$Species == specSpec$Species[i], ]
  specSpec$Count[i] = nrow(psub)
  
  specSpec$d18O.off.m[i] = mean(psub$d18O.off)
  specSpec$d18O.off.sd[i] = sd(psub$d18O.off)
  
  specSpec$d2H.off.m[i] = mean(psub$d2H.off)
  specSpec$d2H.off.sd[i] = sd(psub$d2H.off)
  
  specSpec$d18O.off.high[i] = sum(psub$d18O.off > 1) / nrow(psub)
  specSpec$d2H.off.high[i] = sum(psub$d2H.off > 8) / nrow(psub)
}

subSpec = specSpec[order(specSpec$d18O.off.m, decreasing = TRUE),]
subSpec = head(subSpec, 8)

psub = p[p$Species %in% subSpec$Species, ]

boxplot(d18O.off ~ Species, data = psub)

plot(specSpec$d18O.off.m, specSpec$d2H.off.m)



## Fraction
specSpec$LowFrac = specSpec$LowSS / specSpec$Count

## Get common names from GBIF, gives warnings
specSpec$Vernacular = rep("")
for(i in seq_along(specSpec$Species)){
  gbif = name_backbone(specSpec$Species[i])
  use = as.data.frame(name_usage(gbif$usageKey)$data)
  if(length(use$vernacularName > 0)){
    specSpec$Vernacular[i] = use$vernacularName
  }
}

## Sort and view
specSpec = specSpec[rev(order(specSpec$LowFrac)), ]
View(specSpec)

## Save
write.csv(specSpec, "out/screenedBySpecies.csv", row.names = FALSE)

# Site summaries ----
## Space
siteSpec = data.frame("Site" = unique(p$Site_ID), "Count" = rep(0),
                      "LowSS" = rep(0))

## How many low SS?
for(i in seq_along(siteSpec$Site)){
  psub = p[p$Site_ID == siteSpec$Site[i] & !is.na(p$SlopeShift),]
  siteSpec$Count[i] = nrow(psub)
  siteSpec$LowSS[i] = sum(!psub$Good)
}

## Fraction
siteSpec$LowFrac = siteSpec$LowSS / siteSpec$Count

## Sort and view
siteSpec = siteSpec[rev(order(siteSpec$LowFrac)), ]
View(siteSpec)

## Save
write.csv(siteSpec, "out/screenedBySite.csv", row.names = FALSE)
