library(rgbif)

# Read data
p = read.csv("out/plants.csv")
s = read.csv("out/soils.csv")

# Species summaries ----
## Space
specSpec = data.frame("Species" = unique(p$Species), "Count" = rep(0),
                      "LowSS" = rep(0))

## How many screened?
for(i in seq_along(specSpec$Species)){
  psub = p[p$Species == specSpec$Species[i], ]
  specSpec$Count[i] = nrow(psub)
  specSpec$LowSS[i] = sum(!psub$Good)
}

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
