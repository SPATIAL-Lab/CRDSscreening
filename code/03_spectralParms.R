# File names
fn = list.files("data/rf/")
rf = file.path("data", "rf", fn)
cf = file.path("data", "cf", fn)

# Process metrics- ----
sshift = data.frame("Vial" = numeric(), "BaseShift" = numeric(), 
                    "SlopeShift" = numeric(), "Residuals" = numeric(),
                    "BaseCurve" = numeric(), "CH4" = numeric(),
                    "ID" = character(), "Instrument" = character())
for(i in seq_along(rf)){
  ## Read files
  ids = read.csv(rf[i])
  d = read.csv(cf[i])
  
  ## Get vial #s from Port
  d.vials = strsplit(d$Port, "-")
  d.vials = as.numeric(sapply(d.vials, "[[", 2))
  
  ## Average the metrics per vial
  if(length(unique(d.vials)) > 4){
    spec = data.frame("Vial" = unique(d.vials), "BaseShift" = rep(0), 
                      "SlopeShift" = rep(0), "Residuals" = rep(0),
                      "BaseCurve" = rep(0), "CH4" = rep(0))
    for(j in seq_along(spec$Vial)){
      spec$BaseShift[j] = mean(d$baseline_shift[d.vials == spec$Vial[j]],
                                na.rm = TRUE)
      spec$SlopeShift[j] = mean(d$slope_shift[d.vials == spec$Vial[j]],
                                na.rm = TRUE)
      spec$Residuals[j] = mean(d$residuals[d.vials == spec$Vial[j]],
                                na.rm = TRUE)
      spec$BaseCurve[j] = mean(d$baseline_curvature[d.vials == spec$Vial[j]],
                                na.rm = TRUE)
      spec$CH4[j] = mean(d$ch4_ppm[d.vials == spec$Vial[j]],
                                na.rm = TRUE)
    }
    
    ## Standardize to the vial 4 average
    for(j in 5:nrow(spec)){
      spec[j, 2:6] = spec[j, 2:6] - spec[spec$Vial == 4, 2:6]
    }

    ## Add sample IDs
    ids$ID = paste(ids$Identifier.2, ids$Description, sep = "_")
    spec$ID = ids$ID[match(spec$Vial, ids$Vial)]
    
    ## Add instrument
    spec$Instrument = rep(substr(rf[i], nchar(rf[i]) - 11, nchar(rf[i]) - 4))
    
    ## Bind
    sshift = rbind(sshift, spec)
  }
}

# Condense and match ----
## Average per ID
ssAve = data.frame("ID" = unique(sshift$ID), "BaseShift" = rep(0), 
                   "SlopeShift" = rep(0), "Residuals" = rep(0),
                   "BaseCurve" = rep(0), "CH4" = rep(0), "Instrument" = rep(""))
for(i in seq_along(ssAve$ID)){
  ssSub = sshift[sshift$ID == ssAve$ID[i], ]
  for(j in 2:6){
    ssAve[i, j] = mean(ssSub[, j], na.rm = TRUE)
  }
  if(length(unique(ssSub$Instrument)) == 1){
    ssAve[i, 7] = ssSub$Instrument[1]
  } else{
    ssAve[i, 7] = "Both"
  }
}

ssAve = ssAve[!is.na(ssAve$SlopeShift),]
write.csv(ssAve, "out/sshift.csv", row.names = FALSE)

## Add back to datasets
pirms = read.csv("out/pirms.csv")
sirms = read.csv("out/sirms.csv")
pirms = pirms[, !(names(pirms) %in% names(ssAve))]
sirms = sirms[, !(names(sirms) %in% names(ssAve))]
pirms = merge(pirms, ssAve, by.x = "Sample_ID", by.y = "ID")
sirms = merge(sirms, ssAve, by.x = "Sample_ID", by.y = "ID")

# Save
write.csv(pirms, "out/pirms.csv", row.names = FALSE)
write.csv(sirms, "out/sirms.csv", row.names = FALSE)

