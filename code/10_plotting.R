# Read data
p = read.csv("out/plants.csv")
s = read.csv("out/soils.csv")
pirms = read.csv("out/pirms.csv")
sirms = read.csv("out/sirms.csv")
airms = read.csv("out/airms.csv")
load("out/lm.rda")

plant = "darkgreen"
soil = "lightgoldenrod3"
good = "deepskyblue3"
bad = "darkorange"

# Figure 1 ----
png("out/Figure1.png", 7, 7, units = "in", res = 600)
layout(matrix(1:4, nrow = 2))

## Soil d2H
par(mar = c(5, 5, 1, 1))
plot(sirms$d2H.irms, sirms$d2H, type = "n", 
     xlab = expression(delta^2*"H"["IRMS"]*" (VSMOW)"), 
     ylab = expression(delta^2*"H"["CRDS"]*" (VSMOW)"))
abline(0, 1, lwd = 2)
points(sirms$d2H.irms, sirms$d2H, pch = 21, bg = soil, col = "grey90",
       cex = 1.5)
points(sirms$d2H.irms, sirms$d2H.oc, pch = 21, bg = soil, cex = 1.75, lwd = 2)

bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.1 * diff(bds[3:4]),
     paste0("RSD(raw) = ", round(sd(sirms$d2H - sirms$d2H.irms), 1), "\u2030"), 
     adj = c(1, 0))
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.04 * diff(bds[3:4]),
     paste0("RSD(corrected) = ", 
            round(sd(sirms$d2H.oc - sirms$d2H.irms), 1), 
            "\u2030"), 
     adj = c(1, 0))
text(bds[1] + 0.05 * diff(bds[1:2]), bds[4] - 0.05 * diff(bds[3:4]), "A")

## Plant d2H
plot(pirms$d2H.irms, pirms$d2H.oc, type = "n", 
     xlab = expression(delta^2*"H"["IRMS"]*" (VSMOW)"), 
     ylab = expression(delta^2*"H"["CRDS"]*" (VSMOW)"))
abline(0, 1, lwd = 2)
points(pirms$d2H.irms, pirms$d2H, pch = 21, bg = plant, col = "grey90")
points(pirms$d2H.irms, pirms$d2H.oc, pch = 21, bg = plant, cex = 1.75, lwd = 2)

bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.1 * diff(bds[3:4]),
     paste0("RSD(raw) = ", round(sd(pirms$d2H - pirms$d2H.irms), 1), "\u2030"), 
     adj = c(1, 0))
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.04 * diff(bds[3:4]),
     paste0("RSD(corrected) = ", 
            round(sd(pirms$d2H.oc - pirms$d2H.irms), 1), 
            "\u2030"), 
     adj = c(1, 0))
text(bds[1] + 0.05 * diff(bds[1:2]), bds[4] - 0.05 * diff(bds[3:4]), "C")

## Soil d18O
par(mar = c(5, 5, 1, 1))
plot(sirms$d18O.irms, sirms$d18O, type = "n", xlim = c(-9.5, -3),
     xlab = expression(delta^18*"O"["IRMS"]*" (VSMOW)"), 
     ylab = expression(delta^18*"O"["CRDS"]*" (VSMOW)"))
abline(0, 1, lwd = 2)
points(sirms$d18O.irms, sirms$d18O, pch = 21, bg = soil, col = "grey90")
points(sirms$d18O.irms, sirms$d18O.oc, pch = 21, bg = soil, cex = 1.75, lwd = 2)
bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.1 * diff(bds[3:4]),
     paste0("RSD(raw) = ", round(sd(sirms$d18O - sirms$d18O.irms), 1), "\u2030"), 
     adj = c(1, 0))
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.04 * diff(bds[3:4]),
     paste0("RSD(corrected) = ", 
            round(sd(sirms$d18O.oc - sirms$d18O.irms), 1), 
            "\u2030"), 
     adj = c(1, 0))
text(bds[1] + 0.05 * diff(bds[1:2]), bds[4] - 0.05 * diff(bds[3:4]), "B")

## Plant d18O
plot(pirms$d18O.irms, pirms$d18O, type = "n", ylim = c(-12, 10),
     xlab = expression(delta^18*"O"["IRMS"]*" (VSMOW)"), 
     ylab = expression(delta^18*"O"["CRDS"]*" (VSMOW)"))
abline(0, 1, lwd = 2)
points(pirms$d18O.irms, pirms$d18O, pch = 21, bg = plant, col = "grey90")
points(pirms$d18O.irms, pirms$d18O.oc, pch = 21, bg = plant, cex = 1.75, lwd = 2)
bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.1 * diff(bds[3:4]),
     paste0("RSD(raw) = ", round(sd(pirms$d18O - pirms$d18O.irms), 1), "\u2030"), 
     adj = c(1, 0))
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.04 * diff(bds[3:4]),
     paste0("RSD(corrected) = ", 
            round(sd(pirms$d18O.oc - pirms$d18O.irms), 1), 
            "\u2030"), 
     adj = c(1, 0))
text(bds[1] + 0.05 * diff(bds[1:2]), bds[4] - 0.05 * diff(bds[3:4]), "D")
legend("topright", legend = c("Raw", "Corrected", "Soil", "Xylem"), 
       pch = 21, pt.bg = c("grey50", "grey50", soil, plant), 
       col = c("grey90", "black", "white", "white"), 
       pt.cex = c(1, 1.75, 1.35, 1.35), 
       pt.lwd = c(1, 2, 1, 1), bty = "n", ncol = 2)

dev.off()

# Figure 2 ----
cf = colorRampPalette(c(good, bad))
cols = cf(9)

ci = (sqrt(airms$d18O.off + 0.01 - min(airms$d18O.off))) / 
  (sqrt(diff(range(airms$d18O.off)) + 0.01))
ci = ceiling(ci * 9)

png("out/Figure2.png", width = 9, height = 3, units = "in", res = 600)
layout(matrix(1:3, nrow = 1))
par(mar = c(5, 5, 1, 1))

## d2H - 
plot(airms$SlopeShift * airms$CH4, airms$d2H - airms$d2H.irms, 
     xlab = expression(Delta*"Slope Shift " %*% " "*Delta*"CH"[4]), 
     ylab = expression(delta^2*"H"["CRDS - IRMS"]))
abline(lmH, lwd = 2)
points(airms$SlopeShift * airms$CH4, airms$d2H - airms$d2H.irms, pch = 21,
       bg = "grey50", cex = 2, lwd = 2)

bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]), bds[4] - 0.05 * diff(bds[3:4]), "A")

## d18O - Residuals - CH4
plot(airms$CH4, airms$Residuals, xlab = expression(Delta*"CH"[4]),
     ylab = expression(Delta*"Residuals"))

ch4 = seq(-12, 2)
offset = seq(0, 18, by = 3)
cci = (sqrt(offset + 0.01 - min(airms$d18O.off))) / 
  (sqrt(diff(range(airms$d18O.off)) + 0.01))
cci = ceiling(cci * 9)

for(i in seq_along(offset)){
  res = (offset[i] - ch4 * lmO$coefficients["CH4"]) / 
    lmO$coefficients["Residuals"]
  lines(ch4, res, lty = 3, lwd = 2, col = cols[cci[i]])
}

points(airms$CH4, airms$Residuals, pch = 21, cex = 2, lwd = 2, bg = cols[ci])

bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]), bds[4] - 0.05 * diff(bds[3:4]), "B")

## Legend
xmin = 0.05
xmax = 0.12
ymin = 0.05
ymax = 0.3

rect(bds[1], bds[3], bds[1] + (xmax + 0.16) * diff(bds[1:2]),
     bds[3] + (ymax + 0.06) * diff(bds[3:4]), col = rgb(1, 1, 1, 0.75),
     border = 1)

rasterImage(rev(cols), bds[1] + xmin * diff(bds[1:2]), 
            bds[3] + ymin * diff(bds[3:4]), bds[1] + xmax * diff(bds[1:2]), 
            bds[3] + ymax * diff(bds[3:4]), interpolate = FALSE)
rect(bds[1] + xmin * diff(bds[1:2]), bds[3] + ymin * diff(bds[3:4]),
     bds[1] + xmax * diff(bds[1:2]), bds[3] + ymax * diff(bds[3:4]))

labs = c(0, 3, 9, 18)
lx = bds[1] + (xmax + 0.035) * diff(bds[1:2])
ly = (sqrt(labs + 0.01 - min(airms$d18O.off))) / 
  (sqrt(diff(range(airms$d18O.off)) + 0.01))
ly = bds[3] + (ymin + (ymax - ymin) * ly) * diff(bds[3:4])
for(i in seq_along(ly)){
  text(lx, ly[i], labs[i])
}
text(bds[1] + (xmax + 0.09) * diff(bds[1:2]), 
     bds[3] + mean(c(ymin, ymax)) * diff(bds[3:4]),
     expression(delta^18*"O"["CRDS - IRMS"]), srt = 90)

box()

## d18O - BaseShift - CH4
plot(airms$CH4, airms$BaseShift, xlab = expression(Delta*"CH"[4]),
     ylab = expression(Delta*"Baseline Shift"))

for(i in seq_along(offset)){
  bs = (offset[i] - ch4 * lmO$coefficients["CH4"]) / 
    lmO$coefficients["BaseShift"]
  lines(ch4, bs, lty = 3, lwd = 2, col = cols[cci[i]])
}

points(airms$CH4, airms$BaseShift, pch = 21, cex = 2, lwd = 2, bg = cols[ci])

bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]), bds[4] - 0.05 * diff(bds[3:4]), "C")

box()

dev.off()

# Figure 3 ----
png("out/Figure3.png", 9.2, 5, units = "in", res = 600)
layout(matrix(1:2, nrow = 1), widths = c(lcm(5 * 2.54), lcm(4.2 * 2.54)))

## Raw
par(mai = c(1, 1, 0.2, 0.2))
xlim = range(c(p$d18O, s$d18O, p$d18O.oc, s$d18O.oc))
ylim = range(c(p$d2H, s$d2H, p$d2H.oc, s$d2H.oc))

plot(p$d18O, p$d2H, xlim = xlim, ylim = ylim, pch = 20, cex = 0.5,
     xlab = expression(delta^{18}*"O (VSMOW)"),
     ylab = expression(delta^2*"H (VSMOW)"))
abline(10, 8, lwd = 2)
points(s$d18O, s$d2H, pch = 21, bg = soil)
points(p$d18O, p$d2H, pch = 21, bg = plant)
bds = par("usr")
text(bds[1] + 0.05 * diff(bds[1:2]),
     bds[4] - 0.05 * diff(bds[3:4]), "A")
legend("bottomright", legend = c("Soil", "Xylem"), pch = 21, 
       pt.bg = c(soil, plant), bty = "n")

## Corrected
par(mai = c(1, 0.2, 0.2, 0.2))

plot(p$d18O.oc, p$d2H.oc, xlim = xlim, ylim = ylim, pch = 20, cex = 0.5,
     xlab = expression(delta^{18}*"O (VSMOW)"), axes = FALSE)
axis(1)
axis(2, labels = FALSE)
box()
abline(10, 8, lwd = 2)
points(s$d18O.oc, s$d2H.oc, pch = 21, bg = soil)
points(p$d18O.oc, p$d2H.oc, pch = 21, bg = plant)
bds = par("usr")
text(bds[1] + 0.05 * diff(bds[1:2]),
     bds[4] - 0.05 * diff(bds[3:4]), "B")

dev.off()
