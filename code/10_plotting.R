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
wh = 6
widths = sapply(wh * 2.54 * c(0.08, 0.46, 0.46), lcm)
png("out/Figure1.png", wh, wh, units = "in", res = 600)
layout(matrix(1:9, nrow = 3), widths = widths,
       heights = widths)

## Blank
par(mar = rep(0, 4))
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")

## Soil label
par(mar = c(5, 0, 1, 0))
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
text(0, 0, expression(bold("Soil")), cex = 1.25, srt = 90, 
     col = soil)

## Plant label
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
text(0, 0, expression(bold("Xylem")), cex = 1.25, srt = 90, 
     col = plant)

## d2H label
par(mar = c(0, 5, 0, 1))
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
text(0, 0, expression(bold(delta^2*"H (VSMOW)")), cex = 1.25)

## Soil d2H
par(mar = c(5, 5, 1, 1))
plot(sirms$d2H.irms, sirms$d2H, type = "n", xlab = "IRMS", ylab = "CRDS", 
     cex.lab = 1.25)
abline(0, 1, lwd = 2)
points(sirms$d2H.irms, sirms$d2H, pch = 21, bg = soil, col = "grey90",
       cex = 1.5)
points(sirms$d2H.irms, sirms$d2H.oc, pch = 21, bg = soil, cex = 1.5)
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
plot(pirms$d2H.irms, pirms$d2H.oc, type = "n", xlab = "IRMS", ylab = "CRDS", 
     cex.lab = 1.25)
abline(0, 1, lwd = 2)
points(pirms$d2H.irms, pirms$d2H, pch = 21, bg = plant, col = "grey90")
points(pirms$d2H.irms, pirms$d2H.oc, pch = 21, 
       bg = plant, cex = 1.5)
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

## d18O label
par(mar = c(0, 5, 0, 1))
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
text(0, 0, expression(bold(delta^{18}*"O (VSMOW)")), cex = 1.25)

## Soil d18O
par(mar = c(5, 5, 1, 1))
plot(sirms$d18O.irms, sirms$d18O, type = "n", xlim = c(-9.5, -3),
     xlab = "IRMS", ylab = "CRDS", cex.lab = 1.25)
abline(0, 1, lwd = 2)
points(sirms$d18O.irms, sirms$d18O, pch = 21, bg = soil, col = "grey90")
points(sirms$d18O.irms, sirms$d18O.oc, pch = 21, 
       bg = soil, cex = 1.75)
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
plot(pirms$d18O.irms, pirms$d18O, type = "n", xlab = "IRMS", ylab = "CRDS", 
     ylim = c(-12, 10), cex.lab = 1.25)
abline(0, 1, lwd = 2)
points(pirms$d18O.irms, pirms$d18O, pch = 21, bg = plant, col = "grey90")
points(pirms$d18O.irms, pirms$d18O.oc, pch = 21, bg = plant, cex = 1.75)
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
legend("topright", legend = c("Corrected", "Raw"), pch = 21, pt.bg = plant, 
       col = c( "black", "grey90"), pt.cex = c(1.75, 1), bty = "n")

dev.off()

# Figure 2 ----
## Split data
a.g = airms[airms$Good, ]
a.b = airms[!airms$Good, ]

png("out/Figure2.png", width = 4.5, height = 10, units = "in", res = 600)
layout(matrix(1:5, ncol = 1))
par(mar = c(5, 5, 1, 1))

## Baseline shift
d.g = density(a.g$BaseShift)
d.b = density(a.b$BaseShift)

plot(d.g, xlim = range(c(d.g$x, d.b$x)), ylim = range(c(d.g$y, d.b$y)),
     main = "", xlab = expression(Delta*" Baseline shift"), ylab = "",
     lwd = 3, col = good, zero.line = FALSE)
lines(d.b, lwd = 3, col = bad)
bds = par("usr")
text(bds[1] + 0.04 * diff(bds[1:2]), bds[4] - 0.1 * diff(bds[3:4]), "A")
lines(c(0, 1 / ld$scaling["BaseShift",]), rep(bds[3], 2), lwd = 8, 
      lend = 1, xpd = NA)

## Slope shift
d.g = density(a.g$SlopeShift)
d.b = density(a.b$SlopeShift)

plot(d.g, xlim = range(c(d.g$x, d.b$x)), ylim = range(c(d.g$y, d.b$y)),
     main = "", xlab = expression(Delta*" Slope shift"), ylab = "",
     lwd = 3, col = good, zero.line = FALSE)
lines(d.b, lwd = 3, col = bad)
bds = par("usr")
text(bds[1] + 0.04 * diff(bds[1:2]), bds[4] - 0.1 * diff(bds[3:4]), "B")
lines(c(0, 1 / ld$scaling["SlopeShift",]), rep(bds[3], 2), lwd = 8, 
      lend = 1, xpd = NA)

## Residuals
d.g = density(a.g$Residuals)
d.b = density(a.b$Residuals)

plot(d.g, xlim = range(c(d.g$x, d.b$x)), ylim = range(c(d.g$y, d.b$y)),
     main = "", xlab = expression(Delta*" Residuals"), ylab = "Density",
     lwd = 3, col = good, zero.line = FALSE)
lines(d.b, lwd = 3, col = bad)
bds = par("usr")
text(bds[1] + 0.04 * diff(bds[1:2]), bds[4] - 0.1 * diff(bds[3:4]), "C")
legend("topright", legend = c("Good", "Bad"), col = c(good, bad), lwd = 3,
       bty = "n")
lines(c(0, 1 / ld$scaling["Residuals",]), rep(bds[3], 2), lwd = 8, 
      lend = 1, xpd = NA)

## Baseline curvature
d.g = density(a.g$BaseCurve)
d.b = density(a.b$BaseCurve)

plot(d.g, xlim = range(c(d.g$x, d.b$x)), ylim = range(c(d.g$y, d.b$y)),
     main = "", xlab = expression(Delta*" Baseline curvature"), ylab = "",
     lwd = 3, col = good, zero.line = FALSE)
lines(d.b, lwd = 3, col = bad)
bds = par("usr")
text(bds[1] + 0.04 * diff(bds[1:2]), bds[4] - 0.1 * diff(bds[3:4]), "D")
lines(c(0, 1 / ld$scaling["BaseCurve",]), rep(bds[3], 2), lwd = 8, 
      lend = 1, xpd = NA)

## CH4
d.g = density(a.g$CH4)
d.b = density(a.b$CH4)

plot(d.g, xlim = range(c(d.g$x, d.b$x)), ylim = range(c(d.g$y, d.b$y)),
     main = "", xlab = expression(Delta*" Methane"), ylab = "",
     lwd = 3, col = good, zero.line = FALSE)
lines(d.b, lwd = 3, col = bad)
bds = par("usr")
text(bds[1] + 0.04 * diff(bds[1:2]), bds[4] - 0.1 * diff(bds[3:4]), "E")
lines(c(0, 1 / ld$scaling["CH4",]), rep(bds[3], 2), lwd = 8, 
      lend = 1, xpd = NA)

dev.off()

# Figure 3 ----
png("out/Figure3.png", 9.2, 4.7, units = "in", res = 600)
layout(matrix(1:2, nrow = 1))
cols = c(good, bad)
par(mar = c(5, 5, 1, 1))

plot(SlopeShift ~ BaseShift, airms, pch = 21, cex = 1.75, lwd = 2,
     bg = cols[match(airms$Good, c(TRUE, FALSE))],
     xlab = expression(Delta*" Baseline shift"),
     ylab = expression(Delta*" Slope shift"))
bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[4] - 0.05 * diff(bds[3:4]), "A")
legend("bottomleft", legend = c("Good", "Bad"), pch = 21,
       pt.bg = cols, bty = "n", pt.cex = 1.75, pt.lwd = 2)

plot(CH4 ~ BaseShift, airms, pch = 21, cex = 1.75, lwd = 2,
     bg = cols[match(airms$Good, c(TRUE, FALSE))],
     xlab = expression(Delta*" Baseline shift"),
     ylab = expression(Delta*" Methane"))
bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[4] - 0.05 * diff(bds[3:4]), "B")

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
legend("bottomright", legend = c("Xylem", "Soil"), pch = 21, 
       pt.bg = c(plant, soil), bty = "n")

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




good = "deepskyblue3"
bad = "darkorange"
cf = colorRampPalette(c(good, bad))
cols = cf(9)

ci = (sqrt(airms$d18O.off + 0.01 - min(airms$d18O.off))) / 
  (sqrt(diff(range(airms$d18O.off)) + 0.01))
ci = ceiling(ci * 9)

plot(airms$CH4, airms$Residuals, pch = 21, cex = 2, lwd = 2, bg = cols[ci])

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


plot(airms$CH4, airms$BaseShift, pch = 21, cex = 2, lwd = 2, bg = cols[ci])

for(i in seq_along(offset)){
  bs = (offset[i] - ch4 * lmO$coefficients["CH4"]) / 
    lmO$coefficients["BaseShift"]
  lines(ch4, bs, lty = 3, lwd = 2, col = cols[cci[i]])
}

points(airms$CH4, airms$BaseShift, pch = 21, cex = 2, lwd = 2, bg = cols[ci])

