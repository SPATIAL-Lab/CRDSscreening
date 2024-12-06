# Read data
p = read.csv("out/plants.csv")
s = read.csv("out/soils.csv")
pirms = read.csv("out/pirms.csv")
sirms = read.csv("out/sirms.csv")
airms = read.csv("out/airms.csv")
load("out/ld.rda")

plant = rgb(51, 94, 17, maxColorValue = 255)
soil = rgb(88, 9, 79, maxColorValue = 255)
good = "deepskyblue3"
bad = "darkorange"

# Figure 1 ----
png("out/Figure1.png", 9.2, 5, units = "in", res = 600)
layout(matrix(1:2, nrow = 1), widths = c(lcm(5 * 2.54), lcm(4.2 * 2.54)))

## All, unscreened
par(mai = c(1, 1, 0.2, 0.2))
xlim = range(c(p$d18O, s$d18O))
ylim = range(c(p$d2H, s$d2H))

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

## All, screened and offset-corrected
p = p[p$Good,]
s = s[s$Good,]
par(mai = c(1, 0.2, 0.2, 0.2))
xlim = range(c(p$d18O.oc, s$d18O.oc), na.rm = TRUE)
ylim = range(c(p$d2H, s$d2H))

plot(p$d18O.oc, p$d2H, xlim = xlim, ylim = ylim, pch = 20, cex = 0.5,
     xlab = expression(delta^{18}*"O (VSMOW)"), axes = FALSE)
axis(1)
axis(2, labels = FALSE)
box()
abline(10, 8, lwd = 2)
points(s$d18O.oc, s$d2H, pch = 21, bg = soil)
points(p$d18O.oc, p$d2H, pch = 21, bg = plant)
bds = par("usr")
text(bds[1] + 0.05 * diff(bds[1:2]),
     bds[4] - 0.05 * diff(bds[3:4]), "B")
dev.off()

# Figure 2 ----
wh = 8
widths = sapply(wh * 2.54 * c(0.08, 0.46, 0.46), lcm)
png("out/Figure2.png", wh, wh, units = "in", res = 600)
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
points(sirms$d2H.irms[sirms$Good], sirms$d2H[sirms$Good], pch = 21, 
       bg = soil, cex = 1.5)
bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.05 * diff(bds[3:4]),
     paste0("RSD = ", round(sd(sirms$d2H - sirms$d2H.irms), 1), "\u2030"), 
     adj = c(1, 0))
text(bds[1] + 0.05 * diff(bds[1:2]), bds[4] - 0.05 * diff(bds[3:4]), "A")

## Plant d2H
plot(pirms$d2H.irms, pirms$d2H, type = "n", xlab = "IRMS", ylab = "CRDS", 
     cex.lab = 1.25)
abline(0, 1, lwd = 2)
points(pirms$d2H.irms, pirms$d2H, pch = 21, bg = plant, col = "grey90",
       cex = 1.5)
points(pirms$d2H.irms[pirms$Good], pirms$d2H[pirms$Good], pch = 21, 
       bg = plant, cex = 1.5)
bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.1 * diff(bds[3:4]),
     paste0("RSD = ", round(sd(pirms$d2H - pirms$d2H.irms), 1), "\u2030"), 
     adj = c(1, 0))
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.05 * diff(bds[3:4]),
     paste0("RSD(passed) = ", 
            round(sd(pirms$d2H[pirms$Good] - pirms$d2H.irms[pirms$Good]), 1), 
            "\u2030"), 
     adj = c(1, 0))
text(bds[1] + 0.05 * diff(bds[1:2]), bds[4] - 0.05 * diff(bds[3:4]), "B")

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
points(sirms$d18O.irms[sirms$Good], sirms$d18O[sirms$Good], pch = 21, 
       bg = soil, cex = 1.75)
bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.05 * diff(bds[3:4]),
     paste0("RSD = ", round(sd(sirms$d18O - sirms$d18O.irms), 1), "\u2030"), 
     adj = c(1, 0))
text(bds[1] + 0.05 * diff(bds[1:2]), bds[4] - 0.05 * diff(bds[3:4]), "C")

## Plant d18O
plot(pirms$d18O.irms, pirms$d18O, type = "n", xlab = "IRMS", ylab = "CRDS", 
     ylim = c(-10, 10), cex.lab = 1.25)
abline(0, 1, lwd = 2)
points(pirms$d18O.irms, pirms$d18O, pch = 21, bg = plant, col = "grey90")
points(pirms$d18O.irms[pirms$Good], pirms$d18O[pirms$Good], pch = 21, 
       bg = plant, cex = 1.75)
bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.1 * diff(bds[3:4]),
     paste0("RSD = ", round(sd(pirms$d18O - pirms$d18O.irms), 1), "\u2030"), 
     adj = c(1, 0))
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.05 * diff(bds[3:4]),
     paste0("RSD(passed) = ", 
            round(sd(pirms$d18O[pirms$Good] - pirms$d18O.irms[pirms$Good]), 1), 
            "\u2030"), 
     adj = c(1, 0))
text(bds[1] + 0.05 * diff(bds[1:2]), bds[4] - 0.05 * diff(bds[3:4]), "D")
legend("topright", legend = c("Pass", "Fail"), pch = 21, pt.bg = plant, 
       col = c( "black", "grey90"), pt.cex = c(1.75, 1), bty = "n")

dev.off()

# Figure 3 ----
## Split data
a.g = airms[airms$Good, ]
a.b = airms[!airms$Good, ]

png("out/Figure3.png", width = 4.5, height = 10, units = "in", res = 600)
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

## Slope shift
d.g = density(a.g$SlopeShift)
d.b = density(a.b$SlopeShift)

plot(d.g, xlim = range(c(d.g$x, d.b$x)), ylim = range(c(d.g$y, d.b$y)),
     main = "", xlab = expression(Delta*" Slope shift"), ylab = "",
     lwd = 3, col = good, zero.line = FALSE)
lines(d.b, lwd = 3, col = bad)
bds = par("usr")
text(bds[1] + 0.04 * diff(bds[1:2]), bds[4] - 0.1 * diff(bds[3:4]), "B")

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

## Baseline curvature
d.g = density(a.g$BaseCurve)
d.b = density(a.b$BaseCurve)

plot(d.g, xlim = range(c(d.g$x, d.b$x)), ylim = range(c(d.g$y, d.b$y)),
     main = "", xlab = expression(Delta*" Baseline curvature"), ylab = "",
     lwd = 3, col = good, zero.line = FALSE)
lines(d.b, lwd = 3, col = bad)
bds = par("usr")
text(bds[1] + 0.04 * diff(bds[1:2]), bds[4] - 0.1 * diff(bds[3:4]), "D")

## CH4
d.g = density(a.g$CH4)
d.b = density(a.b$CH4)

plot(d.g, xlim = range(c(d.g$x, d.b$x)), ylim = range(c(d.g$y, d.b$y)),
     main = "", xlab = expression(Delta*" Methane"), ylab = "",
     lwd = 3, col = good, zero.line = FALSE)
lines(d.b, lwd = 3, col = bad)
bds = par("usr")
text(bds[1] + 0.04 * diff(bds[1:2]), bds[4] - 0.1 * diff(bds[3:4]), "E")

dev.off()
