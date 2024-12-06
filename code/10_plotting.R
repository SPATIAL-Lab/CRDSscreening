# Read data
p = read.csv("out/plants.csv")
s = read.csv("out/soils.csv")
pirms = read.csv("out/pirms.csv")
sirms = read.csv("out/sirms.csv")
load("out/ld.rda")

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
points(s$d18O, s$d2H, pch = 21, bg = "salmon2")
points(p$d18O, p$d2H, pch = 21, bg = "seagreen")
bds = par("usr")
text(bds[1] + 0.05 * diff(bds[1:2]),
     bds[4] - 0.05 * diff(bds[3:4]), "A")

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
points(s$d18O.oc, s$d2H, pch = 21, bg = "salmon2")
points(p$d18O.oc, p$d2H, pch = 21, bg = "seagreen")
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
text(0, 0, expression(bold("Soil")), cex = 1.5, srt = 90, 
     col = "salmon2")

## Plant label
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
text(0, 0, expression(bold("Xylem")), cex = 1.5, srt = 90, 
     col = "seagreen")

## d2H label
par(mar = c(0, 5, 0, 1))
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
text(0, 0, expression(bold(delta^2*"H (VSMOW)")), cex = 1.5)

## Soil d2H
par(mar = c(5, 5, 1, 1))
plot(sirms$d2H.irms, sirms$d2H, type = "n", xlab = "IRMS", ylab = "CRDS", 
     cex.lab = 1.5)
abline(0, 1, lwd = 2)
points(sirms$d2H.irms, sirms$d2H, pch = 21, bg = "salmon2", col = "grey90",
       cex = 1.5)
points(sirms$d2H.irms[sirms$Good], sirms$d2H[sirms$Good], pch = 21, 
       bg = "salmon2", cex = 1.5)
bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.05 * diff(bds[3:4]),
     paste0("RSD = ", round(sd(sirms$d2H - sirms$d2H.irms), 1), "\u2030"), 
     adj = c(1, 0))
text(bds[1] + 0.05 * diff(bds[1:2]), bds[4] - 0.05 * diff(bds[3:4]), "A")

## Plant d2H
plot(pirms$d2H.irms, pirms$d2H, type = "n", xlab = "IRMS", ylab = "CRDS", 
     cex.lab = 1.5)
abline(0, 1, lwd = 2)
points(pirms$d2H.irms, pirms$d2H, pch = 21, bg = "seagreen", col = "grey90",
       cex = 1.5)
points(pirms$d2H.irms[pirms$Good], pirms$d2H[pirms$Good], pch = 21, 
       bg = "seagreen", cex = 1.5)
bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.1 * diff(bds[3:4]),
     paste0("RSD = ", round(sd(pirms$d2H - pirms$d2H.irms), 1), "\u2030"), 
     adj = c(1, 0))
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.05 * diff(bds[3:4]),
     paste0("RSD(screened) = ", 
            round(sd(pirms$d2H[pirms$Good] - pirms$d2H.irms[pirms$Good]), 1), 
            "\u2030"), 
     adj = c(1, 0))
text(bds[1] + 0.05 * diff(bds[1:2]), bds[4] - 0.05 * diff(bds[3:4]), "B")

## d18O label
par(mar = c(0, 5, 0, 1))
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
text(0, 0, expression(bold(delta^{18}*"O (VSMOW)")), cex = 1.5)

## Soil d18O
par(mar = c(5, 5, 1, 1))
plot(sirms$d18O.irms, sirms$d18O, type = "n", xlim = c(-9.5, -3),
     xlab = "IRMS", ylab = "CRDS", cex.lab = 1.5)
abline(0, 1, lwd = 2)
points(sirms$d18O.irms, sirms$d18O, pch = 21, bg = "salmon2", col = "grey90",
       cex = 1.5)
points(sirms$d18O.irms[sirms$Good], sirms$d18O[sirms$Good], pch = 21, 
       bg = "salmon2", cex = 1.5)
bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.05 * diff(bds[3:4]),
     paste0("RSD = ", round(sd(sirms$d18O - sirms$d18O.irms), 1), "\u2030"), 
     adj = c(1, 0))
text(bds[1] + 0.05 * diff(bds[1:2]), bds[4] - 0.05 * diff(bds[3:4]), "C")

## Plant d18O
plot(pirms$d18O.irms, pirms$d18O, type = "n", xlab = "IRMS", ylab = "CRDS", 
     ylim = c(-10, 10), cex.lab = 1.5)
abline(0, 1, lwd = 2)
points(pirms$d18O.irms, pirms$d18O, pch = 21, bg = "seagreen", col = "grey90",
       cex = 1.5)
points(pirms$d18O.irms[pirms$Good], pirms$d18O[pirms$Good], pch = 21, 
       bg = "seagreen", cex = 1.5)
bds = par("usr")
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.1 * diff(bds[3:4]),
     paste0("RSD = ", round(sd(pirms$d18O - pirms$d18O.irms), 1), "\u2030"), 
     adj = c(1, 0))
text(bds[2] - 0.05 * diff(bds[1:2]),
     bds[3] + 0.05 * diff(bds[3:4]),
     paste0("RSD(screened) = ", 
            round(sd(pirms$d18O[pirms$Good] - pirms$d18O.irms[pirms$Good]), 1), 
            "\u2030"), 
     adj = c(1, 0))
text(bds[1] + 0.05 * diff(bds[1:2]), bds[4] - 0.05 * diff(bds[3:4]), "D")

dev.off()

predict(ld)
