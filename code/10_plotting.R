# Pull data
p = read.csv("out/plants.csv")
s = read.csv("out/soils.csv")




# CRDS vs IRMS comparisons
wh = 8
widths = sapply(wh * 2.54 * c(0.08, 0.46, 0.46), lcm)
png("out/IRMS.png", wh, wh, units = "in", res = 600)
layout(matrix(1:9, nrow = 3), widths = widths,
       heights = widths)

par(mar = rep(0, 4))
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")

par(mar = c(5, 0, 1, 0))
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
text(0, 0, expression(bold("Soil")), cex = 1.75, srt = 90, 
     col = "salmon2")

plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
text(0, 0, expression(bold("Xylem")), cex = 1.75, srt = 90, 
     col = "seagreen")

par(mar = c(0, 5, 0, 1))
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
text(0, 0, expression(bold(delta^2*"H (VSMOW)")), cex = 1.75)

par(mar = c(5, 5, 1, 1))
plot(s$d2H.irms, s$d2H, pch = 21, bg = "salmon2",
     xlab = "IRMS", ylab = "CRDS", cex.lab = 1.5)
abline(0, 1, lwd = 2)
points(s$d2H.irms, s$d2H, pch = 21, bg = "salmon2", cex = 1.5)
text(par("usr")[2] - 0.05 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.05 * diff(par("usr")[3:4]),
     paste0("RMSE = ", round(sqrt(mean(s.diff.h ^ 2)), 1), "\u2030"), 
     adj = c(1, 0))

plot(p$d2H.irms, p$d2H, pch = 21, bg = "seagreen",
     xlab = "IRMS", ylab = "CRDS", cex.lab = 1.5)
abline(0, 1)
points(p$d2H.irms, p$d2H, pch = 21, bg = "seagreen", cex = 1.5)
text(par("usr")[2] - 0.05 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.05 * diff(par("usr")[3:4]),
     paste0("RMSE = ", round(sqrt(mean(p.diff.h ^ 2)), 1), "\u2030"), 
     adj = c(1, 0))

par(mar = c(0, 5, 0, 1))
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
text(0, 0, expression(bold(delta^{18}*"O (VSMOW)")), cex = 1.75)

par(mar = c(5, 5, 1, 1))
plot(s$d18O.irms, s$d18O, pch = 21, bg = "salmon2", xlim = c(-9.5, -2),
     xlab = "IRMS", ylab = "CRDS", cex.lab = 1.5)
abline(0, 1)
points(s$d18O.irms, s$d18O, pch = 21, bg = "salmon2", cex = 1.5)
text(par("usr")[2] - 0.05 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.05 * diff(par("usr")[3:4]),
     paste0("RMSE = ", round(sqrt(mean(s.diff.o ^ 2)), 1), "\u2030"), 
     adj = c(1, 0))

plot(p$d18O.irms, p$d18O, pch = 21, bg = "seagreen",
     xlab = "IRMS", ylab = "CRDS", cex.lab = 1.5)
abline(0, 1)
points(p$d18O.irms, p$d18O, pch = 21, bg = "seagreen", cex = 1.5)
text(par("usr")[2] - 0.05 * diff(par("usr")[1:2]),
     par("usr")[3] + 0.05 * diff(par("usr")[3:4]),
     paste0("RMSE = ", round(sqrt(mean(p.diff.o ^ 2)), 1), "\u2030"), 
     adj = c(1, 0))

dev.off()
