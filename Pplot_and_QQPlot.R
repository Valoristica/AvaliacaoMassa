library(lattice)
library(latticeExtra)
library(wooldridge)
library(appraiseR)

#df <- read.table("https://online.stat.psu.edu/onlinecourses/sites/stat501/files/data/shortleaf.txt",
#                 header = TRUE)

data("hprice1")

hprice1 <- within(hprice1, {
  lotsize <- lotsize/10.764 # convert sqrft to m2
  PU <- 1000*price/lotsize
  colonial <- factor(colonial, labels = c("no", "yes"))
})

# Exploratory analysis of PU (DV):

histogram(~PU, data = hprice1, subset = -c(47, 77))
Hist(~PU, data = hprice1[-c(47, 77), ], NormalOverlay = T)

densityplot(~PU,data=hprice1[-c(47, 77), ],
            panel=function(x,type,...){
                  panel.densityplot(x,...)
                  panel.histogram(x,col='transparent', breaks = NULL, ...)

                } )

# shows assimetry

xyplot(PU ~ lotsize, data = hprice1[-c(47, 77), ]) +
  layer(panel.ablineq(lm(y ~ x), digits = 2, r.squared = T,
                      col = "darkorange", lwd = 2,
                      rotate = TRUE, at = 0.7, offset = -1.5))

fit <- lm(PU ~ lotsize, data = hprice1[-c(47, 77), ])

## Probability Plot

library(e1071)
probplot(residuals(fit))

## QQ Plot

qqPlot(residuals(fit), envelope = FALSE)

library(ggplot2)
library(appraiseR)
df <- aug(fit)

ggplot(df, aes(sample = .Resid)) +
  stat_qq() +
  stat_qq_line(color = "red")

## Probability-Probability Plot

m <- mean(df$.Resid)
s <- sd(df$.Resid)
n <- nrow(df)
p <- (1:n)/n - 0.5/n

ggplot(df) +
  geom_point(aes(x = p,
                 y = sort(pnorm(.Resid, m, s)))) +
  geom_abline(slope = 1, intercept = 0, col = "red")

library(StatDA)
ppplot.das(df$.Resid, pdist = pnorm, xlab = NULL, ylab = "Probability",
           line = TRUE,
           lwd = 2, pch = 3, cex = 0.7, cex.lab = 1)
