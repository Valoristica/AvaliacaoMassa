# METANÁLISE

library(wooldridge)

data(hprice1)

hprice1$PU <- hprice1$price/hprice1$lotsize

fitHprice1 <- lm(log(PU) ~ log(lotsize) + log(sqrft), data = hprice1,
                 subset = -c(47, 77, 81))

summary(fitHprice1) # beta_1 = -0,77; beta_2 = 0,66; R2adj = 0,73

mean(hprice1$lotsize[-c(47, 77, 81)]) # 8.185

data(hprice3)

hprice3$PU <- hprice3$price/hprice3$land

fitHprice3 <- lm(log(PU) ~ log(land) + log(area), data = hprice3, subset = -72)

summary(fitHprice3) # beta_1 = -0,85; beta_2 = 0,70; R2adj = 0,79

mean(hprice3$land[-72]) # 38.052
median(hprice3$land[-72]) 43560

library(modeldata)

data("ames")

ames <- within(ames, PU <- Sale_Price/Lot_Area)

fitAmes <- lm(log(PU) ~ log(Lot_Area) + log(Gr_Liv_Area), data = ames,
              subset = -c(957, 1571, 2072, 2116))

summary(fitAmes) # beta_1 = -0,905; beta_2 = 0,853; R2adj = 0,71

mean(ames$Lot_Area[-c(957, 1571, 2072, 2116)]) # 9.938
median(ames$Lot_Area[-c(957, 1571, 2072, 2116)]) # 9.429

library(mosaicData)

data("SaratogaHouses")

SaratogaHouses <- within(SaratogaHouses, {
  lotSize <- 43560*lotSize
  PU <- price/lotSize
  
})

fitSaratoga <- lm(log(PU) ~ log(lotSize) + log(livingArea), 
                  data = SaratogaHouses, subset = lotSize > 0)

summary(fitSaratoga) # beta_1 = -0,975; beta_2 = 0,87; R2 = 0,8764

mean(SaratogaHouses$lotSize) # 21.790

library(openintro)

data("duke_forest")

duke_forest <- within(duke_forest,{
  lot <- 43560*lot
  PU <- price/lot
})


fitDuke <- lm(log(PU) ~ log(lot) + log(area), data = duke_forest, 
              subset = -c(65, 70))

summary(fitDuke) # beta1 = -0,67; beta2 = 0,69

mean(duke_forest$lot[-c(65, 70)], na.rm = T) # 24.994

library(appraiseR)

data("homePrices")

homePrices <- within(homePrices,{
  SalePrice <- 1000*SalePrice
  SqFeet <- 1000*SqFeet
  Lot <- 1000*Lot
  PU <- SalePrice/Lot
})

fithomePrices <- lm(log(PU) ~ log(Lot) + log(SqFeet), data = homePrices)

summary(fithomePrices) # beta1 = -0,89405; beta2 = 1,22; R2adj = 0,81

mean(homePrices$Lot) # 24345

library(Stat2Data)

data("HousesNY")

HousesNY <- within(HousesNY, {
  Lot <- 43560*Lot
  Size <- 1000*Size
  PU <- Price/Lot
})

fitNY <- lm(log(PU) ~ log(Lot) + log(Size), data = HousesNY[-c(34, 39), ])

summary(fitNY) # beta1 = -0,915; beta2 = 0.58. R2 = 0,885

mean(HousesNY$Lot[-c(34, 39)]) # 34.933

data("GrinnellHouses")

GrinnellHouses <- within(GrinnellHouses, {
  LotSize <- 43560*LotSize
  PU <- SalePrice/LotSize
})

fitGrinnel <- lm(log(PU) ~ log(LotSize) + log(SquareFeet), 
                 data = GrinnellHouses, subset = LotSize < 1000000)

summary(fitGrinnel) # beta1 = -0,84; beta2 = 0,83

mean(GrinnellHouses$LotSize[GrinnellHouses$LotSize < 10^6], na.rm = T) # 28.319

dados <- data.frame(row.names = c("hprice1", "hprice3", "ames", "Saratoga", 
                                "duke_forest", "homePrices", "HousesNY", 
                                "Grinnel"),
                    beta1 = c(-.8315, -.85, -.9048, -.975, -.62, -.894, -.94, -.84), 
                    beta2 = c(.7624, .713, 0.853, .87, .594, 1.22, .595, .83),
                    Am = c(9020, 39630, 10148, 21790, 24789, 24345, 34782, 31514))

fit <- lm(beta1 ~ Am, data = dados, subset = -5)
summary(fit)

plot(beta1 ~ Am, data = dados, subset = -5)
abline(fit, col = "red")

plot(beta1 ~ log(beta2), data = dados)


