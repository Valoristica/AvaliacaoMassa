## Exemplo

```{r}
#| label: fig-mod
#| fig-cap: "Modelo sem transformações."
mod <- lm(PU ~ AP + Q + S + G + PADRAO, data = aptosBebedouro)
p <- plotModel(mod, residuals = T)
p
# p[["plots"]][[1]] + p[["plots"]][[2]] + p[["plots"]][[3]] + p[["plots"]][[4]] +
#   p[["plots"]][[5]] + plot_annotation(title = 'Gráficos do modelo',
#                             subtitle = "PU ~ AP + Q + S + G + PADRAO") 
```

- Nota-se uma não-linearidade presente na variável AP!
  
  ## Exemplo (cont.)
  
  ```{r}
#| label: fig-mod1
#| fig-cap: "Modelo com uma transformação."
mod1 <- lm(PU ~ rec(AP) + Q + S + G + PADRAO, data = aptosBebedouro)
p <- plotModel(mod1, residuals = T)
p
# p[["plots"]][[1]] + p[["plots"]][[2]] + p[["plots"]][[3]] + p[["plots"]][[4]] +
#   p[["plots"]][[5]] + plot_annotation(title = 'Gráficos do modelo',
#                             subtitle = "PU ~ 1/AP + Q + S + G + PADRAO") 
```

- Nota-se uma não-linearidade presente na variável Q!
  
  ## Exemplo (cont.)
  
  ```{r}
#| label: fig-mod2
#| fig-cap: "Modelo com duas transformações."
mod2 <- lm(PU ~ rec(AP) + sqr(Q) + S + G + PADRAO, data = aptosBebedouro)
p <- plotModel(mod2, residuals = T)
p
# p[["plots"]][[1]] + p[["plots"]][[2]] + p[["plots"]][[3]] + p[["plots"]][[4]] +
#   p[["plots"]][[5]] + plot_annotation(title = 'Gráficos do modelo',
#                             subtitle = "PU ~ 1/AP + Q^2 + S + G + PADRAO") 
```

- Modelo ajustado!
  
  ## Seleção de variáveis
  
  - Com $R^2$:
  
  ```{r}
#| label: fig-regSubR2
#| fig-cap: "Seleção automática de variáveis com $R^2$."
library(leaps)
a <- regsubsets(PU ~ rec(AP) + sqr(Q) + S + G + PADRAO, data = aptosBebedouro)
plot(a, scale = "r2")
```


## Seleção de variáveis

- Com $R^2_{ajustado}$:
  
  ```{r}
#| label: fig-regSubAdjR2
#| fig-cap: "Seleção automática de variáveis com $R^2_{ajustado}$."
plot(a, scale = "adjr2")
```
