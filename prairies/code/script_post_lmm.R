```{r modeles}
LM <- 
  lm(data = pertes_encl_excl_long, perte_biomasse ~ Num_encl_excl
  )
lmm <- 
  lmer(data = pertes_encl_excl_long, perte_biomasse ~ Num_encl_excl + (1|annee)
  )

lmm2 <- 
  lmer(pertes ~ annee + (1 | Id_EE), 
       data = perte_biomasse_encl_excl,
       REML = FALSE)
summary(lmm2)

lmm3 <- 
  lmer(pertes ~ periode_pdc + (1 | Id_EE), 
       data = perte_biomasse_encl_excl_pdc,
       REML = FALSE)
summary(lmm3)



anova(LMM, LM)

#On garde LM

# Calcul des moyennes marginales estimées (EMMs) pour chaque site
emm <- emmeans(LM, ~ Num_encl_excl)

# Comparaisons deux à deux avec ajustement de Tukey
resultats_tukey <- pairs(emm, adjust = "tukey")

# Affichage des résultats
print(resultats_tukey)
resultats_tukey_df <- data.frame(resultats_tukey)

par(mfrow = c(2,2))
qqnorm(resid(LMM))
plot(LMM, resid(.) ~ fitted(.))
VarCorr(LMM)

```