
# STATS EXPLORATION HERE!

# Testing for normality - by treatment
# In general, if you have less than 50 observations, you should use the Shapiro-Wilk test. 
# Otherwise, the Kolmogorov-Smirnov test is better.

# TDR, PAR, and exotic cover by treatment
model1 <- lm(soil_moisture + normalized_par + exotic_cover ~ treatment, data=surv_all)
e <- residuals(model1, type="pearson")
f <- fitted(model1)

# qq plot
qqnorm(e)
qqline(e)

qqnorm(f)
qqline(f)


# 
# 
# 
#checking for normality - by native cover

# TDR, PAR, and exotic cover by native percent cover
model2 <- lm(soil_moisture + normalized_par + exotic_cover ~ native_cover, data=surv_all)
g <- residuals(model2, type="pearson")
h <- fitted(model2)

# qq plot
qqnorm(g)
qqline(g)

qqnorm(h)
qqline(h)


# first code 1
ols_test_normality(model1)


shapiro.test(surv_all$native_cover)
shapiro.test(surv_all$exotic_cover)



hist(surv_all$native_cover, col='steelblue')

model1 <- lm(treatment ~ normalized_par, data=surv_all)






model <- lm( ~ disp + hp + wt + qsec, data = mtcars)

k.test(surv_all$exotic_cover)
pnorm
plot(surv_all$native_cover ~ surv_all$normalized_par)


#
#
#
# r first code 2 - checking for normality, include = FALSE
# Check for normality and heteroskedasticity <-- FROM AUSTEN
par(mfrow=c(2,2)) 
plot(model.presence.sh)
shapiro.test(model.presence.sh$residuals)
ggplot(data = NULL, aes(x = model.presence.sh$residuals)) + geom_density(aes(y=..scaled..))
car::leveneTest(sh.dat.samples.pa$sh, sh.dat.samples.pa$Quadrat.Identifier, center = median)
#https://easystats.github.io/performance/


#
#
#
# r first code 3, include=FALSE
#Initial! 
#Wrong!

# GRAPHDATA <- surv_all %>% 
#     group_by(treatment, month_count, species) %>%
#     summarise_at(vars(alive), funs(mean,sd))



m0 <- lm(alive ~ species + soil_moisture + normalized_par, data=surv_all)
m00 <- lm(alive ~ species + year + month, data=surv_all)
m1 <- lm(alive ~ soil_moisture + normalized_par, data=surv_all)
m3 <- lm(alive ~ cover_percent + soil_moisture + normalized_par + species, data=surv_all)
m4 <- lm(alive ~ treatment + soil_moisture + normalized_par + species, data=surv_all)

anova(m0)
anova(m1)
anova(m3)
anova(m5)

################## a) are the residuals normal? b) 
plot(m00) # viewing residuals... i think...
hist(m00$residuals, main = "Residual Historgram: m0") # histogram of residuals
ols_plot_resid_hist(m00)
boxplot(m00$residuals, main = "Residual Box Plot: m0") # box plot of residuals

# Testing for normality
# In general, if you have less than 50 observations, you should use the Shapiro-Wilk test. 
# Otherwise, the Kolmogorov-Smirnov test is better.
ols_test_normality(m00)


#
#
#
# r first code 4, include=FALSE

m_Envsitemafa <- lmer((Par, TDr, nn%) ~ MAFA treatment * site, data = surv_all)
m_Envsitepc <- lmer((Par, TDr, nn%) ~ percent cover * site, data = surv_all)

m_alive <- lmer(alive ~ myvars + year + month + site + random(1/plot), data = surv_all)


```{r tdrmod - additional stats, include=FALSE}
# DON'T USE THESE VALUE

# TDR by Site and Treatment
# tdrmod$site  <- as.factor(tdrmod$site) # to make site a factor
tdrmod_site_treat <- aov(soil_moisture ~ site*treatment, data = tdrmod) 
anova(tdrmod_site_treat)
TukeyHSD(tdrmod_site_treat)
plot(TukeyHSD(tdrmod_site_treat))
# 2:none-1:full -6.4912037 -12.944263 -0.03814456 0.0477765

# TDR by Site <-- why does this say site is sig when it wasn't before?
tdrmod_site <- aov(soil_moisture ~ site, data = tdrmod) 
anova(tdrmod_site)
# site        1  344.6  344.57  7.7869 0.006242 **

tdrmod_treat <- aov(soil_moisture ~ treatment, data = tdrmod) 
anova(tdrmod_treat)
# treatment   2  154.9  77.446  1.6663 0.1939

# For TDR and nn cover, the significant plots may explain results we see later, but the fact that there are 2-3 plots that are significantly different from each other doesn't tell us the sites or treatments are significantly different from each other. 
```
```{r nnmod - additional stats, include=TRUE}
# non-native cover by treatment
nnmod_treat <- aov(nncover ~ treatment, data = nnmod) 
anova(nnmod_treat)
TukeyHSD(nnmod_treat)
plot(TukeyHSD(nnmod_treat))

# non-native cover by treatment and site
nnmod_treat_site <- aov(nncover ~ treatment*site, data = nnmod) 
anova(nnmod_treat_site)
TukeyHSD(nnmod_treat_site)
plot(TukeyHSD(nnmod_treat_site))

# For TDR and nn cover, the significant plots may explain results we see later, but the fact that there are 2-3 plots that are significantly different from each other doesn't tell us the sites or treatments are significantly different from each other. 
```
```