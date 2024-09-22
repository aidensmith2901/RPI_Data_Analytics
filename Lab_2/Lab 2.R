epi.results = read.csv("C:/Users/aiden/OneDrive/Desktop/Data Analytics/Data/epi2024results06022024.csv") 
population_data = read.csv("C:/Users/aiden/OneDrive/Desktop/Data Analytics/Data/countries_populations_2023.csv")
attach(epi.results)
attach(population_data)

na.indices <- is.na(EPI.new) 
# sets na indices as the constraint for data
data <- data.frame(Country = country[!na.indices], EPI.new = EPI.new[!na.indices], EPI.old = EPI.old[!na.indices], ECO.old = ECO.old[!na.indices], ECO.new = ECO.new[!na.indices],BDH.old = BDH.old[!na.indices],BDH.new=BDH.new[!na.indices]) 
# creates new data for EPI new and EPI old with constraint

# HISTOGRAMS
eco_old_max <- ceiling(max(data$ECO.new) / 10) * 10
eco_density <- density(data$ECO.old)
max_density <- max(eco_density$y)+0.02
hist(ECO.old, prob=TRUE, xlim = c(20,eco_old_max), ylim=c(0, max_density))

# # COMPARING DISTRIBUTION EXERCISE 1
# # ECO.new
# eco.new_mean <- mean(ECO.new)
# plot(ecdf(rnorm(1000, eco.new_mean, 10)), do.points=FALSE, main="ECDF ECO.new")
# lines(ecdf(ECO.new))
# qqplot(rnorm(1000),ECO.new)
# qqline(ECO.new)
# 
# #ECO.new vs ECO.old
# plot(ecdf(ECO.old), do.points=FALSE, main="ECDF ECO.new vs ECO.old", lwd=5)
# lines(ecdf(ECO.new),col="red", lwd=1)
# qqplot(ECO.new,ECO.old)
# abline(0, 1, col="red", lwd=2)
# 
# # BDH.new
# plot(ecdf(rnorm(1000, mean(BDH.new), 10)), do.points=FALSE, main="ECDF BDH.new")
# lines(ecdf(BDH.new))
# qqnorm(BDH.new)
# qqline(BDH.new)
# 
# #ECO.new vs ECO.old
# plot(ecdf(BDH.new), do.points=FALSE, main="ECDF BDH.new vs BDH.old", lwd=5)
# lines(ecdf(BDH.old),col="red", lwd=1)
# qqplot(BDH.new,BDH.old)
# abline(0, 1, col="red", lwd=2)

# BOXPLOTS
boxplot(epi.results$ECO.new, epi.results$BDH.new, epi.results$EPI.new,
        main="Boxplot",
        names=c("ECO.new", "BDH.new", "EPI.new"),
        ylab="Values")

# QQ PLOTS
# EPI.new
lambda <- mean(epi.results$EPI.new, na.rm=TRUE)
qqplot(qpois(ppoints(length(epi.results$EPI.new)), lambda), 
       epi.results$EPI.new, 
       main="Q-Q Plot of EPI.new vs Poisson Distribution", 
       xlab="Theoretical Quantiles (Poisson)", 
       ylab="Sample Quantiles (EPI.new)",
       pch=16)
abline(0, 1, col="grey", lwd=2)

# ECO.new
lambda <- mean(epi.results$ECO.new, na.rm=TRUE)
qqplot(qpois(ppoints(length(epi.results$ECO.new)), lambda), 
       epi.results$ECO.new, 
       main="Q-Q Plot of ECO.new vs Poisson Distribution", 
       xlab="Theoretical Quantiles (Poisson)", 
       ylab="Sample Quantiles (ECO.new)",
       pch=16)
abline(0, 1, col="grey", lwd=2)

# BDH.new
lambda <- mean(epi.results$BDH.new, na.rm=TRUE)
qqplot(qpois(ppoints(length(epi.results$BDH.new)), lambda), 
       epi.results$BDH.new, 
       main="Q-Q Plot of BDH.new vs Poisson Distribution", 
       xlab="Theoretical Quantiles (Poisson)", 
       ylab="Sample Quantiles (BDH.new)",
       pch=16)
abline(0, 1, col="grey", lwd=2)

# ECDF
epi_ecdf <- ecdf(epi.results$EPI.new)
eco_ecdf <- ecdf(epi.results$ECO.new)
bdh_ecdf <- ecdf(epi.results$BDH.new)

# Plot the ECDF for EPI.new
plot(epi_ecdf, 
     main="ECDF Comparison of EPI.new, ECO.new, and BDH.new", 
     xlab="Value", 
     ylab="ECDF", 
     col="blue",
     lwd=1,  
     xlim=range(c(epi.results$EPI.new, epi.results$ECO.new, epi.results$BDH.new), na.rm=TRUE))
lines(eco_ecdf, col="red", lwd=1) 
lines(bdh_ecdf, col="green", lwd=1)
legend("bottomright", 
       legend=c("EPI.new", "ECO.new", "BDH.new"), 
       col=c("blue", "red", "green"), 
       lwd=2)

# EXERCISE 2
# drop countries not in epi results
populations <- population_data[-which(!population_data$Country %in% epi.results$country),]
# sort populations by country
populations <- populations[order(populations$Country),]
# drop countries not in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]
# sort epi results by country
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]
# only keep necessary columns
epi.results.sub <- epi.results.sub[,c("country","EPI.old","EPI.new","ECO.new","ECO.old","BDH.old","BDH.new")]
# convert population to numeric
epi.results.sub$population <- as.numeric(populations$Population)
# compute population log base 10
epi.results.sub$population_log <- log10(epi.results.sub$population)
attach(epi.results.sub)

lin.mod.epinew <- lm(EPI.new~population_log,epi.results.sub)
plot(EPI.new~population_log)
abline(lin.mod.epinew)
summary(lin.mod.epinew)
plot(lin.mod.epinew)
ggplot(epi.results.sub, aes(x = population_log, y = EPI.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

lin.mod.econew <- lm(ECO.new~population_log,epi.results.sub)
plot(ECO.new~population_log)
abline(lin.mod.econew)
summary(lin.mod.econew)
plot(lin.mod.econew)
ggplot(epi.results.sub, aes(x = population_log, y = ECO.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.econew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

lin.mod.bdhnew <- lm(BDH.new~population_log,epi.results.sub)
plot(BDH.new~population_log)
abline(lin.mod.bdhnew)
summary(lin.mod.bdhnew)
plot(lin.mod.bdhnew)
ggplot(epi.results.sub, aes(x = population_log, y = BDH.new)) +
  geom_point() +
  stat_smooth(method = "lm")
ggplot(lin.mod.bdhnew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')
