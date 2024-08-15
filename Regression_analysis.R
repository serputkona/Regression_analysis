getwd()
Suicidal_Behaviours  <- read.csv("GHSH_Pooled_Data1.csv")
marijuana <-Suicidal_Behaviours$Use_Marijuana
alcohol <- Suicidal_Behaviours$Currently_Drink_Alcohol

#1
plot(alcohol ~ marijuana  , pch = 20, col ='darkslategray3',
     main = "Залежність вживання алкоголю від куріння наркотиків")

#2
model <- lm(alcohol ~ marijuana)
model
summary(model)

#3
plot(alcohol ~ marijuana, pch = 20, col = "cyan3",
     main = "Залежність вживання алкоголю від куріння наркотиків")
abline(model, col = "cyan4")

ggplot(Suicidal_Behaviours, aes(Use_Marijuana, Currently_Drink_Alcohol)) +
  geom_point() +
  stat_smooth(method = lm)

#4
# Діаграма "відгук-прогноз"
plot(alcohol ~ fitted(model), xlab = "Прогнозовані моделлю значення", 
     ylab = "Значення спостережень", main = "Діаграма \"відгук-прогноз\"", col="darksalmon")
abline(0, 1, col="darkred")

#5
#діаграма "прогноз-залишки"
plot(resid(model) ~ fitted(model), xlab = "Прогнозовані моделлю значення", 
     ylab = "Залишки моделі",col='darkorchid4', main = "Діаграма \"прогноз-залишки\" ")
abline(h = 0)

#діаграму «відгук-залишки»
plot(resid(model) ~ alcohol, xlab = "Значення спостережень",
     ylab = "Залишки моделі",col='darkslateblue', main = "Діаграма \"відгук-залишки\" ")
abline(h = 0)

#6
plot(c(1:length(alcohol)), resid(model),
     xlab = "Порядковий номер", ylab = "Значення залишків",
     main = "Значення залишків за порядковим номером",
     pch = 20, col = "plum")
abline(h = 0)

#7
qqnorm(resid(model), main = "Q-Q-діаграма для залишків моделі",
       xlab = "Теоретичні квантилі", ylab = "Емпіричні квантилі",
       pch = 20, col = "palevioletred")
qqline(resid(model), col = "paleturquoise4")

#8

