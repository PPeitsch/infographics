library(tidyverse)
library(modelr)

# Miles-per-gallon
# displ: tamaño del motor
# hwy: rendimiento en autopista
ggplot(data=mpg) + geom_point(aes(x=displ, y=hwy))

# hwy = w0 * 1 + w1 * displ
mod <- lm(hwy ~ displ, data=mpg)

# Data grid
grid <- modelr::data_grid(data=mpg, displ) %>% 
  add_predictions(mod)

# Plot de las predicciones
ggplot(data=mpg) + geom_point(aes(x=displ, y=hwy)) +
  geom_line(data=grid, aes(x=displ, y=pred),
            color='red')

# Ver los residuos
# Version a mano (no nos gusta)
# mutate(mpg, resid=hwy - (35.697651 -3.530589 * displ)
mpg_res <- modelr::add_residuals(mpg, mod)

ggplot(data=mpg_res) + 
  geom_point(aes(x=displ, y=resid)) +
  geom_abline(intercept=0, slope=0, color='red')

# Usar R-base
plot(mod)

# Ver los coeficientes / tests estadísticos
# Residual standard error: 3.836 on 232 degrees of freedom
# Multiple R-squared:  0.5868,	Adjusted R-squared:  0.585 
summary(mod)
modelr::rmse(mod, mpg)

###
# Modelo multiple
###
mod2 <- lm(hwy ~ displ + I(displ^2), data=mpg)
# mod2 <- lm(hwy ~ poly(displ, 20), data=mpg)

plot(mod2)
summary(mod2)
# Residual standard error: 3.423 on 231 degrees of freedom
# Multiple R-squared:  0.6725,	Adjusted R-squared:  0.6696

# Data grid
grid <- modelr::data_grid(data=mpg, displ) %>% 
  add_predictions(mod2)

# Plot de las predicciones
ggplot(data=mpg) + geom_point(aes(x=displ, y=hwy)) +
  geom_line(data=grid, aes(x=displ, y=pred),
            color='red')


#### Class
#
ggplot(data=mpg) + geom_point(aes(x=displ, y=hwy,
                                  color=class))
ggplot(data=mpg_res) + 
  geom_point(aes(x=displ, y=resid, color=class)) +
  geom_abline(intercept=0, slope=0, color='red')

mod3 <- lm(hwy ~ displ + I(displ^2) + class, data=mpg)

summary(mod3)

# Data grid
grid <- modelr::data_grid(data=mpg, displ, class) %>%
  add_predictions(mod3)

# Plot de las predicciones
ggplot(data=mpg) + geom_point(aes(x=displ, y=hwy, color=class)) +
  geom_line(data=grid, 
            aes(x=displ, y=pred, color=class)) +
  facet_wrap(~class)

##
## Interacciones
##
mod4 <- lm(hwy ~ displ * class + 
             I(displ^2) * class - class, data=mpg)

summary(mod4)

# Data grid
grid <- modelr::data_grid(data=mpg, displ, class) %>%
  add_predictions(mod4)

# Plot de las predicciones
ggplot(data=mpg) + geom_point(aes(x=displ, y=hwy, color=class)) +
  geom_line(data=grid, 
            aes(x=displ, y=pred, color=class)) +
  facet_wrap(~class)
