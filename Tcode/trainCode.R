library(tidyverse)


data()

view(mpg)
?mpg
?mean


glimpse(mpg)



mpg_efficient <- filter(mpg, cty >= 20)
view(mpg_efficient)

mpg_ford <- filter(mpg, manufacturer == "ford")
view(mpg_ford)

mpg_metric <- mutate(mpg, cty_metric = 0.425144 * cty)
glimpse(mpg_metric)

mpg_metric <- mpg %>% 
  mutate(test = 0.425144 * cty)

view(mpg)

latest_mpg <- mpg %>% 
    group_by(class) %>% 
    mutate(test = mean(cty))
  
# Data summarise in output 
mpg %>% 
  group_by(class) %>%
  mutate(meanCty <- mean(cty), 
         medCty <- mean(hwy)) %>%
  summarise(meanCty,medCty)

mpg %>%
  group_by(class) %>%
  mutate(meanCty = mean(cty), 
         medCty = mean(hwy)) %>%
  summarise(meanCty, medCty)


# Data Viz with ggplot2
ggplot(mpg, aes(x=cty)) +
  geom_freqpoly() + 
  labs(x = "City mileage")

ggplot(mpg, aes(x=cty)) +
  geom_freqpoly() + 
  geom_histogram() + 
  labs(x = "City mileage")

ggplot(mpg, aes(x = cty,
                y = hwy)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(mpg, aes(x = cty,
                y = hwy,
                color = class)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  scale_color_brewer(palette = "Dark2")
  
  





