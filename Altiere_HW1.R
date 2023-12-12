data()

ggplot2::mpg
mpg_data <- ggplot2::mpg #mpg variable

ggplot(mpg_data) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class)) #Relationship between displ and hwy

ggplot(mpg_data) +
  geom_point(mapping = aes(x = cyl, y = hwy)) #Relationship between hwy and cyl
  
mpg_data %>%
  group_by(cyl) %>%
  summarise(count = n())#Display total number of 4-,5-,6-, and 8- cyl vehicles

mpg_data %>%
  filter(hwy == max(hwy)) %>%
  select(manufacturer, model, class, hwy) #Compute an display data of most fuel-efficient vehicles

mpg_data %>%
  filter(hwy == min(hwy)) %>%
  select(manufacturer, model, class, hwy) #Compute and display data of least fuel-efficient vehicles









  
  
  
  



