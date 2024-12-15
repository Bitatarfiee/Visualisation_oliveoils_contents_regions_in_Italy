
library(tidyr)
library(dplyr)
library(ggplot2)

# Part 1
Olive_data <- read.csv("olive.csv", row.names=1)
#colnames(Olive_data)

p=ggplot(data = Olive_data)+ aes(x = oleic , y =  palmitic, color = linoleic) +
  geom_point() +
  labs(x = "Oleic", y = "Palmitic", title = "Scatterplot: Palmitic vs. Oleic (Colored by Linoleic)")
p


# Dividing Linoleic variable into fours classes
Olive_data$Linoleic_class <- cut_interval(Olive_data$linoleic, n = 4)
#Olive_data$Linoleic_class <- as.numeric(Olive_data$Linoleic_class)

ggplot(data = Olive_data)+ aes(x = oleic , y =  palmitic, color =Linoleic_class ) +
  geom_point() +
  labs(x = "Oleic", y = "Palmitic", title = "Scatterplot: Palmitic vs. Oleic (Colored by Discretized Linoleic)")


# Part 2 
# a: Create a scatterplot with discretized Linoleic mapped to color
ggplot(data =Olive_data, aes(x = oleic ,y =  palmitic, color = Linoleic_class)) +
  geom_point() +
  labs(x = "Oleic", y = "Palmitic", title = "Scatterplot: Palmitic vs. Oleic (Colored by Discretized Linoleic)")


# b: Create a scatterplot with discretized Linoleic mapped to size
ggplot(data =Olive_data, aes(x = oleic ,y =  palmitic, size = as.numeric(Linoleic_class)))+
  geom_point() +
  labs(x = "Oleic", y = "Palmitic", title = "Scatterplot: Palmitic vs. Oleic (Sized by Discretized Linoleic)")



# c: Create a scatterplot with discretized Linoleic mapped to orientation angle
ggplot(data =Olive_data, aes(x = oleic ,y =  palmitic, angle =as.numeric( Linoleic_class)))+
  geom_spoke(aes(radius = 0.1), size = 5) +
  labs(x = "Oleic", y = "Palmitic", title = "Scatterplot: Palmitic vs. Oleic (Orientation by Discretized Linoleic)")

# Part 3
# Create a scatterplot of Oleic vs Eicosenoic in which color is defined by numeric values of Region.

p1 = ggplot(data = Olive_data, aes(x = oleic , y =  eicosenoic, color = as.numeric(Region))) +
  geom_point() +
  labs(x = "Oleic", y = "Eicosenoic", title = "Scatterplot: Eicosenoic vs. Oleic (Colored by Region)")
p1


#scatterplot of Oleic vs Eicosenoic in which color is defined by categorical values of Region.

p2 = ggplot(data = Olive_data, aes(x = oleic , y =  eicosenoic, color = as.factor(Region))) +
  geom_point() +
  labs(x = "Oleic", y = "Eicosenoic", title = "Scatterplot: Eicosenoic vs. Oleic (Colored by Region)")
p2


# Part 4
Olive_data$Linoleic_class <- cut_interval(Olive_data$linoleic, n = 3)
Olive_data$Linoleic_class <- as.numeric(Olive_data$Linoleic_class)


Olive_data$Palmitic_class <- cut_interval(Olive_data$palmitic, n = 3)
Olive_data$Palmitic_class <- as.factor(Olive_data$Palmitic_class)


Olive_data$Palmitoleic_class <- cut_interval(Olive_data$palmitoleic, n = 3)
Olive_data$Palmitoleic_class <- as.numeric(Olive_data$Palmitoleic_class)


ggplot(data=Olive_data, aes(x = oleic ,y =  eicosenoic, color = Linoleic_class, shape = Palmitic_class,
                            size = Palmitoleic_class)) +
  geom_point() +
  labs(x = "Oleic", y = "Palmitic", title = "Scatterplot: Palmitic vs. Oleic (Colored by Discretized Linoleic)")


#Part5

ggplot(data =Olive_data, aes(x = oleic , y =  eicosenoic, color=factor(Region), 
                             shape=factor(Palmitic_class), size=Palmitoleic_class)) +
  geom_point() +
  labs(x = "Oleic", y = "Palmitic", title = "Scatterplot: Eicosenoic vs. Oleic (Colored by Region)")


# Part 6
library(plotly)
c1 <- Olive_data %>% group_by(Area) %>% summarise(nArea = n())
c1$nArea <- c1$nArea / sum(c1$nArea)*100
p3 <- plot_ly(c1, labels =~Area, values =~nArea, textinfo=~"none")%>%
  add_pie

p3


# Part 7
library(ggplot2)
p4 <- ggplot(data = Olive_data, aes(x = eicosenoic , y =  linolenic)) +
  geom_density_2d_filled() +
  labs(x = "Eicosenoic", y = "Linolenic", title = "2d-density contour plot: Linolenic vs. Eicosenoic")
p4


# Scatter plot using the  dependence of Linoleic vs Eicosenoic 
library(ggplot2)
p5 <- ggplot(data = Olive_data, aes(x = eicosenoic , y =  linolenic)) +
  geom_point(color = "darkblue") +
  labs(x = "Eicosenoic", y = "Linolenic", title = "scatterplot: Linolenic vs. Eicosenoic")
p5