
# load data / packages / functions
#---------------------------------------------------------
setwd("C:/Users/crossfit_al1985/Documents/UCLA/213C - Applied Multivariate Analysis/2013-12 Project/written report/figures")
library(ggplot2)
library(grid)
source("C:/Users/crossfit_al1985/Documents/EDU/R - Source()/multiplot.txt")

std_m <- std_m[std_m$rank_id <= 44, ]
std_f <- std_f[std_f$rank_id < 44, ]

# Plot men's standardized scores
#---------------------------------------------------------
p1 <- ggplot(std_m, aes(x= pool)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "The Pool") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p2 <- ggplot(std_m, aes(x= row_1)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Row 1") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p3 <- ggplot(std_m, aes(x= row_2)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Row 2") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p4 <- ggplot(std_m, aes(x= burden_run)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Burden Run") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p5 <- ggplot(std_m, aes(x= zigzag_time)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Zig-Zag Sprint") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p6 <- ggplot(std_m, aes(x= legless)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Legless") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p7 <- ggplot(std_m, aes(x= naughty_nancy)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Naughty Nancy") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p8 <- ggplot(std_m, aes(x= c_and_j)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Clean & Jerk") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p9 <- ggplot(std_m, aes(x= X2007)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "2007") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p10 <- ggplot(std_m, aes(x= sprint_chipper)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Sprint Chipper") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p11 <- ggplot(std_m[c(1:30),], aes(x= cinco_1)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Cinco 1") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p12 <- ggplot(std_m[c(1:30),], aes(x= cinco_2)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Cinco 2") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

## compile
png(file= "./densities_men.png", width= 800, height= 1000, units= "px")
multiplot(p1,p4,p7,p10, p2,p5,p8,p11, p3,p6,p9,p12,
          cols= 3)
dev.off()

# Plot women's standardized scores
#---------------------------------------------------------
p1 <- ggplot(std_f, aes(x= pool)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "The Pool") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p2 <- ggplot(std_f, aes(x= row_1)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Row 1") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p3 <- ggplot(std_f, aes(x= row_2)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Row 2") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p4 <- ggplot(std_f, aes(x= burden_run)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Burden Run") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p5 <- ggplot(std_f, aes(x= zigzag_time)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Zig-Zag Sprint") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p6 <- ggplot(std_f, aes(x= legless)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Legless") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p7 <- ggplot(std_f, aes(x= naughty_nancy)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Naughty Nancy") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p8 <- ggplot(std_f, aes(x= c_and_j)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Clean & Jerk") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p9 <- ggplot(std_f, aes(x= X2007)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "2007") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p10 <- ggplot(std_f, aes(x= sprint_chipper)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Sprint Chipper") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p11 <- ggplot(std_f[c(1:30),], aes(x= cinco_1)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Cinco 1") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

p12 <- ggplot(std_f[c(1:30),], aes(x= cinco_2)) + 
  geom_histogram(aes(y= ..density..), binwidth= 0.5, colour= "black", fill= "white") +
  geom_density(alpha= 0.2, fill= "#FF6666") +
  labs(x= "Standardized Score", y= "Density", title= "Cinco 2") +
  theme(plot.title= element_text(size = rel(2)), axis.title= element_text(size= rel(1.5)))

## compile
png(file= "./densities_women.png", width= 800, height= 1000, units= "px")
multiplot(p1,p4,p7,p10, p2,p5,p8,p11, p3,p6,p9,p12,
          cols= 3)
dev.off()
