library(ggplot2)
library(grid)
library(gridExtra)
rm(list=ls())


data = read.csv('Mixed_model_data/base_data.csv')
data$X = NULL
summary(data)



grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

# Burkina Faso

plot_mort = ggplot(na.omit(data[data$admin0Name == 'Burkina Faso',])) + aes(x = Year, y = Mortality, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_wasted = ggplot(na.omit(data[data$admin0Name == 'Burkina Faso',])) + aes(x = Year, y = Wasted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_stunted = ggplot(na.omit(data[data$admin0Name == 'Burkina Faso',])) + aes(x = Year, y = Stunted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()

grid_arrange_shared_legend(plot_mort,plot_wasted,plot_stunted, nrow=2, top = textGrob("Mortality, Stunted and Wasted rates per country",gp=gpar(fontsize=20)))

#Generally speaking most areas are improving or maintaining, some specific ones are getting worse.

# Gambia

plot_mort = ggplot(na.omit(data[data$admin0Name == 'Gambia',])) + aes(x = Year, y = Mortality, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_wasted = ggplot(na.omit(data[data$admin0Name == 'Gambia',])) + aes(x = Year, y = Wasted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_stunted = ggplot(na.omit(data[data$admin0Name == 'Gambia',])) + aes(x = Year, y = Stunted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()

grid_arrange_shared_legend(plot_mort,plot_wasted,plot_stunted, nrow=2, top = textGrob("Mortality, Stunted and Wasted rates per country",gp=gpar(fontsize=20)))

#While mortality is overall going down, wasting is increasing in most areas. Stunting is a bit of maintaining and decreasing.

# Mauritania

plot_mort = ggplot(na.omit(data[data$admin0Name == 'Mauritania',])) + aes(x = Year, y = Mortality, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_wasted = ggplot(na.omit(data[data$admin0Name == 'Mauritania',])) + aes(x = Year, y = Wasted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_stunted = ggplot(na.omit(data[data$admin0Name == 'Mauritania',])) + aes(x = Year, y = Stunted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()

grid_arrange_shared_legend(plot_mort,plot_wasted,plot_stunted, nrow=2, top = textGrob("Mortality, Stunted and Wasted rates per country",gp=gpar(fontsize=20)))

#Stunting decreasing, wasting increasing slightly in some areas, however the worst area, Hodh El Gharbi is improving a lot

# Niger

plot_mort = ggplot(na.omit(data[data$admin0Name == 'Niger',])) + aes(x = Year, y = Mortality, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_wasted = ggplot(na.omit(data[data$admin0Name == 'Niger',])) + aes(x = Year, y = Wasted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_stunted = ggplot(na.omit(data[data$admin0Name == 'Niger',])) + aes(x = Year, y = Stunted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()

grid_arrange_shared_legend(plot_mort,plot_wasted,plot_stunted, nrow=2, top = textGrob("Mortality, Stunted and Wasted rates per country",gp=gpar(fontsize=20)))

#Generally decreasing except wasting in Diffa and stunting that is growing in some areas. Mortality decreasing everywhere.

# Nigeria

plot_mort = ggplot(na.omit(data[data$admin0Name == 'Nigeria',])) + aes(x = Year, y = Mortality, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_wasted = ggplot(na.omit(data[data$admin0Name == 'Nigeria',])) + aes(x = Year, y = Wasted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_stunted = ggplot(na.omit(data[data$admin0Name == 'Nigeria',])) + aes(x = Year, y = Stunted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()

grid_arrange_shared_legend(plot_mort,plot_wasted,plot_stunted, nrow=2, top = textGrob("Mortality, Stunted and Wasted rates per country",gp=gpar(fontsize=20)))

#Everything seems to be improving , except wasting in an specific area.

# Mali

plot_mort = ggplot(na.omit(data[data$admin0Name == 'Mali',])) + aes(x = Year, y = Mortality, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_wasted = ggplot(na.omit(data[data$admin0Name == 'Mali',])) + aes(x = Year, y = Wasted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_stunted = ggplot(na.omit(data[data$admin0Name == 'Mali',])) + aes(x = Year, y = Stunted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()

grid_arrange_shared_legend(plot_mort,plot_wasted,plot_stunted, nrow=2, top = textGrob("Mortality, Stunted and Wasted rates per country",gp=gpar(fontsize=20)))

#Everything improving (some areas faster than others)

# Chad

plot_mort = ggplot(na.omit(data[data$admin0Name == 'Chad',])) + aes(x = Year, y = Mortality, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_wasted = ggplot(na.omit(data[data$admin0Name == 'Chad',])) + aes(x = Year, y = Wasted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_stunted = ggplot(na.omit(data[data$admin0Name == 'Chad',])) + aes(x = Year, y = Stunted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()

grid_arrange_shared_legend(plot_mort,plot_wasted,plot_stunted, nrow=2, top = textGrob("Mortality, Stunted and Wasted rates per country",gp=gpar(fontsize=20)))

#Everything improving (some areas faster than others)

# Cameroon

plot_mort = ggplot(na.omit(data[data$admin0Name == 'Cameroon',])) + aes(x = Year, y = Mortality, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_wasted = ggplot(na.omit(data[data$admin0Name == 'Cameroon',])) + aes(x = Year, y = Wasted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_stunted = ggplot(na.omit(data[data$admin0Name == 'Cameroon',])) + aes(x = Year, y = Stunted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()

grid_arrange_shared_legend(plot_mort,plot_wasted,plot_stunted, nrow=2, top = textGrob("Mortality, Stunted and Wasted rates per country",gp=gpar(fontsize=20)))

#Stunting decreasing everywhere, mortality increasing in an specific region (Ouest). Wasting overall decreasing, increasing in Extreme Nord Area, Est and Sud.


# Senegal

plot_mort = ggplot(na.omit(data[data$admin0Name == 'Senegal',])) + aes(x = Year, y = Mortality, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_wasted = ggplot(na.omit(data[data$admin0Name == 'Senegal',])) + aes(x = Year, y = Wasted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()
plot_stunted = ggplot(na.omit(data[data$admin0Name == 'Senegal',])) + aes(x = Year, y = Stunted, col = admin1Name)+ geom_point(size = 0.5) + geom_quantile(size = 0.5, quantiles = 0.5) + theme_classic()

grid_arrange_shared_legend(plot_mort,plot_wasted,plot_stunted, nrow=2, top = textGrob("Mortality, Stunted and Wasted rates per country",gp=gpar(fontsize=20)))

#Mortality decreasing over time, wasting is decreasing veeeery slowly (almost straight lines). Stunting some are increasing just slightly
# while the rest are maintaining themselves or decreasing slowly.


