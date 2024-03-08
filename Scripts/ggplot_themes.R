# Custom ggplot theme - bar plots

custom_theme <- function(base_size = 14, base_family = "sans") {
  
  library(ggthemes)
  library(grid)
  
  theme_foundation(
    base_size = base_size, base_family = base_family) +
  
      theme(
        plot.title = element_text(face = "bold",
                                  size = rel(1.2), 
                                  hjust = 0.5),
        text = element_text(),
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        panel.border = element_rect(colour = NA),
        axis.title = element_text(face = "bold",
                                  size = rel(1)),
        axis.title.y = element_text(angle = 90,
                                    vjust = 2),
        axis.title.x = element_text(vjust = -0.2),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.line.x = element_line(colour="black"),
        axis.line.y = element_line(colour="black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="#f0f0f0"),
        panel.grid.minor.y = element_blank(),
        axis.ticks.length = unit(2.5, "mm"),
        legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.size= unit(0.2, "cm"),
        legend.spacing.x = unit(.2, "cm"),
        legend.spacing.y = unit(.5, "cm"),
        legend.key.height = unit(.7, "cm"),
        legend.key.width = unit(.5, "cm"),
        legend.title = element_text(face="italic"),
        plot.margin=unit(c(10,5,5,5),"mm")
    )
}

# Custom colour ramp
# colscalex = colorRampPalette(RColorBrewer::brewer.pal(3, name="YlGnBu"))(3)
# 
# # Test out custom theme
# ggplot(refsByYear, aes(x = yearMod, y = n, fill = data_type)) +
#   geom_bar(stat = "identity") +
#   scale_x_date(date_labels = "%Y", date_breaks = "5 year") +
#   labs(x = "Year", y = "Number of publications",
#        fill = "Data type") + 
#   #scale_fill_manual(values = c("darkslategray", "darkslategray4", "darkslategray3")) +
#   scale_fill_manual(values = colscalex) +
#   custom_theme()
