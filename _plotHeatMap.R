plotHeatMap <- function (file) {
  # file should be a csv file of the heatmap with the column and row names in the first column

  library(easypackages) # then we can do the rest in one go
  libraries("Hmisc","ggplot2","gplots","ggthemes","viridis","reshape2")

  df <- read.csv(file, header = TRUE)
  components <- as.character(df$X)
  df <- df[,2:10]
  df <- as.matrix(df)
  colnames(df) <- components
  rownames(df) <- components
  df <- melt(as.matrix(df))


  pdf("heatmap.pdf", width = 12, height = 12)
  ggplot(df, aes(Var1, Var2)) +
  geom_tile(aes(fill = (value))) +
  scale_fill_viridis(option='B') +
  ggtitle("heatmap") +
  theme(
    strip.text = element_text(colour = "white",face="bold", size=9,lineheight=5.0),
    strip.background = element_rect(fill="black", colour="white",size=1),
    plot.title = element_text(color="white",hjust=0.5,vjust=1, size=rel(2)),
    plot.background = element_rect(fill="gray30"),
    panel.background = element_rect(fill="gray30"),
    panel.border = element_rect(fill=NA,color="gray30", size=0.5, linetype="solid"),
    panel.grid.major = element_line(size = 0.2),
    panel.grid.minor = element_line(size = 0.2),
    #axis.line = element_blank(),
    #axis.ticks = element_blank(),
    axis.text = element_text(color="white", size=rel(1)),
    axis.title.y  = element_blank(),
    axis.title.x  = element_blank(),
    axis.text.y  = element_text(color="white",hjust=1),
    axis.text.x = element_text(angle=90,hjust=0.95,vjust=0.2),
    legend.text = element_text(color="white", size=rel(0.7)),
    legend.background = element_rect(fill="gray30"),
    legend.position = "bottom",
    legend.title=element_blank()
  )
  dev.off()

}
