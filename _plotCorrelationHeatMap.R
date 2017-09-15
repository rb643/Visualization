plotCorrelationHeatMap <- function (r_file, p_file) {
  # r_file should be a csv file of the r-values in the matrix and the first column containing the names
  # p_file should be a csv file of the p-values in the matrix

  library(easypackages) # then we can do the rest in one go
  libraries("Hmisc","ggplot2","gplots","ggthemes","viridis","reshape2")

  correlationMatrix <- read.csv(r_file, header = TRUE)
  components <- as.character(correlationMatrix$X)
  correlationMatrix <- correlationMatrix[,2:10]
  correlationMatrix <- as.matrix(correlationMatrix)
  colnames(correlationMatrix) <- components
  rownames(correlationMatrix) <- components
  correlationMatrix <- melt(as.matrix(correlationMatrix))

# load useful functions
source("https://raw.githubusercontent.com/rb643/R_Functions/master/_reorderCormat.R")
source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")

p.mat <- as.matrix(read.csv(p_file, header = FALSE))
reorderInd <- reorder_cormat(correlationMatrix)
correlationMatrix <- correlationMatrix[reorderInd,reorderInd]
p.mat <- p.mat[reorderInd,reorderInd]

p.mat[lower.tri(p.mat,diag = TRUE)] <- NA
correlationMatrix[upper.tri(correlationMatrix, diag = TRUE)] <- NA

test <- melt(as.matrix(p.mat))
test$value <- (test$value)
test2 <- melt(correlationMatrix)
test3 <- merge(test, test2, by = c("Var1","Var2"))

pdf("heatmap.pdf", width = 12, height = 12)
  ggplot(test3, aes(Var1, Var2)) +
    geom_tile(aes(fill = value.y)) +
    geom_text(aes(Var1, Var2, label = round(value.x, 4)),colour = "white") +
    geom_text(aes(Var1, Var2, label = round(value.y, 4)),colour = "black") +
    scale_fill_viridis(option='B') +
    ggtitle("Symptom Correlations") +
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

}
