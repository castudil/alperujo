##load a table from google sheets. assumes the sheet has a single table
loadTableFromGoogleSheet <- function(url, sheet) {
  require(gsheet)
  a <- gsheet2text(url,sheetid=sheet, format='csv')
  data <- read.csv(text=a, stringsAsFactors=FALSE,header = TRUE,row.names = 1)
  return(data)
}


# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

PlotCorrelationMatrix <- function(data) {
  data2<- t(data)
  res <- cor(data2)
  round(res, 2)

  ##http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

  #install.packages("Hmisc")
  library("Hmisc")
  res2 <- rcorr(as.matrix(data2))
  #res2

  # Extract the correlation coefficients
  #res2$r
  # Extract p-values
  #res2$P

  library(Hmisc)
  flattenCorrMatrix(res2$r, res2$P)

  #install.packages("corrplot")
  library(corrplot)

  corrplot(res2$r, type="upper", order="hclust",
           tl.col = "black", tl.srt = 45)
}



buildCorrelationMatrix <- function(data) {
  #install.packages("PerformanceAnalytics")
  require("PerformanceAnalytics")
  data2<- t(data)
  chart.Correlation(data2, histogram=TRUE, pch=19)
}










#https://docs.google.com/spreadsheets/d/10clnt9isJp_8Sr7A8ejhKEZXCQ279wGP4sdygsit1LQ/edit#gid=2031539780&range=A2
url <- "docs.google.com/spreadsheets/d/10clnt9isJp_8Sr7A8ejhKEZXCQ279wGP4sdygsit1LQ"

gid.environment <- 2031539780
gid.humidity    <- 2080295295
gid.fats        <- 409623482
gid.ashes       <- 1827365164
gid.proteins    <- 1048252404
gid.phenolic    <- 45401281
gid.fiber       <- 459445776
gid.mhv         <- 1598560296
gid.ph    <- 1746790860
gid.yeast <- 233363481

data.environment<-loadTableFromGoogleSheet(url,gid.environment)
data.humidity<-loadTableFromGoogleSheet(url,gid.humidity)
data.fats<-loadTableFromGoogleSheet(url,gid.fats)
data.ashes<-loadTableFromGoogleSheet(url,gid.ashes)
data.proteins<-loadTableFromGoogleSheet(url,gid.proteins)
data.phenolic<-loadTableFromGoogleSheet(url,gid.phenolic)
data.fiber<-loadTableFromGoogleSheet(url,gid.fiber)
data.mhv<-loadTableFromGoogleSheet(url,gid.mhv)
data.ph<-loadTableFromGoogleSheet(url,gid.ph)
data.yeast<-loadTableFromGoogleSheet(url,gid.yeast)


#data.environment
table.environment<-rbind(data.environment,data.humidity)
#table.environment
PlotCorrelationMatrix(table.environment)




##loads a dataframe and returns a ggplot object that can be externally modified and plotted
makeMultipleLinePlot <- function(data){
  require(reshape2)
  data$id <-rownames(data)
  melted <- melt(data)
  colnames(melted)<-c("Measurement","Month","Percentage")
  g<-ggplot(data=melted,
       aes(x=Month, y=Percentage, color=Measurement,group=Measurement)) +
  geom_line(size=1,alpha=0.8) + geom_point(size=4,aes(shape=Measurement))
  return(g)
}



ggsave(filename="lineplot/humidity.pdf", plot=makeMultipleLinePlot(data.humidity))
ggsave(filename="lineplot/fats.pdf", plot=makeMultipleLinePlot(data.fats))
ggsave(filename="lineplot/ashes.pdf", plot=makeMultipleLinePlot(data.ashes))
ggsave(filename="lineplot/proteins.pdf", plot=makeMultipleLinePlot(data.proteins))
ggsave(filename="lineplot/phenolic.pdf", plot=makeMultipleLinePlot(data.phenolic))
ggsave(filename="lineplot/fiber.pdf", plot=makeMultipleLinePlot(data.fiber))
ggsave(filename="lineplot/maximum-heating-value.pdf", plot=makeMultipleLinePlot(data.mhv))
ggsave(filename="lineplot/ph.pdf", plot=makeMultipleLinePlot(data.ph))

#makeMultipleLinePlot(data.ph)
#data.proteins
#buildCorrelationMatrix(rbind(data.environment,data.humidity))

ggsave(filename="correlation-matrix/humidity-correlation-matrix.pdf", plot=buildCorrelationMatrix(rbind(data.environment,data.humidity)))
ggsave(filename="correlation-matrix/fats-correlation-matrix.pdf", plot=buildCorrelationMatrix(rbind(data.environment,data.fats)))
ggsave(filename="correlation-matrix/ashes-correlation-matrix.pdf", plot=buildCorrelationMatrix(rbind(data.environment,data.ashes)))
ggsave(filename="correlation-matrix/proteins-correlation-matrix.pdf", plot=buildCorrelationMatrix(rbind(data.environment,data.proteins)))
ggsave(filename="correlation-matrix/phenolic-correlation-matrix.pdf", plot=buildCorrelationMatrix(rbind(data.environment,data.phenolic)))
ggsave(filename="correlation-matrix/fiber-correlation-matrix.pdf", plot=buildCorrelationMatrix(rbind(data.environment,data.fiber)))
ggsave(filename="correlation-matrix/maximum-heating-value-correlation-matrix.pdf", plot=buildCorrelationMatrix(rbind(data.environment,data.mhv)))
ggsave(filename="correlation-matrix/ph-correlation-matrix.pdf", plot=buildCorrelationMatrix(rbind(data.environment,data.ph)))
