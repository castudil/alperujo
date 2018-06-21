
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


PlotCorrelationMatrix <- function(url, sheet) {
  
  a <- gsheet2text(url,sheetid=sheet, format='csv')
  #a
  data <- read.csv(text=a, stringsAsFactors=FALSE,header = TRUE,row.names = 1)
  #data
  #class(data)
  ##data is a data frame
  
  data2<- t(data)
  #data2
  #colnames(data2) <- c("PPT", "HV","Fibres","Fats","RH","Humidity","Proteins","Wind","M&Y","Tmax","Ph","Phenolic","Tmin","Ashes")
  colnames(data2) <- c("PPT", "Tmin","Tmax","Wind","RH","Humidity","Proteins","MHV","Ashes","Fibres","Fats","Ph","M&Y","Phenolics")
  
  

  
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
  
  #The second argument (type=“upper”) is used to display only the upper triangular of the correlation matrix.
  #corrplot(res, type = "upper", order = "hclust", 
  #         tl.col = "black", tl.srt = 45)
  
  # Insignificant correlation are crossed
  #corrplot(res2$r, type="upper", order="hclust", 
  #         p.mat = res2$P, sig.level = 0.01, insig = "blank")
  # Insignificant correlations are leaved blank
  
  #corrplot(res2$r, type="upper", order="hclust", 
  #         p.mat = res2$P, sig.level = 0.05, insig = "blank")
  
  
  #corrplot(res2$r, type="upper", order="original", 
  #         p.mat = res2$P, sig.level = 0.05, insig = "blank")

  corrplot(res2$r, type="upper", order="hclust", 
           tl.col = "black", tl.srt = 45)

}

#install.packages('gsheet')
library(gsheet)

# https://docs.google.com/spreadsheets/d/1GUEcKbZ-F_Ef0yenZUEtH3z5QhF-YBDvFDer7JJxPow/edit?usp=sharing
url<-    "docs.google.com/spreadsheets/d/1GUEcKbZ-F_Ef0yenZUEtH3z5QhF-YBDvFDer7JJxPow"
# you can get this id by selecting any cell in the sheet, right click-> get link for this cell-> copy  paste link and see the id in the url,

### If interested you can take a look at details such as the source for the average values and the respective standard deviations
### the tabulated data was included as suplementary material in the journal version of the paper.
### https://docs.google.com/spreadsheets/d/1kMZ0vKpk2zIYVNq5jCb3VUUv-XzdQbvBkOvGFPOcTVA/edit?usp=sharing

buildCorrelationMatrix <- function(url, sheet) {
  #install.packages("PerformanceAnalytics")
  library("PerformanceAnalytics")
  a <- gsheet2text(url,sheetid=sheet, format='csv')
  data <- read.csv(text=a, stringsAsFactors=FALSE,header = TRUE,row.names = 1)
  data2<- t(data)
  
  #colnames(data2) <- c("PPT", "Tmin","Tmax","Wind","RH","Humidity","Proteins","MHV","Ashes","Fibres","Fats","Ph","M&Y","Phenolics")  
  #colnames(data2) <- c("PPT", "Tmin","Tmax","Wind","RH","Humidity","Proteins","MHV","Ashes","Fibres","Fats","Ph","M&Y","Phenolics")    
  colnames(data2) <- c(
    "PPT",
    "Tmin",
    "Tmax",
    "Wind",
    "RH",
    "\n\nmoisture\ncontent",
    "crude protein", 
    "measuring heating value",
    "ashes",
    "crude fiber",
    "fats",
    "pH",
    "molds and yeasts",  
    "total phenolic")
    
  
  
  ##in this version of the correlation matrix we eliminate certaines rows and column for reasons excplained 
  ##in the paper
  #1.- Corregir gráficos de correlación. para los cuatro gráficos (AT,AM,OT,OM) eliminar 4 columnas
  #1.1.- column 8 MHV
  #1.1.- column 10 Fibres
  #1.2 .- column 12 Ph
  #1.3 .- column 13 M&Y
  data2<-data2[-c(8,10,12,13),]
  data2<-data2[,-c(8,10,12,13)] 
  
  chart.Correlation(data2, histogram=TRUE, pch=19)
}








####################################################################################################
#AT
# gid=320810707&range=B2
##AT  -- firt sheet
sheetAT <- 320810707
pdf(file='AT-correlation-matrix.pdf')
#svg(file='AT-correlation-matrix.pdf') 
buildCorrelationMatrix(url,sheetAT)
dev.off() 
#AM
## AM -- second Sheet
#https://docs.google.com/spreadsheets/d/1GUEcKbZ-F_Ef0yenZUEtH3z5QhF-YBDvFDer7JJxPow/edit#gid=325975436&range=A1
sheetAM <- 325975436
pdf(file='AM-correlation-matrix.pdf') 
buildCorrelationMatrix(url,sheetAM)
dev.off() 
#OT
## OT -- Third Sheet
#https://docs.google.com/spreadsheets/d/1GUEcKbZ-F_Ef0yenZUEtH3z5QhF-YBDvFDer7JJxPow/edit#gid=1972082407&range=C3
sheetOT <- 1972082407
pdf(file='OT-correlation-matrix.pdf') 
buildCorrelationMatrix(url,sheetOT)
dev.off() 
#OM
## OM -- fourth Sheet
#https://docs.google.com/spreadsheets/d/1GUEcKbZ-F_Ef0yenZUEtH3z5QhF-YBDvFDer7JJxPow/edit#gid=1075778825&range=D6
sheetOM <- 1075778825
pdf(file='OM-correlation-matrix.pdf') 
buildCorrelationMatrix(url,sheetOM)
dev.off() 


#A visualization of a correlation matrix
#The areas of circles or squares show the absolute value of corresponding correlation coefficients
# display the upper triangular matrix.


##3.- hacer 4 gráficos tuve va a la sección materiales. Tomas las tablas del excel nuevo que envío el profe y presenta los valores de las muestras para cada parámetro.
##3.- make 4 plots, for the material section. these are the tables given by Diogenes' supervisor

library(ggplot2)
##############################################################################
##loads a dataframe and returns a ggplot object that can be externally modified and plotted
makeMultipleLinePlot <- function(data){
  
  require(reshape2)
  data$id <-rownames(data)
  melted <- melt(data)
  colnames(melted)<-c("Parameter","Month","Value")
  g<-ggplot(data=melted,
            aes(x=Month, y=Value, color=Parameter,group=Parameter)) +
    geom_line(size=1,alpha=0.8) + geom_point(size=4,aes(shape=Parameter))
  return(g)
}  

##############################################################################
##load a table from google sheets. assumes the sheet has a single table
loadTableFromGoogleSheet <- function(url, sheet) {
  require(gsheet)
  a <- gsheet2text(url,sheetid=sheet, format='csv')
  data <- read.csv(text=a, stringsAsFactors=FALSE,header = TRUE,row.names = 1)
  return(data)
}


##############################################################################

##create fig2 - moisture content for OT OM AT AM
#https://docs.google.com/spreadsheets/d/1GUEcKbZ-F_Ef0yenZUEtH3z5QhF-YBDvFDer7JJxPow/edit#gid=800591980&range=B2
url.moisture_content<-"docs.google.com/spreadsheets/d/1GUEcKbZ-F_Ef0yenZUEtH3z5QhF-YBDvFDer7JJxPow"
sheet.moisture_content<-800591980
data.moisture_content<-loadTableFromGoogleSheet(url.moisture_content,sheet.moisture_content)
require(reshape2)
data.moisture_content$id <-rownames(data.moisture_content)
melted <- melt(data.moisture_content)
colnames(melted)<-c("Measurement","Month","Percentage")
pdf(file='moisture_content.pdf') 
g<-ggplot(data=melted,
          aes(x=Month, y=Percentage, color=Measurement,group=Measurement)) +
  geom_line(size=1,alpha=0.8) + geom_point(size=4,aes(shape=Measurement))
#g+ scale_fill_discrete(name  ="Payer")
g+ labs(colour = "Moisture content") + labs(shape = "Moisture content")
dev.off() 

##############################################################################
##fig 3 -- total_phenolic
## https://docs.google.com/spreadsheets/d/1GUEcKbZ-F_Ef0yenZUEtH3z5QhF-YBDvFDer7JJxPow/edit#gid=1631347858&range=C3
url.total_phenolic<-"docs.google.com/spreadsheets/d/1GUEcKbZ-F_Ef0yenZUEtH3z5QhF-YBDvFDer7JJxPow"
sheet.total_phenolic<-1631347858
data.total_phenolic<-loadTableFromGoogleSheet(url.total_phenolic,sheet.total_phenolic)
require(reshape2)
data.total_phenolic$id <-rownames(data.total_phenolic)
melted <- melt(data.total_phenolic)
colnames(melted)<-c("Measurement","Month","Percentage")
pdf(file='total_phenolic.pdf') 
g<-ggplot(data=melted,
          aes(x=Month, y=Percentage, color=Measurement,group=Measurement)) +
  geom_line(size=1,alpha=0.8) + geom_point(size=4,aes(shape=Measurement))
#g+ scale_fill_discrete(name  ="Payer")
g+ labs(colour = "Total phenolic") + labs(shape = "Total phenolic") + labs(y=bquote('g'~kg^-1) )
##g kg^-1
dev.off() 

##############################################################################
##fig 4 -- proteins
##https://docs.google.com/spreadsheets/d/1GUEcKbZ-F_Ef0yenZUEtH3z5QhF-YBDvFDer7JJxPow/edit#gid=116371717&range=D4 
url.proteins<-"docs.google.com/spreadsheets/d/1GUEcKbZ-F_Ef0yenZUEtH3z5QhF-YBDvFDer7JJxPow"
sheet.proteins<-116371717
data.proteins<-loadTableFromGoogleSheet(url.proteins,sheet.proteins)
require(reshape2)
data.proteins$id <-rownames(data.proteins)
melted <- melt(data.proteins)
colnames(melted)<-c("Measurement","Month","Percentage")
pdf(file='proteins.pdf') 
g<-ggplot(data=melted,
          aes(x=Month, y=Percentage, color=Measurement,group=Measurement)) +
  geom_line(size=1,alpha=0.8) + geom_point(size=4,aes(shape=Measurement))
#g+ scale_fill_discrete(name  ="Payer")
g+ labs(colour = "Proteins") + labs(shape = "Proteins") + labs(y="Percentage" )
##g kg^-1
dev.off() 

