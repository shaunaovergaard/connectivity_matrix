# connectivity_matrix
This code pulls in thickness mean and sd for a single subject, computes correlation metrics, and generates a per-subject heat map expressing correlation.


 
 #  Formula:
 #Z (i, j) = (μ (i) − μ  j  )/σ(j).

imu=2.344
jmu=2.52
isd=0.604
jsd=0.618

# Computes Zij, Zji, Cij
 
 Zij_pair <- function(imu, jmu, jsd) {
   Zij <- ((imu - jmu) / jsd) 
   return(Zij)
 } 
 
 Zji_pair <- function(jmu, imu, isd) {
   Zji <- ((jmu - imu) / isd) 
   return(Zji)
 } 
 
 Cij_out <- function(Zij_pair, Zji_pair) {
   Cij <- ((abs(Zij))+(abs(Zji)))/2  
   return(Cij)
 } 
 

# Should be called within the larger function.
Zij_pair(imu,jmu,jsd)
Zji_pair(jmu,imu,isd)
Cij_out(Zij_pair,Zji_pair)

#DATA LOAD
data <- read.csv(file.choose())
View(data)

attach(data)
#Manupulating Dataset
t(data)

cor(data) 

cor
methods(cor)
getAnywhere('cor')

 
######
#   https://www.rdocumentation.org/packages/base/versions/3.4.1/topics/pmatch
#   pmatch(x, table, nomatch = NA_integer_, duplicates.ok = FALSE)

#   apply(X, MARGIN, FUN, ...)
apply(data, 2, Zij_pair) 

unlist(data[1,]) #extracts single row of data (in this case row 1)

##### To be continued... this process needs to be worked on.... a lot...


testing2<-read.csv(file.choose()) #this is a file that has the first column "imu", second "jmu", third "isd", fourth "jsd"

View(testing2) #review data layout

connect<-function(csvfile){ #can input a csv file
  imu=csvfile[,1] #the first column will be stored as imu (mean of node i)
  jmu=csvfile[,2] #the second column will be stored as jmu (mean of node j)
  isd=csvfile[,3] #the third column will be stored as isd (standard deviation of node i)
  jsd=csvfile[,4] #the fourth column will be stored as jsd (standard deviation of node j)
  #sanity checks.. uncomment (remove hashtags) to test (activate code)
  #print(imu) 
  #print(jmu)
  #print(isd)
  #print(jsd)
  Zij <- ((imu - jmu) / jsd) #computes z score of ij
  #print(Zij)
  Zji <- ((jmu - imu) / isd) #computes z score of ji
  #print(Zji)
  Cij <- ((abs(Zij))+(abs(Zji)))/2 #computes connectivity value for the z scores of ij and ji (or average... discuss how this can be manipulated)
  #print(Cij)
  return(Cij)}

connect(testing2)
testit<-connect(testing2)
testit.mat<-matrix(testit, 2,4)
testit.mat
conn_matrix<-testit.mat

View(conn_matrix)
########
# Basics: heatmap visualization using default (guidance: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization)

## Set-up the data
conn_matrix<-read.csv(file.choose())
#conn_matrix
#View(conn_matrix)
library(reshape2)

melted_conn_matrix <- melt(conn_matrix)
head(melted_conn_matrix)
melted_conn_matrix
library(ggplot2)
ggplot(data = melted_conn_matrix, aes(x=Variable1, y=variable, fill=value)) + 
  geom_tile()

####### Colormap using ggplot2 ##########

library(reshape2)
conn_matrix<-read.csv(file.choose())
melted_conn_matrix <- melt(conn_matrix, na.rm = TRUE)
######
# Create ggheatmap
ggheatmap <- ggplot(melted_conn_matrix, aes(variable, Variable1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "white", high = "black", mid = "white", 
                       midpoint = 0.5, limit = c(0,1), space = "Lab", 
                       name="Structural Connectivity") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
          axis.text.y = element_text(vjust = 1, size = 12, hjust = 1))+
  coord_fixed()
#print(ggheatmap)

##### make it even more meaningful (and attractive)
ggheatmap + 
  geom_text(aes(variable, Variable1, label = value), color = "black", size = 4) +
  
theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(1, 0.65),
    legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 15, barheight = 2,
                                title.position = "top", title.hjust = 0.5))
#########







