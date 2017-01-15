library(SnowballC)
library(stringr)
library(proxy)
library(Rglpk)
library(slam)
library(tm)


require(XML)
require(tm)
require(wordcloud)
require(RColorBrewer)
library(tm)

library(dendextend)
library(colorspace)


mymatrix<-function(mdat,columna,TFIDF=FALSE,distanceC=TRUE)
  
{
  #print(TFIDF)
  #print(distanceC)
  data <- data.frame(text=mdat[,columna], stringsAsFactors=FALSE)
  lords<-Corpus(DataframeSource(data),readerControl = list(language = "en"))
  
  #Clean up
  lords <- tm_map(lords, stripWhitespace) #eliminate extra white-spaces
  lords <- tm_map(lords, content_transformer(tolower)) #convert text to lower case
  lords <- tm_map(lords, removeWords, stopwords('english')) #remove English stopwords
  lords <- tm_map(lords, stemDocument) #apply stemming
  
  
  if (TFIDF) tdm = TermDocumentMatrix(lords, control = list(removePunctuation = TRUE, removeNumbers = TRUE, tolower = TRUE, weighting = weightTfIdf))
  else tdm = TermDocumentMatrix(lords, control = list(removePunctuation = TRUE, removeNumbers = TRUE, tolower = TRUE))
  # convert it to a matrix
  m = as.matrix(tdm)
  m<-t(m) #traspose
  
  k<-dist(m, method=distanceC)
  
  k
}


mymatrixbag<-function(mdat,columna)

{
#txt<-do.call(paste, c(as.list(mdat[,columna]), sep=""))## abstract 
lords<-Corpus(VectorSource(mdat),readerControl = list(language = "en"))

#Clean up
lords <- tm_map(lords, stripWhitespace) #eliminate extra white-spaces
lords <- tm_map(lords, content_transformer(tolower)) #convert text to lower case
lords <- tm_map(lords, removeWords, stopwords('english')) #remove English stopwords


tdm = TermDocumentMatrix(lords, control = list(removePunctuation = TRUE, removeNumbers = TRUE, tolower = TRUE))
# convert it to a matrix
m = as.matrix(tdm)

# word count in descending order
wf <- sort(rowSums(m),decreasing=TRUE)
# create a dataframe with words and its frequencies
dm <- data.frame(word = names(wf), freq=wf)
dm
}


schedule<-function(distribucion)
{
  #print(distribucion)
  a2 <- read.csv("matriz_distancias.csv",header = FALSE)
  
  
  #a2<-list()
  #for (i in 1:(dim(dmatT)[1])){
  #  aa2[[length(aa2)+1]]=list(unlist(dmatT[i,]))
  #}
  n2<-as.integer(str_split(distribucion,",")[[1]])
  #print(n2)
  ns<-sum(n2)
  r = length(a2)
  #print(r) 
  #Index -1 Centroides (Puntos Iniciales), se deben configurar para cada dataset
  
  if (r!=ns) return(0)
  
  #c1 = c(12, 30, 62) # icmla14
  #c1 = c(113, 143, 147) # aaai13
  #c1 = c(70, 183, 347) # aaai14
  #c1<-  sample(1:r, length(n2))
  c1<- Bpoints(a2,length(n2))
  C=c1+1 #indices de los centroides
  k = length(C)
  #print("c1")
  #print(c1)
  #Tama?os de los grupos, se deben configurar para cada dataset
  #n2 = c(23, 23, 23) # icmla14
  #n2 = c(45, 52, 53) # aaai13
  #n2 = c(100, 198, 100) # aaai14
  
  n2 = n2-1 # Eliminamos los centroides de cada grupo
  v<-c(1:r)
  v1<-v[-C]  #indices de objetos menos los centroides
  a2<-a2[-v1,]
  a2<-a2[-C] # Matriz de distancias sin centroides
  
  #Creacion de la funcion objetivo
  f<-c()
  for (i in 1:k){
    f<-c(f,a2[i,])}
  f <- c(matrix(unlist(f), ncol = 1, byrow = TRUE))
  
  # Restricciones de pertinencia
  Aeq1 = matrix(0, k, length(f)); 
  for (i in 1:length(a2)){
    for (j in 1:k) {
      Aeq1[j,i+(j-1)*length(a2)] <-1;
    }}
  
  #Restricciones de Tamano
  Aeq2 = diag(length(a2));
  for (i in 1:(k-1)){
    Aeq2 = cbind(Aeq2,diag(length(a2)))
  }
  Aeq=rbind(Aeq1, Aeq2)
  Beq<-c(n2, rep(1,length(a2)))
  
  obj <- f
  #mat=cbind(Aeq, Beq)
  mat<- Aeq
  dir <- rep("==", r)
  rhs <- Beq
  types <- rep("B", r)
  max <- FALSE
  
  sol <- Rglpk_solve_LP(obj, mat, dir, rhs, types = types, max = max)[2]
  R<-sol$solution
  
  #Funcion para insertar elementos en una determinada posicion de un vector
  insertElems = function(vect, pos, elems) {
    l = length(vect)
    j = 0
    for (i in 1:length(pos)){
      if (pos[i]==1)
        vect = c(elems[j+1], vect)
      else if (pos[i] == length(vect)+1)
        vect = c(vect, elems[j+1])
      else
        vect = c(vect[1:(pos[i]-1+j)], elems[j+1], vect[(pos[i]+j):(l+j)])
      j = j+1
    }
    return(vect)
  }
  
  #Se crean los indices de las posiciones de los centroides sobre el vector sol (1xk*n)
  c_index<-c()
  for (i in (1:k)){
    for (j in (((i-1)*k+1):(k*i))){
     # print ((i-1)*r-j+1)
      
      c_index<-c(c_index, C[j-(i-1)*k]+(i-1)*r-j+1)
    }}
  
  #Se crea el vector booleano con los valores de pertinencia de los grupos y centroides
  c_values<-c(1,rep(0, (k*(k-1)+(k-1))))
  pos<-1
  for (i in 1:k){
    c_values[pos]<-1
    pos<-pos+k+1
  }
  
  #Insertamos los valores de pertinencia (0 o 1) de los centroides en cada grupo, en el vector solucion R
  R<-insertElems(vec=R, pos=c_index, elems= c_values)
  
  #Mostramos los grupos con sus respectivos objetos
  r2<-c()
  r1<-c()
  for (i in 1:k){
    r1<-c(r1,which(R[((i-1)*r+1):(i*r)]==1, arr.ind=TRUE))
  #  print(length(r1))
    r2<-c(r2,rep(i, (n2[i]+1)))
  #  print(length(r2))
  }
  Result<-cbind(r1,r2)
  
  Result
}


#Buckshot  Algorithm
Bpoints<-function(dmat,k){
  hierarchical.clustering <- hclust(as.dist(dmat),method="complete") #Clustering 
  hclust <- cutree(hierarchical.clustering,k) #Restricci?n de Tama?o
  C<-c()
  for (i in 1:k) {
    index<-which(hclust==i, arr.ind=TRUE)
    if (length(index)==1) C<-c(C, index) else
      C<-c(C,index[which.min(rowSums(dmat[index, index]))])
  }
  C
}


#Gonzales  Algorithm
Gpoints<-function(dmat,k){
  C<-which.max(rowSums(dmat))
  C<-c(C,which.max(dmat[C[1],]))
  for (i in 1:(k-2)) C<-c(C,which.max(colSums(dmat[C[1:(i+1)],])))
  C}

#Color Dendogram
plot_dendo<-function(dmat, k){
  hierarchical.clustering <- hclust(as.dist(dmat),method="complete") #Clustering 
  #k<-input$ses
  hclust <- cutree(hierarchical.clustering,k) #Restricción de Tamaño
  dend <- as.dendrogram(hierarchical.clustering)
  dend <- color_branches(dend, k) #, groupLabels=iris_species)
  dend <- hang.dendrogram(dend,hang_height=0.1)
  dend <- set(dend, "labels_cex", 0.7)
  namesG <- paste(unique(hclust), " Session", sep="") 
  labels_colors(dend) <-
    rainbow_hcl(k)[sort_levels_values(
      as.numeric(hclust)[order.dendrogram(dend)]
    )]
  labels(dend) <-hierarchical.clustering$order
  
  plot(dend, 
       main = "Dendogram", 
       horiz =  FALSE,  nodePar = list(cex = .1), xlab="Dendrogram of Papers",ylab="Dist")
  legend("bottomright", legend = namesG, fill = rainbow_hcl(k))
}
#plot_dendo(dmat, k)

#Color color cmdscale
#A bagwords or tfidf matrix
#r2 second column to PL solution
plot_cmd<-function(A, r2){
  fit <- cmdscale(A, eig=TRUE, k=2) # k es la dimensión
  # plot solution 
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(y, x, xlab="Coordinate 1", ylab="Coordinate 2", col=as.integer(numses), main="Metric  MDS", pch=19)
  text(y, x, pos = 4, labels = c(1:length(A)[1]), cex=.7)}
