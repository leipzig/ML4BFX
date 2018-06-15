library(ggplot2)
library(dplyr)

hwkmeans<-function(d){
  # Assign each sample randomly to one of the clusters. i.e., initialize I.
  I<-sample(c(TRUE,FALSE), nrow(d), TRUE)
  
  dist_two<-function(a,b){dist(rbind(a,b))}

  closer_center<-function(row){ifelse(dist_two(row,c_1)<dist_two(row,c_2),TRUE,FALSE)}

  plottables<-list()
  for (i in 1:50){
    
    #Calculate the centers of the two clusters.
    c_1<-apply(d[I,],2,mean)
    c_2<-apply(d[!I,],2,mean)
    
    
    plottables[[i]]<-rbind(
                  data.frame(d,i=i,type=I),
                  data.frame(t(as.matrix(c_1)),i=i,type="center one"),
                  data.frame(t(as.matrix(c_2)),i=i,type="center two")
    )
    
    
    #Update I based on whether this sample is closer to one of the centers versus the other.
    new_I<-apply(d,1,closer_center)
    
    #if we have converged on sample assignments then exit
    if(identical(I,new_I)){break}else{I<-new_I}
  }
  
  #plot two dimensions, or pca and take first two components to plot
  plotdf<-do.call(rbind,plottables)
  if(ncol(d)==2){
    names(plotdf)<-c("X","Y","i","type")
    plot<-ggplot(plotdf %>% mutate(type=ifelse(type=="TRUE","cluster one (TRUE)",ifelse(type=="FALSE","cluster two (FALSE)",type))),aes(X,Y))+geom_point(aes(color=type))+facet_wrap( ~ i,ncol=4)
  }else{
    pca_t<-prcomp(plotdf[,!(colnames(plotdf) %in% c("i","type"))])
    plotpca<-cbind(pca_t$x[,c("PC1","PC2")],plotdf[,c("i","type")])
    plot<-ggplot(plotpca %>% mutate(type=ifelse(type=="TRUE","cluster one (TRUE)",ifelse(type=="FALSE","cluster two (FALSE)",type))),aes(PC1,PC2))+geom_point(aes(color=type))+facet_wrap( ~ i,ncol=4)
  }
  return(list(calls=I,plot=plot))
}
