library(dplyr)
library(ggplot2)

#f is the function for which we want the minima
#g is the derivative of function f with respect to x
#guess is the initial x to test
#gamma is the step size coefficient
#episilon is the threshold by which we decide to stop descending
gdescent = function(f, g, guess=0, gamma=.1, epsilon=.01, n=100) {
  descent_df <- data.frame(x=numeric(0), y=numeric(0), type=character(0))
  for (i in seq_len(n)) {
    g_of_x = g(guess)
    f_of_x=f(guess)
    

    descent_df<-rbind(descent_df,data.frame(x=guess,y=f_of_x,type="f(x)",x_axis="guess"))
    descent_df<-rbind(descent_df,data.frame(x=guess,y=g_of_x,type="g(x)",x_axis="guess"))
    descent_df<-rbind(descent_df,data.frame(x=i,y=f_of_x,type="f(x)",x_axis="iterations"))
    descent_df<-rbind(descent_df,data.frame(x=i,y=g_of_x,type="g(x)",x_axis="iterations"))
    
    guess = guess - gamma * g_of_x
    
    if (abs(g_of_x) < epsilon) {
      
      #this is just blather to prevent the last arrows from being rendered
      guess_segments<-descent_df %>% filter(x_axis=='guess') 
      rbind(guess_segments[3:nrow(guess_segments),],
                            data.frame(x=NA,y=NA,type="f(x)",x_axis="guess"),
                            data.frame(x=NA,y=NA,type="g(x)",x_axis="guess")) %>% 
                            select(x,y) %>%
                            rename(xend=x,yend=y) %>% cbind(guess_segments) -> guess_segments
      
      funcplot<-ggplot(guess_segments,aes(x,y))+geom_point()+xlab("x guess")+
        geom_segment(aes(xend=xend,yend=yend),arrow=arrow(length=unit(0.2,"cm")))+
        facet_grid(. ~ type)
      print(funcplot)
      
      iterplot<-ggplot(descent_df %>% filter(x_axis == 'iterations'),aes(x,y))+geom_point()+facet_grid(. ~ type)+xlab("iterations")
      print(iterplot)
      
      break
    }
  }
  return(list(guess=guess,f_of_x=f_of_x,db=descent_df))
}



