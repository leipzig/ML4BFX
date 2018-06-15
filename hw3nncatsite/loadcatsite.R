library(foreign)
library(dplyr)
library(tidyr)
library(data.table)
library(Biostrings)

loadcatsite <- function(filename="NataliaPetrova.catsite.arff"){
  catsites<-read.arff(filename)

   # Convert the AAName1LetterCode feature to numeric data.
   # Each amino acid should be represented by 20 binary numbers. See int2aa()
   # for the conventional aminoacid <-> integer conversion.
   data.frame(aa_ints=sapply(catsites$AAName1LetterCode,function(aa){which(AA_ALPHABET==aa)})) %>% 
     mutate(id = row_number()) %>% 
     dcast(id ~ aa_ints, function(x) 1, fill=0) %>% 
     arrange(id) %>% 
     cbind(catsites) %>%
     select(-id,-AAName1LetterCode) %>%
     mutate(class=as.numeric(ifelse(class=='+1',1,-1))) -> xdf
   
   # Remove all data rows where a numeric value is NaN (not a number).
   # See matlab function ``isnan()''.
   xdf %>% filter(complete.cases(xdf)==TRUE) -> xdf_noNA

   # Convert the class labels to numeric +1/-1.
   # Store the class labels into variable T. remove that column from X.
   T<-(xdf_noNA$class)
   
   # Convert the data into a numeric matrix X
   # X: one row per data sample, and one column for each feature.
   xdf_noNA %>% select(-class) %>% as.matrix() -> X
   
   return(list(X=X,T=T))
}