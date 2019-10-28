library(tidyverse)
library(kableExtra)
library(magrittr)

data("ChickWeight")

a1 <- aov(weight ~ Diet * Time, data = ChickWeight)


pq_anova <- function(x, tname = "ANOVA Table"){

    x <- summary(a1)
    rows <- rownames(x[[1]])
    x %<>% .[[1]]
    
    cols <- c("df", "SumSq", "MeanSq", "Fvalue", "p-value")
    colnames(x) <- cols
    
    x %<>%  mutate(SumSq = format(round(SumSq,0), big.mark = ",")) %>% 
            mutate(MeanSq = format(round(MeanSq,0), big.mark = ","))  %>% 
            mutate(Fvalue = if_else(is.na(Fvalue),"",as.character(round(Fvalue,2)))) 
    
    pvals <- c()
    len <- length(x$`p-value`)
    for(i in 1:len){
      if (is.na(x$`p-value`[i])){
        pvals[i] = ""
      }
      else if(x$`p-value`[i] > 0.001){
        pvals[i] = as.character(round(x$`p-value`,3))
      }
      else{
        pvals[i] = "<0.001"
      }
    }
    
    x %<>% mutate('p-value' = pvals)
    
    rownames(x) <- rows
    cols <- c("df", "SumSq", "MeanSq", "F-value", "p-value")
    colnames(x) <- cols
    
    titlehead <- c(tname = 6)
    names(titlehead) <- tname
    
    out <- x %>% kable(align="crrrc") %>% 
        row_spec(0, extra_css = "border-bottom: solid thin;") %>%
        add_header_above(header = titlehead, 
                      align = "l", extra_css = "border-top: solid; border-bottom: double;") %>% 
        row_spec(nrow(x), extra_css = "border-bottom: solid;") %>% 
        column_spec(2:6, width = "2cm")
    return(out)
}

pq_anova(a1, "ANOVA Chick Weights")
