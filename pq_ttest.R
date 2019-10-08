library(tidyverse)
library(kableExtra)

pq_sum_num <- function(data, varnames, vardisc, tname, na.rm = FALSE){
  
  df <- data.frame(matrix(ncol = 6, nrow = 0))
  
  for(i in 1:length(varnames)) {
    vect <- data[[varnames[i]]]
    if (na.rm == TRUE){vect <- vect[!is.na(vect)]}
    summary_stats <- c(length(vect), round(min(vect),1), round(max(vect),1), round(median(vect), 1), 
                       round(mean(vect),2), round(sd(vect),2))
    df <- rbind(df, summary_stats)
  }
  
  rownames(df) <- vardisc
  
  titlehead <- c(tname = 7)
  names(titlehead) <- tname
  names <- c("n", "Min", "Max", "Median", "Mean", "Std. Dev.")
  colnames(df) <- names
  
  out <- df  %>% kable(align="lcccccc", booktabs = T)  %>% 
    kable_styling(full_width = FALSE) %>% 
    row_spec(0, extra_css = "border-bottom: solid thin;") %>%
    add_header_above(header = titlehead, 
                     align = "l", extra_css = "border-top: solid; border-bottom: double;") %>% 
    row_spec(nrow(df), extra_css = "border-bottom: solid;") 
  
  return(out)
}

pq_ttest <- function(data, dv, iv, dv_desc, iv_desc, tname = "Comparing the mean of DV by IV"){

    y <- as.name(dv)
    x <- as.name(iv)

    lvls <- unique(data[[iv]])

    vec1 <- data %>% filter(UQ(rlang::sym(iv)) == lvls[1]) %>% .[[y]]
    vec2 <- data %>% filter(UQ(rlang::sym(iv)) == lvls[2]) %>% .[[y]]

    sv <- t.test(vec1, vec2)

    t = round(sv$statistic, 2)
    pval <- ifelse(sv$p.value > 0.001, 
                  format(csq_test$p.value, digits = 3, scientific = FALSE), 
                  "<0.001")

    df <- data %>% 
        group_by(UQ(rlang::sym(iv))) %>% 
        summarize(M = mean(UQ(rlang::sym(dv))), SD = sd(UQ(rlang::sym(dv)))) %>% 
        data.frame() %>%
        `row.names<-`(.[, 1]) %>%
        select(-1) %>%
        t() %>%
        data.frame() %>% 
        `colnames<-`(rev(lvls)) %>% 
        round(2) %>% 
        rownames_to_column("stat") %>% 
        mutate(dvar = dv_desc) %>% 
        select(dvar, everything()) %>% 
        mutate("t-value" = t, "p-value" = pval)

    titlehead <- c(tname = 6)
    names(titlehead) <- tname
  
    varhead <- c("x" = 2, iv_desc = 2, "x" = 2)
    names(varhead) <- c(" ", iv_desc, " ")
  
    len1 <- length(vec2)
    len2 <- length(vec1)
  
    colgrp1 <- paste0(lvls[2],"\n(n = ",len1,")")
    colgrp2 <- paste0(lvls[1],"\n(n = ",len2,")")
  
    colhead <- c("x" = 2, "lvl1" = 1, "lvl2" = 1, "t-value" = 1, "p-value" = 1 )
     names(colhead) <- c(" ", colgrp1, colgrp2, "t-value", "p-value")

    out <- df %>% 
        kable(align = "lrccc", booktabs=T) %>% 
        kable_styling(full_width = FALSE, bootstrap_options = "condensed") %>% 
        column_spec(5, width = "2cm")     %>% 
        row_spec(2, extra_css = "border-top: none; border-bottom: solid;") %>% 
        add_header_above(header = colhead, 
                     align = "c", extra_css = "border-top: none; border-bottom: solid thin; vertical-align: middle; ") 

    out <- gsub('<th style="border-bottom:hidden" colspan="2"></th>', '<th style="border-bottom:solid thin" colspan="2"></th>', out)
    
    out <- out %>%     
        add_header_above(header = varhead, 
                         align = "c", extra_css = "border-top: solid thin; border-bottom: solid thin;") %>%
        add_header_above(header = titlehead, 
                         align = "l", extra_css = "border-top: solid; border-bottom: double;") %>% 
        collapse_rows(columns = c(1,5,6), valign = "middle") %>% 
        row_spec(2, extra_css = "border-bottom: solid;" ) 


    out <- gsub('<th style="text.*</th>', "", out)
    out <- gsub('<div style="border-bottom: 1px solid #ddd', '<div style="border-bottom: none', out)
    out <- gsub('vertical-align: middle !important;" rowspan="2"', 
                'vertical-align: middle !important; border-bottom: solid" rowspan="2"', out)
    
    out <- gsub('margin-right: auto;', 'margin-right: auto;padding:20px;', out)

    return(out)
}

