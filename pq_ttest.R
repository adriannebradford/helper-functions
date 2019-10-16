library(tidyverse)
library(kableExtra)

pq_sum_num <- function(data, varnames, vardisc, tname){
  
    if (length(varnames) < length(vardisc)) {
        stop("Vector of Descriptive names contains more values than variables specified.")
    }
    
  df <- data.frame(matrix(ncol = 6, nrow = 0))
  
  for(i in 1:length(varnames)) {
    vect <- data[[varnames[i]]]
    if (anyNA(vect)){warning(paste0("WARNING - variable '", 
                                    varnames[i], "' has missing values that are omitted in calculation" ))}
    vect <- vect[!is.na(vect)]
    summary_stats <- c(length(vect), round(min(vect),1), round(max(vect),1), round(median(vect), 1), 
                       round(mean(vect),2), round(sd(vect),2))
    df <- rbind(df, summary_stats)
  }
  
  
  if (length(varnames) == length(vardisc)){
    rownames(df) <- vardisc
  }
  else {
      warning("WARNING - You have supplied fewer descriptive names than variables, labels will default to variable names")
      name_vect <- vardisc
      st <- length(vardisc) + 1
      end <- length(varnames)
    #  print(st, end)
      for(i in st:end){
      #    print(i)
          name_vect <- rbind(name_vect, varnames[i])
          
      }
    #  print(name_vect)
      rownames(df) <- name_vect
  }
  
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

pq_ttest <- function(data, dv, iv, dv_desc, iv_desc, tname = "Comparing the mean of DV by IV", ...){

    y <- as.name(dv)
    x <- as.name(iv)
    
    if ("paired" %in% names(list(...)) ){stop('use pq_ttest_paired instead')}
    
    else{
        lvls <- unique(data[[iv]])
        # print(lvls)
        vec1 <- data %>% filter(UQ(rlang::sym(iv)) == lvls[1]) %>% .[[y]]
        vec2 <- data %>% filter(UQ(rlang::sym(iv)) == lvls[2]) %>% .[[y]]
        if (anyNA(vec1) | anyNA(vec2)){warning("WARNING - missing values have been omitted in calculation")}
        vec1 <- vec1[!is.na(vec1)]
        vec2 <- vec2[!is.na(vec2)]
    }
    
   

    sv <- t.test(vec1, vec2, ...)

    t = round(sv$statistic, 2)
    pval <- ifelse(sv$p.value > 0.001, 
                  format(sv$p.value, digits = 3, scientific = FALSE), 
                  "<0.001")

    df <- data %>% 
        group_by(UQ(rlang::sym(iv))) %>% 
        summarize(M = mean(UQ(rlang::sym(dv)), na.rm = TRUE), SD = sd(UQ(rlang::sym(dv)), na.rm = TRUE)) %>% 
        data.frame() %>%
        `row.names<-`(.[, 1]) %>%
        select(-1) %>%
        t() %>%
        data.frame() %>% 
        `colnames<-`(lvls) %>% 
        round(2) %>% 
        rownames_to_column("stat") %>% 
        mutate(dvar = dv_desc) %>% 
        select(dvar, everything()) %>% 
        mutate("t-value" = t, "p-value" = pval)

    titlehead <- c(tname = 6)
    names(titlehead) <- tname
  
    varhead <- c("x" = 2, iv_desc = 2, "x" = 2)
    names(varhead) <- c(" ", iv_desc, " ")
  
    len1 <- length(vec1)
    len2 <- length(vec2)
  
    colgrp1 <- paste0(lvls[1],"\n(n = ",len1,")")
    colgrp2 <- paste0(lvls[2],"\n(n = ",len2,")")
  
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

pq_ttest_paired <- function(x1, x2, x1_desc, x2_desc, x_desc, tname = "Comparing the mean of DV by IV", ...){
    
    if (anyNA(x1) | anyNA(x2)){warning("WARNING - missing values have been omitted in calculation")}
    x1 <- x1[!is.na(x1)]
    x2 <- x2[!is.na(x2)]
    #print(x1)
    #print(x2)
    sv <- t.test(x1, x2, paired = TRUE, ...)
    
    t = round(sv$statistic, 2)
    pval <- ifelse(sv$p.value > 0.001, 
                   format(sv$p.value, digits = 3, scientific = FALSE), 
                   "<0.001")
    
    m1 <- mean(x1)
    m2 <- mean(x2)
    sd1 <- sd(x1)
    sd2 <- sd(x2)
    
    df <- data.frame(
        dvar = rep(x_desc, 2),
        stat = c("M", "SD"),
        x1c = c(m1, sd1),
        x2c = c(m2, sd2),
        t = rep(t, 2),
        p = rep(pval, 2)
    )
    
    titlehead <- c(tname = 6)
    names(titlehead) <- tname
    len <- length(x1)
  #  varhead <- c("x" = 2, x_desc = 2, "x" = 2)
   # names(varhead) <- c(" ", paste0(x_desc,"\n(n = ",len,")"), " ")
    
    colhead <- c("x" = 2, "lvl1" = 1, "lvl2" = 1, "t-value" = 1, "p-value" = 1 )
    names(colhead) <- c(" ", x1_desc, x2_desc, "t-value", "p-value")
    
    out <- df %>% 
        kable(align = "lrccc", booktabs=T) %>% 
        kable_styling(full_width = FALSE, bootstrap_options = "condensed") %>% 
        column_spec(5, width = "2cm")     %>% 
        row_spec(2, extra_css = "border-top: none; border-bottom: solid;") %>% 
        add_header_above(header = colhead, 
                         align = "c", extra_css = "border-top: none; border-bottom: solid thin; vertical-align: middle; ") 
    
    out <- gsub('<th style="border-bottom:hidden" colspan="2"></th>', '<th style="border-bottom:solid thin" colspan="2"></th>', out)
    
    out <- out %>%     
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

