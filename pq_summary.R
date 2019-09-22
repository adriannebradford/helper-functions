reorder_fct <- function(z){
  cn1 <-colnames(z)
  cn2 <-colnames(z)
  cn2[1] <- "x1"
  cn2[2] <- "x2"
  colnames(z) <- cn2
  z <- z %>% mutate(x2 = factor(x2)) %>% mutate(x1 = factor(x1))
  if ("Other" %in% levels(z$x1)){
    z <- z %>% mutate(x1 = fct_relevel(x1, "Other", after = Inf))
  }
  if ("TOTAL" %in% levels(z$x1)){
    z <- z %>% mutate(x1 = fct_relevel(x1, "TOTAL", after = Inf))
  }
  # print(levels(z$x2))
  if ("Other" %in% levels(z$x2)){
    z <- z %>% mutate(x2 = fct_relevel(x2, "Other", after = Inf))
  }
  z <- z %>% mutate(x2 = fct_relevel(x2, "TOTAL", after = Inf)) %>% 
    arrange(x1,x2)
  colnames(z) <- cn1
  
  return(z)
}


build_csq <- function(varlist) {
  var1 <- names(varlist[1])
  #print(levels(varlist[[1]]))
  tbl <- table(varlist[[1]], varlist[[2]], useNA = "no")
  pct <- prop.table(tbl)
  pct_df <- data.frame(addmargins(pct))
  df <- data.frame(addmargins(tbl))
  #print(tbl)
  csq_test <- chisq.test(tbl)
  #print(csq_test)
  cell_csq <- (csq_test$observed - csq_test$expected)^2 / csq_test$expected
  cell_df <- data.frame(addmargins(cell_csq))
  
  df <- cbind(df, pct_df[3], cell_df[3])
  colnames(df) <- c("x1", "x2", "Frequency", "Percent", "ChiSq")
  df <- df %>% mutate(Frequency = format(Frequency, big.mark=",")) %>% 
    mutate(ChiSq = format(ChiSq, big.mark=",", digits = 1, scientific = FALSE)) %>%
    mutate(Percent = sprintf("%.1f%%", 100*Percent)) 
  
  df <- df %>% mutate(x1 = if_else(x1 == "Sum", "TOTAL", as.character(x1))) %>% 
    mutate(x2 = if_else(x2 == "Sum", "TOTAL", 
                        as.character(x2)))
  df <- reorder_fct(df)
  #print(df)
  count <- tb_spread(df[1:3], "Frequency")
  percent <- tb_spread(df[c(1:2,4)], "Percent")
  chi <- tb_spread(df[c(1:2,5)], "ChiSq")
  
  newdf <- rbind(count,percent,chi) %>%
    mutate(stat = as.factor(stat)) %>%
    mutate(stat = fct_relevel(stat, "ChiSq", after = 2)) %>% 
    arrange(x1, stat)
  colnames(newdf) <- c(var1,"" , colnames(newdf)[3:length(newdf)])
  #print(newdf[nrow(newdf), ncol(newdf)])
  csqstat <- paste0("Chi-Square = " , newdf[nrow(newdf), ncol(newdf)], ",")
  pval <- ifelse(csq_test$p.value > 0.001, 
                 paste("=", format(csq_test$p.value, digits = 3, scientific = FALSE)), 
                 "< 0.001")
  new_row <- c(csqstat, paste("p-value", pval), rep("", ncol(newdf)-2))
  #print(new_row)
  newdf[1] <- as.character(newdf[[1]])
  newdf[2] <- as.character(newdf[[2]])
  newdf2 <- rbind(newdf,new_row)
  #print(newdf2)
  return(newdf2)
}



vartab <- function(var, vname){
    x <- table(var)
    y <- prop.table(x)
    x <- format(addmargins(x), big.mark=",")
    y <- sprintf("%.1f%%", 100*addmargins(y))
    n <- rep(vname, length(x))
    z <- data.frame(cbind(x,y))
    z <- tibble::rownames_to_column(z, "Category")
    z[nrow(z),]$Category <- "TOTAL"
    z <- cbind(n,z)
    cn <- c("Variable","Category", "Frequency", "Percent")
    colnames(z) <- cn

    z <- reorder_fct(z)
    return(z)
    
}

collect_tab <- function(varlist){
    df <- data.frame(Variable = NA, Category = NA, Frequency = NA, Percent = NA)
    for(i in 1:length(varlist)){
        x <- vartab(varlist[i], names(varlist[i]))
        df <- rbind(df,x)
    }
    df <- na.omit(df)
    rownames(df) <- c()
    return(df)
}

pq_summary <- function(tname = "Summary Statistics", varlist){
  myHeader <- c(tname = 4)
  names(myHeader) <- tname
    sumtab <- collect_tab(varlist)
    x <- sumtab %>% kable(align="clrr") %>% 
        kable_styling(full_width = FALSE) %>% 
        row_spec(0, extra_css = "border-bottom: solid thin;") %>% 
        row_spec(which(sumtab$Category == "TOTAL"), bold = T, extra_css = "border-bottom: solid;") %>%
        column_spec(1, bold=TRUE, extra_css = "border-bottom: solid;") %>%         
        collapse_rows(columns = 1, valign = "middle") %>% 
        add_header_above(header = myHeader, 
                         align = "l", extra_css = "border-top: solid; border-bottom: double;") %>% 
        row_spec(1:nrow(sumtab), extra_css = "line-height: 6px;")
    return(x)
}

tb_spread <- function(x, xstat){
  x <- x %>% spread(x2, xstat)
  x <- x %>% mutate(stat = xstat) %>% select(1, last_col(), 2:length(x))
  return(x)
}



pq_chisq <- function(varlist, tname = "Chi-Square Results"){
  
  newdf <- build_csq(varlist)
  topname <- names(varlist)[2]
  varhead <- c("x" = 2, topname = 5)
  names(varhead) <- c(" ", topname)
  tab_width <- length(newdf)
  titlehead <- c(tname = tab_width)
  names(titlehead) <- tname
  al <- c("c", "l", rep("c", length(newdf) - 2))
  out <- newdf %>% 
    kable(align = al, booktabs=T) %>% 
    kable_styling(full_width = FALSE, bootstrap_options = "condensed") %>% 
    row_spec(0, extra_css = "border-bottom: solid thin;") %>%
    column_spec(2, extra_css = "font-size: xx-small;") %>% 
    add_header_above(header = varhead, 
                     align = "c", extra_css = "border-top: solid thin; border-bottom: solid thin;") %>% 
    add_header_above(header = titlehead, 
                     align = "l", extra_css = "border-top: solid; border-bottom: double;") %>% 
    row_spec(which(newdf[2] == "ChiSq"), 
             extra_css = "border-bottom: solid thin; border-top: initial; vertical-align: middle; line-height: 6px;") %>%
    row_spec(which(newdf[2] == "Frequency" | newdf[2] == "Percent"), 
             extra_css = "border-top: initial; vertical-align: middle; line-height: 6px;") %>%
    column_spec(1, bold=TRUE, extra_css = "border-bottom: solid thin;") %>%         
    collapse_rows(columns = 1, valign = "middle")  %>% 
    row_spec(nrow(newdf), bold = T, extra_css = "border-bottom: solid; border-top: double; font-size: small;") 
  return(out)
}

