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
    tot <- z %>% filter(Category == "TOTAL")
    oth <- NULL
    if (length(filter(z, Category == "Other")) != 0){
        oth <- z %>% filter(Category == "Other")
        z <- z %>% filter(Category != "Other" & Category != "TOTAL") %>% arrange(desc(Frequency))
    }
    else{
        z <- z %>% filter(Category != "TOTAL") %>% arrange(desc(Frequency))
    }
    z <- rbind(z, oth, tot)
    
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
                         align = "l", extra_css = "border-top: solid; border-bottom: double;")
    return(x)
}

pq_chisq <- function(varlist, tname = "Chi-Square Results"){
  newdf <- build_csq(varlist)
  varhead <- c("x" = 2, names(varlist[2]) = 5)
  names(varhead) <- c(" ", names(varlist[2]))
  tab_width <- length(newdf)
  titlehead <- c(tname = tab_width)
  names(titlehead) <- tname
  al <- c("c", "l", rep("c", length(newdf) - 2))
  out <- newdf2 %>% 
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
    row_spec(nrow(newdf2), bold = T, extra_css = "border-bottom: solid; border-top: double; font-size: small;") 
  return(out)
}

tb_spread <- function(x, xstat){
  x <- x %>% spread(partyid_fct, xstat)
  x <- x %>% mutate(stat = xstat) %>% select(1, last_col(), 2:length(x))
  return(x)
}