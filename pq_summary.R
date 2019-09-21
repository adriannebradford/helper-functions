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

tb_spread <- function(x, xstat){
  x <- x %>% spread(x2, xstat)
  x <- x %>% mutate(stat = xstat) %>% select(1, last_col(), 2:length(x))
  return(x)
}



