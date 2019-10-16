## EXAMPLES USE ANES DATA - FILES ON ELMS

#read in functions from github
#summary functions from chi-square - need to use the cat variable summary for IV
devtools::source_url("https://raw.githubusercontent.com/adriannebradford/inst314-helper-functions/master/pq_summary.R")

#new summary functions for numerical var summary, t-test, paired t-test
devtools::source_url("https://raw.githubusercontent.com/adriannebradford/inst314-helper-functions/master/pq_ttest.R")

# read data
anes <- readRDS("anes.rds")
anes2 <- readRDS("anes2.rds")

#take a peek
head(anes)
head(anes2)


## example of numerical summary, one variable
## The pq_sum_num function has 4 arguments:
### data = your dataframe where your variable(s) are located
### varnames = a vector of varnames of variables you want to summarize in table
             # as strings.
### vardisc = a character vector of descriptive names to make your table PQ
### tname = a character string to put at the top of your table

pq_sum_num(data = anes, 
           varnames = "ft_sci", 
           vardisc = "Feeling Thermometer - Scientists", 
           tname = "Summary Table - Numerical Variables")

## example of numerical summary, two variables
pq_sum_num(data = anes2, 
           varnames = c("ft_pre_dem", "ft_post_dem"), 
           vardisc = c("FT - Clinton Pre-election", "FT - Clinton Post-election"), 
           tname = "Summary Table - Two Numerical Variables")

## example of t-test results - two sample
## The basic two sample pq_ttest function takes:
### data = dataframe
### dv = character string of the name of your dv (as in the df)
### iv = character string of the name of your iv (as in the df)
### dv_desc and iv_desc = character string of the descriptive 
                        # names of these variables
### tname = a character string to put at the top of your table
### OPTIONAL = you can also add additional t-test options to the function
             # call like var.equal = TRUE, pooled = TRUE, or 
             # or alternative = "less"

pq_ttest(data = anes, dv = "ft_sci", iv = "educ", 
            dv_desc = "FT Science" , 
            iv_desc = "Level of Education", 
            tname = "Comparing the mean of DV by IV")

## example of paired t-test results
## The paired two sample pq_ttest_paired function takes:
### x1 = vector of first observation (numerical)
### x2 = vector of second observation (numerical)
### x1_desc and x2_desc = character string of the descriptive 
                        # names of these variables/observations
### x_desc = character string of overall description of what 
           # your x values are measuring
### tname = a character string to put at the top of your table
### OPTIONAL = you can also add additional t-test options to the function
# call like var.equal = TRUE, pooled = TRUE, or 
# or alternative = "less"
pq_ttest_paired(anes2$ft_pre_dem, anes2$ft_post_dem, 
                x1_desc = "Pre-election", x2_desc = "Post-election", 
                x_desc = "Feeling Thermometer - Hilary Clinton", 
                tname = "Paired t-test Results")


## you can also pass t-test options to the PQ function - 
## if you use any of these options in your t-test for your report
## you MUST also add them here.

pq_ttest(data = anes, dv = "ft_sci", iv = "educ", 
         dv_desc = "FT Science" , 
         iv_desc = "Level of Education", 
         tname = "Comparing the mean of DV by IV",
         var.equal = TRUE) ## this gets passed to t.test inside the function

pq_ttest_paired(anes2$ft_pre_dem, anes2$ft_post_dem, 
                x1_desc = "Pre-election", x2_desc = "Post-election", 
                x_desc = "Feeling Thermometer - Hilary Clinton", 
                tname = "Paired t-test Results",
                pooled = TRUE) #you don't need to pass paired 
                               #paired is already included

## this includes if you want to do a one-tailed test
pq_ttest(data = anes, dv = "ft_sci", iv = "educ", 
         dv_desc = "FT Science" , 
         iv_desc = "Level of Education", 
         tname = "Comparing the mean of DV by IV",
         alternative = "greater")

## notice the p-value is 1 because mu_2 is less than mu_1.

## also note, the mu_1 and mu_2 is determined 
## by the order of your factor levels.

anes <- anes %>% mutate(educ = factor(educ, levels = c("less than BA", "coll grad or higher")))

pq_ttest(data = anes, dv = "ft_sci", iv = "educ", 
         dv_desc = "FT Science" , 
         iv_desc = "Level of Education", 
         tname = "Comparing the mean of DV by IV")

## we probably want to make your factor levels PQ
## make sure you rename them in order!!!
levels(anes$educ) <- c("Less than College", "College Degree or Higher")
pq_ttest(data = anes, dv = "ft_sci", iv = "educ", 
         dv_desc = "FT Science" , 
         iv_desc = "Level of Education", 
         tname = "Comparing the mean of DV by IV")

## summary of two level IV - same function as chi-square project
varlist <- list("Level of education" = anes$educ)
pq_summary(varlist, tname = "Summary Statistics")
