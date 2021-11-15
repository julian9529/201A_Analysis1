201A Analysis Project
================

### 201A Analysis Project

### Julián Ponce

``` r
senic_data =
  read_csv (file = "./data/SENIC.csv")%>%
janitor::clean_names()
```

    ## Parsed with column specification:
    ## cols(
    ##   id = col_double(),
    ##   length = col_double(),
    ##   age = col_double(),
    ##   risk = col_double(),
    ##   culture = col_double(),
    ##   xray = col_double(),
    ##   beds = col_double(),
    ##   msch = col_double(),
    ##   region = col_double(),
    ##   census = col_double(),
    ##   nurses = col_double(),
    ##   svcs = col_double()
    ## )

``` r
senic_data =
  senic_data %>%
  #c is to include only these while -c is remove these
  subset(select = c(id, length, age,risk, beds,region,census ))
#exclude data for culture, xrays, msch, nurses, and svcs 
```

``` r
sapply(senic_data, class)
```

    ##        id    length       age      risk      beds    region    census 
    ## "numeric" "numeric" "numeric" "numeric" "numeric" "numeric" "numeric"

Create a new chunk comman +opton+i Change region from numeric to factor

``` r
senic_data = senic_data%>%
  mutate(region = as.factor(region), 
        id = as.factor(id),
        age = as.numeric(age),
        length=as.numeric(length))%>%
  mutate(risk=(risk*.100))
  
senic_data1 =
  senic_data %>%
  #c is to include only these while -c is remove these
  subset(select = c(length, age,risk, beds,census   ))
#exclude data for culture, xrays, msch, nurses, and svcs 
skimr::skim(senic_data1) 
```

|                                                  |              |
| :----------------------------------------------- | :----------- |
| Name                                             | senic\_data1 |
| Number of rows                                   | 113          |
| Number of columns                                | 5            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |              |
| Column type frequency:                           |              |
| numeric                                          | 5            |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |              |
| Group variables                                  | None         |

Data summary

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |   mean |     sd |    p0 |    p25 |    p50 |    p75 |   p100 | hist  |
| :------------- | ---------: | -------------: | -----: | -----: | ----: | -----: | -----: | -----: | -----: | :---- |
| length         |          0 |              1 |   9.65 |   1.91 |  6.70 |   8.34 |   9.42 |  10.47 |  19.56 | ▇▇▁▁▁ |
| age            |          0 |              1 |  53.23 |   4.46 | 38.80 |  50.90 |  53.20 |  56.20 |  65.90 | ▁▂▇▃▁ |
| risk           |          0 |              1 |   0.44 |   0.13 |  0.13 |   0.37 |   0.44 |   0.52 |   0.78 | ▂▃▇▃▁ |
| beds           |          0 |              1 | 252.17 | 192.84 | 29.00 | 106.00 | 186.00 | 312.00 | 835.00 | ▇▅▂▁▁ |
| census         |          0 |              1 | 191.37 | 153.76 | 20.00 |  68.00 | 143.00 | 252.00 | 791.00 | ▇▃▂▁▁ |

\#Library Janitor needed for tabyl

``` r
senic_data %>%
tabyl(region)%>%
knitr::kable(digits=3)
```

| region |  n | percent |
| :----- | -: | ------: |
| 1      | 28 |   0.248 |
| 2      | 32 |   0.283 |
| 3      | 37 |   0.327 |
| 4      | 16 |   0.142 |

\#1\_Univariate Histograms in case needed but i think SKIMR IS okay

``` r
histogram_age =
ggplot(senic_data, aes(x = age)) + 
  geom_histogram(bins =11, color="darkblue", fill="lightblue") + labs (title = "Figure 1.Age Distribution", x="Age", y ="Count") 

histogram_length=
ggplot(senic_data, aes(x = length)) + 
   geom_histogram(bins =11, color="darkblue", fill="lightblue") + labs (title = "Figure 2.Length of Stay Distribution", x="Length of Stay(Days)", y ="Count") 

histogram_risk =
ggplot(senic_data, aes(x = risk)) + 
   geom_histogram(bins =11, color="darkblue", fill="lightblue") + labs (title = "Figure 3.Infection Distribution", x="Infection Risk(Years)", y ="Count") 

histogram_beds =
ggplot(senic_data, aes(x = beds)) + 
   geom_histogram(bins =11, color="darkblue", fill="lightblue") + labs (title = "Figure 4. Bed Distribution", x="Number of Beds", y ="Count") 

histogram_census =
ggplot(senic_data, aes(x = census)) + 
     geom_histogram(bins =11, color="darkblue", fill="lightblue") + labs (title = "Figure 5.Census Distribution", x="Average Number of Hospital Patients Per Day", y ="Count") 

histogram_age+histogram_length
```

<img src="Final-draft_files/figure-gfm/histograms 1-1.png" width="90%" />

``` r
histogram_risk+histogram_beds
```

<img src="Final-draft_files/figure-gfm/histograms 1-2.png" width="90%" />

``` r
histogram_census
```

<img src="Final-draft_files/figure-gfm/histograms 1-3.png" width="90%" />

\#2\_Correlations
-<https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/>

``` r
cor.test(senic_data$age, senic_data$length, method="pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  senic_data$age and senic_data$length
    ## t = 2.0268, df = 111, p-value = 0.04508
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.004335447 0.361044064
    ## sample estimates:
    ##      cor 
    ## 0.188914

``` r
cor.test(senic_data$age, senic_data$risk, method="pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  senic_data$age and senic_data$risk
    ## t = 0.011517, df = 111, p-value = 0.9908
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1836737  0.1857855
    ## sample estimates:
    ##         cor 
    ## 0.001093166

``` r
ggplot(senic_data) +
  aes(x = age, y = length) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()
```

<img src="Final-draft_files/figure-gfm/correlations-1.png" width="90%" />

``` r
#corr matrix, run corrplot lib 

corrplot(cor(senic_data1),
  method = "number", 
  type = "upper" # show only upper side 
  )
```

<img src="Final-draft_files/figure-gfm/correlations-2.png" width="90%" />

``` r
#source cormat function to show p values ("http://www.sthda.com/upload/rquery_cormat.r") 
```

``` r
#+++++++++++++++++++++++++
# Computing of correlation matrix
#+++++++++++++++++++++++++
# Required package : corrplot
# x : matrix
# type: possible values are "lower" (default), "upper", "full" or "flatten";
  #display lower or upper triangular of the matrix, full  or flatten matrix.
# graph : if TRUE, a correlogram or heatmap is plotted
# graphType : possible values are "correlogram" or "heatmap"
# col: colors to use for the correlogram
# ... : Further arguments to be passed to cor or cor.test function
# Result is a list including the following components :
  # r : correlation matrix, p :  p-values
  # sym : Symbolic number coding of the correlation matrix
rquery.cormat<-function(x,
                        type=c('lower', 'upper', 'full', 'flatten'),
                        graph=TRUE,
                        graphType=c("correlogram", "heatmap"),
                        col=NULL, ...)
{
  library(corrplot)
  # Helper functions
  #+++++++++++++++++
  # Compute the matrix of correlation p-values
  cor.pmat <- function(x, ...) {
    mat <- as.matrix(x)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # Get lower triangle of the matrix
  getLower.tri<-function(mat){
    upper<-mat
    upper[upper.tri(mat)]<-""
    mat<-as.data.frame(upper)
    mat
  }
  # Get upper triangle of the matrix
  getUpper.tri<-function(mat){
    lt<-mat
    lt[lower.tri(mat)]<-""
    mat<-as.data.frame(lt)
    mat
  }
  # Get flatten matrix
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  # Define color
  if (is.null(col)) {
    col <- colorRampPalette(
            c("#67001F", "#B2182B", "#D6604D", "#F4A582",
              "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
             "#4393C3", "#2166AC", "#053061"))(200)
    col<-rev(col)
  }
  
  # Correlation matrix
  cormat<-signif(cor(x, use = "complete.obs", ...),2)
  pmat<-signif(cor.pmat(x, ...),2)
  # Reorder correlation matrix
  ord<-corrMatOrder(cormat, order="hclust")
  cormat<-cormat[ord, ord]
  pmat<-pmat[ord, ord]
  # Replace correlation coeff by symbols
  sym<-symnum(cormat, abbr.colnames=FALSE)
  # Correlogram
  if(graph & graphType[1]=="correlogram"){
    corrplot(cormat, type=ifelse(type[1]=="flatten", "lower", type[1]),
             tl.col="black", tl.srt=45,col=col,...)
  }
  else if(graphType[1]=="heatmap")
    heatmap(cormat, col=col, symm=TRUE)
  # Get lower/upper triangle
  if(type[1]=="lower"){
    cormat<-getLower.tri(cormat)
    pmat<-getLower.tri(pmat)
  }
  else if(type[1]=="upper"){
    cormat<-getUpper.tri(cormat)
    pmat<-getUpper.tri(pmat)
    sym=t(sym)
  }
  else if(type[1]=="flatten"){
    cormat<-flattenCorrMatrix(cormat, pmat)
    pmat=NULL
    sym=NULL
  }
  list(r=cormat, p=pmat, sym=sym)
}

#need to run all code above for cormat to work
rquery.cormat(senic_data1, type="flatten", graph=FALSE)  
```

    ## $r
    ##       row column     cor       p
    ## 1     age   beds -0.0590 5.4e-01
    ## 2     age census -0.0550 5.6e-01
    ## 3    beds census  0.9800 6.9e-81
    ## 4     age length  0.1900 4.5e-02
    ## 5    beds length  0.4100 6.8e-06
    ## 6  census length  0.4700 1.1e-07
    ## 7     age   risk  0.0011 9.9e-01
    ## 8    beds   risk  0.3600 9.1e-05
    ## 9  census   risk  0.3800 3.1e-05
    ## 10 length   risk  0.5300 1.2e-09
    ## 
    ## $p
    ## NULL
    ## 
    ## $sym
    ## NULL

``` r
ols_test_bartlett(senic_data, 'length','region')
```

    ## 
    ##     Bartlett's Test of Homogenity of Variances    
    ## ------------------------------------------------
    ## Ho: Variances are equal across groups
    ## Ha: Variances are unequal for atleast two groups
    ## 
    ##           Data           
    ##  ------------------------
    ##  Variables: length region 
    ## 
    ##          Test Summary          
    ##  ------------------------------
    ##  DF            =    1 
    ##  Chi2          =    42.66464 
    ##  Prob > Chi2   =    6.49763e-11

``` r
one.way <- oneway.test(length ~ region, data = senic_data, var.equal = TRUE )

one.way <- aov(length ~ region, data = senic_data, )


one.way %>%  
    broom::tidy()%>% 
knitr::kable(digits=3)
```

| term      |  df |   sumsq | meansq | statistic | p.value |
| :-------- | --: | ------: | -----: | --------: | ------: |
| region    |   3 | 103.554 | 34.518 |    12.309 |       0 |
| Residuals | 109 | 305.656 |  2.804 |        NA |      NA |

``` r
TukeyHSD(one.way) %>% 
    broom::tidy()%>% 
knitr::kable(digits=3)
```

| term   | contrast | null.value | estimate | conf.low | conf.high | adj.p.value |
| :----- | :------- | ---------: | -------: | -------: | --------: | ----------: |
| region | 2-1      |          0 |  \-1.405 |  \-2.536 |   \-0.275 |       0.008 |
| region | 3-1      |          0 |  \-1.898 |  \-2.992 |   \-0.803 |       0.000 |
| region | 4-1      |          0 |  \-2.975 |  \-4.344 |   \-1.606 |       0.000 |
| region | 3-2      |          0 |  \-0.492 |  \-1.547 |     0.563 |       0.617 |
| region | 4-2      |          0 |  \-1.570 |  \-2.907 |   \-0.232 |       0.015 |
| region | 4-3      |          0 |  \-1.078 |  \-2.385 |     0.230 |       0.144 |

## Regression \#3

``` r
linearmod = lm(length ~ age , data=senic_data) 
# build linear regression model on full data
linearmod1 = lm(length ~ beds, data=senic_data)
linearmod2 = lm(length ~ census, data=senic_data) 
linearmod3 = lm(length ~risk , data=senic_data) 

linearmod %>%  
    broom::tidy()%>% 
knitr::kable(digits=3)
```

| term        | estimate | std.error | statistic | p.value |
| :---------- | -------: | --------: | --------: | ------: |
| (Intercept) |    5.340 |     2.133 |     2.503 |   0.014 |
| age         |    0.081 |     0.040 |     2.027 |   0.045 |

``` r
linearmod1 %>%  
    broom::tidy()%>% 
knitr::kable(digits=3)
```

| term        | estimate | std.error | statistic | p.value |
| :---------- | -------: | --------: | --------: | ------: |
| (Intercept) |    8.625 |     0.272 |    31.704 |       0 |
| beds        |    0.004 |     0.001 |     4.726 |       0 |

``` r
linearmod2 %>%  
    broom::tidy()%>% 
knitr::kable(digits=3)
```

| term        | estimate | std.error | statistic | p.value |
| :---------- | -------: | --------: | --------: | ------: |
| (Intercept) |    8.521 |     0.255 |    33.464 |       0 |
| census      |    0.006 |     0.001 |     5.670 |       0 |

``` r
linearmod3 %>%  
    broom::tidy()%>% 
knitr::kable(digits=3)
```

| term        | estimate | std.error | statistic | p.value |
| :---------- | -------: | --------: | --------: | ------: |
| (Intercept) |    6.337 |     0.521 |    12.156 |       0 |
| risk        |    7.604 |     1.144 |     6.645 |       0 |

``` r
 senic_region1 =
senic_data %>% 
  filter(region == "1")



linearmod = lm(length ~ risk , data=senic_region1)  # build linear regression model on full data

confint(lm(length ~ risk , data=senic_region1))
```

    ##                2.5 %    97.5 %
    ## (Intercept) 1.279450  7.796392
    ## risk        6.984451 19.970467

``` r
linearmod %>%  
    broom::tidy()%>% 
knitr::kable(digits=3)
```

| term        | estimate | std.error | statistic | p.value |
| :---------- | -------: | --------: | --------: | ------: |
| (Intercept) |    4.538 |     1.585 |     2.863 |   0.008 |
| risk        |   13.477 |     3.159 |     4.267 |   0.000 |

``` r
 senic_region2 =
senic_data %>% 
  filter(region == "2")

linearmod = lm(length ~  risk , data=senic_region2) 

confint(lm(length ~ risk , data=senic_region2))
```

    ##                2.5 %   97.5 %
    ## (Intercept) 6.280526 8.840486
    ## risk        2.041384 7.622031

``` r
linearmod %>%  
    broom::tidy()%>% 
knitr::kable(digits=3)
```

| term        | estimate | std.error | statistic | p.value |
| :---------- | -------: | --------: | --------: | ------: |
| (Intercept) |    7.561 |     0.627 |    12.063 |   0.000 |
| risk        |    4.832 |     1.366 |     3.536 |   0.001 |

``` r
 senic_region3 =
senic_data %>% 
  filter(region == "3")

 confint(lm(length ~ risk , data=senic_region3))
```

    ##                2.5 %   97.5 %
    ## (Intercept) 6.189059 8.069615
    ## risk        3.002664 7.498990

``` r
linearmod = lm(length ~  risk , data=senic_region3)  # build linear regression model on full data

linearmod %>%  
    broom::tidy()%>% 
knitr::kable(digits=3)
```

| term        | estimate | std.error | statistic | p.value |
| :---------- | -------: | --------: | --------: | ------: |
| (Intercept) |    7.129 |     0.463 |    15.393 |       0 |
| risk        |    5.251 |     1.107 |     4.742 |       0 |

``` r
senic_region4 =
senic_data %>% 
  filter(region == "4")

linearmod = lm(length ~  risk , data=senic_region4)  # build linear regression model on full data

  confint(lm(length ~ risk , data=senic_region4))
```

    ##                 2.5 %    97.5 %
    ## (Intercept)  5.110812 10.965285
    ## risk        -6.386564  6.732136

``` r
linearmod %>%  
    broom::tidy()%>% 
knitr::kable(digits=3)
```

| term        | estimate | std.error | statistic | p.value |
| :---------- | -------: | --------: | --------: | ------: |
| (Intercept) |    8.038 |     1.365 |     5.889 |   0.000 |
| risk        |    0.173 |     3.058 |     0.056 |   0.956 |

\#\#regression on X6

``` r
senic_data=senic_data %>% 
mutate(freebeds= beds-census)

linearmod = lm(length ~  freebeds , data=senic_data)  
linearmod %>%  
    broom::tidy()%>% 
knitr::kable(digits=3)
```

| term        | estimate | std.error | statistic | p.value |
| :---------- | -------: | --------: | --------: | ------: |
| (Intercept) |    9.383 |     0.278 |    33.732 |   0.000 |
| freebeds    |    0.004 |     0.003 |     1.248 |   0.215 |
