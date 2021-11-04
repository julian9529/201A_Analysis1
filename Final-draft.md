201A Analysis Project
================

### Project Title

201A Analysis Project \#\#\# Members:

Julián Ponce

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
skimr::skim(senic_data)
```

|                                                  |             |
| :----------------------------------------------- | :---------- |
| Name                                             | senic\_data |
| Number of rows                                   | 113         |
| Number of columns                                | 12          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |             |
| Column type frequency:                           |             |
| numeric                                          | 12          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |             |
| Group variables                                  | None        |

Data summary

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |   mean |     sd |   p0 |    p25 |    p50 |    p75 |   p100 | hist  |
| :------------- | ---------: | -------------: | -----: | -----: | ---: | -----: | -----: | -----: | -----: | :---- |
| id             |          0 |              1 |  57.00 |  32.76 |  1.0 |  29.00 |  57.00 |  85.00 | 113.00 | ▇▇▇▇▇ |
| length         |          0 |              1 |   9.65 |   1.91 |  6.7 |   8.34 |   9.42 |  10.47 |  19.56 | ▇▇▁▁▁ |
| age            |          0 |              1 |  53.23 |   4.46 | 38.8 |  50.90 |  53.20 |  56.20 |  65.90 | ▁▂▇▃▁ |
| risk           |          0 |              1 |   4.35 |   1.34 |  1.3 |   3.70 |   4.40 |   5.20 |   7.80 | ▂▃▇▃▁ |
| culture        |          0 |              1 |  15.79 |  10.23 |  1.6 |   8.40 |  14.10 |  20.30 |  60.50 | ▇▆▂▁▁ |
| xray           |          0 |              1 |  81.63 |  19.36 | 39.6 |  69.50 |  82.30 |  94.10 | 133.50 | ▃▅▇▃▁ |
| beds           |          0 |              1 | 252.17 | 192.84 | 29.0 | 106.00 | 186.00 | 312.00 | 835.00 | ▇▅▂▁▁ |
| msch           |          0 |              1 |   1.85 |   0.36 |  1.0 |   2.00 |   2.00 |   2.00 |   2.00 | ▂▁▁▁▇ |
| region         |          0 |              1 |   2.36 |   1.01 |  1.0 |   2.00 |   2.00 |   3.00 |   4.00 | ▆▇▁▇▃ |
| census         |          0 |              1 | 191.37 | 153.76 | 20.0 |  68.00 | 143.00 | 252.00 | 791.00 | ▇▃▂▁▁ |
| nurses         |          0 |              1 | 173.25 | 139.27 | 14.0 |  66.00 | 132.00 | 218.00 | 656.00 | ▇▅▂▁▁ |
| svcs           |          0 |              1 |  43.16 |  15.20 |  5.7 |  31.40 |  42.90 |  54.30 |  80.00 | ▁▆▇▆▂ |

``` r
senic_data =
  senic_data %>%
  subset(select = -c(id, length, age,risk, beds,region, census   ))
#exclude data for culture, xrays, msch, nurses, and svcs 
```
