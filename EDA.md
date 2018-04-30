Exploratory Data Analysis
================

``` r
bank=read.csv('D://bank.CSV')
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(rmarkdown)
```

    ## Warning: package 'rmarkdown' was built under R version 3.4.3

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.4.3

``` r
dim(bank)
```

    ## [1] 45211    17

data availability
-----------------

``` r
x=c()
for(i in 1:ncol(bank))
{
  x=append(x,length(which(is.na(bank[i]))))
}
x
```

    ##  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

``` r
x=c()
for(i in 1:ncol(bank))
{
  nulls_count=length(which(is.na(bank[i])))
  x=append(x,nulls_count)
  print(nulls_count)
}
```

    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0
    ## [1] 0

``` r
x=c()
num.na=colSums(is.na(bank))
num.na
```

    ##       age       job   marital education   default   balance   housing 
    ##         0         0         0         0         0         0         0 
    ##      loan   contact       day     month  duration  campaign     pdays 
    ##         0         0         0         0         0         0         0 
    ##  previous  poutcome         y 
    ##         0         0         0

``` r
num.na=colSums(!is.na(bank))*100/nrow(bank)
num.na
```

    ##       age       job   marital education   default   balance   housing 
    ##       100       100       100       100       100       100       100 
    ##      loan   contact       day     month  duration  campaign     pdays 
    ##       100       100       100       100       100       100       100 
    ##  previous  poutcome         y 
    ##       100       100       100

### frequency distribution of each dimension

``` r
freq=bank %>% group_by(job) %>% summarise(job_count=n())

freq
```

    ## # A tibble: 12 x 2
    ##              job job_count
    ##           <fctr>     <int>
    ##  1        admin.      5171
    ##  2   blue-collar      9732
    ##  3  entrepreneur      1487
    ##  4     housemaid      1240
    ##  5    management      9458
    ##  6       retired      2264
    ##  7 self-employed      1579
    ##  8      services      4154
    ##  9       student       938
    ## 10    technician      7597
    ## 11    unemployed      1303
    ## 12       unknown       288

``` r
data=bank %>% group_by(marital) %>% summarise(marital_count=n())

data
```

    ## # A tibble: 3 x 2
    ##    marital marital_count
    ##     <fctr>         <int>
    ## 1 divorced          5207
    ## 2  married         27214
    ## 3   single         12790

bank
----

``` r
freq=bank %>% group_by(job) %>% summarise(job_count=n())

freq
```

    ## # A tibble: 12 x 2
    ##              job job_count
    ##           <fctr>     <int>
    ##  1        admin.      5171
    ##  2   blue-collar      9732
    ##  3  entrepreneur      1487
    ##  4     housemaid      1240
    ##  5    management      9458
    ##  6       retired      2264
    ##  7 self-employed      1579
    ##  8      services      4154
    ##  9       student       938
    ## 10    technician      7597
    ## 11    unemployed      1303
    ## 12       unknown       288

``` r
ggplot(freq, aes(x=job,y=job_count))+ geom_bar(stat='identity') 
```

![](EDA_files/figure-markdown_github/unnamed-chunk-9-1.png)

education
---------

``` r
freq=bank %>% group_by(education) %>% summarise(education_count=n())

freq
```

    ## # A tibble: 4 x 2
    ##   education education_count
    ##      <fctr>           <int>
    ## 1   primary            6851
    ## 2 secondary           23202
    ## 3  tertiary           13301
    ## 4   unknown            1857

``` r
ggplot(freq, aes(x=education,y=education_count))+ geom_bar(stat='identity') 
```

![](EDA_files/figure-markdown_github/unnamed-chunk-10-1.png)

age
---

``` r
freq=bank %>% group_by(age) %>% summarise(age_count=n())

freq
```

    ## # A tibble: 77 x 2
    ##      age age_count
    ##    <int>     <int>
    ##  1    18        12
    ##  2    19        35
    ##  3    20        50
    ##  4    21        79
    ##  5    22       129
    ##  6    23       202
    ##  7    24       302
    ##  8    25       527
    ##  9    26       805
    ## 10    27       909
    ## # ... with 67 more rows

``` r
ggplot(freq, aes(x=age,y=age_count))+ geom_bar(stat='identity') 
```

![](EDA_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
col_name='education'
education_percentage=bank %>% group_by (a=get(col_name)) %>% summarise(count=n()*100/nrow(bank))
education_percentage
```

    ## # A tibble: 4 x 2
    ##           a     count
    ##      <fctr>     <dbl>
    ## 1   primary 15.153392
    ## 2 secondary 51.319369
    ## 3  tertiary 29.419831
    ## 4   unknown  4.107407

``` r
ggplot(education_percentage, aes(x=a,y=count))+ geom_bar(stat='identity') 
```

![](EDA_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
col_name='job'
job_percentage=bank %>% group_by (a=get(col_name)) %>% summarise(count=n()*100/nrow(bank))
job_percentage
```

    ## # A tibble: 12 x 2
    ##                a      count
    ##           <fctr>      <dbl>
    ##  1        admin. 11.4374820
    ##  2   blue-collar 21.5257349
    ##  3  entrepreneur  3.2890226
    ##  4     housemaid  2.7426954
    ##  5    management 20.9196877
    ##  6       retired  5.0076309
    ##  7 self-employed  3.4925129
    ##  8      services  9.1880295
    ##  9       student  2.0747163
    ## 10    technician 16.8034328
    ## 11    unemployed  2.8820420
    ## 12       unknown  0.6370131

``` r
ggplot(job_percentage, aes(x=a,y=count))+ geom_bar(stat='identity') +coord_flip()
```

![](EDA_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
col_name='marital'
contact_percentage=bank %>% group_by (a=get(col_name)) %>% summarise(count=n()*100/nrow(bank))
contact_percentage
```

    ## # A tibble: 3 x 2
    ##          a    count
    ##     <fctr>    <dbl>
    ## 1 divorced 11.51711
    ## 2  married 60.19332
    ## 3   single 28.28958

``` r
ggplot(contact_percentage, aes(x=a,y=count))+ geom_bar(stat='identity') +coord_flip()
```

![](EDA_files/figure-markdown_github/unnamed-chunk-14-1.png) \#\# frequency distribution of each numerical column(ideal is between not group) \#\# histogram(skewness)

``` r
quantile(bank$age)
```

    ##   0%  25%  50%  75% 100% 
    ##   18   33   39   48   95

``` r
sum(bank$age>70.5)*100/nrow(bank)
```

    ## [1] 1.077171

``` r
rows_above_70=filter(bank,age>70.5)
age_outlier_perc=nrow(rows_above_70)/nrow(bank) *100
age_outlier_perc
```

    ## [1] 1.077171

histogram
=========

``` r
hist(bank$balance)
```

![](EDA_files/figure-markdown_github/unnamed-chunk-18-1.png)

boxplot
=======

``` r
boxplot(bank$balance)
```

![](EDA_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
summary(bank$balance)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   -8019      72     448    1362    1428  102127

``` r
quantile(bank$balance)
```

    ##     0%    25%    50%    75%   100% 
    ##  -8019     72    448   1428 102127

``` r
boxplot(bank$balance)
```

![](EDA_files/figure-markdown_github/unnamed-chunk-21-1.png)

``` r
iqr=1428-72
outlier_upper=1428+1.5*iqr
outlier_perc=sum(bank$balance > outlier_upper)/nrow(bank)*100
outlier_perc
```

    ## [1] 10.42224

``` r
job_balance=bank %>% group_by(job) %>% summarise(avg_balance=mean(balance),count=n()) %>% arrange(-count)
job_balance
```

    ## # A tibble: 12 x 3
    ##              job avg_balance count
    ##           <fctr>       <dbl> <int>
    ##  1   blue-collar   1078.8267  9732
    ##  2    management   1763.6168  9458
    ##  3    technician   1252.6321  7597
    ##  4        admin.   1135.8389  5171
    ##  5      services    997.0881  4154
    ##  6       retired   1984.2151  2264
    ##  7 self-employed   1647.9709  1579
    ##  8  entrepreneur   1521.4701  1487
    ##  9    unemployed   1521.7460  1303
    ## 10     housemaid   1392.3952  1240
    ## 11       student   1388.0608   938
    ## 12       unknown   1772.3576   288

``` r
ggplot(job_balance, aes(x=job,y=count,fill=-avg_balance))+ geom_bar(stat='identity')
```

![](EDA_files/figure-markdown_github/unnamed-chunk-22-1.png)

creating bins for ages
======================

``` r
bank$age_group=cut(bank$age,breaks=c(0,10,25,40,60,90,Inf),labels=c('0-10', '10-25', '25-40','40-60', '60-90','90+'))

grouping=bank %>% group_by(age_group)%>% summarise(count=n())
grouping
```

    ## # A tibble: 5 x 2
    ##   age_group count
    ##      <fctr> <int>
    ## 1     10-25  1336
    ## 2     25-40 23381
    ## 3     40-60 19306
    ## 4     60-90  1181
    ## 5       90+     7
