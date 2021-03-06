# Exploratory-Data-Analysis
This is done on a bank Data with 45k+ rows and 17 variables.
```{r}
bank=read.csv('D://bank.CSV')
library(dplyr)
library(rmarkdown)

library(ggplot2)
```

```{r}
dim(bank)
```

## data availability
```{r}
x=c()
for(i in 1:ncol(bank))
{
  x=append(x,length(which(is.na(bank[i]))))
}
x
```

```{r}
x=c()
for(i in 1:ncol(bank))
{
  nulls_count=length(which(is.na(bank[i])))
  x=append(x,nulls_count)
  print(nulls_count)
}

```

```{r}
x=c()
num.na=colSums(is.na(bank))
num.na

```

```{r}
num.na=colSums(!is.na(bank))*100/nrow(bank)
num.na

```
### frequency distribution of each dimension
```{r}
freq=bank %>% group_by(job) %>% summarise(job_count=n())

freq
```
```{r}
data=bank %>% group_by(marital) %>% summarise(marital_count=n())

data
```

## bank
```{r}
freq=bank %>% group_by(job) %>% summarise(job_count=n())

freq
ggplot(freq, aes(x=job,y=job_count))+ geom_bar(stat='identity') 
```

##education
```{r}

freq=bank %>% group_by(education) %>% summarise(education_count=n())

freq
ggplot(freq, aes(x=education,y=education_count))+ geom_bar(stat='identity') 
```

## age
```{r}

freq=bank %>% group_by(age) %>% summarise(age_count=n())

freq
ggplot(freq, aes(x=age,y=age_count))+ geom_bar(stat='identity') 
```

```{r}
col_name='education'
education_percentage=bank %>% group_by (a=get(col_name)) %>% summarise(count=n()*100/nrow(bank))
education_percentage
ggplot(education_percentage, aes(x=a,y=count))+ geom_bar(stat='identity') 

```
```{r}
col_name='job'
job_percentage=bank %>% group_by (a=get(col_name)) %>% summarise(count=n()*100/nrow(bank))
job_percentage
ggplot(job_percentage, aes(x=a,y=count))+ geom_bar(stat='identity') +coord_flip()

```
```{r}
col_name='marital'
contact_percentage=bank %>% group_by (a=get(col_name)) %>% summarise(count=n()*100/nrow(bank))
contact_percentage
ggplot(contact_percentage, aes(x=a,y=count))+ geom_bar(stat='identity') +coord_flip()

```
## frequency distribution of each numerical column(ideal is between not group)
## histogram(skewness)
```{r}

quantile(bank$age)
```

```{r}
sum(bank$age>70.5)*100/nrow(bank)
```

```{r}
rows_above_70=filter(bank,age>70.5)
age_outlier_perc=nrow(rows_above_70)/nrow(bank) *100
age_outlier_perc
```

# histogram
```{r}
hist(bank$balance)
```

# boxplot
```{r}
boxplot(bank$balance)
```
```{r}
summary(bank$balance)
quantile(bank$balance)
```

```{r}
boxplot(bank$balance)
iqr=1428-72
outlier_upper=1428+1.5*iqr
outlier_perc=sum(bank$balance > outlier_upper)/nrow(bank)*100
outlier_perc
```
```{r}
job_balance=bank %>% group_by(job) %>% summarise(avg_balance=mean(balance),count=n()) %>% arrange(-count)
job_balance
ggplot(job_balance, aes(x=job,y=count,fill=-avg_balance))+ geom_bar(stat='identity')

```


# creating bins for ages
```{r}
bank$age_group=cut(bank$age,breaks=c(0,10,25,40,60,90,Inf),labels=c('0-10', '10-25', '25-40','40-60', '60-90','90+'))

grouping=bank %>% group_by(age_group)%>% summarise(count=n())
grouping

```




