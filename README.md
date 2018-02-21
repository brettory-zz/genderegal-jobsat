---
title: "Is there a tradeoff between gender diversity and job satisfaction?"
# author: "Brett Ory"
thumbnailImagePosition: left
thumbnailImage: http://images.all-free-download.com/images/graphicthumb/simple_world_maps_vector_578784.jpg
coverImage: http://images.all-free-download.com/images/graphicthumb/simple_world_maps_vector_578784.jpg
metaAlignment: center
coverMeta: out
date: 2018-02-13T21:13:14-05:00
categories: ["Personal projects"]
tags: ["world maps", "cross validation", "plot", "surveys", "Kaggle", "regression"]
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(lme4)
library(tidyr)
library(ggplot2)
library(rgdal) # for reading shapefile
library(RColorBrewer)
library(classInt)
library(cvTools) 

ks <- read.csv("data/multipleChoiceResponses.csv")
cr <- read.csv("data/conversionRates.csv")
```


Two weeks ago I claimed that women report higher job satisfaction when they work in countries where tech is more male-dominated. And then instead of backing up my claim last week, I got sidetracked by questions of sample size and statistical power. 

<br>

<p align="center">
  <img src="https://i.ytimg.com/vi/7EswDwY-944/hqdefault.jpg" />
</p>


<br>

In a [previous blog post](http://www.brettory.com/2018/02/embedding-a-shiny-app-in-blogdown/) I introduced the Kaggle survey on women in tech and I did some basic data cleaning for that survey. To save time and get to the point, I now pick up where I left off. In this post I use linear regression, multilevel regression, and k-fold cross validation techniques to show how job satisfaction varies across countries depending on the percentage of women working in tech.  

```{r prep data, echo=FALSE}
# FEMALE
ks$female <- as.numeric(with(ks, ifelse(ks$GenderSelect=="Female",1,ifelse(ks$GenderSelect=="Male",0,NA))))
# table(ks$GenderSelect, ks$female)


# COUNTRY
# table(ks$Country) 
# China listed under People 's Republic of China and Republic of China. 
levels(ks$Country)[levels(ks$Country)=="People 's Republic of China"] <- "China"
levels(ks$Country)[levels(ks$Country)=="Republic of China"] <- "China"
# get rid of other and NA countries
ks <- ks[ks$Country!="Other" & ks$Country!="" & !is.na(ks$Country),]
ks$Country <- factor(ks$Country)

ks <- ks[,c("Country", "female", "CompensationAmount", "CompensationCurrency", "JobSatisfaction")]
cr <- cr[,c("originCountry", "exchangeRate")]

```

The survey has a lot of interesting variables, but for today we're just interested in a subset of the data.    

* From the file "multipleChoiceResponses.csv": female, Country, CompensationAmount, JobSatisfaction, Tenure       
* From the file "conversionRates.csv": originCountry, exchangeRate

```{r data intro ks, warning=FALSE, message=FALSE}
library(knitr)

Varname <- names(ks)
Description <- c("Country of residence", "Is respondent female? 1 = female, 0 = male. Nonbinary = removed", "Current total yearly compensation (salary + bonus)", "Three letter country code abbreviation for country of currency", "Satisfaction with current job? (scale 0-10)")
table <- as.data.frame(cbind(Varname, Description))
kable(table[1:5,])

Varname <- names(cr)
Description <- c("Three letter country code abbreviation for country of currency", "Exchange rate to USD")
table <- as.data.frame(cbind(Varname, Description))
kable(table[1:2,])
```

<br>

## Preparing the variables

CompensationAmount is (often) given in local currency, so we need to merge the survey responses with the conversionRates.csv file to convert local salary to USD equivalent.  

```{r merge with conversionRates, error=FALSE, message=FALSE, warning=FALSE}
cr$CompensationCurrency <- cr$originCountry # rename variable for matching datasets
cr <- cr[,c("CompensationCurrency","exchangeRate")]

# merge datasets
ks <- merge(ks,cr,by="CompensationCurrency", all=TRUE)
# View(ksi[,c("CompensationAmount","CompensationCurrency","exchangeRate")])

# clean up global environment
rm(cr)

# clean up CompensationAmount
# missing values are sometimes strings instead of NA
is.na(ks$CompensationAmount) <- ks$CompensationAmount=="" 

# numbers are inconsistently written. Change comma as hundreds separator to "" and make numeric
ks$CompensationAmount <- as.numeric(gsub(",", "", ks$CompensationAmount)) 
ks$CompensationAmount <- ifelse(ks$CompensationAmount<=0,NA,ks$CompensationAmount) # make negatives and 0 missing, because these are weird income values

# multiply income*exchangeRate to get income in USD
ks$CompUSD <- ks$CompensationAmount*ks$exchangeRate

# income not normally distributed, take log
ks$CompUSDLog <- log(ks$CompUSD)

```

JobSatifaction is a factor, but with 10 categories we can treat it like a numeric variable. 

```{r job satisfaction}
## convert from factor to numeric
ks$jobsat <- as.numeric(with(ks, ifelse((ks$JobSatisfaction=="1 - Highly Dissatisfied"), "1", 
                                          ifelse((ks$JobSatisfaction=="10 - Highly Satisfied"), "10", 
                                                 ifelse((ks$JobSatisfaction %in% c("I prefer not to share", "")), NA, 
                                                        ifelse((ks$JobSatisfaction==2),2,
                                                               ifelse((ks$JobSatisfaction==3),3,
                                                                      ifelse((ks$JobSatisfaction==4),4,
                                                                             ifelse((ks$JobSatisfaction==5),5,
                                                                                    ifelse((ks$JobSatisfaction==6),6,
                                                                                           ifelse((ks$JobSatisfaction==7),7,
                                                                                                  ifelse((ks$JobSatisfaction==8),8,
                                                                                                         ifelse((ks$JobSatisfaction==9),9,NA)))))))))))))

```


<br>

## Country descriptives

Now the data is clean, lets run some country-level descriptives

```{r Country descriptives, message=F, error=F, warning=F}    
# Make country level descriptives
Countrydescriptives <- ks %>%
  group_by(Country) %>%
  summarize(meanfem=mean(female, na.rm=T),sdfem=sd(female, na.rm=T),
            meaninc=mean(CompUSDLog, na.rm=T),
            meanJobSat=mean(jobsat, na.rm=T),sdJobSat=sd(jobsat,na.rm=T))

# I also want mean of job satisfaction by country and gender. To do this I make two datasets for male and female country averages, then merge
Countryfemdesc <- ks[ks$female==1,] %>%
  group_by(Country) %>%
  summarize(meanfemJobSat=mean(jobsat, na.rm=T))
Countrymaledesc <- ks[ks$female==0,] %>%
  group_by(Country) %>%
  summarize(meanmaleJobSat=mean(jobsat, na.rm=T))
Countrygenderdesc <- merge(Countryfemdesc,Countrymaledesc,by="Country")
rm(Countryfemdesc,Countrymaledesc)

# merge both country level datasets 
Countrydescriptives <- merge(Countrydescriptives,Countrygenderdesc,by="Country")

# country descriptives
Countrydescriptives[,c("Country","meanfem","meanJobSat")]

# merge with individual level data
ks <- merge(ks,Countrydescriptives,by="Country")
rm(Countrygenderdesc)

```

In the above table you can search per country to see the proportion of women and the mean job satisfaction. 

<br>

## Plotting descriptives

The table has a lot of information and it's not very informative in an of itself. In the following figures I try to make this information more tangible, starting with countries ranked by the proportion of women in tech.
```{r figure proportion women}
# Subset Countrydescriptives and create confidence intervals for proportion women in tech per country
CDSmall <- Countrydescriptives[order(Countrydescriptives$meanfem, decreasing = TRUE),c("Country","meanfem","sdfem")]
CDSmall <- CDSmall[!is.na(CDSmall$Country),]
CDSmall$lbfem <- CDSmall$meanfem - 1.96*(CDSmall$sdfem/(sqrt(length(CDSmall$meanfem)))) ## lower bound confidence interval 
CDSmall$ubfem <- CDSmall$meanfem + 1.96*(CDSmall$sdfem/(sqrt(length(CDSmall$meanfem)))) ## upper bound confidence interval
CDSmall <- CDSmall[,c("Country","meanfem","lbfem","ubfem")]

# order by point
CDSmall$Country <- factor(CDSmall$Country, levels=CDSmall[order(CDSmall$meanfem, decreasing = FALSE), "Country"])

# figure
p <- ggplot(CDSmall, aes(x=Country, y=meanfem, ymin=lbfem, ymax=ubfem))+
  geom_pointrange()+
  geom_hline(yintercept = 0, linetype=2)+
  coord_flip()+
  xlab('Country')
p

```

In another figure I sort countries by job satisfaction (ranked highest to lowest).

```{r figure job satisfacton}
# Subset Countrydescriptives and create confidence intervals for job satisfaction
rm(CDSmall)
CDSmall <- Countrydescriptives[order(Countrydescriptives$meanJobSat, decreasing = TRUE),c("Country","meanJobSat","sdJobSat")]
CDSmall <- CDSmall[!is.na(CDSmall$Country),]
CDSmall$lbjs <- CDSmall$meanJobSat - 1.96*(CDSmall$sdJobSat/(sqrt(length(CDSmall$meanJobSat)))) ## lower bound confidence interval 
CDSmall$ubjs <- CDSmall$meanJobSat + 1.96*(CDSmall$sdJobSat/(sqrt(length(CDSmall$meanJobSat)))) ## upper bound confidence interval
CDSmall <- CDSmall[,c("Country","meanJobSat","lbjs","ubjs")]

# order by point
CDSmall$Country <- factor(CDSmall$Country, levels=CDSmall[order(CDSmall$meanJobSat, decreasing = FALSE), "Country"])

# figure
p <- ggplot(CDSmall, aes(x=Country, y=meanJobSat, ymin=lbjs, ymax=ubjs))+
  geom_pointrange()+
  geom_hline(yintercept = 0, linetype=2)+
  coord_flip()+
  xlab('Country')
p

```

This is slightly more informative, but lists of countries are still hard to process. Now I graph the proportion of women working in tech in a map.

```{r map proportion female,warning=F, error=F, message=F}
## GLOBAL MAPS
polygons <- readOGR(dsn = "TM_WORLD_BORDERS_SIMPL-0.3", layer = "TM_WORLD_BORDERS_SIMPL-0.3")

# some country names are different in polygons and ks files 
levels(polygons$NAME)[levels(polygons$NAME)=="Iran (Islamic Republic of)"] <- "Iran"
levels(polygons$NAME)[levels(polygons$NAME)=="Viet Nam"] <- "Vietnam"

# prep colors for maps
my.palette <- brewer.pal(n = 9, name = "Reds")

polygons2 <- merge(polygons,Countrydescriptives,by.x="NAME",by.y="Country")
breaks.qt <- classIntervals(polygons2$meanfem, n = 9, style = "quantile", intervalClosure = "right")
meanfemMap <- spplot(polygons2,"meanfem",main=paste(polygons2$meanfem),sub="Proportion of Women in Tech",
                        col.regions=my.palette,at=breaks.qt$brks)

print(meanfemMap)
```

And a map of average job satisfaction per country

```{r map job satisfaction,warning=F, error=F, message=F}
polygons3 <- merge(polygons,Countrydescriptives,by.x="NAME",by.y="Country")
breaks.qt <- classIntervals(polygons3$meanJobSat, n = 9, style = "quantile", intervalClosure = "right")
JobSatMap <- spplot(polygons3,"meanJobSat",main=paste(polygons3$meanJobSat),sub="Job Satisfaction",
                        col.regions=my.palette,at=breaks.qt$brks)

print(JobSatMap)
```

In Europe, Canada, and especially South America it looks like there is generally low gender diversity but high job satisfaction. This might be an indication that diversity and job satisfaction are inversely related, but we would need to conduct an analysis to tell for sure. 

<br>

## Analyses

I will now perform some regressions to see if people are more satisfied with their jobs when they work in less diverse settings. 

```{r country level desc}
jobsat.1 <- lm(meanJobSat ~ meanfem, data=Countrydescriptives)
summary(jobsat.1) 
```

The above analysis shows that the greater the proportion of women, the lower the overall level of job satisfaction, but is this driven by women's job satisfaction or men's?

```{r meanfemjobsat regression}
# women's job satisfaction
jobsat.2 <- lm(meanfemJobSat ~ meanfem, data=Countrydescriptives)
summary(jobsat.2) 
```

The proportion of women has no significant effect on women's mean job satisfaction. As shown in the following analysis the proportion of women seems to decrease men's mean job satisfaction by 2.5 points, which is a huge amount on a scale of 0-10. 

```{r meanmalejobsat regression}
# men's job satisfaction
jobsat.3 <- lm(meanmaleJobSat ~ meanfem, data=Countrydescriptives)
summary(jobsat.3) 
```

Is this perhaps due to employers paying women less, thus bringing down the overall salary/reimbursement package for all employees?

```{r jobsat.4}
# control for income
jobsat.4 <- lm(meanmaleJobSat ~ meanfem + meaninc, data=Countrydescriptives)
summary(jobsat.4) 
```

Could be! Indeed, once we control for the mean income of tech workers, the effect of the proportion of women on men's job satisfaction dissappears. This might imply that employers are discriminating against women by paying them less, and that discrimination brings down the overall salary levels. However, since we are only observing cross-sectional, macro-level phenomena, we can't say anything about causality.

<br>

### Individual level

You'll notice that in the above analyses, I look at country level averages and find that the _average_ level of job satisfaction is lower in countries where the proportion of women in tech is greater That's all fine and well, but the question was if individuals experience a tradeoff between job satisfaction and diversity. That means that it's not enough to look at national averages, we have to consider how gender diversity affects (or is correlated with) individuals' job satisfaction. That we do with a multilevel analysis.

```{r random intercept}
mljobsat.1 <- lmer(jobsat ~ meanfem + (1 | Country), data=ks)
summary(mljobsat.1) 
```

In the above random intercept model we allow the mean job satisfaction per country to vary. We can see from the size of the variance of the intercept (0.1479) relative to its standard deviation (0.3846) that there is not much cross-national variation in job satisfaction. Interesting! (Reported) life satisfaction is known to vary quite a bit [between countries](https://ourworldindata.org/happiness-and-life-satisfaction). Apparently job satisfaction is less culturally determined, or perhaps tech workers are a particularly international bunch who tend towards one shared global culture. 

We can also calculate the interclass correlation, which tells us how similar respondents from the same country are.

```{r interclass correlation}
(ICC <- 0.1479/(0.1479 + 4.4752))
```

3% of variation in job satisfaction between respondents can be explained by the country level (after controlling for proportion of women working in tech). Incidentally, the variance in job satisfaction explained by country of residence is not much bigger when we don't include the proportion of women.

The R command `lmer()` purposefully does not print confidence intervals or p-values out of an ideological objection to basing decisions purely on p-values. Their point is that we should consider the size of the effect as well as its statistical significance. Nonetheless, I find it useful to know if we repeated our test 100 times, how often would our estimated effect be different from 0? We can calculate a confidence interval using the standard error of the estimate, with the result that our confidence interval for the effect of meanfem on jobsat is:

```{r confidence interval}
(CI <- c(-2.6003 - 1.96*1.1636 , -2.600 + 1.96*1.1636))
```

Thus, people are significantly less satisfied with their jobs when there is a higher proportion of women working in tech. Again we distinguish between men's satisfaction and women's:


```{r mljobsat.2}
# women's job satisfaction
mljobsat.2 <- lmer(jobsat ~ meanfem + (1 | Country), data=ks[ks$female==1,])
summary(mljobsat.2) 
```

Here we performed the same analysis but only on the women in our sample. We do see a negative effect of diversity for women, but it's not significant. 


```{r ci mljobsat.2}
(CI <- c(-1.0444 - 1.96*2.2909 , -1.0444 + 1.96*2.2909))
```


```{r mljobsat.3}
# men's job satisfaction
mljobsat.3 <- lmer(jobsat ~ meanfem + (1 | Country), data=ks[ks$female==0,])
summary(mljobsat.3) 
```

For men we also see a negative effect, which is likewise not significant. 

```{r ci mljobsat.3}
(CI <- c(-2.3693 - 1.96*1.2414 , -2.3693 + 1.96*1.2414))
```

In the country-level analyses we also included income, so let's do that here, too, but at the individual level.

```{r mljobsat.4}
mljobsat.4 <- lmer(jobsat ~ meanfem + CompUSDLog + (1 | Country), data=ks[ks$female==0,])
summary(mljobsat.4) 
```

It's a pretty wide standard error again, so let's compute the confidence interval. 

```{r ci mljobsat.4}
(CI <- c(-2.75309 - 1.96*1.30205 , -2.75309 + 1.96*1.30205))
```

Here the proportion of women in tech is significant. Thus while the effect of income seems to overrule the effect of gender diversity as a macro phenomenon, at the individual-level, the positive effect of income actually _compensates_ for some of the negative effect of gender diversity. 

<br>

### Cross-level interactions

There's one final way to test whether the gender composition has a different effect for men and women, and that's with a cross-level interaction. In this model we ask whether the effect of being female varies depending on the proportion of women in tech. 

```{r cljobsat}
cljobsat <- lmer(jobsat ~ female*meanfem + (1 + female | Country), data=ks)
summary(cljobsat) 
```

The interaction between female and meanfem is not significant, but the negative effect of gender diversity on job satisfaction remains. I suspect this model is producing wide confidence intervals because we've demanded more of it than the data can handle. But for argument's sake, let's run the same model above including income in the equation. 

```{r cljobsat.2}
cljobsat.2 <- lmer(jobsat ~ female*meanfem + CompUSDLog + (1 + female | Country), data=ks)
summary(cljobsat.2) 
```

This model produces the same results as the previous cross-level interaction model, with an additional significant and positive effect of income. 

<br>

## Cross validation

Let's now compare our three models (jobsat.4, mljobsat.4, and cljobsat.2) which illustrate the effect of gender diversity on job satisfaction after controlling for income. This i do by segmenting the data into 10 subsets, running the model on each subset, and then correlating predicted and observed values.  

```{r compare models}
# jobsat.4
folds <- cvFolds(NROW(Countrydescriptives), K=10)
Countrydescriptives$pred.4 <- rep(0,nrow(Countrydescriptives))

for(i in 1:10){
  train <- Countrydescriptives[folds$subsets[folds$which != i], ] #Set the training set
  validation <- Countrydescriptives[folds$subsets[folds$which == i], ] #Set the validation set

  newjobsat.4 <- lm(meanmaleJobSat ~ meanfem + meaninc, data=train) #Get your new linear model (just fit on the train data)
  predjobsat.4 <- predict(newjobsat.4,newdata=validation) #Get the predicitons for the validation set (from the model just fit on the train data)

  Countrydescriptives[folds$subsets[folds$which == i], ]$pred.4 <- predjobsat.4 #Put the hold out prediction in the data set for later use
}

cor_jobsat.4 <- cor(Countrydescriptives[,c("meanmaleJobSat", "pred.4")], use="complete.obs", method="pearson")


# mljobsat.4
ksmen <- ks[ks$female==0,]
ksmen <- ksmen[!is.na(ksmen$Country),]
foldsksmen <- cvFolds(NROW(ksmen), K=10)
ksmen$predml.4 <- rep(0,nrow(ksmen))

for(i in 1:10){
  train <- ksmen[foldsksmen$subsets[foldsksmen$which != i], ] 
  validation <- ksmen[foldsksmen$subsets[foldsksmen$which == i], ] 

  newmljobsat.4 <- lmer(jobsat ~ meanfem + CompUSDLog + (1 | Country), data=train) 
  predmljobsat.4 <- predict(newmljobsat.4,newdata=validation) 

  ksmen[foldsksmen$subsets[foldsksmen$which == i], ]$predml.4 <- predmljobsat.4
}

cor_mljobsat.4 <- cor(ksmen[,c("jobsat", "predml.4")], use="complete.obs", method="pearson")


# cljobsat.2
ks <- ks[!is.na(ks$Country),]
foldsks <- cvFolds(NROW(ks), K=10)
ks$predcl.2 <- rep(0,nrow(ks))

for(i in 1:10){
  train <- ks[foldsks$subsets[foldsks$which != i], ] 
  validation <- ks[foldsks$subsets[foldsks$which == i], ] 

  newcljobsat.2 <- lmer(jobsat ~ female*meanfem + CompUSDLog + (1 + female | Country), data=train)
  predcljobsat.2 <- predict(newcljobsat.2,newdata=validation) 

  ks[foldsks$subsets[foldsks$which == i], ]$predcl.2 <- predcljobsat.2
}

cor_cljobsat.2 <- cor(ks[,c("jobsat", "predcl.2")], use="complete.obs", method="pearson")

models <- c("jobsat.4","mljobsat.4","cljobsat.2")
correlations <- c(round(cor_jobsat.4[2,1],2),round(cor_mljobsat.4[2,1],2),round(cor_cljobsat.2[2,1],2))
(corres <- cbind(models,correlations))

```

Here I assess accuracy by comparing the correlation between the predicted outcomes based on the regression model and the observed outcomes. The first model, jobsat.4 that only looked at country-level descriptives, has the highest correlation at approximately .30. The individual level models with a simple multilevel structure are less accurate. Therefore, from purely a predictive standpoint, the simplest model performs the best. However, the models also answer different questions. The first model, jobsat.4, describes how gender diversity affects average job satisfaction (*it doesn't*), while the other two models, mljobsat.4 and cljobsat.2, describe how gender diversity affects _individual_ job satisfaction (*it does--negatively*). Thus, the question of which model to use is not purely a question of which model makes better predictions, but also of what question are we trying to answer. 

<br>

## Conclusion
I started out by asking if there is a tradeoff between gender diversity and job satisfaction, and it does appear that individual job satisfaction is lower in countries where there are more women working in tech, particularly for men. That's the bad news. The good news is that these models are quite poor predictors of overall job satisfaction. Luckily for employers and policy makers who want to increase gender diversity, there are many other factors that can compensate for its negative effect on job satisfaction. 

This post can be found on [GitHub](https://github.com/brettory/genderegal-jobsat).

