---
      title       : Bagging
subtitle    : 
author      : Jeffrey Leek 
job         : Johns Hopkins Bloomberg School of Public Health
logo        : bloomberg_shield.png
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow   # 
url:
      lib: ../../librariesNew
assets: ../../assets
widgets     : [mathjax]            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
---
      
      
      ```{r setup, cache = F, echo = F, message = F, warning = F, tidy = F}
# make this an external chunk that can be included in any file
options(width = 100)
opts_chunk$set(message = F, error = F, warning = F, comment = NA, fig.align = 'center', dpi = 100, tidy = F, cache.path = '.cache/', fig.path = 'fig/')

options(xtable.type = 'html')
knit_hooks$set(inline = function(x) {
      if(is.numeric(x)) {
            round(x, getOption('digits'))
      } else {
            paste(as.character(x), collapse = ', ')
      }
})
knit_hooks$set(plot = knitr:::hook_plot_html)
```


## Bootstrap aggregating (bagging)

__Basic idea__: 
      
1. Resample cases and recalculate predictions
2. Average or majority vote

__Notes__:
      
* Similar bias 
* Reduced variance
* More useful for non-linear functions


---
      
      ## Ozone data
      
      ```{r ozoneData, cache=TRUE}
install.packages("ElemStatLearn")
library(ElemStatLearn); data(ozone,package="ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)
```
[http://en.wikipedia.org/wiki/Bootstrap_aggregating](http://en.wikipedia.org/wiki/Bootstrap_aggregating)


---
      
      ## Bagged loess
      
      ```{r baggedOzone, dependson="ozoneData",cache=TRUE}
ll <- matrix(NA,nrow=10,ncol=155)
for(i in 1:10){
      ss <- sample(1:dim(ozone)[1],replace=T)
      ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
      loess0 <- loess(temperature ~ ozone,data=ozone0,span=0.2)
      ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
}
```

---
      
      ## Bagged loess
      
      ```{r, dependson="baggedOzone",fig.height=4.5,fig.width=4.5}
plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for(i in 1:10){lines(1:155,ll[i,],col="grey",lwd=2)}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)
```


---
      
      ## Bagging in caret
      
      * Some models perform bagging for you, in `train` function consider `method` options 
* `bagEarth` 
* `treebag`
* `bagFDA`
* Alternatively you can bag any model you choose using the `bag` function

---
      
      ## More bagging in caret
      
      ```{r bag1}
predictors = data.frame(ozone=ozone$ozone)
temperature = ozone$temperature
library("caret")
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))
```

http://www.inside-r.org/packages/cran/caret/docs/nbBag


---
      
      ## Example of custom bagging (continued)
      
      ```{r,dependson="bag1",fig.height=4,fig.width=4}
plot(ozone$ozone,temperature,col='lightgrey',pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
points(ozone$ozone,predict(treebag,predictors),pch=19,col="blue")
```


---
      
      ## Parts of bagging
      
      ```{r}
ctreeBag$fit
```

---
      
      ## Parts of bagging
      
      ```{r}
ctreeBag$pred
```


---
      
      ## Parts of bagging
      
      ```{r}
ctreeBag$aggregate
```


---
      
      ## Notes and further resources
      
      __Notes__:
      
      * Bagging is most useful for nonlinear models
* Often used with trees - an extension is random forests
* Several models use bagging in caret's _train_ function

__Further resources__:

* [Bagging](http://en.wikipedia.org/wiki/Bootstrap_aggregating)
* [Bagging and boosting](http://stat.ethz.ch/education/semesters/FS_2008/CompStat/sk-ch8.pdf)
* [Elements of Statistical Learning](http://www-stat.stanford.edu/~tibs/ElemStatLearn/)
