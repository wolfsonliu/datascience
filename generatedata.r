library(RColorBrewer)
library(ggplot2)
library(class)
set.seed(2017)

## -----------------
## 生成 Bivariate Normal Distribution 二分类数据
## -----------------

group <- list()

group[['bm']] <- data.frame(
    x=rnorm(n=10, mean=1, sd=1),
    y=rnorm(n=10, mean=0, sd=1),
    color=rep(brewer.pal(9,'Blues')[6], len=10)
)
group[['om']] <- data.frame(
    x=rnorm(n=10, mean=0, sd=1),
    y=rnorm(n=10, mean=1, sd=1),
    color=rep(brewer.pal(9,'Oranges')[6], len=10)
)

group[['blue']] <- data.frame(
    x=unlist(
        lapply(
            group[['bm']][sample(seq(10), size=10, replace=TRUE), 'x'],
            function(z) {
                rnorm(n=20, mean=z, sd=1/2)
            }
        )
    ),
    y=unlist(
        lapply(
            group[['bm']][sample(seq(10), size=10, replace=TRUE), 'y'],
            function(z) {
                rnorm(n=20, mean=z, sd=1/2)
            }
        )
    ),
    color=rep(brewer.pal(9,'Blues')[6], len=200),
    label=rep(0, 200)
)

group[['orange']] <- data.frame(
    x=unlist(
        lapply(
            group[['om']][sample(seq(10), size=10, replace=TRUE), 'x'],
            function(z) {
                rnorm(n=20, mean=z, sd=1/2)
            }
        )
    ),
    y=unlist(
        lapply(
            group[['om']][sample(seq(10), size=10, replace=TRUE), 'y'],
            function(z) {
                rnorm(n=20, mean=z, sd=1/2)
            }
        )
    ),
    color=rep(brewer.pal(9,'Oranges')[6], len=200),
    label=rep(1, 200)
)

group[['trainid']] <- sample(seq(200), 100, replace=FALSE)
group[['testid']] <- seq(200)[is.na(match(seq(200), trainid))]
group[['train']] <- rbind(
    group[['blue']][group[['trainid']],],
    group[['orange']][group[['trainid']],]
)
group[['test']] <- rbind(
    group[['blue']][group[['testid']],],
    group[['orange']][group[['testid']],]
)

group[['lm']] <- lm(label ~ y + x, data=group[['train']])$coefficients

## 按照三元的办法来解线性方程, 则在最后画图的时候,
## line 的 label 应取 label 所有可取值的中间值.

p <- ggplot(group[['train']], aes(x=x, y=y, color=I(color))) +
    geom_point(
    ) + geom_abline(
            slope=-group[['lm']][3] / group[['lm']][2],
            intercept=-(group[['lm']][1]-(1+0)/2) / group[['lm']][2]
        )


## -----------------
