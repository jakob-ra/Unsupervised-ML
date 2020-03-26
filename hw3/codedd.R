library('dummies')
library('ramify')

dat <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
dat = na.omit(dat)

# Remember to recover categorical variables 
dat <- dat[,!(names(dat) %in% c('Entity','Code','Year'))]

plot(log(dat$GDP.per.capita..PPP..constant.2011.international.....Rate.),dat$Per.capita.mismanaged.plastic.waste..kilograms.per.person.per.day.)
text(log(dat$GDP.per.capita..PPP..constant.2011.international.....Rate.),dat$Per.capita.mismanaged.plastic.waste..kilograms.per.person.per.day., labels = dat$Code, cex=0.7, pos=3)

df <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")
df = na.omit(df)
plot(log(df$GDP.per.capita..PPP..constant.2011.international.....constant.2011.international...),df$Per.capita.plastic.waste..kilograms.per.person.per.day.)
text(log(df$GDP.per.capita..PPP..constant.2011.international.....constant.2011.international...),df$Per.capita.plastic.waste..kilograms.per.person.per.day., labels = df$Entity, cex=0.4, pos=3)

df <- subset(df, Entity != 'Trinidad and Tobago')


kmeans <- function(dat, k ,eps){
  p <- length(dat)
  n <- length(dat[[1]])
  groups <- sample(1:k, n, replace=TRUE)
  dums <- dummy(groups)
  centers <- c()
  for (i in (1:k)){
    centers <- rbind(centers, colSums(dat * dums[,i])/sum(dums[,i]) )
  }
  conv <- 100000
  while (conv > eps){
    dist <- c()
    for (i in (1:k)){
      dist <- cbind(dist, sqrt(rowSums((dat-centers[i,])^2)))
    }
    groups <- argmin(dist, rows=TRUE)
    dums <- dummy(groups)
    new_centers <- c()
    for (i in (1:k)){
      new_centers <- rbind(new_centers, colSums(dat * dums[,i])/sum(dums[,i]) )
    }
    conv <- sum(sqrt(rowSums((centers-new_centers)^2)))
    centers <- new_centers
  }
  return(list(centers, cbind(dat,groups)))
}

res <- kmeans(dat, 3, 1)
dat1 <- res[[2]]



