d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

# d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
# print(nrow(d3)) # 382 students

mathmod <- lm(g3)
d1 <- d1[,-grep("G1",colnames(d1))]
d1 <- d1[,-grep("G2",colnames(d1))]
mathmod = lm(G3 ~ ., data=d1)
summary(mathmod)

d2 <- d2[,-grep("G1",colnames(d2))]
d2 <- d2[,-grep("G2",colnames(d2))]

portmod <- lm(G3 ~ ., data=d2)
summary(portmod)

for (i in 1:length(names(d1))) {
  if (is.numeric(d1[[i]][[1]])) {
    hist(d1[[i]], main=colnames(d1)[[i]], xlab = colnames(d1)[[i]])
  }
}

defaultmath = lm(G3 ~ 1, data=d1)

forwardmath = step(mathmod)
backwardmath = step(mathmod, scope=list(lower=defaultmath), direction="backward")
bothmath = step(defaultmath, scope=list(upper=mathmod), direction="both")
