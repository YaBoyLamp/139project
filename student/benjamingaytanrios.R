d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

 d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
# print(nrow(d3)) # 382 students

# marginal linear models for alcohol consumption
lm.math.dalc = lm(G3 ~ Dalc, data = d1)
lm.port.dalc = lm(G3 ~ Dalc, data = d2)
lm.math.walc = lm(G3 ~ Walc, data = d1)
lm.port.walc = lm(G3 ~ Walc, data = d2)
lm.math.alc = lm(G3 ~ Walc + Dalc, data = d1)
lm.port.alc = lm(G3 ~ Walc + Dalc, data = d2)
summary(lm.math.dalc)
summary(lm.port.dalc)
summary(lm.math.walc)
summary(lm.port.walc)
summary(lm.math.alc)
summary(lm.port.alc)

# ESS F test for weekday and weekend alcohol consumption
anova(lm.math.dalc, lm.math.alc)["Pr(>F)"]
anova(lm.port.dalc, lm.port.alc)["Pr(>F)"]
anova(lm.math.walc, lm.math.alc)["Pr(>F)"]
anova(lm.port.walc, lm.port.alc)["Pr(>F)"]

# paired T-test to compare mean of math and portuguese
t.test(d3$G3.x, d3$G3.y, paired = T)

# unpooled T-test to compare mean of math and portuguese
t.test(d1$G3, d2$G3, alternative = c("two.sided"))

hist(d1$G3, main="Math G3")
hist(d2$G3, main="Portuguese G3")

r.sqrd.math = list()
r.sqrd.port = list()
p.val.math = list()
p.val.port = list()

# modifying categoricals to factors
Mjob = d1$Mjob = as.factor(d1$Mjob) 
Mjobdummies = model.matrix(~Mjob-1)[,1:5]
d1.2 = cbind(d1,Mjobdummies)
d1 = d1.2[names(d1.2)!='Mjob']

Mjob = d2$Mjob = as.factor(d2$Mjob) 
Mjobdummies = model.matrix(~Mjob-1)[,1:5]
d2.2 = cbind(d2,Mjobdummies)
d2 = d2.2[names(d2.2)!='Mjob']

Fjob = d1$Fjob = as.factor(d1$Fjob) 
Fjobdummies = model.matrix(~Fjob-1)[,1:5]
d1.2 = cbind(d1,Fjobdummies)
d1 = d1.2[names(d1.2)!='Fjob']

Fjob = d2$Fjob = as.factor(d2$Fjob) 
Fjobdummies = model.matrix(~Fjob-1)[,1:5]
d2.2 = cbind(d2,Fjobdummies)
d2 = d2.2[names(d2.2)!='Fjob']

reason = d1$reason = as.factor(d1$reason) 
reasondummies = model.matrix(~reason-1)[,1:4]
d1.2 = cbind(d1,reasondummies)
d1 = d1.2[names(d1.2)!='reason']

reason = d2$reason = as.factor(d2$reason) 
reasondummies = model.matrix(~reason-1)[,1:4]
d2.2 = cbind(d2,reasondummies)
d2 = d2.2[names(d2.2)!='reason']

guardian = d1$guardian = as.factor(d1$guardian) 
guardiandummies = model.matrix(~guardian-1)[,1:3]
d1.2 = cbind(d1,guardiandummies)
d1 = d1.2[names(d1.2)!='guardian']

guardian = d2$guardian = as.factor(d2$guardian) 
guardiandummies = model.matrix(~guardian-1)[,1:3]
d2.2 = cbind(d2,guardiandummies)
d2 = d2.2[names(d2.2)!='guardian']

# finding marginal plots for all predictors and factors
for (n in colnames(d1)[grep("G", colnames(d1), invert = T)]) {
  fit = lm(formula(paste("G3~", n)), data = d1)
  r.sqrd.math[n] = summary(fit)$r.squared
  r.sqrd.port[n] = summary(lm(formula(paste("G3~", n)), data = d2))$r.squared
  p.val.math[n] = summary(fit)$coefficients[8]
  p.val.port[n] = summary(lm(formula(paste("G3~", n)), data = d2))$coefficients[8]
}

# printing marginal plots in order to largest r squared
r.sqrd.math[order(unlist(r.sqrd.math),decreasing=TRUE)]
r.sqrd.port[order(unlist(r.sqrd.port),decreasing=TRUE)]

d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

# get all main effects as string
effectTerms = ""
for (n in colnames(d1)[grep("G", colnames(d1), invert = T)]) {
  effectTerms = paste(effectTerms, n)
  effectTerms = paste(effectTerms, "+")
}

# remove trailing +
effectTerms = substr(effectTerms, 1, nchar(effectTerms) -1)

# math model with all main effects
mathmod = lm(formula(paste("G3 ~", effectTerms)), data=d1)
summary(mathmod)

d2 <- d2[,-grep("G1",colnames(d2))]
d2 <- d2[,-grep("G2",colnames(d2))]
d1 <- d1[,-grep("G1",colnames(d1))]
d1 <- d1[,-grep("G2",colnames(d1))]

# portuguese model with all main effects
portmod <- lm(formula(paste("G3 ~", effectTerms)), data=d2)
summary(portmod)

# ordered Vcov with Dalc
port.cov = cov2cor(vcov(portmod))
math.cov = cov2cor(vcov(mathmod))
port.dalc.cov = port.cov[,grep("Dalc", colnames(port.cov))]
math.dalc.cov = math.cov[,grep("Dalc", colnames(math.cov))]
port.dalc.cov[order(unlist(port.dalc.cov),decreasing=TRUE)]
math.dalc.cov[order(unlist(math.dalc.cov),decreasing=TRUE)]

cov2cor(vcov(lm(G3 ~ Dalc + goout, data = d2)))
cov2cor(vcov(lm(G3 ~ Dalc + Walc + freetime + traveltime, data = d2)))
for (i in 1:length(d1)) {
  if (is.numeric(d1[i][[1]][[1]])) {
    print(cov2cor(cov(d1[,c(i,grep("Dalc",colnames(d1))) ])))
  }
}

# partial model to analyze alcohol signficance
partial.alc1 = lm(G3 ~ sex + goout + absences + famrel + Mjob + guardian + Dalc, data = d2)
summary(partial.alc1)

# model with all predictors except Walc
partial.alc2 = lm(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob + Fjob + reason + guardian + traveltime + studytime + failures + schoolsup + famsup + paid + activities + nursery + higher + internet + romantic + famrel + freetime + goout + Dalc + health + absences, data = d2)
summary(partial.alc2)


# code for plotting predictors and histogrmas
for (i in 1:length(names(d2))) {
  if (is.numeric(d1[[i]][[1]])) {
#    hist(d1[[i]], main=colnames(d1)[[i]], xlab = colnames(d1)[[i]])
  }
#  plot(d1$G3 ~ d1[[i]], xlab = colnames(d1)[[i]])
  plot(d2$G3 ~ d2[[i]], xlab = colnames(d2)[[i]])
}

# single predictor models for every predictor for both subjects
for (n in colnames(d1)[grep("G", colnames(d1), invert = T)]) {
  print(summary(lm(formula(paste("G3~", n)), data = d1))$coefficients)
  print(summary(lm(formula(paste("G3~", n)), data = d2))$coefficients)
}

# adding in quadratic terms
d1$absences.sq = d1$absences^2
d2$absences.sq = d2$absences^2
d1$Dalc.sq = d1$Dalc^2
d2$Dalc.sq = d2$Dalc^2
d1$Walc.sq = d1$Walc^2
d2$Walc.sq = d2$Walc^2
d1$failures.sq = d1$failures^2
d2$failures.sq = d2$failures^2
d1$age.sq = d1$age^2
d2$age.sq = d2$age^2

# intercept model for stepping
defaultmath = lm(G3 ~ 1, data=d1)
defaultport = lm(G3 ~ 1, data = d2)

# step models to get scope
forwardmath = step(mathmod)
backwardmath = step(mathmod, scope=list(lower=defaultmath), direction="backward")
bothmath = step(defaultmath, scope=list(upper=mathmod), direction="both", k = 2)
bothport = step(defaultport, scope=list(upper=portmod), direction = "both", k = 2)
summary(bothmath)
summary(bothport)

# stepping through pruned models determined by bothmath and bothport
fullModel.math = lm(G3 ~ (failures + Mjob + sex + goout + Medu + romantic + 
                      famsup + studytime + absences + schoolsup + age + famsize + 
                      freetime)^2, data = d1)
fullModel.port = lm(G3 ~ (failures + school + higher + studytime + schoolsup + 
                      Dalc + health + Fedu + sex + absences + age + romantic + 
                      internet)^2, data = d2)
mid.math = lm(G3 ~(failures + Mjob + sex + goout + Medu + romantic + 
                     famsup + studytime + absences + schoolsup + age + famsize + 
                     freetime), data = d1)
mid.port = lm(G3 ~ (failures + school + higher + studytime + schoolsup + 
                            Dalc + health + Fedu + sex + absences + age + romantic + 
                            internet), data = d2)
both_with_interactions_math = step(mid.math, scope=list(upper=fullModel.math, lower = defaultmath), direction="both", k = 2)
both_with_interactions_port = step(mid.port, scope=list(upper=fullModel.port, lower = defaultport), direction="both", k = 2)

# backward stepping for fullModels
backwardmath = step(fullModel.math, scope=list(lower=defaultmath), direction="backward")
backwardport = step(fullModel.port, scope=list(lower=defaultport), direction="backward")

summary(both_with_interactions_math)
summary(both_with_interactions_port)
summary(backwardmath)
summary(backwardport)

# model selection criteria
AIC(both_with_interactions_math)
AIC(both_with_interactions_port)
AIC(backwardmath)
AIC(backwardport)

#assumption checking
plot(both_with_interactions_math)
plot(both_with_interactions_port)
