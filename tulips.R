
## Load required libraries
library(dplyr)
library(ggplot2)
library(splines)
library(car)
library(ROCR)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


## Read in the data
tulips <- read.csv(header=TRUE,'Germination.csv')

## Data cleaning
tulips$Population <- as.factor(tulips$Population)
tulips$Germin <- as.numeric(tulips$Germinated == "Y")

## EDA
#Looking at Monotonicity
p1 <- tulips %>%
  filter(Population==1) %>%
  ggplot(aes(x=ChillingTime, y=Germin)) + 
  geom_point() + geom_smooth(method="loess", se=FALSE) +
  ylab("Germination") +
  ggtitle("Population 1")
p2 <- tulips %>%
  filter(Population==2) %>%
  ggplot(aes(x=ChillingTime, y=Germin)) + 
  geom_point() + geom_smooth(method="loess", se=FALSE) +
  ylab("Germination") +
  ggtitle("Population 2")
p3 <- tulips %>%
  filter(Population==3) %>%
  ggplot(aes(x=ChillingTime, y=Germin)) + 
  geom_point() + geom_smooth(method="loess", se=FALSE) +
  ylab("Germination") +
  ggtitle("Population 3")
p4 <- tulips %>%
  filter(Population==4) %>%
  ggplot(aes(x=ChillingTime, y=Germin)) + 
  geom_point() + geom_smooth(method="loess", se=FALSE) +
  ylab("Germination") +
  ggtitle("Population 4")
p5 <- tulips %>%
  filter(Population==5) %>%
  ggplot(aes(x=ChillingTime, y=Germin)) + 
  geom_point() + geom_smooth(method="loess", se=FALSE) +
  ylab("Germination") +
  ggtitle("Population 5")
p6 <- tulips %>%
  filter(Population==6) %>%
  ggplot(aes(x=ChillingTime, y=Germin)) + 
  geom_point() + geom_smooth(method="loess", se=FALSE) +
  ylab("Germination") +
  ggtitle("Population 6")
p7 <- tulips %>%
  filter(Population==7) %>%
  ggplot(aes(x=ChillingTime, y=Germin)) + 
  geom_point() + geom_smooth(method="loess", se=FALSE) +
  ylab("Germination") +
  ggtitle("Population 7")
p8 <- tulips %>%
  filter(Population==8) %>%
  ggplot(aes(x=ChillingTime, y=Germin)) + 
  geom_point() + geom_smooth(method="loess", se=FALSE) +
  ylab("Germination") +
  ggtitle("Population 8")
p9 <- tulips %>%
  filter(Population==9) %>%
  ggplot(aes(x=ChillingTime, y=Germin)) + 
  geom_point() + geom_smooth(method="loess", se=FALSE) +
  ylab("Germination") +
  ggtitle("Population 9")
p10 <- tulips %>%
  filter(Population==10) %>%
  ggplot(aes(x=ChillingTime, y=Germin)) + 
  geom_point() + geom_smooth(method="loess", se=FALSE) +
  ylab("Germination") +
  ggtitle("Population 10")
p11 <- tulips %>%
  filter(Population==11) %>%
  ggplot(aes(x=ChillingTime, y=Germin)) + 
  geom_point() + geom_smooth(method="loess", se=FALSE) +
  ylab("Germination") +
  ggtitle("Population 11")
p12 <- tulips %>%
  filter(Population==12) %>%
  ggplot(aes(x=ChillingTime, y=Germin)) + 
  geom_point() + geom_smooth(method="loess", se=FALSE) +
  xlim(0,12) + ylim(0,1) +
  ylab("Germination") +
  ggtitle("Population 12")

multiplot(p1,p2,p3,p4,p5,p6,cols=3)
multiplot(p7,p8,p9,p10,p11,p12,cols=3)

# Tables of the data
# How often each population germinates
tulips %>%
  group_by(Population) %>%
  summarise(Proportion = mean(Germin), N_Obs=n() )
# Look to see how the year and day collected play into this
tulips %>%
  group_by(Population, YearCollected, DayCollected) %>%
  summarise(Proportion = mean(Germin) )
# Chilling Time germination rates
tulips %>%
  group_by(ChillingTime) %>%
  summarise(Proportion = mean(Germin) )

# Population and chilling time combined
germin_by_chill <- tulips %>%
  group_by(Population, ChillingTime) %>%
  summarise(Proportion = mean(Germin))
chill_by_pop <- round( rbind(germin_by_chill$Proportion[germin_by_chill$Population==1],
      germin_by_chill$Proportion[germin_by_chill$Population==2],
      germin_by_chill$Proportion[germin_by_chill$Population==3],
      germin_by_chill$Proportion[germin_by_chill$Population==4],
      germin_by_chill$Proportion[germin_by_chill$Population==5],
      germin_by_chill$Proportion[germin_by_chill$Population==6],
      germin_by_chill$Proportion[germin_by_chill$Population==7],
      germin_by_chill$Proportion[germin_by_chill$Population==8],
      germin_by_chill$Proportion[germin_by_chill$Population==9],
      germin_by_chill$Proportion[germin_by_chill$Population==10],
      germin_by_chill$Proportion[germin_by_chill$Population==11],
      germin_by_chill$Proportion[germin_by_chill$Population==12]), 2)


# Find the correct model
fit1 <- glm(Germin ~ Population + ChillingTime, data=tulips, family="binomial")
fit2 <- glm(Germin ~ Population + Population:ChillingTime, data=tulips, family="binomial")
fit3 <- glm(Germin ~ Population + Population:ns(ChillingTime,2), data=tulips, family="binomial")
fit4 <- glm(Germin ~ Population + ns(ChillingTime,2), data=tulips, family="binomial")
fit5 <- glm(Germin ~ Population + Population:ChillingTime + Population:I(ChillingTime^2), data=tulips, family="binomial")
fit6 <- glm(Germin ~ Population:ns(ChillingTime,2), data=tulips, family="binomial")
fit7 <- glm(Germin ~ Population, data=tulips, family="binomial")
fit8 <- glm(Germin ~ Population + Population:ChillingTime + Population:I(ChillingTime^2) + Population:I(ChillingTime^3), data=tulips, family="binomial")
fit9 <- glm(Germin ~ Population + Population:ChillingTime + Population:I(ChillingTime^2) + Population:I(ChillingTime^3) + Population:I(ChillingTime^4), data=tulips, family="binomial")
fit10 <- glm(Germin ~ Population + Population:ns(ChillingTime,3), data=tulips, family="binomial")
fit11 <- glm(Germin ~ Population + Population:ns(ChillingTime,4), data=tulips, family="binomial")
fit12 <- glm(Germin ~ as.factor(YearCollected) + Population + Population:ChillingTime + Population:I(ChillingTime^2), data=tulips, family="binomial")
fit13 <- glm(Germin ~ Population + ChillingTime + Population:I(ChillingTime^2), data=tulips, family="binomial")
anova(fit5,fit13, test="LRT")
anova(fit1,fit2, test="LRT")
anova(fit2,fit5, test="LRT")
anova(fit5,fit8, test="LRT")
anova(fit8,fit9, test="LRT")

finalFit <- fit10
summary(finalFit)

X <- model.matrix(~ Population + Population:ChillingTime + Population:I(ChillingTime^2), data=tulips)
dim(X)

# Look at glm fitted probabilities
all_fit <- data.frame(cbind(tulips$Population, finalFit$fitted.values))
all_fit %>%
  filter(X2==0)
BIC(fit1,fit2,fit3,fit4,fit5,fit6,fit7)
vif(finalFit)


# Diagnostics for out of sample prediction, AUC, and BIC
set.seed(12)
train <- sample(1:2520,2016,replace=FALSE)
tulips_train <- tulips[train,]
tulips_test <- tulips[-train,]
out_fit <- glm(Germin ~ Population + Population:ns(ChillingTime,4), data=tulips_train, family="binomial")
test_pred <- predict(out_fit, newdata=tulips_test, type="response")
test.pred <- prediction(test_pred,tulips_test$Germin)
performance(test.pred,measure="auc")

choice <- 0.6
accept <- as.numeric(test_pred >= choice)
conf_tbl_test <- table(tulips_test$Germinated,accept)
(conf_tbl_test[1,1] + conf_tbl_test[2,2])/504

#BIC for models I am interested in
BIC(fit1,fit2,fit5,fit8,fit9,fit3,fit10,fit11)


# Choosing a cutoff value for confusion matrix
cutoff <- seq(0,1,by=0.01)
pred.vals <- predict(finalFit,type="response")
sens <- rep(0,length(cutoff))
spec <- rep(0,length(cutoff))
erro <- rep(0,length(cutoff))
for (i in 1:length(cutoff)) {
  actual_pred <- data.frame(Germin=as.numeric(tulips$Germin),as.numeric(pred.vals>cutoff[i]))
  true_vals <- subset(actual_pred,Germin==1)
  false_vals <- subset(actual_pred,Germin==0)
  sens[i] <- 1-sum(true_vals[,2])/nrow(true_vals)
  spec[i] <- min(1-(nrow(true_vals)-sum(false_vals[,2]))/nrow(true_vals),1)
  erro[i] <- sum(actual_pred[,1] + actual_pred[,2] == 1)/nrow(actual_pred)
}
plot(cutoff,sens,type="l",col="blue",ylab="Error Rate",main="Comparing Cutoff Values")
lines(cutoff,spec, col="red")
lines(cutoff,erro)
legend(0.4,0.9,legend=c("Error Rate","1-Sensitivity","1-Specificity"),col=c("black","blue","red"),cex=0.8,lty=1)

# Reporting AUC and ROCR
train.pred <- prediction(predict(finalFit,type="response"),tulips$Germin)
train.perf <- performance(train.pred,
                          measure="tpr", x.measure="fpr")
par(mfrow=c(1,1))
plot(train.perf,xlab="1-specificity",ylab="sensitivity",main="ROC Curve",col="blue")
abline(0,1,col="gray")
performance(train.pred,measure="auc")

fit_prob <- finalFit$fitted.values
choice <- 0.6
accept <- as.numeric(fit_prob >= choice)
conf_tbl <- table(tulips$Germinated,accept)
(conf_tbl[1,1] + conf_tbl[2,2])/2520
conf_tbl


# Look at out of sample ROCR and confusion matrix
set.seed(12)
train <- sample(1:2520,2016,replace=FALSE)
tulips_train <- tulips[train,]
out_fit <- glm(Germin ~ Population + Population:ChillingTime + Population:I(ChillingTime^2), data=tulips_train, family="binomial")
tulips_test <- tulips[-train,]
test_pred <- predict(out_fit, newdata=tulips_test, type="response")

test.pred <- prediction(test_pred,tulips_test$Germin)
test.perf <- performance(test.pred,
                          measure="tpr", x.measure="fpr")
par(mfrow=c(1,1))
plot(test.perf,xlab="1-specificity",ylab="sensitivity",main="ROC Curve",col="blue")
abline(0,1,col="gray")
performance(test.pred,measure="auc")

choice <- 0.6
accept <- as.numeric(test_pred >= choice)
conf_tbl_test <- table(tulips_test$Germinated,accept)
(conf_tbl_test[1,1] + conf_tbl_test[2,2])/504





# The fitted probabilities
tulips_fit <- cbind(tulips, fit_prob=fit_prob)
ggplot(tulips_fit, aes(x=ChillingTime,y=fit_prob,color=Population)) +
  geom_line()

# Fitted probabilities with confidence intervals
f_dat <- data.frame(Population=as.factor(c(rep(1,25),rep(2,25),rep(3,25),rep(4,25),rep(5,25),rep(6,25),rep(7,25),rep(8,25),rep(9,25),rep(10,25),rep(11,25),rep(12,25))),
                    ChillingTime=rep(seq(0,12,by=0.5),12))
pred_resp <- predict(finalFit, newdata = f_dat, type="response")
pred_prob <- predict(finalFit, newdata = f_dat, type="link", se.fit=TRUE)
logit.U <- pred_prob$fit + 1.96*pred_prob$se.fit
logit.L <- pred_prob$fit - 1.96*pred_prob$se.fit
phat.U <- exp(logit.U)/(1+exp(logit.U))
phat.L <- exp(logit.L)/(1+exp(logit.L))
expected <- exp(pred_prob$fit)/(1+exp(pred_prob$fit))
all_probs <- data.frame(cbind(f_dat, fit = pred_resp, expected = expected, upper = phat.U, lower = phat.L))
all_probs$upper[276:300] <- rep(0,25)
f1 <- all_probs %>% 
  filter(Population==1) %>%
  ggplot(aes(x=ChillingTime)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Population 1") +
  geom_line(aes(y=expected),color="black",size=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.25,fill="blue")
f2 <- all_probs %>% 
  filter(Population==2) %>%
  ggplot(aes(x=ChillingTime)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Population 2") +
  geom_line(aes(y=expected),color="black",size=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.25,fill="blue")
f3 <- all_probs %>% 
  filter(Population==3) %>%
  ggplot(aes(x=ChillingTime)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Population 3") +
  geom_line(aes(y=expected),color="black",size=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.25,fill="blue")
f4 <- all_probs %>% 
  filter(Population==4) %>%
  ggplot(aes(x=ChillingTime)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Population 4") +
  geom_line(aes(y=expected),color="black",size=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.25,fill="blue")
f5 <- all_probs %>% 
  filter(Population==5) %>%
  ggplot(aes(x=ChillingTime)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Population 5") +
  geom_line(aes(y=expected),color="black",size=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.25,fill="blue")
f6 <- all_probs %>% 
  filter(Population==6) %>%
  ggplot(aes(x=ChillingTime)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Population 6") +
  geom_line(aes(y=expected),color="black",size=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.25,fill="blue")
f7 <- all_probs %>% 
  filter(Population==7) %>%
  ggplot(aes(x=ChillingTime)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Population 7") +
  geom_line(aes(y=expected),color="black",size=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.25,fill="blue")
f8 <- all_probs %>% 
  filter(Population==8) %>%
  ggplot(aes(x=ChillingTime)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Population 8") +
  geom_line(aes(y=expected),color="black",size=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.25,fill="blue")
f9 <- all_probs %>% 
  filter(Population==9) %>%
  ggplot(aes(x=ChillingTime)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Population 9") +
  geom_line(aes(y=expected),color="black",size=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.25,fill="blue")
f10 <- all_probs %>% 
  filter(Population==10) %>%
  ggplot(aes(x=ChillingTime)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Population 10") +
  geom_line(aes(y=expected),color="black",size=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.25,fill="blue")
f11 <- all_probs %>% 
  filter(Population==11) %>%
  ggplot(aes(x=ChillingTime)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Population 11") +
  geom_line(aes(y=expected),color="black",size=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.25,fill="blue")
f12 <- all_probs %>% 
  filter(Population==12) %>%
  ggplot(aes(x=ChillingTime)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Population 12") +
  geom_line(aes(y=expected),color="black",size=1)
multiplot(f1,f2,f3,f4,cols=2)
multiplot(f5,f6,f7,f8,cols=2)
multiplot(f9,f10,f11,f12,cols=2)

cbind(pred_prob$fit, pred_prob$se.fit)

ggplot(all_probs, aes(x=ChillingTime,color=Population)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("All Populations") +
  geom_line(aes(y=expected),size=1)

# Populations 1 and 5
c15 <- all_probs %>%
  filter(Population %in% c(1,5)) %>%
  ggplot(aes(x=ChillingTime,color=Population)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Populations 1 and 5") +
  geom_line(aes(y=expected),size=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.25)
# Populations 6 and 7
c76 <- all_probs %>%
  filter(Population %in% c(7,6)) %>%
  ggplot(aes(x=ChillingTime,color=Population)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Populations 6 and 7") +
  geom_line(aes(y=expected),size=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.25)
# Populations 8 and 9
c89 <- all_probs %>%
  filter(Population %in% c(8,9)) %>%
  ggplot(aes(x=ChillingTime,color=Population)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Populations 8 and 9") +
  geom_line(aes(y=expected),size=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.25)
# Populations 1 2 and 3
c123 <- all_probs %>%
  filter(Population %in% c(1,2,3)) %>%
  ggplot(aes(x=ChillingTime,color=Population)) +
  ylim(0,1) +
  ylab("Probability of Germinating") +
  ggtitle("Populations 1, 2 and 3") +
  geom_line(aes(y=expected),size=1) + 
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.25)
multiplot(c15,c89,c123,c76,cols=2)

# Look at the coefficients
the.glm <- summary(finalFit)
upper <- the.glm$coef[,1] + qnorm(0.975)*the.glm$coef[,2]
lower <- the.glm$coef[,1] - qnorm(0.975)*the.glm$coef[,2]
round(cbind(exp(t(t(coef(finalFit)))),round(exp(upper),2), exp(lower)),2)

# Find the maximum
all_probs %>%
  filter(ChillingTime %in% c(0,2,4,6,8,10,12)) %>%
  group_by(Population) %>%
  summarise(ChillMax=ChillingTime[which.max(fit)],fitted_prob=max(fit))

# Decrease from 10 to 8
climate <- geom_vline(xintercept=c(8,10),color=c("red","black"))
f1 + climate
multiplot(f1+climate,f2+climate,f3+climate,f4+climate,f5+climate,f6+climate,cols=3)
multiplot(f7+climate,f8+climate,f9+climate,f10+climate,f11+climate,f12+climate,cols=3)
