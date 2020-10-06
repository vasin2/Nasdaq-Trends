library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library("IDPmisc")
library("dplyr")

# Data wrangling 
### Keep 3 columns

keeps<- c("date", "close", "symbol")
fh_5yrs1<- fh_5yrs[keeps]


# Remove missing data

fh_5yrs2<-NaRV.omit(fh_5yrs1)

fh_5yrs3 <- fh_5yrs2 %>% filter( symbol %in% 
                    c('AAPL', 'AMZN', 'GOOGL', "MSFT", "FB"))

#Test Assumptions for Linear Regression

## test for linearity

ggplot(fh_5yrs3,aes(date, close)) + geom_line()

## plotted variables in tableau, showed linear relationship.


## Test for homoscedasticity
lmMod <- lm(close~date, data=fh_5yrs3)


par(mfrow=c(2,2))
plot(lmMod)

## could not examine residual plots well, decided to examine with breush pagan test.
lmtest::bptest(lmMod)
## p-value is significant, didn't meet assumptions of homoscedasticity


## Test for homogeneity of variance
gvlma(lmMod)

### assumptions not all met

#test for outliers
CookD(lmMod, group=NULL, plot=TRUE, idn=3, newwd=TRUE)

lev = hat(model.matrix(lmMod))
plot(lev)

fh_5yrs3[lev>.2,]

car::outlierTest(lmMod)

summary(influence.measures(lmMod))

summary(lmMod)

### p-value is 1, not significant. No linear relationship showing between date and close of stocks in the Nasdaq


               
