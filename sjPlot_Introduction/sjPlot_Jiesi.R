
##Installation

# load packages
library(sjPlot)
library(sjmisc)

pisa<-read_spss("PISA12-AUS-motivation.sav", 
                enc="UTF-8",                # character encoding
                autoAttachVarLabels = TRUE) # attach variable labels
# overview your data using `view_spss` and `sjt.df`
view_spss(pisa[,1:8],
          #  showFreq = TRUE, 
          #  showPerc = TRUE
)


# display data description like 'describe' function in 'psych' package

# print correlation 
sjt.corr(pisa[,20:28], 
         fadeNS = FALSE,  
         pvaluesAsNumbers = TRUE,  # FALSE: significant levels are indicated by asterisks
         showPValues = TRUE       # show exact pvalues
         #triangle = "lower"      # print the values in the lower triangle of the table
         # val.rm = 0.1           # specify a value where correlation values are not printed to the table
)

# print summaries of linear models using `sjt.lm`
# model1: math behavior is predicted by math self-concept, interest value, intrinsic value and gender
model1 <- lm(MATBEH ~ SCMAT + INTMAT + INSTMOT + Gender, data=pisa)
# model2: math workethic is predicted by math self-concept, interest value, intrinsic value and gender
model2 <- lm(MATWKETH ~ SCMAT + INTMAT + INSTMOT + Gender, data=pisa)

sjt.lm(model1, model2)


# add custom labels and convert to APA output
sjt.lm(model1, model2,
       showHeaderStrings = TRUE,
       stringB = "Estimate",
       showStdBeta = TRUE,
#       stringCI = "Conf. Int.",
       pvaluesAsNumbers = FALSE,
       showStdError = TRUE,
       showConfInt = FALSE,
       stringDependentVariables = "Outcomes",
       stringPredictors = "Predictors",
       labelDependentVariables = c("Math behaviors", 
                                  "Math work ethic")
#      CSS = list(css.topcontentborder = "+font-size: 0px;")  # remove intercept
#      remove.estimates = 2  # remove the second coefficient after intercept
       )       
                                  
# another package [texteg](http://cran.r-project.org/web/packages/texreg/texreg.pdf) - extract output from a vast majority of models                                  
# extract similar output using `texreg`
install.packages("texreg")
library(texreg)
screenreg(list(
  extract(model1, summaries = c("Observations", "CFI", "SRMR")),
  extract(model2, summaries = c("Observations", "CFI", "SRMR"))),
  single.row=TRUE)

#  plot coefficients of linear model using `sjp.lm`
sjp.lm(model1, type = "std") # plot standardized coefficients
# testing model assumption including (tests for outliers, multicollinearity, non-normality of residual and outliers, non-normlity of residual, homoscedasticity)
sjp.lm(model1, type = "ma",
       showOriginalModelOnly = TRUE) # FLASE: the following graphical tests will be both applied to the original and to the updated model
# investigate each single predictor of the model
sjp.lm(model1, type = "pred")         

# plotting interaction using `sjp.int`, which accepts _lm_, _glm_ and _lme4_.[see more examples](http://www.strengejacke.de/sjPlot/sjp.int/)
install.packages("effects") 
#Plotting interaction betweenn self-concept and intrinsic value in preiding math workethic
mod_inter <- lm(scale(MATWKETH) ~ scale(SCMAT)#scale(INSTMOT), data=pisa, x = TRUE)
sjt.lm(mod_inter)
library(effects)
sjp.int(mod_inter, type = "eff",
        #showValueLabels = TRUE, # show value labels
        #showCI = TRUE           # show confidence interval
        moderatorValues = "meansd") #use the mean as well as one standard deviation above and below the mean value.

# provide an base-graphic-example for simple-slopes plot 
f1<- function(x)(.38)#x+(.34)#(+1)+(.07)#(+1)#x
f2<- function(x)(.38)#x+(.34)#0+(.07)#(0)#x
f3<- function(x)(.38)#x+(.34)#(-1)+(.07)#(-1)#x

plot(f1, from=-1, to=1, lty=1,lwd=3, add=TRUE, ylab="MATWKETH (SD)", xlab="SCMAT (SD)", xlim=c(-1,1), ylim=c(-1,1))
plot(f2, from=-1, to=1, lty=3,lwd=3, add=TRUE, ylab="MATWKETH (SD)", xlab="SCMAT (SD)", xlim=c(-1,1), ylim=c(-1,1))
plot(f3, from=-1, to=1, lty=5,lwd=3, add=TRUE, ylab="MATWKETH (SD)", xlab="SCMAT (SD)", xlim=c(-1,1), ylim=c(-1,1))
legend("topleft", lty=c(1,2,3),lwd=c(3,3,3),legend=c('INSTMOT = +1SD','INSTMOT = Mean','INSTMOT = -1SD'))
# summaries odds ratios (forest plots) of generalized linear models using `sjt.glm`
# outcome: ST48Q01 - "Intention to attend Math vs. Language courses after school"
#          ST48Q03 - "Intention to study hard in Math vs. language classes"
model3 <- glm(ST48Q01 ~ SCMAT + INTMAT + INSTMOT + Gender, data=pisa, family = binomial(link = "logit"))
model4 <- glm(ST48Q03 ~ SCMAT + INTMAT + INSTMOT + Gender, data=pisa, family = binomial(link = "logit"))
#summary(fit4)
sjt.glm(model3,model4, 
        labelDependentVariables = c("Intention to attend Math vs. Language courses after school", 
                                    "Intention to study hard in Math vs. language classes"))

# plot probability curves of predictors using `sjp.glm`
#The odds ratios for continous predictors are a bit more difficult to interprete than categorial variables. Thus, you can also plot probability curves of all predictors (covariates, coefficients)
sjp.glm(model3,
        axisLabels.y = predlab,
        type = "prob",
        show.se = TRUE)
# plot ramdon effects of linear mixed effects using `sjp.lmer`
library(lme4)
pisa1 <-pisa[as.numeric(pisa$SCHOOLID) < 21, ] # select the first 20 classes
modelRandomInt <- lmer(MATWKETH ~ SCMAT + INTMAT + INSTMOT + (1|SCHOOLID) , data = pisa1)
summary(modelRandomInt)

# plot standardized fixed effects
sjp.lmer(modelRandomInt, type = "fe.std")
# plot fixed effects depending on group levels
sjp.lmer(modelRandomInt, type = "fe.ri",
        vars = "SCMAT") # only for predictior math self-concept
# view ramdom effets of linear mixed effects using `sjp.lmer`
modelRandomSlope <- lmer(MATWKETH ~ SCMAT + INTMAT + INSTMOT + (SCMAT + INTMAT + INSTMOT|SCHOOLID) , data = pisa1)
summary(modelRandomSlope)
#Lets compare a random intercept to a random slope model
anova(modelRandomInt, modelRandomSlope)
# plot standardized fixed effects
sjp.lmer(modelRandomInt, type = "fe.std")
# View random effects 
sjp.lmer(modelRandomSlope)
# note: random effects can not be ploted in this version
# Plotting random effects of generalized linear mixed effects models using [sjp.glmer] (http://www.strengejacke.de/sjPlot/sjp.glmer/) (not cover here)
# polt principal component analyses (PCA) using `sjp.pca`
# we need to specify the items which are used in PCA
# recveive first item of scale containing self-concept, intrinsic value and utility value
start <- which(colnames(pisa)=="ST42Q02")
# recveive last item of scale containing self-concept, intrinsic value and utility value
end <- which(colnames(pisa)=="ST29Q08")
sjt.pca(pisa[,c(start:end)], 
        numberOfFactors = 3,
        showVariance=TRUE)


