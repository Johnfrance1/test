##### LINEAR REGRESSION #######
###https://stackoverflow.com/questions/9238038/pass-a-vector-of-variables-into-lm-formula###
#https://www.datacamp.com/community/tutorials/linear-regression-R#
#https://statisticsglobe.com/write-model-formula-with-many-variables-in-r#
#http://www.cookbook-r.com/Formulas/Creating_a_formula_from_a_string/#
##############################################
# Graphing Function
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(col = "black", size = 3) +
    stat_smooth(method = "lm", col = "firebrick") +
    labs(subtitle = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5))) +
    theme_classic(base_size = 17) + 
    theme(title = element_text(size = 18,face = "bold"), axis.title = element_text(size = 16)) + 
    labs(title = paste(Predictors[1],"predicting",Dependant), y = Dependant, x = Predictors[1]) + 
    theme(plot.subtitle=element_text(size=16, hjust=0.5, face="italic", color="black"))
}
###############################################

#### LOAD YOUR DATA ####
setwd("/Users/gc9598/Wayne State University/Current Projects - General/facesXstartle/analysis/updated")
getwd()

data<-read.csv("facesXstartle_complete_allcontrasts_startle-21.csv")

#### MODEL SUMMARY ####
#lm([Dependant variable] ~ [Independant variables], data = [data source])

#### CREATE YOUR MODEL ####
## View col numbers ##
colnames(data)
## Select columns and enter column number into c(,) ## 
Dependant <- colnames(data_oc)[341]
Predictors <- colnames(data_oc)[c(312, 403)] #colnames(data)[1:5] colnames(data)[c(300,1:5)] 
#### RUN THE MODEL ####
# This creates the appropriate string:
paste(Dependant, paste(Predictors, collapse=" + "), sep=" ~ ")
# This returns the formula:
my_formula <- as.formula(paste(Dependant, paste(Predictors, collapse=" + "), sep=" ~ "))
# Estimate model based on formula
mod5 <- lm(my_formula, data)              
summary(mod5) 
summary(mod5)$coefficient

### PLOT THE RESULTS ###
plot(mod5, pch=16, col = "blue")
# plot the residuals #
plot(mod5$residuals, pch = 16, col = "red")
# plot cooks distance
plot(cooks.distance(mod5), pch = 16, col = "blue") #Plot the Cooks Distances.

### Plot For publication ### 
ggplotRegression(mod5)

##### Get table for publication ####





