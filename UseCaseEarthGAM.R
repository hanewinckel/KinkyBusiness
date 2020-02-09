#library(sqldf)
library(data.table)
library(mgcv)
library(earth)


#read in subset of data columns
dat <- fread("C:/Users/hfi/Documents/ILEC_2009-15 Data 20180601.txt",
             select = c('Observation Year','Gender','Smoker Status','Insurance Plan','Issue Age','Duration','Attained Age','Face Amount Band','SOA Anticipated Level Term Period',
                      'SOA Post level term indicator','Number of Preferred Classes', 'Preferred Class','Policies Exposed','Number of Deaths'),
             stringsAsFactors = TRUE,check.names = TRUE)

#Select Rows for simplified view
dat <- dat[Insurance.Plan == 'Term' & !SOA.Anticipated.Level.Term.Period %in% c('Unknown','Not Level Term') & 
             SOA.Post.level.term.indicator == 'Within Level Term' & Attained.Age %between% c(35,75) & 
             Number.of.Preferred.Classes >= 1 & Policies.Exposed > 0]

#Engineering - features and offsets
dat[,ClassProportion := Preferred.Class/Number.of.Preferred.Classes]
dat[,logExposure := log(Policies.Exposed)]

#Test Set is most recent year of observations - to observe extraploation
dat[,testSet := ifelse(Observation.Year == 2015,TRUE,FALSE)]

#Earth Model
modelEarth1 <- earth(Number.of.Deaths~Attained.Age+Duration+Gender+Smoker.Status+Face.Amount.Band+ClassProportion+offset(logExposure),
                     data = dat[testSet == FALSE],
                     glm = list(family = quasipoisson),
                     degree = 1
                     )
plot(modelEarth1) #Show fit plots, including terms used
plotmo(modelEarth1) #Show effect of predictors on poisson response
plotmo(modelEarth1,inverse.func = log) #Show effect of predictors on relative rates - shows "kinks" in hinges

modelEarth2 <- earth(Number.of.Deaths~Attained.Age+Duration+Gender+Smoker.Status+Face.Amount.Band+ClassProportion+offset(logExposure),
                     data = dat[testSet == FALSE],
                     glm = list(family = quasipoisson),
                     degree = 2
                     )

##GAM Model

modelGAM <- gam(Number.of.Deaths~Smoker.Status+Face.Amount.Band+Gender+
                  te(Attained.Age,Duration,by=Gender)+s(ClassProportion,k=5)+offset(logExposure),
    data = dat[testSet == FALSE],
    family = quasipoisson,
    control = list(nthreads = 5)) #Your (physical) cores here

#20mins
print(b)

