
# load dataset
sd <- read.csv('~/Kaggles/Speeddating/Speed Dating Data.csv')

# create binarized variables 
# to improve interpretability 
# and to generalize better 
# (i.e. avoid over-fitting)
sd <- within(sd, {
  male                        <- gender
  gender                      <- NULL
  age_diff                    <- age - age_o
  goout_frequently            <- go_out==1 | go_out==2 | go_out==3
  date_frequently             <- date==1 | date==2 | date==3
  has_met                     <- met==1
  likes_clubbing              <- clubbing>median(clubbing, na.rm=T)
  likes_gaming                <- gaming>median(gaming, na.rm=T)
  likes_exercising            <- exercise>median(exercise, na.rm=T)
  likes_sports                <- sports>median(sports, na.rm=T)
  likes_reading               <- reading>median(reading, na.rm=T)
  religion_important          <- imprelig>median(imprelig, na.rm=T)
  high_sd_expectations        <- exphappy>median(exphappy, na.rm=T)
  believes_self_attractive    <- attr3_1>median(attr3_1, na.rm=T)
})

# declare model formula
ff <- dec_o ~ male+
              goout_frequently+
              date_frequently+
              likes_clubbing+
              likes_gaming+
              likes_exercising+
              likes_sports+
              likes_reading+
              has_met+
              age_diff+
              religion_important+
              believes_self_attractive

# create model 
m <- glm(ff, data=sd, family=binomial(logit))

# summarize model attributes
sm <- summary(m)
sm

# view & interpret coeffients
exp(sm$coefficients[,1])-1
exp(sm$coefficients[,1]-sm$coefficients[,2]*1.96)-1
exp(sm$coefficients[,1]+sm$coefficients[,2]*1.96)-1
