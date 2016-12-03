A# Author: Frank Tamborello
# contact: frank.tamborello@cogscent.com
# CC-BY-SA 2016 Cogscent, LLC
#
# This library is free software; you can redistribute it or modify it under the terms of Creative Commons Attribute-ShareAlike 4.0 (CC BY) International License: http://creativecommons.org/licenses/by-sa/4.0/
#
# This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#
# Description: Hypothetical Brunswickian Lens model of phishing attackee decision-making behavior highlighting personality factors
#
# Acknowledgment: This research is sponsored by Measurement Science and Engineering grant 70NANB15H252 from the National Institute of Standards and Technology (NIST). Special acknowledgements are due to Drs. Mary Theofanos and Kristen K. Greene of NIST's Information Technology Laboratory.









# Test Data, Hypothetical Scenario: Personality traits predict phishing detection performance
# Execute this first, then linear models, then Lens model measures, to see a hypothetical effect of personality factors.
phishData <-
    data.frame(
        "judgment"=c(sample(c(0,1), size=500, replace=T, prob=c(.9, .1)),
                     sample(c(0, 1), size=500, replace=T, prob=c(.1, .9))),
        "state"=c(sample(c(0,1), size=500, replace=T, prob=c(.9, .1)),
                  sample(c(0, 1), size=500, replace=T, prob=c(.1, .9))),
        "anxiety"=c(rnorm(n=500, mean=-1),
                    rnorm(n=500, mean=1)),
        "selfConsciousness"=c(rnorm(n=500, mean=-1),
                              rnorm(n=500, mean=1)),
        "intellect"=c(rnorm(n=500, mean=1),
                      rnorm(n=500, mean=-1)),
        "trust"=c(rnorm(n=500, mean=-1),
                  rnorm(n=500, mean=1)),
        "cautiousness"=c(rnorm(n=500, mean=1),
                         rnorm(n=500, mean=-1)),
        "agreeableness"=c(rnorm(n=500, mean=-1),
                          rnorm(n=500, mean=1)),
        "gender"=sample(c(0, 1), size=1000, replace=T),
        "age"=rnorm(n=1000, mean=45, sd=5),
        "education"=rnorm(n=1000),
        "nYearsAtOrg"=rnorm(n=1000),
        "CareerPath"=rnorm(n=1000),
        "messageProperties"=c(rnorm(n=500, mean=0.5),
                              rnorm(n=500, mean=-0.5)));




# Build the linear models
lm.judgment <- lm(judgment ~ anxiety + selfConsciousness + intellect + trust + cautiousness + agreeableness + gender + age + education + nYearsAtOrg + CareerPath + messageProperties, data=phishData);
summary(lm.judgment);

lm.state <- lm(state ~ anxiety + selfConsciousness + intellect + trust + cautiousness + agreeableness + gender + age + education + nYearsAtOrg + CareerPath + messageProperties, data=phishData);
summary(lm.state);

# Get the model coefficients
coef.judgment <- coefficients(lm.judgment); coef.judgment
coef.state <- coefficients(lm.state); coef.state


# Get the predicted judgment & state values
pred.judgment <- fitted(lm.judgment);
pred.state <- fitted(lm.state);


# Compute measures of the Lens model
knowledge <- cor(pred.judgment, pred.state) # correlation between predicted judgment and state

achievement <- cor(phishData$judgment, phishData$state) # correlation between actual judgement and state

consistency <- cor(pred.judgment, phishData$judgment) # correlation between predicted and actual judgments

predictability <- cor(pred.state, phishData$state) # corrlation between predicted and actual states

# See the values of the Lens model
knowledge; achievement; consistency; predictability






# plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(pred.judgment)
plot(pred.state)
plot(phishData$judgment)
plot(phishData$state)



# Reference
http://www.statmethods.net/stats/regression.html
