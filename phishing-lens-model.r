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
#
#
# Revision 5
#
# Revision History
# 2016.07.08 fpt 1
# Init
#
# 2016.07.09 fpt 2
# Added header
#
# 2016.11.21 fpt 3
# Deleted the random data scenario
#
# 2016.12.03 fpt 4
# Added factors for user work state context, such as the accountant who receives a phish ostensibly regarding an unpaid invoice, and the personality factor consciensciousness.
#
# 2016.12.07 fpt 5
# Manipulate factor weights







# Test Data, Hypothetical Scenario: Personality traits predict phishing detection performance
# Execute this first, then linear models, then Lens model measures, to see a hypothetical effect of personality factors.
phishData <-
    data.frame(
        "judgment"=c(sample(c(0,1), size=500, replace=T, prob=c(.95, .05)),
                     sample(c(0, 1), size=500, replace=T, prob=c(.05, .95))),
        "state"=c(sample(c(0,1), size=500, replace=T, prob=c(.95, .05)),
                  sample(c(0, 1), size=500, replace=T, prob=c(.05, .95))),
        "messageProperties"=c(rnorm(n=500, mean=0.5),
                              rnorm(n=500, mean=-0.5)),
        "context"=c(sample(c(0,0.5,1), size=450, replace=T, prob=c(.9, .05, .05)),
                    sample(c(0,0.5,1), size=100, replace=T, prob=c(.05, .9, .05)),
                    sample(c(0,0.5,1), size=450, replace=T, prob=c(.05, .05, .9))),
        "conscientiousness"=c(rnorm(n=500, mean=-1, sd=0.5),
                               rnorm(n=500, mean=1, sd=0.5)),
        "anxiety"=c(rnorm(n=500, mean=-0.75, sd=0.75),
                    rnorm(n=500, mean=0.75, sd=0.75)),
        "selfConsciousness"=c(rnorm(n=500, mean=-0.75, sd=0.75),
                              rnorm(n=500, mean=0.75, sd=0.75)),
        "intellect"=c(rnorm(n=500, mean=1, sd=0.2),
                      rnorm(n=500, mean=-1, sd=0.2)),
        "trust"=c(rnorm(n=500, mean=-1, sd=0.2),
                  rnorm(n=500, mean=1, sd=0.2)),
        "cautiousness"=c(rnorm(n=500, mean=1, sd=0.25),
                         rnorm(n=500, mean=-1, sd=0.25)),
        "agreeableness"=c(rnorm(n=500, mean=-0.75, sd=0.75),
                          rnorm(n=500, mean=0.75, sd=0.75)),
        "gender"=sample(c(0, 1), size=1000, replace=T),
        "age"=rnorm(n=1000, mean=45, sd=5),
        "education"=rnorm(n=1000),
        "nYearsAtOrg"=rnorm(n=1000),
        "CareerPath"=rnorm(n=1000));




# Build the linear models
lm.judgment <- lm(judgment ~ context + conscientiousness +anxiety + selfConsciousness + intellect + trust + cautiousness + agreeableness + gender + age + education + nYearsAtOrg + CareerPath + messageProperties, data=phishData);
summary(lm.judgment);

lm.state <- lm(state ~ context + conscientiousness + anxiety + selfConsciousness + intellect + trust + cautiousness + agreeableness + gender + age + education + nYearsAtOrg + CareerPath + messageProperties, data=phishData);
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
