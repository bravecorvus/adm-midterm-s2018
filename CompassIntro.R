library(tidyverse)


#######################################################
raw_data <- read.csv("./compas-scores-two-years.csv")
nrow(raw_data)
names(raw_data)


df <- dplyr::select(raw_data, age, c_charge_degree, race, age_cat, score_text, sex, priors_count,
                    days_b_screening_arrest, decile_score, is_recid, two_year_recid, c_jail_in, c_jail_out) %>%
        filter(days_b_screening_arrest <= 30) %>%
        filter(days_b_screening_arrest >= -30) %>%
        filter(is_recid != -1) %>%
        filter(c_charge_degree != "O") %>%
        filter(score_text != 'N/A')
nrow(df)
with(df,table(is_recid,two_year_recid))
head(df)

############################################
df <- df%>%
    mutate(length_of_stay =as.numeric(as.Date(df$c_jail_out) - as.Date(df$c_jail_in)))


with(df,cor(length_of_stay, decile_score))

summary(df$age_cat)
summary(df$race)

#######################################################
library(grid)
library(gridExtra)
pblack <- ggplot(data=filter(df, race =="African-American"), aes(ordered(decile_score))) +
          geom_bar() + xlab("Decile Score") +
          ylim(0, 650) + ggtitle("Black Defendant's Decile Scores")
pwhite <- ggplot(data=filter(df, race =="Caucasian"), aes(ordered(decile_score))) +
          geom_bar() + xlab("Decile Score") +
          ylim(0, 650) + ggtitle("White Defendant's Decile Scores")
grid.arrange(pblack, pwhite,  ncol = 2)


#######################################################
##
df2 <- df%>%
    mutate(crime_factor = factor(c_charge_degree),
           age_factor = as.factor(age_cat),
           age_factor = relevel(age_factor, ref = 1),
           race_factor = factor(race),
           race_factor =relevel(race_factor, ref = 3),
           gender_factor = factor(sex, labels= c("Female","Male")),
           gender_factor= relevel(gender_factor, ref = 2),
           score_factor = factor(score_text != "Low", labels = c("LowScore","HighScore")))

model <- glm(score_factor ~ gender_factor + age_factor + race_factor +
                            priors_count + crime_factor + two_year_recid, family="binomial", data=df2)

summary(model)

names(df)

model2 <- glm(two_year_recid ~ gender_factor + age_factor + race_factor +
                            priors_count + crime_factor , family="binomial", data=df2)
summary(model2)


vals <- predict(model2,newdata=df2)
df2$prob <- exp(vals)/(1+exp(vals))
names(df2)
with(df2,table(score_factor))
with(df2,plot(prob,decile_score))
