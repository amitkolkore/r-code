Exploration_LHS_ACF <- read.csv("~/TU Delft/Honours programme/Project/ACF Data (q1)/Exploration_LHS_ACF.csv", header=FALSE)
`1_model_ACF_exp_0` <- read.csv("~/TU Delft/Honours programme/Project/ACF Data (q1)/1_model_ACF_exp_0.csv")
`1_model_ACF_exp_1` <- read.csv("~/TU Delft/Honours programme/Project/ACF Data (q1)/1_model_ACF_exp_1.csv")
`1_model_ACF_exp_2` <- read.csv("~/TU Delft/Honours programme/Project/ACF Data (q1)/1_model_ACF_exp_2.csv")
`1_model_ACF_exp_3` <- read.csv("~/TU Delft/Honours programme/Project/ACF Data (q1)/1_model_ACF_exp_3.csv")
`1_model_ACF_exp_4` <- read.csv("~/TU Delft/Honours programme/Project/ACF Data (q1)/1_model_ACF_exp_4.csv")
`1_model_ACF_exp_5` <- read.csv("~/TU Delft/Honours programme/Project/ACF Data (q1)/1_model_ACF_exp_5.csv")
`1_model_ACF_exp_6` <- read.csv("~/TU Delft/Honours programme/Project/ACF Data (q1)/1_model_ACF_exp_6.csv")
`1_model_ACF_exp_7` <- read.csv("~/TU Delft/Honours programme/Project/ACF Data (q1)/1_model_ACF_exp_7.csv")

library(ggplot2)
##Putting everything in one vector
vec1 = c(`1_model_ACF_exp_0`$Agenda_issue, `1_model_ACF_exp_2`$Agenda_issue, `1_model_ACF_exp_4`$Agenda_issue, `1_model_ACF_exp_6`$Agenda_issue)
vec2 = c(`1_model_ACF_exp_1`$Agenda_issue, `1_model_ACF_exp_3`$Agenda_issue, `1_model_ACF_exp_5`$Agenda_issue, `1_model_ACF_exp_7`$Agenda_issue)
vec3 = c(`1_model_ACF_exp_0`$Chosen_instrument, `1_model_ACF_exp_2`$Chosen_instrument, `1_model_ACF_exp_4`$Chosen_instrument, `1_model_ACF_exp_6`$Chosen_instrument)
vec4 = c(`1_model_ACF_exp_1`$Chosen_instrument, `1_model_ACF_exp_3`$Chosen_instrument, `1_model_ACF_exp_5`$Chosen_instrument, `1_model_ACF_exp_7`$Chosen_instrument)
##Creating the time column for all the modeloutput
time = 0:499
timelst = c(time, time, time, time)
##Creating an Affiliationlist for the issues
afflst = c(rep(0, 500), rep(1, 500), rep(2, 500), rep(3, 500))
##Making the plot
df1 = data.frame(timelst, vec1, afflst)
#plot(timelst, vec1)
df2 = data.frame(timelst, vec2, afflst)
ggplot(df1, aes(x=timelst, y=vec1)) + geom_line(colour="red") + facet_grid(. ~ afflst) + geom_line(aes(y = vec3), colour="blue")
ggplot(df2, aes(x=timelst, y=vec2)) + geom_line(colour="red") + facet_grid(. ~ afflst) + geom_line(aes(y = vec4), colour="blue")