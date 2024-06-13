setwd('D:\\SCMA 2024\\Data')
df_p = read.csv('IPL_ball_by_ball_updated till 2024.csv')

names(df_p)
dim(df_p)
library(dplyr)
head(df_p)

df_bat = df_p%>%
select(Match.id,Season,Batting.team, Innings.No,Ball.No,Bowler,Striker,            
runs_scored,score,score.wicket,wicket_confirmation,Player.Out)%>%
filter(Season=='2021'|Season=='2022'|Season=='2023' | Season=='2024')%>%
group_by(Striker,Match.id)%>%
summarise(runs= sum(runs_scored), wicket= sum(wicket_confirmation))
View(df_bat)

unique(df_p$Season)
df_player = df_p%>%
select(Match.id,Season,Innings.No,Bowler,Striker,            
runs_scored,score,score.wicket,wicket_confirmation)%>%
filter(Season=='2020/21'|Season=='2022'|Season=='2023' | Season=='2024')%>%
group_by(Striker,Bowler,Match.id,Season)%>%
summarise(runs= sum(runs_scored), wicket= sum(wicket_confirmation))%>%
arrange(Striker,Season)
View(df_player)

unique(df_player$Striker)
unique(df_player$Bowler)






batsmen = df_player%>%
group_by(Striker,Season)%>%
summarise(runs=sum( runs))%>%
arrange(desc(runs))
View(batsmen)

dim(batsmen )

any(is.na(batsmen))

#ANOVA
#install.packages('agricolae')
library(agricolae)
model<-aov(runs ~Striker ,data=batsmen)
summary(model)

comparison <- LSD.test(model,"Striker",alpha=0.05,group=TRUE)
print(comparison$groups)
oldpar<-par(cex=1.5)
bar.group(comparison$groups,horiz=TRUE,density=8,col="blue",border="red", xlim=c(0,50),las=1)
title(cex.main=0.8,main="Comparison between\ntreatment means",xlab="Yield",ylab="Virus")

# Example 2
model<-aov(runs ~Striker + Season ,data=batsmen)
out <- duncan.test(model,"Striker",
main="Yield of sweetpotato. Dealt with different virus")
plot(out,variation="IQR")
duncan.test(model,"Striker",alpha=0.01,console=TRUE)

# Example 3
model<-aov(runs ~Striker + Season ,data=batsmen)
out <- HSD.test(model,"Striker", group=TRUE,console=TRUE,
main="Yield of sweetpotato\nDealt with different virus")

# Example 4
model<-aov(runs ~Striker + Season ,data=batsmen)
out <- HSD.test(model,"Season", group=TRUE,console=TRUE,
main="Yield of sweetpotato\nDealt with different virus")
summary(model)


#stargraph
# Variation range: max and min
plot(out)
#endgraph
out<-HSD.test(model,"Season", group=FALSE)
print(out$comparison)



df_player = df_p%>%
select(Match.id,Season,Innings.No,Bowler,Striker,            
runs_scored,score,score.wicket,wicket_confirmation)%>%
filter(Season=='2020/21'|Season=='2022'|Season=='2023' | Season=='2024')%>%
group_by(Striker,Bowler,Match.id,Season)%>%
summarise(runs= sum(runs_scored), wicket= sum(wicket_confirmation))%>%
arrange(Striker,Season)
View(df_player)

head(df_player)

bowler = df_player%>%
group_by(Bowler,Season)%>%
summarise(wickets=sum(wicket))%>%
arrange(desc(wickets))
View(bowler )


model<-aov(wickets~Bowler+ Season ,data=bowler)
summary(model)
out <- HSD.test(model,"Bowler", group=TRUE,console=TRUE,
main="Wickets Taken\nduring different seasons")

# Example 3
model<-aov(wickets~Bowler+ Season ,data=bowler)
out <- HSD.test(model,"Bowler", group=TRUE,console=TRUE,
main="Wickets Taken\nduring different seasons")

===========================================================================
#CATEGORISE THE PLAYERS

library(dplyr)

# Load necessary library
library(dplyr)

# Assuming 'bowler' is your data frame and 'wickets' is the continuous variable
# Calculate the quantiles
quantiles <- quantile(bowler$wickets, probs = c(0, 0.25, 0.5, 0.75, 1))

# Use the quantiles to cut the data into categories
bowler <- bowler %>%
  mutate(cat_wkt = cut(wickets, breaks = quantiles, labels = c("low", "middle", "upper", "high"), include.lowest = TRUE))

# Filter the data frame for rows where cat_wkt is 'high'
bowler_high <- bowler %>%
  filter(cat_wkt == 'high')

# View the resulting data frame
print(bowler_high)

unique(bowler_high$Bowler)

# Example 3
model<-aov(wickets~Bowler+ Season ,data=bowler_high)
out <- HSD.test(model,"Bowler", group=TRUE,console=TRUE,
main="Wickets Taken\nduring different seasons")
