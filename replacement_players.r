library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)
library(ggrepel)

bat.data <- read.csv("batting.csv")

head(bat.data)
# str(bat.data)

#Batting Average
bat.data$BA <- bat.data$H / bat.data$AB

#on base percentage
bat.data$OBP <- (bat.data$H + bat.data$BB + bat.data$HBP) / (bat.data$AB + bat.data$BB + bat.data$HBP + bat.data$SF)

#singles
bat.data$X1B <- bat.data$H - (bat.data$X2B + bat.data$X3B + bat.data$HR)

#slugging percentage
bat.data$SLG <- (bat.data$X1B + (2 * bat.data$X2B) + (3 * bat.data$X3B) + (4 * bat.data$HR) ) / bat.data$AB

sal <- read.csv("Salaries.csv")

# summary(sal)

#salaries data start from 1985
bat.data <- subset(bat.data, bat.data$year >= 1985)

#merging the datasets
bat_sal.data <- merge(bat.data, sal, by = intersect(names(bat.data), names(sal)))

#players to replace
lost_players <- c('giambja01','damonjo01','saenzol01')

#lost players data
lp_data <- subset(bat_sal.data, bat_sal.data$playerID %in% lost_players)

#filtering by year
lp_data_2001 <- subset(lp_data, lp_data$yearID == 2001)

#relevant attributes
lp_data_2001 <- select(lp_data_2001, playerID, H, X2B, X3B, HR, OBP, SLG, BA, AB, salary)

#constraints for replacements
constr.data <- data.table(salary = 15000000)
constr.data$AB <- sum(lp_data_2001$AB)
constr.data$OBP <- mean(lp_data_2001$OBP)

# constr.data

#filtering the main dataset by year
bat_sal_2001.data <- subset(bat_sal.data, yearID == 2001)

#sorting by asc salary and then desc AB, OBP
bat_sal_2001.data <- arrange(bat_sal_2001.data, salary, desc(AB), desc(OBP))

#required players
reqd_players <- subset(bat_sal_2001.data, bat_sal_2001.data$AB >= median(lp_data_2001$AB) & bat_sal_2001.data$OBP >= median(lp_data_2001$OBP))

reqd_players <- select(reqd_players, playerID, H, X2B, X3B, HR, OBP, SLG, BA, AB, salary)

reqd_players$salarySlabs = ifelse( reqd_players$salary <= 5000000, 1, ifelse( reqd_players$salary <= 10000000, 2, 3))

reqd_players <- subset(reqd_players, salarySlabs != 3 )
#plotting the filtered players. sizes of point relative to their salary slabs
pl <- plot_ly(type = 'scatter', mode = 'markers') %>%
  add_trace( data = lp_data_2001,
             x = ~AB, 
             y = ~OBP,
             text = ~playerID,
             hoverinfo = 'text',
             marker = list(color='red', symbol = 18, size = 15),
             showlegend = F
  )

pl <- add_trace(pl,data = reqd_players,
          x = ~AB, 
          y = ~OBP,
          text = ~playerID,
          hoverinfo = 'text',
          marker = list(color='blue', size = ~I(8*salarySlabs)),
          showlegend = F
)