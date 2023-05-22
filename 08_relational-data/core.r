# Library imports
library(tidyverse)

# Folder names
data_folder <- "data"
keys_folder <- "keys"
fig_folder <- "fig"

# Datasets folder files list
list_files <- list.files(data_folder)
# Keys folder files list
list_pdf <- list.files(keys_folder)

# Number of datasets
n <- length(list_files)

# Changing size of plot
options(repr.plot.width=25, repr.plot.height=12)

# Name list of datasets
d_names <- list()
f_names <- list()
for (i in 1:n){
    f_names[i] <- list_files[i]
    temp_name <- list_files[i]
    temp_name <- tools::file_path_sans_ext(temp_name)
    d_names[i] <- temp_name
    temp_df <- read.csv(file.path(data_folder, f_names[i]), dec=",", encoding="UTF-8")
    assign(d_names[[i]], temp_df)
}

# Top ten scoring teams
df_team <- inner_join(teams, teamstats, by="teamID")
team_goals <- group_by(df_team, teamID) %>% mutate(total_goals=sum(goals)) %>%
    subset(select=-c(yellowCards, redCards, result, season, gameID, date, location, goals, fouls, corners, ppda)) %>% arrange(desc(total_goals))

best_10_goals <- distinct(team_goals, teamID, .keep_all=TRUE)
best_10_goals <- head(best_10_goals, 10)

# Graph of top ten scoring teams
p1 <- ggplot(best_10_goals, aes(x=reorder(name, total_goals), y=total_goals)) + 
    geom_bar(stat = "identity") +
    geom_label(aes(label=total_goals),
                #vjust=-0.9, 
                color="dark orange", 
                hjust="center", 
                angle=0, 
                size=6.0,
                fontface="bold"
            ) +
    coord_cartesian(ylim=c(450, 720)) +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 24)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), plot.title = element_text(color="Black", size=28, face="bold")) +
    labs(x="Teams", y="Goals per year, seasons 2015-2020", title="Top 10 scoring teams")
fig1_name <- "fig1.jpeg"

# Top ten teams with the highest number of shots on goal
best_10_shots <- group_by(team_goals, teamID) %>% mutate(total_shots=sum(shots)) %>%
    summarise(name, total_goals, total_shots, total_deep=sum(deep), total_OnTarget=sum(shotsOnTarget)) %>%
    arrange(desc(total_shots)) %>% distinct(teamID, .keep_all=TRUE)
best_10_shots <- head(best_10_shots, 10)

# Graph of top ten teams with the highest number of shots on goal
p2 <- ggplot(best_10_shots, aes(x=reorder(name, total_shots), y=total_shots, fill=total_OnTarget)) + 
    geom_bar(stat = "identity") +
    geom_label(aes(label=total_shots),
                color="white", 
                hjust="center", 
                angle=0, 
                size=6.0,
                fontface="bold"
            ) +
    coord_cartesian(ylim=c(3900, 4700)) +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 24)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), plot.title = element_text(color="Dark Blue", size=28, face="bold")) +
    labs(x="Teams", y="Goals per year, seasons 2015-2020", fill="On Target Shots", title="Top 10 teams with the most shots on goal")
fig2_name <- "fig2.jpeg"

# Top ten scoring teams and with the highest number of shots on goal
teams_stats <- group_by(df_team, teamID) %>% mutate(total_shots=sum(shots), total_goals=sum(goals), total_deep=sum(deep), total_OnTarget=sum(shotsOnTarget)) %>%
    arrange(desc(total_goals)) %>% subset(select=-c(yellowCards, redCards, result, season, gameID, date, location, goals, fouls, corners, ppda))
teams_stats <- distinct(teams_stats, teamID, .keep_all=TRUE) %>%
    summarise(name, total_goals, total_shots, total_deep, total_OnTarget) %>%
    arrange(desc(total_goals))
best_10 <- head(teams_stats, 10)

# Graph
p3 <- ggplot(best_10, aes(x=reorder(name, total_goals))) + 
    geom_point(aes(y=total_shots), size=5, color="dark green") +
    geom_label(aes(label=total_shots, y=total_shots),
                vjust="bottom",
                color="dark green", 
                hjust="left", 
                angle=0, 
                size=6.0,
                fontface="bold",
                nudge_x=0.05,
            ) +
    geom_point(aes(y=total_goals), size=5, color="dark orange") +
    geom_label(aes(label=total_goals, y=total_goals),
                vjust="bottom",
                color="dark orange", 
                hjust="left", 
                angle=0, 
                size=6.0,
                fontface="bold",
                nudge_x=0.05,
            ) +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 24)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), plot.title = element_text(color="Dark Blue", size=28, face="bold")) +
    labs(x="Teams", y="Goals per year, seasons 2015-2020", title="Top 10 scoring teams and with the most shots on goal", color="Tendencies")
fig3_name <- "fig3.jpeg"

# Graph
p4 <- ggplot(best_10, aes(x=reorder(name, total_goals))) + 
    geom_point(aes(y=total_shots), size=5, color="dark green") +
    geom_label(aes(label=total_shots, y=total_shots),
                vjust="bottom",
                color="dark green", 
                hjust="left", 
                angle=0, 
                size=6.0,
                fontface="bold",
                nudge_x=0.05,
            ) +
    geom_point(aes(y=total_goals), size=5, color="dark orange") +
    geom_label(aes(label=total_goals, y=total_goals),
                vjust="bottom",
                color="dark orange", 
                hjust="left", 
                angle=0, 
                size=6.0,
                fontface="bold",
                nudge_x=0.05,
            ) +
    geom_point(aes(y=total_OnTarget), size=5, color="red") +
    geom_label(aes(label=total_OnTarget, y=total_OnTarget),
                vjust="bottom",
                color="red", 
                hjust="left", 
                angle=0, 
                size=6.0,
                fontface="bold",
                nudge_x=0.05,
            ) +
    geom_point(aes(y=total_deep), size=5, color="dark violet") +
    geom_label(aes(label=total_deep, y=total_deep),
                vjust="bottom",
                color="dark violet", 
                hjust="left", 
                angle=0, 
                size=6.0,
                fontface="bold",
                nudge_x=0.05,
            ) +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 24)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), plot.title = element_text(color="Dark Blue", size=28, face="bold")) +
    labs(x="Teams", y="Goals per year, seasons 2015-2020", title="Top 10 scoring teams and with the most shots on goal", color="Tendencies")
fig4_name <- "fig4.jpeg"

# Graph
p5 <- ggplot(teams_stats, aes(x=reorder(name, total_goals))) + 
    geom_point(aes(y=total_shots), size=5, color="dark green") +
    geom_point(aes(y=total_goals), size=5, color="dark orange") +
    geom_point(aes(y=total_OnTarget), size=5, color="red") +
    geom_point(aes(y=total_deep), size=5, color="dark violet") +
    theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 24)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), plot.title = element_text(color="Dark Blue", size=28, face="bold")) +
    labs(x="Teams", y="Goals per year, seasons 2015-2020", title="Top 10 scoring teams and with the most shots on goal", color="Tendencies")
fig5_name <- "fig5.jpeg"



# League selection
df_leagues <- inner_join(leagues, games, by="leagueID") %>% group_by(leagueID)

league_goals <- summarise(df_leagues, leagueID, name, awayGoals_l=sum(awayGoals), homeGoals_l=sum(homeGoals)) %>%
    distinct(leagueID, .keep_all=TRUE) %>% mutate(sumGoals=awayGoals_l+homeGoals_l) %>% arrange(., -sumGoals)



# League selection
df_leagues <- inner_join(leagues, games, by="leagueID") %>% group_by(leagueID)

league_goals <- summarise(df_leagues, leagueID, name, awayGoals_l=sum(awayGoals), homeGoals_l=sum(homeGoals)) %>%
    distinct(leagueID, .keep_all=TRUE) %>% mutate(sumGoals=awayGoals_l+homeGoals_l) %>% arrange(., -sumGoals)



# Top scoring players
df_shots <- inner_join(df_leagues, shots, by="gameID") %>%
    summarise(name, gameID, homeTeamID, awayTeamID, homeGoals, awayGoals, shooterID, minute)

df_players <- inner_join(df_leagues, appearances, by=c("leagueID", "gameID")) %>%
    summarise(leagueID, gameID, playerID, goals, shots, homeGoals, awayGoals, date, time)

best_shooters <- inner_join(df_players, players, by="playerID") %>% group_by(playerID) %>%
    mutate(total_goals=sum(goals), total_homeGoals=sum(homeGoals), total_awayGoals=sum(awayGoals))
best_5_shooters <- summarise(best_shooters, playerID, name, total_goals) %>%
    distinct(playerID, .keep_all=TRUE) %>% arrange(desc(total_goals))
best_5_shooters <- head(best_5_shooters, 5)


# Statistics of all players 
all_players <- inner_join(df_leagues, appearances, by=c("leagueID", "gameID")) %>%
    summarise(leagueID, gameID, playerID, goals, shots, homeGoals, awayGoals, assists, keyPasses, yellowCard, redCard, date, time)

# Most influencer players of all leagues
influencer_players <- inner_join(players, shots, by=c("playerID"="shooterID"))
influencer_players <- inner_join(influencer_players, all_players, by=c("playerID", "gameID")) %>%
    summarise(playerID, player_name=name, gameID, assisterID, minute, shotResult, assists, date, time)


# Top 5 assisters
best_assister <- group_by(influencer_players, assisterID) %>%
    summarise(assisterID, assists) %>%
    inner_join(players, by=c("assisterID"="playerID")) %>%
    group_by(assisterID) %>%
    mutate(total_asistencias=sum(assists)) %>%
    arrange(-total_asistencias) %>%
    distinct(assisterID, .keep_all = TRUE) %>%
    filter(!assisterID==8978)
best_5_assiter <- head(best_assister, 5)


# Top 5 players according a score, score = (total_goals + total_asistencias)/total_players
best_players <- inner_join(best_shooters, best_assister, by=c('playerID'='assisterID')) %>%
    summarise(name=name.x, total_asistencias, total_goals, score=total_goals+total_asistencias) %>%
    arrange(-score) %>% distinct(playerID, .keep_all = TRUE) %>%
    ungroup() %>% mutate(total_players=n(), score=score/total_players)
best_5_players <- head(best_players, 5)
best_5_players_names <- best_5_players$name

# Graph
p6 <- ggplot(data=best_players, aes(x=score)) +
        geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
        geom_vline(data=best_5_players, aes(xintercept=score, color=name, size=1.5)) +
        scale_y_sqrt() +
        theme(axis.line = element_line(colour = "black", size = 1), text = element_text(size = 24))
fig6_name <- "fig6.jpeg"


# Top 5 fairplay players
fairplay_players <- inner_join(players, all_players, by="playerID") %>%
    summarise(playerID, player_name=name, yellowCard, redCard, date, time) %>% group_by(playerID) %>%
    mutate(amarillas=sum(yellowCard), rojas=sum(redCard))

df_rojas <- mutate(fairplay_players, total=(amarillas/4 + rojas)) %>%
    group_by(total) %>% summarise(player_name, total, rojas, amarillas) %>% arrange(total)
best_fairplay <- head(unique(df_rojas), 5)
best_fairplay_names <- best_fairplay$player_name

