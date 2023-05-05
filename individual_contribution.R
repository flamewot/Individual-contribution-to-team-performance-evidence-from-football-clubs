####ЗАГРУЗКА И ПЕРВИЧНАЯ ОБРАБОТКА ДАННЫХ####
#remove.packages("worldfootballR")
#library(remotes)
#remotes::install_github("JaseZiv/worldfootballR")
#devtools::install_github("FCrSTATS/SBpitch")

#пакеты
library(SBpitch)
library(worldfootballR)
library(qvcalc)
library(stargazer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(janitor)
library(tidyverse)
library(reshape2)
library(data.table)
library(car)
library(lmtest)
library(MASS)
library(LaplacesDemon)
library(footBayes)
library(egg)
library(ggpubr)
library(rstatix)
library(showtext)
library(sandwich)
library(wesanderson)
library(openxlsx)
library(pscl)

#как команды закончили сезон
standings <- fb_season_team_stats("ENG", "M", 2022, "1st", "league_table")
standings <- standings %>% dplyr::select('Squad', 'Rk')
standings <- standings[order(standings$Rk, decreasing=TRUE),]
print(standings)

#порядок команд
team_order <- c("Norwich City", "Watford", "Burnley", "Leeds United", "Everton",   
                "Southampton", "Aston Villa", "Brentford", "Crystal Palace", "Newcastle United",
                "Wolverhampton Wanderers", "Brighton & Hove Albion", "Leicester City", "West Ham United", "Manchester United",
                "Arsenal", "Tottenham Hotspur", "Chelsea", "Liverpool", "Manchester City")
team_standings <- as.data.frame(team_order)
team_standings$team_place <- c(20:1)
colnames(team_standings)[1] <- 'club'

#список дат в нужном формате(YYYYMMDD), за которые нужны матчи
dates <- read.csv("dates1.csv", header = TRUE, sep = ";", dec = ",")
dates <- dplyr::pull(dates, dates) #превратили в вектор

#результаты матчей по подгруженным датам
results <- fotmob_get_matches_by_date(date = dates)
results_1 <- results[results$id == '47', ] #оставил только АПЛ
results_1 <- results_1[complete.cases(results_1$match_status_score_str),] #оставил только сыгранные матчи

#идентификаторы матчей по результатам
match_ids <- results %>%
  dplyr::select(primary_id, ccode, league_name = name, match_id) %>%
  dplyr::filter(league_name == "Premier League", ccode == "ENG") %>%
  dplyr::pull(match_id)

#составы матчей по id
squads <- fotmob_get_match_players(match_ids)
squads <- apply(squads,2,as.character)
squads <- as.data.frame(squads)
squads <- squads[squads$is_starter == TRUE, ] #нужны только стартовые составы
squads <- squads %>% distinct() #удалил дубли
write.csv(squads, "squads.csv")

#приклеил к игрокам результаты игр по id матча
lineups_plus_results <- merge(squads, results_1, by = "match_id", all.x = TRUE)

#столбец с полным именем игрока
lineups_plus_results_1 <- lineups_plus_results %>% 
  mutate(Player_Name = paste(first_name,last_name,sep=" "))

#столбец с кодом состава
lineups_plus_results_1$tmp <- with(lineups_plus_results_1, ifelse(home_long_name == team_name, team_name, away_long_name))
lineups_plus_results_2 <- lineups_plus_results_1 %>% 
  mutate(code_squad = paste(tmp,match_tournament_stage,sep="_"))

#столбец с количеством очков
lineups_plus_results_2$points <- with(lineups_plus_results_2, ifelse((tmp == home_long_name & home_score > away_score) | (tmp == away_long_name & home_score < away_score), 3, 
                              ifelse((tmp == away_long_name & home_score > away_score) | (tmp == home_long_name & home_score < away_score), 0, 1)))






####ВЫЧИСЛЕНИЕ ШЕПЛИ ЗАДАНИЕМ КОЛИЧЕСТВА ОЧКОВ####
#сводник - по столбцам код состава, по строкам игроки
grouped_players <- lineups_plus_results_2 %>% 
  group_by(Player_Name, code_squad) %>%
  summarize(points_sum = sum(points, na.rm = TRUE))
all_clubs <- pivot_wider(grouped_players, names_from = code_squad, values_from = points_sum, values_fill = NA)
player_club <- unique(lineups_plus_results_2 %>% dplyr::select('Player_Name', 'team_name'))
agg_tbl <- aggregate(data = lineups_plus_results_2, Player_Name ~ team_name, function(Player_Name) length(unique(Player_Name)))
colnames(agg_tbl)[2] <- 'count'
player_count <- c(agg_tbl$Player_Name)
player_club <- merge(player_club, agg_tbl, by = "team_name", all.x = TRUE)
all_clubs <- merge(all_clubs, player_club, by = "Player_Name", all.x = TRUE)

#функция для подсчета Шепли игрока n
shapley <- function(df, n){
  sum <- sum(df[n,2:(ncol(df)-2)], na.rm=TRUE) 
  length <- rowSums(!is.na(df))
  return((sum)/(df[n,ncol(df)]*length[n]))
}

#составил таблицу со всеми значениями Шепли
all_clubs_shapley <- all_clubs["Player_Name"]

#циклом заполнил строки таблицы с помощью вышенаписанной функции
for (i in 1:nrow(all_clubs_shapley)) {
  all_clubs_shapley$Shapley[i] <- shapley(all_clubs, i)
}

#умножил все значения Шепли на 100 для простоты восприятия
all_clubs_shapley$Shapley <- all_clubs_shapley$Shapley*100

#посчитал средний рейтинг fotmob за сезон для каждого игрока
lineups_plus_results_2$stats_fot_mob_rating <- as.numeric(lineups_plus_results_2$stats_fot_mob_rating)
final_by_player <- aggregate(stats_fot_mob_rating ~ Player_Name, data = lineups_plus_results_2, FUN ="mean")
names(final_by_player) <- c("Player_Name", "rating")

#склеил Шепли и средний рейтинг в одну таблицу
final <- merge(all_clubs_shapley, final_by_player, by = "Player_Name", all.x = TRUE)

#визуализация связи среднего рейтинга и Шепли
ggplot(data = final, aes(x = rating, y = Shapley))+geom_point(color = 'darkblue')+xlab('FotMob rating')+ylab('Shapley value')






####ЗАДАНИЕ ХФ ЧЕРЕЗ РАЗНИЦУ ГОЛОВ####
#разница голов
lineups_plus_results_2$goal_diff <- 
  with(lineups_plus_results_2,
  ifelse(home_long_name == tmp, home_score-away_score, away_score-home_score))





####ВЫЧИСЛЕНИЕ ШЕПЛИ ЧЕРЕЗ РАЗНИЦУ ГОЛОВ####
#сводник - по столбцам код состава, по строкам игроки
grouped_players_gd <- lineups_plus_results_2 %>% 
  group_by(Player_Name, code_squad) %>%
  summarize(gd_sum = sum(goal_diff, na.rm = TRUE))
all_clubs_gd <- pivot_wider(grouped_players_gd, names_from = code_squad, values_from = gd_sum, values_fill = NA)
player_club_gd <- unique(lineups_plus_results_2 %>% dplyr::select('Player_Name', 'team_name'))
agg_tbl_gd <- aggregate(data = lineups_plus_results_2, Player_Name ~ team_name, function(Player_Name) length(unique(Player_Name)))
colnames(agg_tbl_gd)[2] <- 'count'
player_count_gd <- c(agg_tbl_gd$Player_Name)
player_club_gd <- merge(player_club_gd, agg_tbl_gd, by = "team_name", all.x = TRUE)
all_clubs_gd <- merge(all_clubs_gd, player_club_gd, by = "Player_Name", all.x = TRUE)

#составил таблицу со всеми значениями Шепли
all_clubs_shapley_gd <- all_clubs_gd["Player_Name"]

#циклом заполнил строки таблицы с помощью вышенаписанной функции
for (i in 1:nrow(all_clubs_shapley_gd)) {
  all_clubs_shapley_gd$Shapley[i] <- shapley(all_clubs_gd, i)
}

#умножил все значения Шепли на 100 для простоты восприятия
all_clubs_shapley_gd$Shapley <- all_clubs_shapley_gd$Shapley*100

#посчитал средний рейтинг fotmob за сезон для каждого игрока
lineups_plus_results_2$stats_fot_mob_rating <- as.numeric(lineups_plus_results_2$stats_fot_mob_rating)
final_by_player_gd <- aggregate(stats_fot_mob_rating ~ Player_Name, data = lineups_plus_results_2, FUN ="mean")
names(final_by_player_gd) <- c("Player_Name", "rating")

#склеил Шепли и средний рейтинг в одну таблицу
final_gd <- merge(all_clubs_shapley_gd, final_by_player_gd, by = "Player_Name", all.x = TRUE)

ggplot(data = final_gd, aes(x = rating, y = Shapley))+geom_point()+geom_abline()



####ЗАДАНИЕ ХФ ЧЕРЕЗ СГЛАЖЕННЫЕ ОЧКИ####
#выбрал нужные столбцы
results_2 <- results_1[ -c(1:4,10,14) ]

#подготовка дф для вычисления
results_3 <- results_2
results_3$home_long_name <- results_2$away_long_name
results_3$away_long_name <- results_2$home_long_name
results_3$home_score <- results_2$away_score
results_3$away_score <- results_2$home_score

colnames(results_2)[6] <- 'team_1'
colnames(results_2)[9] <- 'team_2'
colnames(results_2)[5] <- 'team_1_score'
colnames(results_2)[8] <- 'team_2_score'

colnames(results_3)[6] <- 'team_1'
colnames(results_3)[9] <- 'team_2'
colnames(results_3)[5] <- 'team_1_score'
colnames(results_3)[8] <- 'team_2_score'

results_4 <- rbind(results_2, results_3)
results_4$match_tournament_stage <- as.numeric(results_4$match_tournament_stage)
results_4$points <- with(results_4, ifelse(team_1_score > team_2_score, 3, ifelse(team_1_score < team_2_score, 0, 1)))

results_5 <- results_4[ c(6,12,30) ]
colnames(results_5) <- c('team', 'stage', 'points')

names <- c(unique(results_5$team))
names[3]

#вычисление силы через сглаженные очки
test_hw <- results_5[results_5$team == names[1], ]
hw_model <- HoltWinters(x = test_hw$points, beta = FALSE, gamma = FALSE, alpha = 0.1484089)
all_HW <- cbind(hw_model$x, hw_model$fitted, hw_model$alpha)
rep_name <- as.data.frame(rep(names[1], 38))
tour <- as.data.frame(c(1:38))
all_HW <- as.data.frame(cbind(rep_name, tour, all_HW))
colnames(all_HW) <- c("team", "tour", "points", "emwa", "emwa_2", "alpha")

for (i in 2:20) {
  test_hw <- results_5[results_5$team == names[i], ]
  hw_model <- HoltWinters(x = test_hw$points, beta = FALSE, gamma = FALSE, alpha = 0.1484089)
  rep_name <- as.data.frame(rep(names[i], 38))
  tour <- as.data.frame(c(1:38))
  HW <- cbind(hw_model$x, hw_model$fitted, hw_model$alpha)
  HW <- as.data.frame(HW)
  HW <- cbind(rep_name, tour, HW)
  colnames(HW) <- c("team", "tour", "points", "emwa", "emwa_2", "alpha")
  all_HW <- rbind(all_HW, HW)
}

mean(all_HW$alpha)

#подготовка дф
all_HW <- all_HW %>% 
  mutate(code_squad = paste(team,tour,sep="_"))
all_HW$emwa <- with(all_HW, ifelse(tour == 1, points, emwa))
all_HW <- all_HW[complete.cases(all_HW$team),]
unique(all_HW$team)

#визуализация (плохая)
ggplot(all_HW, aes(tour, emwa))+geom_line(aes(color = team))
#визуализация (хорошая) - сглаженные и не сглаженные
showtext_auto()
theme( text=element_text(family="Times New Roman"))
gg_pts_2 <- ggplot(all_HW, aes(y = factor(team, team_order), x = tour, colour = emwa))+
  geom_line(size=4)+xlab('Номер тура')+ylab('')+guides(color=guide_legend("Сила"))+
  theme(text=element_text(family="Times New Roman", size=16))

gg_pts_1 <- ggplot(all_HW, aes(y = factor(team, team_order), x = tour, colour = points))+
  geom_line(size=4)+xlab('Номер тура')+ylab('Команда')+guides(color=guide_legend("Сила"))+
theme(text=element_text(family="Times New Roman", size=16))

#объединяем картинки в одну
ggarrange(gg_pts_1, gg_pts_2, labels = c("а)", "б)"), ncol = 2, nrow = 1)

#приклеил к дф
lineups_plus_results_2 <- merge(lineups_plus_results_2, all_HW, by = "code_squad", all.x = TRUE)





####ЗАДАНИЕ ХФ ЧЕРЕЗ СГЛАЖЕННУЮ РАЗНИЦУ ГОЛОВ####
results_4$gd <- results_4$team_1_score-results_4$team_2_score
results_6 <- results_4[ c(6,12,31) ]
colnames(results_6) <- c('team', 'stage', 'gd')

names <- c(unique(results_6$team))

#вычисление силы через сглаженную рг
test_hw_gd <- results_6[results_6$team == names[1], ]
hw_model_gd <- HoltWinters(x = test_hw_gd$gd, beta = FALSE, gamma = FALSE, alpha = 0.1464351)
all_HW_gd <- cbind(hw_model_gd$x, hw_model_gd$fitted, hw_model_gd$alpha)
rep_name <- as.data.frame(rep(names[1], 38))
tour <- as.data.frame(c(1:38))
all_HW_gd <- as.data.frame(cbind(rep_name, tour, all_HW_gd))
colnames(all_HW_gd) <- c("team", "tour", "gd", "emwa_gd", "emwa_gd_2", "alpha")

for (i in 2:20) {
  test_hw_gd <- results_6[results_6$team == names[i], ]
  hw_model_gd <- HoltWinters(x = test_hw_gd$gd, beta = FALSE, gamma = FALSE, alpha = 0.1464351)
  rep_name <- as.data.frame(rep(names[i], 38))
  tour <- as.data.frame(c(1:38))
  HW_gd <- cbind(hw_model_gd$x, hw_model_gd$fitted, hw_model_gd$alpha)
  HW_gd <- as.data.frame(HW_gd)
  HW_gd <- cbind(rep_name, tour, HW_gd)
  colnames(HW_gd) <- c("team", "tour", "gd", "emwa_gd", "emwa_gd_2", "alpha")
  all_HW_gd <- rbind(all_HW_gd, HW_gd)
}

mean(all_HW_gd$alpha)

#подготовка дф
all_HW_gd <- all_HW_gd %>% 
  mutate(code_squad = paste(team,tour,sep="_"))
all_HW_gd$emwa_gd <- with(all_HW_gd, ifelse(tour == 1, gd, emwa_gd))

#визуализация (плохая)
ggplot(all_HW_gd, aes(tour, emwa_gd))+geom_line(aes(color = team))
#визуализация (хорошая) - сглаженные и не сглаженные
gg_gd_2 <- ggplot(all_HW_gd, aes(x = tour, y = factor(team, team_order), colour = emwa_gd))+
  geom_line(size=4)+xlab('Номер тура')+ylab('')+guides(color=guide_legend("Сила"))+
  theme(text=element_text(family="Times New Roman", size=16))

gg_gd_1 <- ggplot(all_HW_gd, aes(x = tour, y = factor(team, team_order), colour = gd))+
  geom_line(size=4)+xlab('Номер тура')+ylab('Команда')+guides(color=guide_legend("Сила"))+
  theme(text=element_text(family="Times New Roman", size=16))

#объединяем картинки в одну
ggarrange(gg_gd_1, gg_gd_2, labels = c("а)", "б)"), ncol = 2, nrow = 1)

#приклеил к дф
lineups_plus_results_2 <- merge(lineups_plus_results_2, all_HW_gd, by = "code_squad", all.x = TRUE)





####ВЫЧИСЛЕНИЕ ШЕПЛИ ЧЕРЕЗ СГЛАЖЕННЫЕ ОЧКИ####
#сводник - по столбцам код состава, по строкам игроки
grouped_players_emwa <- lineups_plus_results_2 %>% 
  group_by(Player_Name, code_squad) %>%
  summarize(emwa_sum = sum(emwa, na.rm = TRUE))
all_clubs_emwa <- pivot_wider(grouped_players_emwa, names_from = code_squad, values_from = emwa_sum, values_fill = NA)
player_club_emwa <- unique(lineups_plus_results_2 %>% dplyr::select('Player_Name', 'team_name'))
agg_tbl_emwa <- aggregate(data = lineups_plus_results_2, Player_Name ~ team_name, function(Player_Name) length(unique(Player_Name)))
colnames(agg_tbl_emwa)[2] <- 'count'
player_count_emwa <- c(agg_tbl_emwa$Player_Name)
player_club_emwa <- merge(player_club_emwa, agg_tbl_emwa, by = "team_name", all.x = TRUE)
all_clubs_emwa <- merge(all_clubs_emwa, player_club_emwa, by = "Player_Name", all.x = TRUE)

#составил таблицу со всеми значениями Шепли
all_clubs_shapley_emwa <- all_clubs_emwa["Player_Name"]

#циклом заполнил строки таблицы с помощью вышенаписанной функции
for (i in 1:nrow(all_clubs_shapley_emwa)) {
  all_clubs_shapley_emwa$Shapley[i] <- shapley(all_clubs_emwa, i)
}

#умножил все значения Шепли на 100 для простоты восприятия
all_clubs_shapley_emwa$Shapley <- all_clubs_shapley_emwa$Shapley*100

#посчитал средний рейтинг fotmob за сезон для каждого игрока
lineups_plus_results_2$stats_fot_mob_rating <- as.numeric(lineups_plus_results_2$stats_fot_mob_rating)
final_by_player_emwa <- aggregate(stats_fot_mob_rating ~ Player_Name, data = lineups_plus_results_2, FUN ="mean")
names(final_by_player_emwa) <- c("Player_Name", "rating")

#склеил Шепли и средний рейтинг в одну таблицу
final_emwa <- merge(all_clubs_shapley_emwa, final_by_player_emwa, by = "Player_Name", all.x = TRUE)

ggplot(data = final_emwa, aes(x = rating, y = Shapley))+geom_point()+geom_abline()




####ВЫЧИСЛЕНИЕ ШЕПЛИ ЧЕРЕЗ СГЛАЖЕННУЮ РАЗНИЦУ ГОЛОВ####
#сводник - по столбцам код состава, по строкам игроки
grouped_players_emwa_gd <- lineups_plus_results_2 %>% 
  group_by(Player_Name, code_squad) %>%
  summarize(emwa_gd_sum = sum(emwa_gd, na.rm = TRUE))
all_clubs_emwa_gd <- pivot_wider(grouped_players_emwa_gd, names_from = code_squad, values_from = emwa_gd_sum, values_fill = NA)
player_club_emwa_gd <- unique(lineups_plus_results_2 %>% dplyr::select('Player_Name', 'team_name'))
agg_tbl_emwa_gd <- aggregate(data = lineups_plus_results_2, Player_Name ~ team_name, function(Player_Name) length(unique(Player_Name)))
colnames(agg_tbl_emwa_gd)[2] <- 'count'
player_count_emwa_gd <- c(agg_tbl_emwa_gd$Player_Name)
player_club_emwa_gd <- merge(player_club_emwa_gd, agg_tbl_emwa_gd, by = "team_name", all.x = TRUE)
all_clubs_emwa_gd <- merge(all_clubs_emwa_gd, player_club_emwa_gd, by = "Player_Name", all.x = TRUE)

#составил таблицу со всеми значениями Шепли
all_clubs_shapley_emwa_gd <- all_clubs_emwa_gd["Player_Name"]

#циклом заполнил строки таблицы с помощью вышенаписанной функции
for (i in 1:nrow(all_clubs_shapley_emwa_gd)) {
  all_clubs_shapley_emwa_gd$Shapley[i] <- shapley(all_clubs_emwa_gd, i)
}

#умножил все значения Шепли на 100 для простоты восприятия
all_clubs_shapley_emwa_gd$Shapley <- all_clubs_shapley_emwa_gd$Shapley*100

#посчитал средний рейтинг fotmob за сезон для каждого игрока
lineups_plus_results_2$stats_fot_mob_rating <- as.numeric(lineups_plus_results_2$stats_fot_mob_rating)
final_by_player_emwa_gd <- aggregate(stats_fot_mob_rating ~ Player_Name, data = lineups_plus_results_2, FUN ="mean")
names(final_by_player_emwa_gd) <- c("Player_Name", "rating")

#склеил Шепли и средний рейтинг в одну таблицу
final_emwa_gd <- merge(all_clubs_shapley_emwa_gd, final_by_player_emwa_gd, by = "Player_Name", all.x = TRUE)

ggplot(data = final_emwa, aes(x = rating, y = Shapley))+geom_point()+geom_abline()





####ЗАДАНИЕ ХФ ЧЕРЕЗ ВЕРОЯТНОСТЬ ПОБЕДИТЬ В КОНКРЕТНОМ МАТЧЕ####
all_HW$win_A <- with(all_HW, ifelse(points == 3, 1, 0))
all_HW$win_B <- with(all_HW, ifelse(points == 0, 1, 0))
all_HW$code_squad_B <- all_HW$code_squad
results_4 <- results_4 %>% 
  mutate(code_squad = paste(team_1,match_tournament_stage,sep="_"))
results_4 <- results_4 %>% 
  mutate(code_squad_B = paste(team_2,match_tournament_stage,sep="_"))
data_tmp <- merge(results_4, all_HW, by = "code_squad", all.x = TRUE)
all_HW$code_squad_B.x <- all_HW$code_squad_B
data <- merge(data_tmp, all_HW, by = "code_squad_B.x", all.x = TRUE)
colnames(data)
data_1 <- data %>% dplyr::select('tour.x', 'team_1', 'team_2', 'win_A.x', 'emwa.x', 'emwa.y', 'code_squad.x')
colnames(data_1) <- c('tour', 'team_A', 'team_B', 'win_A', 'emwa_A', 'emwa_B', 'code_squad')
data_1$emwa_diff <- data_1$emwa_A-data_1$emwa_B

#оценил логит-модель
mod_log <- glm(win_A ~ emwa_diff, family = binomial, data = data_1)
summary(mod_log)
pR2(mod_log)['McFadden']


coeftest(mod_log, vcov = vcovHC(mod_log, type = 'HC0'))
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

stargazer(mod_log, type="text", df=FALSE, digits=3, header = FALSE, se=list(cse(mod_log)), out = "logit_1.htm")

Pr <- as.data.frame(predict(mod_log))
data_1$Pr <- 100/(1+exp(-Pr))
colnames(data_1)
data_1$fifty <- if_else(data_1$Pr>=50, 1, 0)

#визуализация силы через сглаженные очки
ggplot(data_1, aes(tour, Pr$`predict(mod_log)`))+geom_line(aes(color = team_A))

ggplot(data_1, aes(x = tour, y = factor(team_A, team_order), colour = Pr$`predict(mod_log)`))+
  geom_line(size=4)+xlab('Номер тура')+ylab('Команда')+guides(color=guide_legend("Вероятность"))+
  theme(text=element_text(family="Times New Roman", size=16))

ggplot(data_1, aes(x = tour, y = factor(team_A, team_order), colour = fifty))+
  geom_line(size=4)+xlab('Номер тура')+ylab('Команда')+guides(color=guide_legend("Вероятность"))+
  theme(text=element_text(family="Times New Roman", size=16))

#приклеил к дф
lineups_plus_results_2 <- merge(lineups_plus_results_2, data_1, by = "code_squad", all.x = TRUE)





####ВЫЧИСЛЕНИЕ ШЕПЛИ ЧЕРЕЗ ВЕРОЯТНОСТЬ ПОБЕДИТЬ В КОНКРЕТНОМ МАТЧЕ####
#сводник - по столбцам код состава, по строкам игроки
grouped_players_pr <- lineups_plus_results_2 %>% 
  group_by(Player_Name, code_squad) %>%
  summarize(pr_sum = sum(Pr$`predict(mod_log)`, na.rm = TRUE))
all_clubs_pr <- pivot_wider(grouped_players_pr, names_from = code_squad, values_from = pr_sum, values_fill = NA)
player_club_pr <- unique(lineups_plus_results_2 %>% dplyr::select('Player_Name', 'team_name'))
agg_tbl_pr <- aggregate(data = lineups_plus_results_2, Player_Name ~ team_name, function(Player_Name) length(unique(Player_Name)))
colnames(agg_tbl_pr)[2] <- 'count'
player_count_pr <- c(agg_tbl_pr$Player_Name)
player_club_pr <- merge(player_club_pr, agg_tbl_pr, by = "team_name", all.x = TRUE)
all_clubs_pr <- merge(all_clubs_pr, player_club_pr, by = "Player_Name", all.x = TRUE)

#составил таблицу со всеми значениями Шепли
all_clubs_shapley_pr <- all_clubs_pr["Player_Name"]

#циклом заполнил строки таблицы с помощью вышенаписанной функции
for (i in 1:nrow(all_clubs_shapley_pr)) {
  all_clubs_shapley_pr$Shapley[i] <- shapley(all_clubs_pr, i)
}

#умножил все значения Шепли на 100 для простоты восприятия
all_clubs_shapley_pr$Shapley <- all_clubs_shapley_pr$Shapley*100

#посчитал средний рейтинг fotmob за сезон для каждого игрока
lineups_plus_results_2$stats_fot_mob_rating <- as.numeric(lineups_plus_results_2$stats_fot_mob_rating)
final_by_player_pr <- aggregate(stats_fot_mob_rating ~ Player_Name, data = lineups_plus_results_2, FUN ="mean")
names(final_by_player_pr) <- c("Player_Name", "rating")

#склеим Шепли и средний рейтинг в одну таблицу
final_pr <- merge(all_clubs_shapley_pr, final_by_player_pr, by = "Player_Name", all.x = TRUE)

#посмотрим на графике
ggplot(data = final_pr, aes(x = rating, y = Shapley))+geom_point()





####КОРРЕЛЯЦИЯ ШЕПЛИ И ТРАНСФЕРНОЙ СТОИМОСТИ####
#загружаем трансферные стоимости
transfer_values <- tm_player_market_values(country_name = "England", start_year = 2021)
colnames(transfer_values)[7] <- "Player_Name"

#обработанная по игроку статистика
all_stats <- read_excel("all_stats.xlsx")
final_1 <- merge(final, all_stats, by = "Player_Name", all.x = TRUE)
final_2 <- merge(final_1, transfer_values, by = "Player_Name", all.x = TRUE)

#корреляция с Шепли по очкам
cor_tr_1 <- cor(final_2$Shapley,log(final_2$player_market_value_euro), use = "pairwise.complete.obs",
                method = c("pearson", "kendall", "spearman"))
cor.test(final_2$Shapley,log(final_2$player_market_value_euro), use = "pairwise.complete.obs",
         method = c("pearson", "kendall", "spearman"))

#корреляция с Шепли по сглаженным очкам
final_emwa_2_cor <- merge(final_emwa, transfer_values, by = "Player_Name", all.x = TRUE)
cor_tr_2 <- cor(final_emwa_2_cor$Shapley,log(final_emwa_2_cor$player_market_value_euro), use = "pairwise.complete.obs", 
                method = c("pearson", "kendall", "spearman"))
cor.test(final_emwa_2_cor$Shapley,log(final_emwa_2_cor$player_market_value_euro), use = "pairwise.complete.obs",
         method = c("pearson", "kendall", "spearman"))

#корреляция с Шепли по разнице голов
final_gd_2_cor <- merge(final_gd, transfer_values, by = "Player_Name", all.x = TRUE)
cor_tr_3 <- cor(final_gd_2_cor$Shapley,log(final_gd_2_cor$player_market_value_euro), use = "pairwise.complete.obs",
        method = c("pearson", "kendall", "spearman"))
cor.test(final_gd_2_cor$Shapley,log(final_gd_2_cor$player_market_value_euro), use = "pairwise.complete.obs",
         method = c("pearson", "kendall", "spearman"))

#корреляция с Шепли по сглаженной разнице голов
final_emwa_gd_2_cor <- merge(final_emwa_gd, transfer_values, by = "Player_Name", all.x = TRUE)
cor_tr_4 <- cor(final_emwa_gd_2_cor$Shapley,log(final_emwa_gd_2_cor$player_market_value_euro), use = "pairwise.complete.obs",
                method = c("pearson", "kendall", "spearman"))
cor.test(final_emwa_gd_2_cor$Shapley,log(final_emwa_gd_2_cor$player_market_value_euro), use = "pairwise.complete.obs",
         method = c("pearson", "kendall", "spearman"))

#корреляция с Шепли по вероятности
final_pr_2_cor <- merge(final_pr, transfer_values, by = "Player_Name", all.x = TRUE)
cor_tr_5 <- cor(final_pr_2_cor$Shapley,log(final_pr_2_cor$player_market_value_euro), use = "pairwise.complete.obs",
                method = c("pearson", "kendall", "spearman"))
cor.test(final_pr_2_cor$Shapley,log(final_pr_2_cor$player_market_value_euro), use = "pairwise.complete.obs",
         method = c("pearson", "kendall", "spearman"))

#все корреляции в один вектор
cor_tr <- c(cor_tr_1, cor_tr_1, cor_tr_2, cor_tr_2, cor_tr_3, cor_tr_3, cor_tr_4, cor_tr_4, cor_tr_5, cor_tr_5)





####КОРРЕЛЯЦИЯ ШЕПЛИ И РЕЙТИНГА FOTMOB####
#корреляция между средним рейтингом и Шепли
cor_r_1 <- cor(final$Shapley, final$rating, method = c("pearson", "kendall", "spearman"))
cor_r_test_1 <- cor.test(final$Shapley, final$rating, method=c("pearson", "kendall", "spearman"))

#корреляция с Шепли по сглаженным очкам
cor_r_2 <- cor(final_emwa$Shapley, final_emwa$rating, method = c("pearson", "kendall", "spearman"))
cor_r_test_2 <- cor.test(final_emwa$Shapley, final_emwa$rating, method=c("pearson", "kendall", "spearman"))

#корреляция с Шепли по разнице голов
cor_r_3 <- cor(final_gd$Shapley, final_gd$rating, method = c("pearson", "kendall", "spearman"))
cor_r_test_3 <- cor.test(final_gd$Shapley, final_gd$rating, method=c("pearson", "kendall", "spearman"))

#корреляция с Шепли по сглаженной разнице голов
cor_r_4 <- cor(final_emwa_gd$Shapley, final_emwa_gd$rating, method = c("pearson", "kendall", "spearman"))
cor_r_test_4 <- cor.test(final_emwa_gd$Shapley, final_emwa_gd$rating, method=c("pearson", "kendall", "spearman"))

#корреляция с Шепли по вероятности
cor_r_5 <- cor(final_pr$Shapley, final_pr$rating, method = c("pearson", "kendall", "spearman"))
cor_r_test_5 <- cor.test(final_pr$Shapley, final_pr$rating, method=c("pearson", "kendall", "spearman"))

#все корреляции в один вектор
cor_r <- c(cor_r_1, cor_r_1, cor_r_2, cor_r_2, cor_r_3, cor_r_3, cor_r_4, cor_r_4, cor_r_5, cor_r_5)
cor_r_test_p <- c(cor_r_test_1$p.value, cor_r_test_1$p.value, 
                  cor_r_test_2$p.value, cor_r_test_2$p.value, 
                  cor_r_test_3$p.value, cor_r_test_3$p.value, 
                  cor_r_test_4$p.value, cor_r_test_4$p.value, 
                  cor_r_test_5$p.value, cor_r_test_5$p.value)
cor_r_test_conf <- c(cor_r_test_1$conf.int, cor_r_test_2$conf.int, cor_r_test_3$conf.int, cor_r_test_4$conf.int, cor_r_test_5$conf.int)





####ВАЛИДАЦИЯ МЕТРИКИ С ПОМОЩЬЮ РЕГРЕССИЙ####
#определяем какие переменные нужны
colnames(final_2)
#отбираем полные столбцы с нужными переменными
final_mod <- left_join(final_2, team_standings, by = c("club"="club"))
final_mod_AIC <- final_mod[complete.cases(final_mod$player_market_value_euro),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_minutes_played),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_accurate_passes),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_recoveries),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_expected_assists_x_a),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_touches),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_total_shots),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_chances_created),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_passes_into_fil_third),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_successful_dribbles),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_fouls_committed),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_was_fouled),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_interceptions),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_blocks),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_expected_goals_x_g),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_shot_accuracy),]
final_mod_AIC <- final_mod_AIC[complete.cases(final_mod_AIC$stats_offsides),]

#первая модель без Шепли
mod_1 <- lm(data = final_mod, player_market_value_euro ~ stats_minutes_played+
              stats_accurate_passes+stats_recoveries+stats_expected_assists_x_a+stats_touches+
              stats_total_shots+stats_chances_created+stats_passes_into_fil_third+
              stats_successful_dribbles+stats_tackles_won+stats_was_fouled+stats_fouls_committed+
              stats_interceptions+stats_blocks+stats_expected_goals_x_g+stats_shot_accuracy+
              stats_offsides+team_place)
summary(mod_1)

#проверяем на мультиколлинеарность
vif(mod_1)

#выбрасываем ненужные переменные
mod_1 <- lm(data = final_mod, player_market_value_euro ~ stats_minutes_played+stats_accurate_passes+
              stats_recoveries+stats_expected_assists_x_a+stats_total_shots+
              stats_passes_into_fil_third+stats_successful_dribbles+stats_tackles_won+stats_was_fouled+
              stats_fouls_committed+stats_interceptions+stats_blocks+stats_expected_goals_x_g+
              stats_shot_accuracy+stats_offsides+team_place)
summary(mod_1)

#повторная проверка
vif(mod_1) #теперь все ок

#точки высокой напряженности
#influencePlot(mod_1)
#final_2_v2 <- final_mod %>% slice(-c(77, 453, 469, 484))
#скорректированная модель
#mod_1 <- lm(data = final_2_v2, log(player_market_value_euro) ~ stats_minutes_played+stats_accurate_passes+stats_recoveries+stats_expected_assists_x_a+stats_total_shots+stats_chances_created+stats_passes_into_fil_third+stats_successful_dribbles+stats_tackles_won+stats_was_fouled+stats_fouls_committed+stats_interceptions+stats_blocks+stats_expected_goals_x_g+stats_shot_accuracy+stats_offsides)
#summary(mod_1)

#тест Рамсея
resettest(mod_1)

#добавляем в регрессию Шепли
mod_2 <- lm(data = final_mod, player_market_value_euro ~ Shapley+stats_minutes_played+stats_accurate_passes+
              stats_recoveries+stats_expected_assists_x_a+stats_total_shots+
              stats_passes_into_fil_third+stats_successful_dribbles+stats_tackles_won+stats_was_fouled+
              stats_fouls_committed+stats_interceptions+stats_blocks+stats_expected_goals_x_g+
              stats_shot_accuracy+stats_offsides+team_place)
summary(mod_2)

#спецификация с логарифмом
mod_3 <- lm(data = final_mod, log(player_market_value_euro) ~ Shapley+stats_minutes_played+stats_accurate_passes+
              stats_recoveries+stats_expected_assists_x_a+stats_total_shots+
              stats_passes_into_fil_third+stats_successful_dribbles+stats_tackles_won+stats_was_fouled+
              stats_fouls_committed+stats_interceptions+stats_blocks+stats_expected_goals_x_g+
              stats_shot_accuracy+stats_offsides+team_place)
summary(mod_3)

#тест Рамсея
resettest(mod_3)

#линия регрессии
ggplot(final_mod, aes(Shapley, player_market_value_euro))+geom_point()+geom_smooth(method = "lm")

#применил отбор по критерию Акаике
mod_AIC <- lm(data = final_mod_AIC, log(player_market_value_euro) ~ Shapley+stats_minutes_played+stats_accurate_passes+
                stats_recoveries+stats_expected_assists_x_a+stats_total_shots+
                stats_passes_into_fil_third+stats_successful_dribbles+stats_tackles_won+stats_was_fouled+
                stats_fouls_committed+stats_interceptions+stats_blocks+stats_expected_goals_x_g+
                stats_shot_accuracy+stats_offsides+team_place)
mod_4 <- stepAIC(mod_AIC)

#финальный набор регрессоров
mod_4$anova

#финальная модель с Шепли
mod_5 <- lm(data = final_mod, log(player_market_value_euro) ~ Shapley+stats_minutes_played + 
              stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
              stats_passes_into_fil_third + stats_successful_dribbles + 
              stats_was_fouled + stats_interceptions + stats_offsides + 
              team_place)
summary(mod_5)

#тест Рамсея
resettest(mod_5)

#финальная модель без Шепли
mod_6 <- lm(data = final_mod, log(player_market_value_euro) ~ stats_minutes_played + 
              stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
              stats_passes_into_fil_third + stats_successful_dribbles + 
              stats_was_fouled + stats_interceptions + stats_offsides + 
              team_place)
summary(mod_6)

#тест Рамсея
resettest(mod_6)

#сравнение моделей с Шепли и без
stargazer(mod_6, mod_5, se=list(cse(mod_6), cse(mod_5)), out = "shapley_models_1.htm", type = "text")


#перебираем модели с Шепли для проверки робастности
m1 <- lm(data = final_mod, log(player_market_value_euro) ~ Shapley)
stargazer(m1, type = "text")

m2 <- lm(data = final_mod, log(player_market_value_euro) ~ Shapley+stats_minutes_played)
stargazer(m2, type = "text")

m3 <- lm(data = final_mod, log(player_market_value_euro) ~ Shapley+stats_minutes_played + 
           stats_accurate_passes)
stargazer(m2, m3, type = "text", out = "shit.htm")

m4 <- lm(data = final_mod, log(player_market_value_euro) ~ Shapley+stats_minutes_played + 
           stats_accurate_passes + stats_expected_assists_x_a)

m5 <- lm(data = final_mod, log(player_market_value_euro) ~ Shapley+stats_minutes_played + 
           stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots)

m6 <- lm(data = final_mod, log(player_market_value_euro) ~ Shapley+stats_minutes_played + 
           stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
           stats_passes_into_fil_third)

m7 <- lm(data = final_mod, log(player_market_value_euro) ~ Shapley+stats_minutes_played + 
           stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
           stats_passes_into_fil_third)

  m8 <- lm(data = final_mod, log(player_market_value_euro) ~ Shapley+stats_minutes_played + 
  stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
  stats_passes_into_fil_third + stats_successful_dribbles)

  m9 <- lm(data = final_mod, log(player_market_value_euro) ~ Shapley+stats_minutes_played + 
  stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
  stats_passes_into_fil_third + stats_successful_dribbles + 
  stats_was_fouled)

  m10 <- lm(data = final_mod, log(player_market_value_euro) ~ Shapley+stats_minutes_played + 
  stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
  stats_passes_into_fil_third + stats_successful_dribbles + 
  stats_was_fouled + stats_interceptions)

  m11 <- lm(data = final_mod, log(player_market_value_euro) ~ Shapley+stats_minutes_played + 
  stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
  stats_passes_into_fil_third + stats_successful_dribbles + 
  stats_was_fouled + stats_interceptions + stats_offsides)

  m12 <- lm(data = final_mod, log(player_market_value_euro) ~ Shapley+stats_minutes_played + 
  stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
  stats_passes_into_fil_third + stats_successful_dribbles + 
  stats_was_fouled + stats_interceptions + stats_offsides + 
  team_place)

#detach("package:stargazer",unload=T)
#remove.packages("stargazer")
#download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
#untar("stargazer_5.2.3.tar.gz")
#stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
#stargazer_src[1990] <- stargazer_src[1995]
#stargazer_src[1995] <- ""
#writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
#install.packages("stargazer", repos = NULL, type="source")

#сводник - лесенка моделей
stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, type="text",
          df=FALSE, digits=3, se=list(cse(m1), cse(m2), cse(m3), cse(m4), 
                                      cse(m5), cse(m6), cse(m7), cse(m8), 
                                      cse(m9), cse(m10), cse(m11), cse(m12)), 
          header = FALSE, out = "models.htm")

#модель с Шепли как сглаженные очки
final_emwa_2 <- left_join(final_emwa, all_stats, by = c("Player_Name"="Player_Name"))
final_emwa_2 <- left_join(final_emwa_2, transfer_values, by = c("Player_Name"="Player_Name"))
final_emwa_2 <- left_join(final_emwa_2, team_standings, by = c("club"="club"))

mod_emwa <- lm(data = final_emwa_2, log(player_market_value_euro) ~ Shapley+stats_minutes_played + 
                 stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
                 stats_passes_into_fil_third + stats_successful_dribbles + 
                 stats_was_fouled + stats_interceptions + stats_offsides + 
                 team_place)
summary(mod_emwa)
resettest(mod_emwa)

#такая же модель без Шепли
mod_emwa_1 <- lm(data = final_emwa_2, log(player_market_value_euro) ~ stats_minutes_played + 
                   stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
                   stats_passes_into_fil_third + stats_successful_dribbles + 
                   stats_was_fouled + stats_interceptions + stats_offsides + 
                   team_place)
summary(mod_emwa_1)
resettest(mod_emwa_1)


#модель с Шепли как разница голов
final_gd_2 <- left_join(final_gd, all_stats, by = c("Player_Name"="Player_Name"))
final_gd_2 <- left_join(final_gd_2, transfer_values, by = c("Player_Name"="Player_Name"))
final_gd_2 <- left_join(final_gd_2, team_standings, by = c("club"="club"))

mod_gd <- lm(data = final_gd_2, log(player_market_value_euro) ~ Shapley+stats_minutes_played + 
               stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
               stats_passes_into_fil_third + stats_successful_dribbles + 
               stats_was_fouled + stats_interceptions + stats_offsides + 
               team_place)
summary(mod_gd)
resettest(mod_gd)


#модель с Шепли как сглаженная разница голов
final_emwa_gd_2 <- left_join(final_emwa_gd, all_stats, by = c("Player_Name"="Player_Name"))
final_emwa_gd_2 <- left_join(final_emwa_gd_2, transfer_values, by = c("Player_Name"="Player_Name"))
final_emwa_gd_2 <- left_join(final_emwa_gd_2, team_standings, by = c("club"="club"))

mod_emwa_gd <- lm(data = final_emwa_gd_2, log(player_market_value_euro) ~ Shapley+stats_minutes_played + 
                    stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
                    stats_passes_into_fil_third + stats_successful_dribbles + 
                    stats_was_fouled + stats_interceptions + stats_offsides + 
                    team_place)
summary(mod_emwa_gd)
resettest(mod_emwa_gd)




#модель с Шепли как веротность
final_pr_2 <- left_join(final_pr, all_stats, by = c("Player_Name"="Player_Name"))
final_pr_2 <- left_join(final_pr_2, transfer_values, by = c("Player_Name"="Player_Name"))
final_pr_2 <- left_join(final_pr_2, team_standings, by = c("club"="club"))

mod_pr <- lm(data = final_pr_2, log(player_market_value_euro) ~ Shapley+stats_minutes_played + 
               stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
               stats_passes_into_fil_third + stats_successful_dribbles + 
               stats_was_fouled + stats_interceptions + stats_offsides + 
               team_place)
summary(mod_pr)
resettest(mod_pr)


#сводник по всем моделям Шепли
stargazer(mod_6, mod_5, mod_emwa, mod_gd, mod_emwa_gd, mod_pr, type="text",
          df=FALSE, digits=3, header = FALSE, out = "cf_models.htm", 
          se=list(cse(mod_6), cse(mod_5), cse(mod_emwa), cse(mod_gd), cse(mod_emwa_gd), cse(mod_pr)),
          column.labels = c('Without Shapley', 'Points', 'Smoothed Points', 'Goal differece', 'Smoothed goal differece', 'Probability'))
(exp(0.008)-1)*100

#модели с рейтнгом фотмоб
#модель с рейтингом без Шепли
mod_rating <- lm(data = final_mod, log(player_market_value_euro) ~ rating+stats_minutes_played + 
                   stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
                   stats_passes_into_fil_third + stats_successful_dribbles + 
                   stats_was_fouled + stats_interceptions + stats_offsides + 
                   team_place)
summary(mod_rating)
vif(mod_rating)

#модель с рейтингом и Шепли
mod_rating_1 <- update(mod_rating, log(player_market_value_euro)~Shapley+rating+stats_minutes_played + 
                         stats_accurate_passes + stats_expected_assists_x_a + stats_total_shots + 
                         stats_passes_into_fil_third + stats_successful_dribbles + 
                         stats_was_fouled + stats_interceptions + stats_offsides + 
                         team_place)
summary(mod_rating_1)
vif(mod_rating_1)

#сводник по моделям для сравнения
stargazer(mod_6, mod_5, mod_rating, mod_rating_1, se = list(cse(mod_6), cse(mod_5), cse(mod_rating), cse(mod_rating_1)),
          type = "text", out = "rating_vs_shapley.htm",
          column.labels = c('Without metric', 'With Shapley', 'With FotMob score', 'With Shapley andFotMob score'))







####ВЫЧИСЛЕНИЕ СРЕДНЕГО ШЕПЛИ ПО ПОЗИЦИЯМ####
#средний Шепли по позициям (вся лига)
final_by_position <- aggregate(Shapley ~ position, data = final_emwa_2, FUN ="mean")
final_emwa_2_2 <- final_emwa_2[complete.cases(final_emwa_2$position),]
names(final_by_player) <- c("Position", "Shapley")

#вычисление среднего и стандартного отклонения по лиге
ag <- aggregate(Shapley ~ position, final_emwa_2, function(x) c(mean = mean(x), sd = sd(x)))
Average <- c(5.956508, 5.415398, 6.138406, 5.304764)
SD <- c(4.587482, 4.267614, 6.453260, 4.275641)
position_order <- c('DEF', 'FW', 'GK', 'MID')

#визуализация среднего Шепли по позициям в лиге
ggplot(final_by_position, aes(x=position, y=Shapley, fill=position))+
  geom_col()+xlab('Position')+ylab('Average Shapley value') #+theme(legend.position="none")+geom_errorbar(aes(ymin = Average - 1.96*SD, ymax = Average + 1.96*SD), color = "darkblue")

#график с плотностью
gg_dens_pos <- ggplot(final_emwa_2_2, aes(x = Shapley, fill = position)) + 
  geom_density(alpha = 0.3)+xlab("Значение Шепли")+ylab("Плотность")+
  theme(legend.position = "top")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(fill="Позиция")

#ранжируем позиции, чтобы они были одного цвета
final_emwa_2_2$position <- factor(final_emwa_2_2$position, levels = c('DEF', 'FW', 'GK', 'MID'))

#боксплот по позициям
gg_box_pos <- ggboxplot(final_emwa_2_2, x = "position", y = "Shapley", 
          fill = "position",
          ylab = "Значение Шепли", xlab = "Позиция")+ theme(legend.position = "none")

#cредние с довреительным интервалом
data_mean <- final_emwa_2_2 %>% dplyr::select(position, Shapley) %>% na.omit() %>%  
  group_by(position) %>% dplyr::summarise(value = mean(Shapley), sd = sd(Shapley)/sqrt(n())) %>% 
  mutate(position = as.factor(position))

gg_bar_pos <- ggplot(data_mean) +
  geom_bar(aes(x = position, y = value, fill=position, alpha = 0.9), stat="identity") +
  geom_errorbar(aes(x = position, y = value, ymin = value - 2*sd, ymax = value + 2*sd), 
                width = 0.4, colour = "gray20", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Позиция", y = "Среднее значение Шепли")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank())+ theme(legend.position = "none")


#два графика в один
ggarrange(gg_dens_pos, gg_bar_pos, labels = c("а)", "б)"))

#средний Шепли по позициям во всех клубах
normalit<-function(x){
  x/sum(x)
}
final_by_position_clubs <- aggregate(Shapley ~ club+position, data = final_1, FUN ="mean")

#проверка
proverka <- aggregate(Shapley ~ club, data = final_1, FUN ="sum")
proverka$Shapley <- proverka$Shapley*38

#подготовка дф для визуализации (нормировка)
final_by_position_clubs_1 <- aggregate(Shapley ~ club, data = final_by_position_clubs, FUN ="sum")
final_by_position_clubs_2 <- merge(final_by_position_clubs, final_by_position_clubs_1, by = "club", all.x = TRUE)
final_by_position_clubs_2$Shapley_norm <- final_by_position_clubs_2$Shapley.x/final_by_position_clubs_2$Shapley.y
final_by_position_clubs <- subset(final_by_position_clubs_2, select = -c(Shapley.y))


#визуализация среднего Шепли по позициям в клубе
gg_pos_cl_1 <- ggplot(final_by_position_clubs, aes(x=Shapley_norm, y=factor(club, team_order), 
                                                   fill=position))+
  geom_col()+xlab('Нормированное значение Шепли')+ylab('')+
  labs(fill='Позиция')+geom_text(aes(label = scales::percent(Shapley_norm,2), group = position),
            position = position_stack(vjust = .5), color = "azure")

#в абсолютных значениях
gg_pos_cl_2 <- ggplot(final_by_position_clubs, aes(x=Shapley.x, y=factor(club, team_order), 
                                                   fill=position))+
  geom_col()+xlab('Значение Шепли')+ylab('Команда')+
  labs(fill='Позиция')+geom_text(aes(label = round(Shapley.x,2), group = position),
                                 position = position_stack(vjust = .5), color = "azure")+
  theme(legend.position = "none")

#два графика в один
ggarrange(gg_pos_cl_2, gg_pos_cl_1, labels = c("а)", "б)"))





####ВЗАИМОСВЯЗЬ С ТРАНСФЕРОМ####
#подтянул трансферы на след сезон
tm_urls <- tm_league_team_urls(country_name = "England", start_year = 2022)
#подтянул вылетевшие клубы
tm_urls_1 <- c("https://www.transfermarkt.com/fc-burnley/startseite/verein/1132",
               "https://www.transfermarkt.com/norwich-city/startseite/verein/1123",
               "https://www.transfermarkt.com/fc-watford/startseite/verein/1010")
tm_team_transfers <- tm_team_transfers(team_url = tm_urls, transfer_window = "all")
tm_team_transfers <- tm_team_transfers[tm_team_transfers$transfer_type == 'Arrivals', ]
tm_team_transfers_1 <- tm_team_transfers(team_url = tm_urls_1, transfer_window = "all")
tm_team_transfers_1 <- tm_team_transfers_1[tm_team_transfers_1$transfer_type == 'Arrivals', ]

tm_team_transfers <- rbind(tm_team_transfers, tm_team_transfers_1)

#переименовал позиции
unique(tm_team_transfers$player_position)
tm_team_transfers$position <- NA

#полузащитники
tm_team_transfers$position <- with(tm_team_transfers, ifelse(player_position == 'Attacking Midfield', 
                                                             'MID', position))
tm_team_transfers$position <- with(tm_team_transfers, ifelse(player_position == 'Central Midfield', 
                                                             'MID', position))
tm_team_transfers$position <- with(tm_team_transfers, ifelse(player_position == 'Defensive Midfield', 
                                                             'MID', position))
tm_team_transfers$position <- with(tm_team_transfers, ifelse(player_position == 'Right Midfield', 
                                                             'MID', position))
tm_team_transfers$position <- with(tm_team_transfers, ifelse(player_position == 'Left Midfield', 
                                                             'MID', position))

#нападающие
tm_team_transfers$position <- with(tm_team_transfers, ifelse(player_position == 'Centre-Forward', 
                                                             'FW', position))
tm_team_transfers$position <- with(tm_team_transfers, ifelse(player_position == 'Right Winger', 
                                                             'FW', position))
tm_team_transfers$position <- with(tm_team_transfers, ifelse(player_position == 'Left Winger', 
                                                             'FW', position))
tm_team_transfers$position <- with(tm_team_transfers, ifelse(player_position == 'Second Striker', 
                                                             'FW', position))

#защитники
tm_team_transfers$position <- with(tm_team_transfers, ifelse(player_position == 'Centre-Back', 
                                                             'DEF', position))
tm_team_transfers$position <- with(tm_team_transfers, ifelse(player_position == 'Left-Back', 
                                                             'DEF', position))
tm_team_transfers$position <- with(tm_team_transfers, ifelse(player_position == 'Right-Back', 
                                                             'DEF', position))

#вратарь
tm_team_transfers$position <- with(tm_team_transfers, ifelse(player_position == 'Goalkeeper', 
                                                             'GK', position))

#сумма потраченная клубом на усиление каждой позиции
transfers <- aggregate(transfer_fee ~ team_name+position, data = tm_team_transfers, FUN ="sum")
colnames(transfers)[1] <- 'club'

#переименовал клубы
unique(transfers$club)
transfers$club <- with(transfers, ifelse(club == 'Arsenal FC', 'Arsenal', club))
transfers$club <- with(transfers, ifelse(club == 'Brentford FC', 'Brentford', club))
transfers$club <- with(transfers, ifelse(club == 'Chelsea FC', 'Chelsea', club))
transfers$club <- with(transfers, ifelse(club == 'Everton FC', 'Everton', club))
transfers$club <- with(transfers, ifelse(club == 'Liverpool FC', 'Liverpool', club))
transfers$club <- with(transfers, ifelse(club == 'Southampton FC', 'Southampton', club))
transfers$club <- with(transfers, ifelse(club == 'Watford FC', 'Watford', club))
transfers$club <- with(transfers, ifelse(club == 'Burnley FC', 'Burnley', club))


final_by_position_clubs_transfers <- merge(final_by_position_clubs, transfers, by=c("club","position"), 
                                           all.x = TRUE)
final_by_position_clubs_transfers_no_na <- na.omit(final_by_position_clubs_transfers)

#рассчет корреляции
cor(final_by_position_clubs_transfers_no_na$Shapley_norm, 
    final_by_position_clubs_transfers_no_na$transfer_fee, method = c("pearson", "kendall", "spearman"))
cor.test(final_by_position_clubs_transfers_no_na$Shapley_norm, 
         final_by_position_clubs_transfers_no_na$transfer_fee, method = c("pearson", "kendall", "spearman"))

#подготовка дф к визуализации
final_by_position_clubs_transfers$position_cd <- NA
final_by_position_clubs_transfers$position_cd <- with(final_by_position_clubs_transfers, 
                                                      ifelse(position == 'MID', 0.5, position_cd))
final_by_position_clubs_transfers$position_cd <- with(final_by_position_clubs_transfers, 
                                                      ifelse(position == 'GK', 0.65, position_cd))
final_by_position_clubs_transfers$position_cd <- with(final_by_position_clubs_transfers, 
                                                      ifelse(position == 'FW', 0.8, position_cd))
final_by_position_clubs_transfers$position_cd <- with(final_by_position_clubs_transfers, 
                                                      ifelse(position == 'DEF', 0.95, position_cd))

final_by_position_clubs_transfers$transfer_fee <- final_by_position_clubs_transfers$transfer_fee/10^6

#взаимосвязь нужности позиции и затрат на трансферы
ggplot(final_by_position_clubs_transfers, aes(x=Shapley_norm, y=factor(club, team_order)))+
  geom_point(aes(size=transfer_fee, color=position))+xlab('Нормированное значение Шепли')+
  ylab('Команда')+labs(color="Позиция", size="Сумма трансферов (в млн)")

mod_tr <- lm(data=final_by_position_clubs_transfers, transfer_fee~Shapley_norm)
summary(mod_tr)



####СИЛА КОМАНД РАЗНЫМИ СПОСОБАМИ НА ПРИМЕРЕ ОДНОГО КЛУБА####
MU <- cbind(all_HW,all_HW_gd, data_1)
MU <- MU[MU$team == 'Manchester United', ]
colnames(MU)
MU <- MU %>% dplyr::select('tour', 'points', 'emwa', 'gd', 'emwa_gd', 'Pr')
MU <- as.data.frame(MU)
MU$Pr <- as.numeric(unlist(MU$Pr))

scale <- 10

#график для манчестер юнайтед
ggplot(MU, aes(x=tour))+geom_line(aes(y=points, colour = "points"))+geom_line(aes(y=emwa, color = "emwa"))+
  geom_line(aes(y=gd, color = "gd"))+geom_line(aes(y=emwa_gd, color = "emwa_gd"))+
  geom_line(aes(y=Pr/scale), color = "darkgreen")+
  scale_y_continuous(sec.axis = sec_axis(~.*scale, name="Вероятность (%)"))+
  theme(axis.title.y.right = element_text(color = "darkgreen"))+ylab("Сила состава")+xlab("Номер тура")+
  scale_color_manual(name = "Способ", values = c("points" = "red", "emwa" = "darkred", 
                                              "gd" = "blue", "emwa_gd" = "darkblue"),
                     labels = c("Сглаженные очки", "Сглаженная разница голов", "Разница голов", "Очки"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))





####ВЗАИМОСВЯЗЬ ШЕПЛИ ТРАНСФЕРА РЕЙТИНГА И ПОЗИЦИИ####
final_2_gg <- final_2[complete.cases(final_2$position),]
ggplot(data = final_2, aes(y = log(player_market_value_euro), x = Shapley))+
  geom_point(aes(color = rating))+xlab('Значение Шепли')+ylab('Логарифм трансферной стоимости')+
  labs(color='Рейтинг FotMob') 





####РАСПРЕДЕЛЕНИЕ ШЕПЛИ ПО ЛИГЕ####
g_dens_1 <-ggplot(final_2, aes(x = Shapley))+
  geom_histogram(aes(y = ..density..), colour = 1, fill = "lightblue")+geom_density(size=1.5)+
  ylab("Плотность")+xlab("Значение Шепли")

g_dens_2 <- ggplot(final_emwa_2, aes(x = Shapley))+
  geom_histogram(aes(y = ..density..), colour = 1, fill = "pink")+geom_density(size=1.5)+
  ylab("Плотность")+xlab("Значение Шепли")

g_dens_3 <-ggplot(final_gd_2, aes(x = Shapley))+
  geom_histogram(aes(y = ..density..), colour = 1, fill = "yellow")+geom_density(size=1.5)+
  ylab("Плотность")+xlab("Значение Шепли")

g_dens_4 <-ggplot(final_emwa_gd_2, aes(x = Shapley))+
  geom_histogram(aes(y = ..density..), colour = 1, fill = "lightgreen")+geom_density(size=1.5)+
  ylab("Плотность")+xlab("Значение Шепли")

g_dens_5 <-ggplot(final_pr_2, aes(x = Shapley))+
  geom_histogram(aes(y = ..density..), colour = 1, fill = "lightgrey")+geom_density(size=1.5)+
  ylab("Плотность")+xlab("Значение Шепли")

#все графики в один
ggarrange(g_dens_1, g_dens_2, g_dens_3, g_dens_4, g_dens_5, 
          labels = c("а)", "б)", "в)", "г)", "д)"), ncol = 3, nrow = 2)

#все Шепли в одну таблицу
Shapley_1 <- final_2 %>% dplyr::select("Player_Name", "Shapley", "rating", "player_market_value_euro")
colnames(Shapley_1)[2] <- 'Shapley_1'
Shapley_2 <- final_emwa_2 %>% dplyr::select("Player_Name", "Shapley")
colnames(Shapley_2)[2] <- 'Shapley_2'
Shapley_3 <- final_gd_2 %>% dplyr::select("Shapley")
colnames(Shapley_3)[1] <- 'Shapley_3'
Shapley_4 <- final_emwa_gd_2 %>% dplyr::select("Shapley")
colnames(Shapley_4)[1] <- 'Shapley_4'
Shapley_5 <- final_pr_2 %>% dplyr::select("Shapley")
colnames(Shapley_5)[1] <- 'Shapley_5'

Shapley <- cbind(Shapley_2, Shapley_3, Shapley_4, Shapley_5)

Shapley <- left_join(Shapley, Shapley_1, by = c("Player_Name"))

#описательные статистики по разным способам подсчета Шепли
stargazer(Shapley_2, Shapley_3, Shapley_4, Shapley_5, type="text", median = TRUE, digits = 2,
          font.size = "tiny", out = "statistika_shapley.htm")





####РАСПРЕДЕЛЕНИЕ ШЕПЛИ ВНУТРИ КЛУБА####
final_n <- aggregate(Shapley ~ club, data = final_2, FUN ="sum")
colnames(final_n)[2] <- 'Shapley_club'
final_n_1 <- merge(final_2, final_n, by = "club", all.x = TRUE)
final_n_1$Shapley_norm <- final_n_1$Shapley/final_n_1$Shapley_club*100
final_n_1 <- final_n_1[complete.cases(final_n_1$club), ]
ggplot(final_n_1, aes(x=Shapley_norm, y=factor(club, team_order)))+geom_point(color = 'darkblue')+xlab('Shapley value')





####КОРРЕЛЯЦИЯ ШЕПЛИ С ДРУГИМИ СТАТИСТИКАМИ####
#работа со статистикой игроков
final_obrez <- final_1[-c(1,4,6)]
final_cor <- cor(final_obrez[ , colnames(final_obrez) != "Shapley"],
                 final_obrez$Shapley, use = "pairwise.complete.obs")
final_cor <- as.data.frame(final_cor)

#таблица для корреляций
final_obrez <- final_obrez[-c(1)]
final_cor$stat <- colnames(final_obrez)
final_cor <- cbind(final_cor$stat, final_cor$V1)
final_cor <- as.data.frame(final_cor)
names(final_cor) <- c("stat", "value")

#визуализация корреляций
write.csv(final_cor, "final_cor.csv")
final_cor <- read.csv("final_cor.csv", header = TRUE, sep = ",", dec = ".")
final_cor <- final_cor[,-1]
ggplot(final_cor, aes(stat, value))+geom_bar(stat = "identity", fill = "darkblue")+coord_flip()+ylab("Correlation")+xlab("Statistics")





####ВЕРОЯТНОСТЬ ПОБЕДЫ ОСНОВЫВАЯСЬ НА СРАВНЕНИИ ПОЗИЦИЙ####
position_shapley_A <- final_by_position_clubs
colnames(position_shapley_A) <- c('team_A', 'position_A', 'Shapley_position_A', 'Shapley_norm_position_A')

position_shapley_B <- final_by_position_clubs
colnames(position_shapley_B) <- c('team_B', 'position_B', 'Shapley_position_B', 'Shapley_norm_position_B')

position_pr <- merge(data_1, position_shapley_A, by = "team_A", all.x = TRUE)
position_pr <- merge(position_pr, position_shapley_B, by = "team_B", all.x = TRUE)

#выбираем только нужные колонки
colnames(position_pr)
position_pr <- position_pr %>% dplyr::select('tour', 'team_A', 'team_B', 'win_A', 'position_A', 'position_B', 
                                             'Shapley_position_A', 'Shapley_position_B', 
                                             'Shapley_norm_position_A', 'Shapley_norm_position_B')
position_pr <- position_pr[position_pr$position_A == position_pr$position_B, ]
position_pr$Shapley_position_diff <- position_pr$Shapley_position_A-position_pr$Shapley_position_B
position_pr$Shapley_norm_position_diff <- position_pr$Shapley_norm_position_A-position_pr$Shapley_norm_position_B

position_pr_mod <- position_pr %>%
  reshape2::dcast(team_A+team_B ~ position_A, value.var = "Shapley_position_diff", sum)

position_pr_mod$DEF <- position_pr_mod$DEF/2
position_pr_mod$MID <- position_pr_mod$MID/2
position_pr_mod$GK <- position_pr_mod$GK/2
position_pr_mod$FW <- position_pr_mod$FW/2

position_pr_tmp <- unique(position_pr %>% dplyr::select('tour', 'team_A', 'team_B', 'win_A'))

position_pr_mod <- left_join(position_pr_mod, position_pr_tmp, by=c('team_A'='team_A', 'team_B'='team_B'))

#логит модель
mod_log_position <- glm(win_A ~ DEF+MID+FW+GK, family = binomial, data = position_pr_mod)
summary(mod_log_position)
pR2(mod_log_position)['McFadden']

stargazer(mod_log_position, se=list(cse(mod_log_position)), type="text", out="mod_log_position.htm")

Pr_positions <- as.data.frame(predict(mod_log_position))
position_pr_mod$Pr <- 100/(1+exp(-Pr_positions))
colnames(position_pr_mod)
position_pr_mod_1 <- position_pr_mod %>% dplyr::select('team_A', 'team_B', 'Pr')
position_pr_mod_1 <- unique(position_pr_mod_1)
df = position_pr_mod_1[seq(1, nrow(position_pr_mod_1), 2), ]
df$Pr = df$Pr/100

#матрица вероятностей побед
ggplot(df, aes(x=factor(team_B, rev(team_order)), y=factor(team_A, team_order)))+
  geom_raster(aes(fill=Pr$`predict(mod_log_position)`))+
  scale_fill_gradient(low="lightblue", high="darkblue", na.value = "grey")+theme_bw()+
  geom_text(aes(label = scales::percent(Pr$`predict(mod_log_position)`, 2)), color = "white", size = 2)+
  theme(axis.text.x = element_text(angle = 90))+labs(y = "Команда А", x = "Команда B", fill = "Вероятность победы")





####СВЯЗЬ С ЗАРПЛАТАМИ####
salary_league_urls <- fb_league_urls(country = "ENG", gender = "M", season_end_year = 2022, tier = '1st')
salary_teams_urls <- fb_teams_urls(salary_league_urls)
salary_players <- fb_squad_wages(team_urls = salary_teams_urls)
salary_players <- salary_players %>% dplyr::select('Player', 'AnnualWageEUR')

final_salaries <- left_join(final_emwa_2, salary_players,by=c('Player_Name'='Player'))
final_salaries_1 <- final_salaries[complete.cases(final_salaries$AnnualWageEUR),]

#корреляция
cor(final_salaries_1$Shapley, final_salaries_1$AnnualWageEUR, method = c("pearson", "kendall", "spearman"))
cor.test(final_salaries_1$Shapley, final_salaries_1$AnnualWageEUR, method=c("pearson", "kendall", "spearman"))

#ивзуализация
ggplot(final_salaries, aes(x=AnnualWageEUR, y=Shapley))+geom_point() +geom_text(aes(label=Player_Name))

#регрессия
mod_salary <- lm(data=final_salaries, log(AnnualWageEUR)~Shapley)
summary(mod_salary)
resettest(mod_salary)

#самые большие по остаткам
residuals_salary <- resid(mod_salary)
final_salaries_2 <- cbind(final_salaries_1,residuals_salary)
colnames(final_salaries_2)
final_salaries_3 <- final_salaries_2 %>% dplyr::select('Player_Name', 'Shapley', 'club', 'position', 'AnnualWageEUR', 'residuals_salary')

#визуализация
ggplot(final_salaries_2, aes(y=log(AnnualWageEUR), x=Shapley, label=Player_Name))+
  geom_point(aes(color=residuals_salary))+geom_text(check_overlap = TRUE, hjust = 0, nudge_x = 0.2)+
  xlab("Значение Шепли")+ylab("Логарифм годовой зарплаты")+labs(color="Остаток регрессии")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

write.xlsx(final_salaries_3, 'final_salaries_3.xlsx')
stargazer(final_salaries_3, summary = FALSE, out = "final_salaries_2.htm")





####КАРТА УДАРОВ####
dd <- understat_league_match_results("EPL", 2021)
dd_1 <- understat_team_meta(team_name = c("Liverpool", "Manchester City"))
dd_2 <- understat_league_season_shots("EPL", 2021)

#фиктивная с голом
dd_2$goal <- ifelse(dd_2$result == "Goal", 1, 0)

#на примере
dd_2_cristiano <- dd_2[dd_2$player == "Cristiano Ronaldo", ]
dd_2_cristiano_pen <- dd_2_cristiano[dd_2_cristiano$situation == "Penalty", ]
dd_2_cristiano_pen$X <- dd_2_cristiano_pen$X*122
dd_2_cristiano_pen$Y<- dd_2_cristiano_pen$Y*80

dd_2_cristiano$X <- dd_2_cristiano$X*122
dd_2_cristiano$Y<- dd_2_cristiano$Y*80

#карта ударов роналду
create_Pitch(grass_colour = "#538032", 
             line_colour =  "#ffffff", 
             background_colour = "#538032", 
             goal_colour = "#000000")+
  geom_point(dd_2_cristiano, mapping = aes(x = X, y = Y, col=goal, size=xG))+
  ggtitle("Cristiano Ronaldo shot map")

#выбираем лучших и худших нападающих
fw_shapley <- final_2[final_2$position == "FW", ]
fw_shapley <- final_2[final_2$Shapley > 0, ]
fw_shapley <- fw_shapley %>% dplyr::select('Player_Name', 'Shapley')
fw_shapley <- fw_shapley[complete.cases(fw_shapley$Shapley),]
m <- median(fw_shapley$Shapley)
qn_1 <- quantile(fw_shapley$Shapley)[1]
qn_5 <- quantile(fw_shapley$Shapley)[5]

#подвыборки с медианой
fw_big <- fw_shapley[fw_shapley$Shapley>=m,]
fw_small <- fw_shapley[fw_shapley$Shapley<m,]

fw_big <- left_join(fw_big, dd_2, by=c('Player_Name'='player'))
fw_small <- left_join(fw_small, dd_2, by=c('Player_Name'='player'))

fw_big <- fw_big[complete.cases(fw_big$xG),]
fw_small <- fw_small[complete.cases(fw_small$xG),]

#модели с медианой
shot_model_big <- glm(goal ~ xG, family = binomial, data = fw_big)
summary(shot_model_big)

influencePlot(shot_model_big)
fw_big_1 <- fw_big %>% slice(-c(30,34,932,1156,1695,1713))
shot_model_big_1 <- glm(goal ~ xG, family = binomial, data = fw_big_1)

res_big <- shot_model_big_1$residuals
res_big <- 1 - predict(shot_model_big_1, type = 'response')

fw_big_1$residual <- res_big

#карта
fw_big_1$X <- fw_big_1$X*122
fw_big_1$Y<- fw_big_1$Y*80
create_Pitch(grass_colour = "#538032", 
             line_colour =  "#ffffff", 
             background_colour = "#538032", 
             goal_colour = "#000000")+
  geom_point(fw_big_1, mapping = aes(x = X, y = Y, col=goal, size=xG))+
  coord_flip(xlim = c(60, 120), ylim = c(0,80))

shot_model_small <- glm(goal ~ xG, family = binomial, data = fw_small)
summary(shot_model_small)

influencePlot(shot_model_small)
fw_small_1 <- fw_small %>% slice(-c(131,209,531,694,728,901))
shot_model_small_1 <- glm(goal ~ xG, family = binomial, data = fw_small_1)

res_small <- shot_model_small_1$residuals
res_small <- 1 - predict(shot_model_small_1, type = 'response')

fw_small_1$residual <- res_small

#карта
fw_small_1$X <- fw_small_1$X*122
fw_small_1$Y<- fw_small_1$Y*80
create_Pitch(grass_colour = "#538032", 
             line_colour =  "#ffffff", 
             background_colour = "#538032", 
             goal_colour = "#000000")+
  geom_point(fw_small_1, mapping = aes(x = X, y = Y, col=goal, size=xG))+
  coord_flip(xlim = c(60, 120), ylim = c(0,80))

res_big <- as.data.frame(res_big)
res_big$group <- 'top'
colnames(res_big)[1] <- 'residual'

res_small <- as.data.frame(res_small)
res_small$group <- 'bottom'
colnames(res_small)[1] <- 'residual'

res_all <- rbind(res_big, res_small)

mean(res_big$residual)-mean(res_small$residual)

group_by(res_all, group) %>% summarise(count = n(), mean = mean(residual, na.rm = TRUE),
                                       sd = sd(residual, na.rm = TRUE))

t.test(res_big$residual, res_small$residual, alternative = "two.sided", var.equal = FALSE)
res_all %>%  cohens_d(residual ~ group, var.equal = TRUE)

#боксплот
ggboxplot(res_all, x = "group", y = "residual", 
          color = "group", palette = c("lightblue", "darkblue"),
          ylab = "Residual", xlab = "Group of players")

fw_all <- rbind(fw_big_1, fw_small_1)
ggplot(fw_all, aes(x=Shapley, y=residual, color=goal))+geom_point()+geom_vline(xintercept=m, color="red")

#плотность
ggplot(res_all, aes(x = residual, fill = group)) + geom_density(alpha = 0.5)

#подвыборки с квинтелями
fw_big_q <- fw_shapley[fw_shapley$Shapley>=qn_5,]
fw_small_q <- fw_shapley[fw_shapley$Shapley<=qn_1,]

fw_big_q <- left_join(fw_big_q, dd_2, by=c('Player_Name'='player'))
fw_small_q <- left_join(fw_small_q, dd_2, by=c('Player_Name'='player'))

fw_big_q <- fw_big[complete.cases(fw_big$xG),]
fw_small_q <- fw_small[complete.cases(fw_small$xG),]

#модели с квинтелями
shot_model_big_q <- glm(goal ~ xG, family = binomial, data = fw_big_q)
summary(shot_model_big_q)

influencePlot(shot_model_big_q)
fw_big_q <- fw_big_q %>% slice(-c(360,1586,1975,2714,2798,4902))
shot_model_big_q <- glm(goal ~ xG, family = binomial, data = fw_big_q)

res_big_q <- shot_model_big_q$residuals
res_big_q <- 1 - predict(shot_model_big_q, type = 'response')

fw_big_q$residual <- res_big_q

#карта
fw_big_q$X <- fw_big_q$X*122
fw_big_q$Y<- fw_big_q$Y*80
fw_big_q$goal_fact <- if_else(fw_big_q$goal == 1, TRUE, FALSE)
gg_map_1 <- create_Pitch(grass_colour = "#224C56", 
             line_colour =  "#B3CED9", 
             background_colour = "#224C56", 
             goal_colour = "#15393D")+
  geom_point(fw_big_q, mapping = aes(x = X, y = Y, col=goal_fact, size=xG))+
  coord_flip(xlim = c(60, 120), ylim = c(0,80))+labs(col="Гол", size="xG")+
  scale_colour_manual(values = c("TRUE"="chartreuse3", "FALSE"="firebrick3"))+theme(legend.position = "top")

#модель
shot_model_small_q <- glm(goal ~ xG, family = binomial, data = fw_small_q)
summary(shot_model_small_q)

influencePlot(shot_model_small_q)
fw_small_q <- fw_small %>% slice(-c(304,363,898,1264,2304,2686))
shot_model_small_q <- glm(goal ~ xG, family = binomial, data = fw_small_q)

res_small_q <- shot_model_small_q$residuals
res_small_q <- 1 - predict(shot_model_small_q, type = 'response')

fw_small_q$residual <- res_small_q

#карта
fw_small_q$X <- fw_small_q$X*122
fw_small_q$Y<- fw_small_q$Y*80
fw_small_q$goal_fact <- if_else(fw_small_q$goal == 1, TRUE, FALSE)
gg_map_2 <- create_Pitch(grass_colour = "#224C56", 
                         line_colour =  "#B3CED9", 
                         background_colour = "#224C56", 
                         goal_colour = "#15393D")+
  geom_point(fw_small_q, mapping = aes(x = X, y = Y, col=goal_fact, size=xG))+
  coord_flip(xlim = c(60, 120), ylim = c(0,80))+labs(col="Гол", size="xG")+
  scale_colour_manual(values = c("TRUE"="chartreuse3", "FALSE"="firebrick3"))+theme(legend.position = "top")

res_big_q <- as.data.frame(res_big_q)
res_big_q$group <- 'top'
colnames(res_big_q)[1] <- 'residual'

res_small_q <- as.data.frame(res_small_q)
res_small_q$group <- 'bottom'
colnames(res_small_q)[1] <- 'residual'

res_all_q <- rbind(res_big_q, res_small_q)

mean(res_big_q$residual)-mean(res_small_q$residual)

group_by(res_all_q, group) %>% summarise(count = n(), mean = mean(residual, na.rm = TRUE),
                                       sd = sd(residual, na.rm = TRUE))

t.test(res_big_q$residual, res_small_q$residual, alternative = "two.sided", var.equal = FALSE)
0.8868022-0.9180551 

res_all_q %>%  cohens_d(residual ~ group, var.equal = TRUE)

ggboxplot(res_all_q, x = "group", y = "residual", 
          color = "group", palette = c("lightblue", "darkblue"),
          ylab = "Residual", xlab = "Group of players")

#плотность
ggplot(res_all_q, aes(x = residual, fill = group)) + geom_density(alpha = 0.5)+
  xlab("Значение остатка")+ylab("Плотность")+labs(fill="Квинтиль")





####РАСШИРЕНИЕ СОСТАВА ДО ВСЕХ КТО УЧАСТВОВАЛ В МАТЧЕ####
#squads_new <- fotmob_get_match_players(match_ids)
squads_new <- apply(squads_new,2,as.character)
squads_new <- as.data.frame(squads_new)
squads_dont_touch <- squads_new
squads_new <- squads_new[squads_new$stats_minutes_played >=30, ] #кто сыграл не меньше 30 минут
squads_new <- squads_new %>% distinct() #удалил дубли
write.csv(squads_new, "squads_new.csv")

#приклеил к игрокам результаты игр по id матча
lineups_plus_results_new <- merge(squads_new, results_1, by = "match_id", all.x = TRUE)

#столбец с полным именем игрока
lineups_plus_results_new_1 <- lineups_plus_results_new %>% 
  mutate(Player_Name = paste(first_name,last_name,sep=" "))

#столбец с кодом состава
lineups_plus_results_new_1$tmp <- with(lineups_plus_results_new_1, ifelse(home_long_name == team_name, team_name, away_long_name))
lineups_plus_results_new_2 <- lineups_plus_results_new_1 %>% 
  mutate(code_squad = paste(tmp,match_tournament_stage,sep="_"))

#столбец с количеством очков
lineups_plus_results_new_2$points <- with(lineups_plus_results_new_2, ifelse((tmp == home_long_name & home_score > away_score) | (tmp == away_long_name & home_score < away_score), 3, 
                                                                             ifelse((tmp == away_long_name & home_score > away_score) | (tmp == home_long_name & home_score < away_score), 0, 1)))

#сводник - по столбцам код состава, по строкам игроки
grouped_players_new <- lineups_plus_results_new_2 %>% 
  group_by(Player_Name, code_squad) %>%
  summarize(points_sum = sum(points, na.rm = TRUE))
all_clubs_new <- pivot_wider(grouped_players_new, names_from = code_squad, values_from = points_sum, values_fill = NA)
sort(colnames(all_clubs_new))
t_all_clubs_new <-  as.data.frame(transpose(all_clubs_new))
colnames(t_all_clubs_new) <- all_clubs_new$Player_Name
rownames(t_all_clubs_new) <- colnames(all_clubs_new)
t_all_clubs_new <- t_all_clubs_new[-1,]
t_all_clubs_new_1 <- t_all_clubs_new %>% distinct()
n_occur <- data.frame(table(unlist(t_all_clubs_new)))
t_all_clubs_new[,t_all_clubs_new %in% n_occur$Var1[n_occur$Freq > 1]]



