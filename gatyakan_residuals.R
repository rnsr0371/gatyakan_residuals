#必要なライブラリのインポート
library(tidyverse)

#データのインポート
teams=read_csv("data/teams.csv")
boxscore=read_csv("data/games_boxscore_202021.csv")

#teamsから2020-21_B1のデータだけ抜き出す
teams=teams %>% filter(League=="B1") %>% filter(Season=="2020-21")

#boxscoreからB1のデータだけ抜き出す
boxscore=teams %>% left_join(boxscore,by="TeamId")


#MINでPTSを回帰する線形回帰モデルを立てる
model=boxscore %>% lm(PTS?MIN,data=.)

#modelの決定係数の確認
summary(model)#0.549

#モデルの残差をboxscoreに追加する
boxscore=boxscore %>% mutate(res=model$residuals)

#残差の要約統計量・ヒストグラムを表示
summary(boxscore$res)
boxscore %>% ggplot(aes(x=res))+geom_histogram()+xlab("残差")+ggtitle("データ全体の残差の平均のヒストグラム")

#残差を選手ごとに集計
data=boxscore %>% group_by(Player) %>% 
  summarise(PTS=mean(PTS),MIN=mean(MIN),res_mean=mean(res),res_std=sd(res))

#残差の記述統計量の確認
summary(data$res_mean)
sd(data$res_mean)
summary(data$res_std)
data %>% ggplot(aes(x=res_mean))+geom_histogram()+xlab("選手ごとの残差の平均")+ggtitle("選手ごとの残差の平均のヒストグラム")
data %>% ggplot(aes(x=res_std))+geom_histogram()+xlab("選手ごとの残差の標準偏差")+ggtitle("選手ごとの残差の標準偏差のヒストグラム")

#ランキングの表示
rank=data %>% arrange(desc(res_std)) %>% head(10)
rank

#残差の変動係数を算出
data=data %>% mutate(res_CV=res_std/res_mean)

#変動係数の絶対値でランキングを表示
#絶対値が大きい選手
rank=data %>% arrange(desc(abs(res_CV))) %>% head(10)
show(rank)

#平均得点が5点以上で絶対値が小さい選手
rank=data %>% filter(PTS>=5) %>% arrange(abs(res_CV)) %>% head(10)
show(rank)


#変動係数が小さい選手（ライス選手）と大きい選手（角野選手）のヒストグラムを表示
boxscore %>% filter(Player=="レイヴォンテ・ライス") %>% 
  ggplot(aes(.$res))+
  geom_histogram()+
  xlab("得点を出場時間で回帰したときの残差")+
  ylab("試合数")+
  ggtitle("レイヴォンテ・ライス選手の残差のヒストグラム")


boxscore %>% filter(Player=="角野亮伍") %>% 
  ggplot(aes(.$res))+
  geom_histogram()+
  xlab("得点を出場時間で回帰したときの残差")+
  ylab("試合数")+
  ggtitle("角野亮伍選手の残差のヒストグラム")


#補足的分析
#角野選手が10分以上出場した場合の変動係数を見る
boxscore %>% filter(Player=="角野亮伍") %>% filter(MIN>=10) %>% group_by(Player) %>% summarise(res_avg=mean(res),res_std=sd(res),CV=res_std/res_avg)

#角野選手の出場時間の推移を見る
boxscore %>% filter(Player=="角野亮伍") %>% ggplot(aes(x=ScheduleKey,y=res))+geom_line()                                                          

#角野選手が0点だった試合を見る
kadono_=boxscore %>% filter(Player=="角野亮伍") %>% filter(PTS==0)



