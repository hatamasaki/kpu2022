#エクセルも読み込めるようにするパッケージだよ〜
library(readxl)
#trialdata.xlsという新しいデータを秦のサーバ（遊戯王で言えばデッキ？）からfiles2という名前で入れ込む（遊戯王で言えば手札？）に！
trialdata <- download_and_extract("http://hatam.sakura.ne.jp/trialdata.zip")
#trialdataという名前のファイルをhatadataという愛称をつけて呼び出す（召喚！）
hatadata <- read_excel("trialdata.xls")
#これ以降，hatadata以外のデータは受け付けないという魔法効果的やおまじない
attach(hatadata)
#hatadataには何が入ってるのか見てみる
names(hatadata)

#たとえば，q2_1は年齢のようだ見てみよう！
summary(q2_1)
table(q2_1)

#たとえば，q2_2は学歴のようだ見てみよう!
summary(q2_2)
#ん？意味がなさそうだ→尺度！
table(q2_2)
#6（言いたくない）は分析に使えないので消してやる
edu <- q2_2
edu[q2_2== 6] <- NA 
#いらないコードは消してやったぜチェック
table(edu)

#世帯収入（21は不要なので"income"という変数にして欠損値処理してみよう！）
summary(q2_3)


#こたえ
income <- q2_3
income[q2_3== 21] <- NA 
table(income)

#q7_5をみてみよう→今の日本の政治はおかしい
okashi <- q7_5
table(okashi)
#5だけでなく6もいらない子！
okashi[q7_5== 5|q7_5== 6] <- NA 
table(okashi)
#わかりにくい！高いほうが「そう思う」っしょ普通？！
okashi[q7_5== 1] <- 4 
okashi[q7_5== 2] <- 3 
okashi[q7_5== 3] <- 2
okashi[q7_5== 4] <- 1

table(okashi)

#では，今のテレビ報道・韓国のやり方・安倍政権・野党・今の政治の5つの変数でどれが人気でしょうか？
#「今の日本政治はおかしい」と思ってる人の平均値の計算 
mean(okashi, na.rm = TRUE)
