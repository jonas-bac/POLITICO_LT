
# Seimo rinkimai 2016
# various aspects of voting patterns from LT embassies and consulates
# naudojami daugiamandachiu duomenys (ne galutinis VRK freeze'as)
# naudojami tik pirmumo balsai (atspindi samoningesne publika)

library(dplyr)
library(corrplot)

# load digested VRK data
load("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/konsulines_daugiamandates_pirmumo_balsai_20161011-0800.RData")


####### 1) preview winners and loosers party-wise and embassy-wise

# EMBASSIES
# rank all consulates based on their activity
df = group_by(dat,apylinke) %>% summarise(s=sum(n)) %>% ungroup()
df = as.data.frame(df)
df = df[order(df$s,decreasing = T),]
df$apylinke[grep("Almata",df$apylinke)] = "KzchstnAlmata"
# visualize
barplot(df$s,names.arg = df$apylinke,horiz = F,las=2,cex.names = 0.6,cex.axis = 0.9,col="darkcyan",space = 0.5)

## PARTIES
# rank all parties based on their accumulated priority votes
df = group_by(dat,partija) %>% summarise(s=sum(n)) %>% ungroup()
df = as.data.frame(df)
df = df[order(df$s,decreasing = T),]
df$partija[which(df$partija=="TvarkaTeisingumas")] = "TT"  # shorten it up
# visualize
barplot(df$s,names.arg = df$partija,horiz = T,las=1,cex.names = 0.6,cex.axis = 0.9,col="deepskyblue4")



####### 2) a propos: find outliers (overachievers) in each political party
# this part is extended in a separate script 
#party_names = unique(dat$partija)
#pdf("~/Downloads/temptemp.pdf")
#for (i in 1:length(party_names)) {
#        sub = dat[which(dat$partija==party_names[i]),]
#        dd = as.data.frame(group_by(dat,pre,vardas) %>% summarise(s=sum(n)) %>% ungroup())
#        #barplot(dd$s,names.arg = dd$pre)
#        plot(dd$s ~ dd$pre,main=party_names[i],xlab="Kandidato eilės numeris sąrašę",
#             ylab="Surinktų pirmumo balsų skaičius")
#        mod = loess(dd$s ~ dd$pre,span = 0.8)
#        points(dd$pre,predict(mod,dd$pre),type="l",col="red")
#}
#dev.off()


####### 2) visualize party-embassy and candidate-embassy combinations


###########
###########  PARTY-CENTERED VIEW (all parties)
###########

df = as.data.frame(group_by(dat,apylinke) %>% summarise(s=sum(n)) %>% ungroup())
df = df[which(df$s>=300),]  # tik nors kiek rimtesnes ambasados/konsulatai
n_part = length(unique(dat$partija))
n_apyl = length(unique(df$apylinke))
partijos = unique(dat$partija) # chia butu protinga partijassortinti pagal total votes
apylinkes = df$apylinke[order(df$s,decreasing = T)]
m = matrix(NA,nr=n_part,nc=n_apyl) # rezultatu kaupimo matrica
row.names(m) = partijos; colnames(m) = apylinkes # matricos meta duomenys
for (i in 1:n_part) { # kiekvienai partijai
        for(j in 1:n_apyl) { #kiekvienai ambasadai/konsulatui
                ix = which((dat$partija==partijos[i])&(dat$apylinke==apylinkes[j]))
                fr = sum(dat[ix,"n"]) / df$s[which(df$apylinke==apylinkes[j])]
                # paaishkinimai:
                # fr = frakcija
                # sum(dat[ix,"n"]) = partijos surinktu pirmumo balsu pateiktu diplomat atstovybeje skaichius 
                # df$s[which(df$apylinke==apylinkes[j])] = diplomat atstovybeje pateiktu pirmumo balsu skaichius
                m[i,j] = fr # accumulate results
                rm(ix,fr) # cleanup
        }
}
# visualize:
corrplot(m,is.corr = F,method = "square",tl.col = "royalblue4",tl.cex=0.7)


# supplement: rank all consulates based on their activity
df = group_by(dat,apylinke) %>% summarise(s=sum(n)) %>% filter(s>=300) %>% ungroup()
df = df[order(df$s,decreasing = T),]
df$apylinke[grep("Almata",df$apylinke)] = "KzchstnAlmata" # shorten it up
barplot(df$s,names.arg = df$apylinke,horiz = F,las=2,cex.names = 0.6,cex.axis = 0.9,col="steelblue",space = 0.5)


###########
###########  CANDIDATE-CENTERED VIEW (only top candidates)
###########

# apylinkes (ambasados)
df = as.data.frame(group_by(dat,apylinke) %>% summarise(s=sum(n)) %>% ungroup())
df = df[which(df$s>=300),]
df = df[order(df$s,decreasing = T),]
apylinkes = df$apylinke[order(df$s,decreasing = T)]

# top most popular candidates
n_kand = 25
ss = group_by(dat,vardas_partija,vardas) %>% summarise(s=sum(n)) %>% ungroup()
ss = ss[order(ss$s,decreasing = T),]
kandidatai = ss$vardas[1:n_kand]
kandidatai_part = ss$vardas_partija[1:n_kand]

# alternatively: 5 best candidates from 5 most popular parties
#tmp = group_by(dat,partija) %>% summarise(s=sum(n)) %>% ungroup()
#tmp = tmp[order(tmp$s,decreasing = T),] # rank all parties based on their popularity
#ss = NULL
#for (part in tmp$partija[1:5]) {
#        sub = dat[which(dat$partija==part),]
#        tmp = group_by(sub,vardas,vardas_partija) %>% summarise(s=sum(n)) %>% ungroup()
#        tmp = as.data.frame(tmp[order(tmp$s,decreasing = T),])
#ss = rbind(ss,tmp[1:5,]); rm(sub,tmp)
#}
#kandidatai = ss$vardas
#kandidatai_part = ss$vardas_partija

n_kand = length(unique(kandidatai))
n_apyl = length(unique(df$apylinke))
m = matrix(NA,nr=n_kand,nc=n_apyl)
row.names(m) = kandidatai_part
colnames(m) = apylinkes
for (i in 1:n_kand) {
        for(j in 1:n_apyl) {
                ix = which((dat$vardas==kandidatai[i])&(dat$apylinke==apylinkes[j]))
                fr = sum(dat[ix,"n"]) / df$s[which(df$apylinke==apylinkes[j])]
                # paaishkinimai:
                # fr = frakcija
                # sum(dat[ix,"n"]) = kandidato surinktu pirmumo balsu pateiktu diplomat atstovybeje skaichius 
                # df$s[which(df$apylinke==apylinkes[j])] = diplomat atstovybeje pateiktu pirmumo balsu skaichius
                m[i,j] = fr # accumulate results
                rm(ix,fr)
        }
}
corrplot(m,is.corr = F,method = "square",tl.col = "royalblue4",tl.cex=0.7)
#note: matrix is not sorted by party yet


##### EXPERIMENT: candidate-normalized view
# kitokia interpretacija. normalizuojama ne apylinkes
# visuminiams balsams, o kandidato visuminiams balsams.
# = "kokia dalis kandidato balsu atkeliauja ish kokios ambasados"
# apylinkes (ambasados)
df = as.data.frame(group_by(dat,apylinke) %>% summarise(s=sum(n)) %>% ungroup())
df = df[which(df$s>=1000),]  #tik didesnes ambasados/konsulatai
df = df[order(df$s,decreasing = T),]
apylinkes = df$apylinke[order(df$s,decreasing = T)]
# most popular candidates
ss = group_by(dat,vardas) %>% summarise(s=sum(n)) %>% ungroup()
ss = ss[order(ss$s,decreasing = T),]
kandidatai = ss$vardas[1:30]  #tik populiaresni kandidatai

n_kand = length(unique(kandidatai))
n_apyl = length(unique(df$apylinke))
m = matrix(NA,nr=n_kand,nc=n_apyl)
row.names(m) = kandidatai; colnames(m) = apylinkes
for (i in 1:n_kand) {
        for(j in 1:n_apyl) {
                ix = which((dat$vardas==kandidatai[i])&(dat$apylinke==apylinkes[j]))
                fr = sum(dat[ix,"n"]) / ss$s[which(ss$vardas==kandidatai[i])]
                # paaishkinimai:
                # fr = frakcija
                # sum(dat[ix,"n"]) = kandidato surinktu pirmumo balsu pateiktu diplomat atstovybeje skaichius 
                # ss$s[which(ss$vardas==kandidatai[i])] = visuminis kandidato gautu pirmumo balsu skaichius
                m[i,j] = fr
                rm(ix,fr)
        }
}
corrplot(m,is.corr = F,method = "square",tl.col = "royalblue4",tl.cex=0.7)
as.data.frame(ss)[1:30,]
