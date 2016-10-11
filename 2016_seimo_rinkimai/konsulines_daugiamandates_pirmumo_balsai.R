
####  Seimo rinkimai 2016, daugiamandates
####  pirmumo balsu, surinktu Lietuvos konsulatuose, analize 
####  kiekvienam konsulatui, kiekvienai partijai, kiekvienam kandidatui

#### 1) run Mac Automator to extract data from VRK website
#### 2) run this script to format the web-to-text file data

# load VRK data
a = readLines("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/vrk_extract_foreign_20161011-0800.txt")

# check the freshness of the data
unique(a[grep("Puslapis atnaujintas",a)])

# check the web-scraping quality
# each embassy should have a report on vote counts for each party
ap_ix = grep("Puslapis atnaujintas",a) + 4 # apylinke
pa_ix = grep("Puslapis atnaujintas",a) + 6 # partija
table(table(a[ap_ix]))  # apylinkiu duomenys apie 14 partiju
table(table(a[pa_ix]))  # partiju duomenys ish 49 konsuliniu apylinkiu

### identify the positions in the data that contain relevant information
from = grep("^balsai",a) + 1
till = grep("^Pastaba. Daugiamandatėje",a) - 1
apyl = grep("^Puslapis atnaujintas",a)  + 4
part = grep("^Puslapis atnaujintas",a)  + 6

### doublecheck for inconsistencies (warning flags)
if( !all(length(from)==length(till),length(apyl)==length(part),length(part)==length(till))) {
        warning("nesutampa start/end indikatoriu ilgiai")
}

### create the document map (index)
map = data.frame(apyl=apyl,part=part,from = from,till=till,stringsAsFactors = F)
head(map)

### extract only relevant data in a
cum = NULL # cumulator of results
for (i in 1:nrow(map)) {
        apylinke = a[map[i,"apyl"]]
        partija = a[map[i,"part"]]
        print(paste(apylinke,partija,sep="__"))
        rows = (map[i,"from"]):(map[i,"till"])
        for (j in rows) { # kiekvienam kandidatui ishraukti surinktus pirmumo balsus
                xtr = unlist(strsplit(a[j],"\t"))
                tmp = data.frame(apyl=apylinke,part=partija,
                                 vardas=xtr[2],post=xtr[1],pre=xtr[3], # numeris sarashe priesh ir po rinkimu
                                 n=xtr[4],stringsAsFactors = F) # gauti pirmumo balsai
                cum = rbind(cum,tmp)
                rm(xtr,tmp)
        }
        rm(rows)
}
cum$n[which(is.na(cum$n))] = 0 # assign zero's where NA's are found
head(cum)

#####  assign shorter political party names
cum$partija = NA # to be filles using translator
transl = read.table("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/partiju_zodynas.txt",sep="\t",h=T,stringsAsFactors = F)
for (i in 1:nrow(transl)) {
        cum$partija[grep(transl[i,"pavadinimo_fragmentas"],cum$part)] = transl[i,"trumpinys"]
}
cum = cum[,-2]
head(cum)
table(cum$partija,useNA="a")

#####  assign shorter consulate names
cum$apylinke = NA # to be filles using translator
transl = read.table("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/konsulatu_zodynas.txt",sep="\t",h=T,stringsAsFactors = F)
for (i in 1:nrow(transl)) {
        cum$apylinke[grep(transl[i,"lokacijos_fragmentas"],cum$apyl)] = transl[i,"trumpinys"]
}
cum = cum[,-1]
head(cum)
table(cum$apylinke,useNA="a")

### doublecheck
cum[which(cum$vardas=="GABRIELIUS LANDSBERGIS"),]

cum$n = as.numeric(cum$n)
cum$post = as.numeric(cum$post)
cum$pre = as.numeric(cum$pre)

# save for future reference
save(list="cum",file="~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/konsulines_daugiamandates_pirmumo_balsai.RData")

######################################################


library(dplyr)
head(cum)
df = as.data.frame(group_by(cum,apylinke) %>% summarise(pirmBals=sum(n)) %>% ungroup())
dat = merge(tmp,df,by="apylinke",all=T)
dat$eff = dat$n / dat$pirmBals * 100

df = df[order(df$pirmBals),]
barplot(df$pirmBals,names.arg = df$apylinke,horiz = T)


cum[which((cum$apylinke=="Švedija")&(cum$partija=="Konservatoriai")),]


unique(cum$partija)
sub = cum[which(cum$partija=="ValstiečiaiŽalieji"),]
dd = as.data.frame(group_by(sub,pre) %>% summarise(s=sum(n)) %>% ungroup())
barplot(dd$s,names.arg = dd$pre)



###########  PER PARTY (all parties)
df = as.data.frame(group_by(cum,apylinke) %>% summarise(s=sum(n)) %>% ungroup())
df = df[which(df$s>=500),]
n_part = length(unique(cum$partija))
n_apyl = length(unique(df$apylinke))
partijos = unique(cum$partija)
apylinkes = df$apylinke[order(df$s,decreasing = T)]
m = matrix(NA,nr=n_part,nc=n_apyl)
row.names(m) = partijos
colnames(m) = apylinkes
for (i in 1:n_part) {
        for(j in 1:n_apyl) {
                
ix = which((cum$partija==partijos[i])&(cum$apylinke==apylinkes[j]))
fr = sum(cum[ix,"n"]) / df$s[which(df$apylinke==apylinkes[j])]
m[i,j] = fr
rm(ix,fr)
        }
}
library(corrplot)
corrplot(m,is.corr = F)


###########  PER PERSON  (12 most popular ones)
# apylinkes (ambasados)
df = as.data.frame(group_by(cum,apylinke) %>% summarise(s=sum(n)) %>% ungroup())
df = df[which(df$s>=300),]
df = df[order(df$s,decreasing = T),]
apylinkes = df$apylinke[order(df$s,decreasing = T)]

ss = group_by(cum,vardas) %>% summarise(s=sum(n)) %>% ungroup()
ss = ss[order(ss$s,decreasing = T),]
kandidatai = ss$vardas[1:24]

n_kand = length(unique(kandidatai))
n_apyl = length(unique(df$apylinke))
m = matrix(NA,nr=n_kand,nc=n_apyl)
row.names(m) = kandidatai
colnames(m) = apylinkes
for (i in 1:n_kand) {
        for(j in 1:n_apyl) {
                ix = which((cum$vardas==kandidatai[i])&(cum$apylinke==apylinkes[j]))
                fr = sum(cum[ix,"n"]) / df$s[which(df$apylinke==apylinkes[j])]
                m[i,j] = fr
                rm(ix,fr)
        }
}
library(corrplot)
corrplot(m,is.corr = F,method = "square",tl.col = "royalblue4",tl.cex=0.7)
df
