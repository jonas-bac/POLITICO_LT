# Seimo rinkimai 2016, pirmas turas
# visuminiai pirmumo balsai (daugiamandates), vrk freeze 20161012-0700
# (no split based on apygarda ar apylinke)
#
# idea: identify candidates that got large and honest support from electors, i.e.,
# adjusting for the candidate's rank number in pre-election list,
# party size (number of candidates)
# total number of votes assigned to the party via prime-votes for party candidates)
#
# Jonas B. 2016 Oct 11
#
# sheme
#### 1) run Mac Automator to grab data from VRK website (done)
#### 2) run this script to digest and format web-grabbed data
#### 3) continue to identify "extreme-support" candidates
#### 4) vizualize
#
# see the text file with explained methodology:
# "metodika_ypatingam_kandidatu_palaikymui_aptikti.txt"
# and also the dataset used in this analysis:
# "raw_pirmumo_balsai_totals_20161012-0700.txt"
# also the result table "kandidatai_su_ypatingu_rinkeju_palankumu.txt"
# @ https://github.com/jonas-bac/POLITICO_LT/tree/master/2016_seimo_rinkimai
#

library(dplyr)

# load VRK data
a = readLines("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/raw_pirmumo_balsai_totals_20161012-0700.txt")

# check the freshness of the data
unique(a[grep("Puslapis atnaujintas",a)])

# check the web-grab quality
# there should be 14 parties 
pa_ix = grep("Puslapis atnaujintas",a) + 2 # partija
length(table(a[pa_ix]))  # 14 partiju duomenys 

### identify the positions in the data-file that contain relevant information
from = grep("^balsai",a) + 1  # pirmoji balsu eilute
till = grep("^Pastaba. Daugiamandatėje",a) - 1 # paskutine balsu eilute
part = grep("^Puslapis atnaujintas",a)  + 2 # partijos pavadinimas

### doublecheck for inconsistencies (warning flags)
if( !all(length(from)==length(till),length(from)==length(part))) {
        warning("nesutampa start/end indikatoriu ilgiai")
}

### create the document map (index)
map = data.frame(part=part,from = from,till=till,stringsAsFactors = F)
if(sum(map$till<map$from)>0) warning("map problems")

### extract only relevant data in object "a"
cum = NULL # cummulator of results
for (i in 1:nrow(map)) { # kiekvienai partijai
        partija = a[map[i,"part"]]
        print(partija)
        rows = (map[i,"from"]):(map[i,"till"])
        for (j in rows) { # kiekvienam kandidatui ishraukti surinktus pirmumo balsus
                xtr = unlist(strsplit(a[j],"\t"))
                tmp = data.frame(part=partija,vardas=xtr[2],post=xtr[1],pre=xtr[3], # numeris sarashe priesh ir po rinkimu
                                 n=xtr[4],stringsAsFactors = F) # gauti pirmumo balsai
                cum = rbind(cum,tmp)
                rm(xtr,tmp)
        }
        rm(rows)
}
# make sure that all numers are numbers
cum$post = as.integer(cum$post) # porinkiminio sarasho pozicija
cum$pre = as.integer(cum$pre) # prieshrinkiminio sarasho pozicija
cum$n = as.integer(cum$n) # surinktu pirmumo balsu skaichius ish visu apylinkiu ir diplomatiniu atstovybiu


#####  assign to each candidate party-dependency (name-partyAbbrev)

# load translator file for party name abbreviations
transl1 = read.table("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/partiju_zodynas.txt",sep="\t",h=T,stringsAsFactors = F)

# merge results with translator
cum = merge(cum,transl1,by.x="part",by.y="full_name",all=T)
cum = cum[,c("vardas","post","pre","n","trumpinys","abbrv","party_hash")] # reduce
cum$vardas_partija = paste(cum$vardas," (",cum$abbrv,")",sep="") # names for plotting
head(cum)


##### assign to each candidate the sum of prime votes assigned to that party

# helper (mini translator to be merged with main result object)
hlp = group_by(cum,trumpinys) %>% summarise(n_cand=n(),s=sum(n)) %>% ungroup() # s = party sum votes, n_cand = party size
hlp = as.data.frame(hlp) 
hlp = hlp[order(-hlp$s),] # most important - on top

## choose a color for every party
cols = c("royalblue2", # konserv
        "limegreen", # olivedrab2  # valstzhal
        "red", # firebrick1    socdem
        "orange", # liberalai
        "darkgray", #  46 krivickas
        "sienna3", # lenku rinkimu akcija
        "slateblue4", # tvarka teisingumas
        "lightsteelblue1", #skyblue1 #darbo partija
        "turquoise", # or orangered3    lts sarasas
        "deeppink", #laisves sajunga
        "olivedrab1", # yellowgreen springgreen1 #zalieji
        "chocolate1",#liaudies partija
        "yellow", #tautininkai
        "purple") # drasos kelias
barplot(1:length(cols),col=cols) # inspect
hlp$col = cols # assign colors

# add party-vote-sum and color data (helper object) with the results
dat = merge(cum,hlp,by="trumpinys",all=T)

## normalize vote count based on the size of the party and party total votes
dat$votes_norm = dat$n / dat$s * dat$n_cand  # normalize for total votes and number of candidates
dat$votes_norm = dat$votes_norm * 1e5 / 140 # restore to "standard" 1e5 votes per party and 140 condidates
# n = kandidato surinkti pirmumo balsai
# s = jo partijos surinkti visu kandidatu pirmumo balsai
# n_cand = kandidatu skaichius partijoje
# see the text file with more detailed exlanations: "metodika_ypatingam_kandidatu_palaikymui_aptikti.txt" @github


### only include major parties (some are small and incomparible without further normalization)
tbl = table(dat$trumpinys)
df = data.frame(partija=names(tbl),n_kand=as.numeric(tbl),stringsAsFactors = F)
df = df[order(-df$n_kand),]
df = df[which(df$n_kand>130),] # mazhos partijos eina lauk. lieka 7-ios
dat = dat[which(dat$trumpinys %in% df$partija),]


####  now follows estimation of upper-normVoteCount threshold for each party-list position

ys_med = ys_upp = NULL # median and upper threshold values for each (pre-election) position in party list
xs = 1:max(dat$pre) # partijos sarasho galimos poziciju reikshmes
for (x in xs) { # kiekvienai pozicijai
        # define offsets (+/- how many positions from this position will be used for comparissons)
        if (x %in% 1:5) ofs = 0  # none
        if (x %in% 6:10) ofs = 1 # small
        if (x %in% 11:20) ofs = 2
        if (x %in% 21:40) ofs = 3
        if (x %in% 41:60) ofs = 4 # large
        if (x %in% 61:135) ofs = 5 # large
        if (x %in% 136:141) ofs = 0 # small
        sub = dat[which(dat$pre %in% (x-ofs):(x+ofs)),]; rm(ofs) # subset of comparable candidates
        y_med = median(sub$votes_norm) # median for this preelection party-list position
        y_upp = median(sub$votes_norm) + 2*sd(sub$votes_norm) # upper threshold (above which candidates are "cool")
        #y = quantile(sub$votes_norm,probs = 0.90) # not good, since some parties have less candidates
        # also consider log-norm'ing normalized votes to get something more like Gaussian distribution
        ys_med = c(ys_med,y_med)
        ys_upp = c(ys_upp,y_upp)
}

# preview
plot(dat$votes_norm ~ dat$pre,ylab="normalizuotas pirmumo balsų skaičius",xlab="Priešrinkiminis NR sąraše") 
points(xs,ys_med,col="red",type="l",lwd=2)
points(xs,ys_upp,col="blue",type="l",lwd=2) # notice the spiky behaviour. solution below


####  FINAL PLOT 1
# smooth the spiky upper threshold line and on the same go - the median line
ys_med_smth = predict(loess(ys_med ~ xs,span = 0.2),xs) # smoothed medians
ys_upp_smth = predict(loess(ys_upp ~ xs,span = 0.2),xs) # smoothed upper thresholds
plot(dat$votes_norm ~ dat$pre,ylab="normalizuotas pirmumo balsų skaičius",xlab="Priešrinkiminis NR sąraše") 
points(xs,ys_med_smth,type="l",col="cornflowerblue",lwd=2)
points(xs,ys_upp_smth,type="l",col="orange",lwd=2)
legend(60,10000,legend = c("extremalumo riba","balsų mediana"),
       col=c("orange","cornflowerblue"),lwd=2,bty = "n")


######### combine estimated thresholds with the main results
thr = data.frame(xs,ys_upp_smth,stringsAsFactors = F) # threshold data
mrg = merge(dat,thr,by.x="pre",by.y="xs",all=T) # merge by pre-election position in party list
mrg = mrg[which( mrg$votes_norm > mrg$ys_upp_smth),] # leave only threshold-passers
mrg = mrg[order(mrg$trumpinys,-(mrg$ys_upp_smth)),] # sort by party and then by upper thr

# report on how many candidates were there in the party originally
tbl1 = table(dat$trumpinys)
df1 = data.frame(partija=names(tbl1),n_kand=as.numeric(tbl1),stringsAsFactors = F)
df1 = df1[order(-df1$n_kand),]

# report on how many candidates from each party are "extraordinary"
tbl2 = table(mrg$trumpinys)
df2 = data.frame(partija=names(tbl2),extrem=as.numeric(tbl2),stringsAsFactors = F)
df2 = df2[order(-df2$extrem),]

# combine both reports
df  = merge(df1,df2,by="partija",all=T)
df = df[order(-df$extrem),]
df

final_report_1 = df  # this is final report 1

#########   FINAL PLOT 2  (in the context of pre and post election position jumps)
dat = dat[order(-dat$s),]
plot(dat$post ~ dat$pre,col="grey",cex=0.7,main="išskirtinai populiarūs kandidatai",
     xlab="Priešrinkiminis NR sąraše",ylab="Porinkiminis NR sąraše")
abline(v=seq(10,130,10),col="grey",lty=2,lwd=0.5)
abline(h=seq(10,130,10),col="grey",lty=2,lwd=0.5)
abline(0,1,col="darkgrey")
sub = dat[which(dat$vardas %in% mrg$vardas),]
points(sub$post ~ sub$pre,cex=1.2)
points(sub$post ~ sub$pre,pch=19,col = sub$col,cex=1)
tmp = sub[which(!duplicated(sub$trumpinys)),]
tmp = tmp[order(-tmp$s),]
legend(0,135,legend = tmp$trumpinys,col = tmp$col,lwd=8,bty = "n",cex=0.8) # included parties
#oth = hlp$trumpinys[which(!hlp$trumpinys %in% tmp$trumpinys)]
legend(0,80,legend = oth,col = rep("white",length(oth)),lwd=8,bty = "n",cex=0.6) # not included parties


### final report 2 (most important)

colnames(mrg)[grep("^n$",colnames(mrg))] = "votes"
colnames(mrg)[grep("^s$",colnames(mrg))] = "party_votes"
colnames(mrg)[grep("^ys_upp_smth$",colnames(mrg))] = "votes_thr"
colnames(mrg)[grep("^trumpinys$",colnames(mrg))] = "partija"
colnames(mrg)[grep("^pre$",colnames(mrg))] = "pre_rank"
colnames(mrg)[grep("^post$",colnames(mrg))] = "post_rank"

mrg = mrg[,c("partija","abbrv","vardas","vardas_partija","pre_rank",
             "post_rank","votes","votes_norm","votes_thr")] # "n_cand","party_votes"
head(mrg)

write.table(mrg,"~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/kandidatai_su_ypatingu_rinkeju_palankumu.txt",row.names = F,col.names = T,sep="\t",quote=F)

### preambule output failui:
# Duomenys: vrk.lt 2016 Spalio 12, 07:00
# Atlikta: 2016 Spalio 12, jonasB
# Skaičiavimams naudotos tik partijos su 130+ kandidatų (mažesnėms partijoms reikalingas sudėtingesnis normalizavimas)
# Skaičiavimams naudojami tik pirmumo balsai (daugiamandatės) iš visų apylinkių ir LT diplomatinių atstovybių
# Kandidato gautų pirmumo balsų skaičius normalizuotas pagal partijos gautų balsų skaičių ir partijos kandidatų skaičių
# Kandidato išskirtinis populiarumas apibrėžiamas kaip 2 standartiniai nuokrypiai (SD) nuo gautų balsų medianos palyginamoje kandidatų grupėje (kandidatai su panašiu priešrinkiminiu sąrašo numeriu)
#
# post_rank - kandidato numeris sąraše po rinkimų pirmo turo
# pre_rank - kandidato numeris sąraše prieš rinkimų pirmą turą
# votes - kandidato surinkti pirmumo balsai
# votes_norm - kandidato surinkti pirmumo balsai normalizuoti partijos dydzhiui ir sekmingumui
# votes_thr - slenkstis kuri turi perzhengti konkretus kandidatas, kad butu skelbtinas "netiketai sekmingu"
#
# see the text file with more detailed exlanations:
# "metodika_ypatingam_kandidatu_palaikymui_aptikti.txt"
# @ https://github.com/jonas-bac/POLITICO_LT/tree/master/2016_seimo_rinkimai


# a propos: pavyzdys kazhkuriam fcb kritishkam komentarui
#ix = grep("SKVERNELIS|KARBAUSKIS|ŠIMONYTĖ|PAPEČKYS|ULINSKAITĖ|NAGEVIČIUS|SIREIKA",dat$vardas)
#keli_vardai = dat$vardas[ix]
#points(dat$pre[ix],dat$votes_norm[ix],pch=19,col=1:7)
#legend(80,5000,legend = keli_vardai,col=1:7,pch=19,bty = "n",cex=0.8)

#ixs = grep("NAGEVIČIUS|PAPEČKYS|ULINSKAITĖ|SIREIKA",dat$vardas) # nuskriaustieji?
#ixs = grep("SKVERNELIS|KARBAUSKIS|ŠIMONYTĖ",dat$vardas) # nepelnytai apdovanotieji?
#ofs = 2
# surinkti balsai (nenormalizuoti)
#par(mfrow=c(2,2))
#for (i in 1:length(ixs)) {
#        tmp = dat[which(dat$pre %in% (dat[ixs[i],"pre"]-ofs):(dat[ixs[i],"pre"]+ofs)),]
#        hist(tmp$n,main=dat$vardas[ixs[i]],xlab="surinkti pirmumo balsai",ylab="kandidatai")
#        abline(v=dat$n[ixs[i]],col="red",lwd=2)
#}
# normalizuoti balsai
#par(mfrow=c(2,2))
#for (i in 1:length(ixs)) {
#        tmp = dat[which(dat$pre %in% (dat[ixs[i],"pre"]-ofs):(dat[ixs[i],"pre"]+ofs)),]
#        hist(tmp$votes_norm,main=dat$vardas[ixs[i]],xlab="normalizuoti pirmumo balsai")
#        abline(v=dat$votes_norm[ixs[i]],col="red",lwd=2)
#}
#par(mfrow=c(1,1))



