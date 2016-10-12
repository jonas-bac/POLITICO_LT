

# seimo rinkimai 2016
# visuminiai pirmumo balsai (daugiamandates)
# no split based on apygarda ar apylinke

# idea: identify candidates that got large and honest support from electors, i.e.,
# adjusting for the candidate's rank number in pre-election list,
# party size (number of candidates)
# total number of votes assigned to the party candidates)


#### 1) run Mac Automator to extract data from VRK website
#### 2) run this script to format the web-to-text file data
#### and generate support-outliers

# load VRK data
a = readLines("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/raw_pirmumo_balsai_totals_20161012-0700.txt")

# check the freshness of the data
unique(a[grep("Puslapis atnaujintas",a)])


# check the web-scraping quality
# there should be 14 parties 
pa_ix = grep("Puslapis atnaujintas",a) + 2 # partija
table(a[pa_ix])  # 14 partiju duomenys 

### identify the positions in the data that contain relevant information
from = grep("^balsai",a) + 1  # pirmoji balsu eilute
till = grep("^Pastaba. Daugiamandatėje",a) - 1 # paskutine balsu eilute
part = grep("^Puslapis atnaujintas",a)  + 2 # partijos pavadinimas

### doublecheck for inconsistencies (warning flags)
if( !all(length(from)==length(till),length(from)==length(part))) {
        warning("nesutampa start/end indikatoriu ilgiai")
}

### create the document map (index)
map = data.frame(part=part,from = from,till=till,stringsAsFactors = F)
head(map)

### extract only relevant data in a
cum = NULL # cumulator of results
for (i in 1:nrow(map)) {
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
head(cum)

#####  assign shorter political party names
cum$partija = cum$abbrev = NA # to be filles using translator
transl1 = read.table("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/partiju_zodynas.txt",sep="\t",h=T,stringsAsFactors = F)
for (i in 1:nrow(transl1)) {
        cum$partija[grep(transl1[i,"pavadinimo_fragmentas"],cum$part)] = transl1[i,"trumpinys"]
        cum$abbrev[grep(transl1[i,"pavadinimo_fragmentas"],cum$part)] = transl1[i,"abbrv"]
}
cum = cum[,-1]
cum$vardas_partija = paste(cum$vardas," (",cum$abbrev,")",sep="")
cum$post = as.numeric(cum$post)
cum$pre = as.numeric(cum$pre)
cum$n = as.numeric(cum$n)

# assign to each candidate the sum of prime votes assigned to that party
hlp = as.data.frame(group_by(cum,partija) %>% summarise(n_cand=n(),s=sum(n)) %>% ungroup()) # party sum votes
hlp = hlp[order(-hlp$s),]

## each party gets the color
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
barplot(1:length(cols),col=cols)
hlp$col = cols

# add party-vote-sum and color data
dat = merge(cum,hlp,by="partija",all=T)

## normalize votes based on the size of the party and party total votes
dat$votes_norm = dat$n / dat$s * dat$n_cand  # normalize for total votes and number of candidates
dat$votes_norm = dat$votes_norm * 1e5 / 140 # restore to standard 1e5 votes per party and 140 condidates

### only include major parties (some are small and incomparible without further normalisation)
tbl = table(dat$partija)
df = data.frame(partija=names(tbl),n_kand=as.numeric(tbl),stringsAsFactors = F)
df = df[order(-df$n_kand),]
df = df[which(df$n_kand>130),]
dat = dat[which(dat$partija %in% df$partija),]


##########################

ys_med = ys_upp = NULL
xs = 1:max(dat$pre)
for (x in xs) {
        # define offsets
        if (x %in% 1:5) ofs = 0
        if (x %in% 6:10) ofs = 1
        if (x %in% 11:20) ofs = 2
        if (x %in% 21:40) ofs = 3
        if (x %in% 41:60) ofs = 4
        if (x %in% 61:135) ofs = 5
        if (x %in% 136:141) ofs = 0
        sub = dat[which(dat$pre %in% (x-ofs):(x+ofs)),]; rm(ofs)
        y_med = median(sub$votes_norm)
        y_upp = median(sub$votes_norm) + 2*sd(sub$votes_norm)
        #hist(sub$votes_norm); abline(v=y,col="red")
        #y = quantile(sub$votes_norm,probs = 0.90) # not good, since some parties have less candidates
        # THINK ABOUT EXCLUDING THE PARTY IN QUESTION WHEN ESTIMATING WHAT IS EXPECTED VOTE
        ys_med = c(ys_med,y_med)
        ys_upp = c(ys_upp,y_upp)
}
plot(dat$votes_norm ~ dat$pre,ylab="normalizuotas pirmumo balsų skaičius",xlab="Priešrinkiminis NR sąraše") 
#points(xs,ys,col="red",type="l",lwd=2)
ys_med_smth = predict(loess(ys_med ~ xs,span = 0.2),xs)
ys_upp_smth = predict(loess(ys_upp ~ xs,span = 0.2),xs)
points(xs,ys_med_smth,type="l",col="cornflowerblue",lwd=2)
points(xs,ys_upp_smth,type="l",col="orange",lwd=2)
legend(60,10000,legend = c("extremalumo riba","balsų mediana"),
       col=c("orange","cornflowerblue"),lwd=2,bty = "n")

thr = data.frame(xs,ys_upp_smth,stringsAsFactors = F)
mrg = merge(dat,thr,by.x="pre",by.y="xs",all=T)
mrg = mrg[which( mrg$votes_norm >mrg$ys_upp_smth),]
mrg = mrg[order(mrg$partija,-mrg$ys_upp_smth),]
table(mrg$partija)

tbl1 = table(dat$partija)
df1 = data.frame(partija=names(tbl1),n_kand=as.numeric(tbl1),stringsAsFactors = F)
df1 = df1[order(-df1$n_kand),]

tbl2 = table(mrg$partija)
df2 = data.frame(partija=names(tbl2),extrem=as.numeric(tbl2),stringsAsFactors = F)
df2 = df2[order(-df2$extrem),]
df2
df  = merge(df1,df2,by="partija",all=T)
df = df[order(-df$extrem),]
df




dat = dat[order(-dat$s),]
plot(dat$post ~ dat$pre,col="grey",cex=0.7,main="išskirtinai populiarūs kandidatai",
     xlab="Priešrinkiminis NR sąraše",ylab="Porinkiminis NR sąraše")
abline(v=seq(10,130,10),col="grey",lty=2,lwd=0.5)
abline(h=seq(10,130,10),col="grey",lty=2,lwd=0.5)
abline(0,1,col="darkgrey")
sub = dat[which(dat$vardas %in% mrg$vardas),]
points(sub$post ~ sub$pre,cex=1.2)
points(sub$post ~ sub$pre,pch=19,col = sub$col,cex=1)
tmp = sub[which(!duplicated(sub$partija)),]
tmp = tmp[order(-tmp$s),]
legend(0,135,legend = tmp$partija,col = tmp$col,lwd=8,bty = "n",cex=0.8)
oth = hlp$partija[which(!hlp$partija %in% tmp$partija)]
legend(0,105,legend = oth,col = rep("white",length(oth)),lwd=8,bty = "n",cex=0.8)


cix = c("partija","vardas_partija","post","pre","n","n_cand","s")
out = sub[,cix]
colnames(out) =  c("partija","vardas_partija","NRpo","NRprieš","pirmumoBalsų","partijojKandidatų","partijosPirmumoBalsai")
write.table(out,"~/Downloads/deleteme.nonsense.txt",row.names = F,col.names = T,sep="\t",quote=F)

