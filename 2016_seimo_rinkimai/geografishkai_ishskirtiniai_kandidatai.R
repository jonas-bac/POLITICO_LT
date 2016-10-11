

### GEOGRAFISHKAI ISHSKIRTINIAI KANDIDATAI
### gave neproporcingai daugiau balsu ish tam tikros atstovybes nei tiketina
### (atsizhvelgus i tai, kuriai partijai jie priklauso)

### note: analizuojami tik Lietuvos diplomatiniu atstovybiu pateikti pirmumo balsai
### visi reitingai nustatomi TIK PAGAL PIRMUMO BALSUS
### (pirmumo balsus pateikiantys rinkejai yra labiau motyvuoti ir samoningi)

library(dplyr)
library(corrplot)

load("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/konsulines_daugiamandates_pirmumo_balsai_20161011-0800.RData")
head(dat)

# foninis apylinkiu (ambasadu) aktyvumas atsizhvelgiant i partija
fon = as.data.frame(group_by(dat,apylinke,partija) %>% summarise(s=sum(n)) %>% ungroup())
fon = fon[order(fon$s,decreasing = T),]
nrow(fon) # 49*14 = 686


# kandidatai kurie gavo nors kiek rimchiau balsu (pagreitina skaichiavima)
tt = group_by(dat,vardas,vardas_partija) %>% summarise(s = sum(n,na.rm=T)) %>% filter(s>10) %>% ungroup()
kands = unique(tt$vardas_partija)

# alternative: kandidatai kurie pateko i Seima ir tuom yra idomus
#...
#kands = ...

### initial scan for interesting candidates
selection = NULL
for (kand in kands) {
        # kandidato gauti pirmumo balsai
        sub1 = dat[which(dat$vardas_partija==kand),] # note: butina imti visas dipl.atstovybes
        
        # kandidato partijos gautu pirmumo balsu foninis dazhnis
        sub2 = fon[which(fon$partija == sub1$partija[1]),]
        sub2 = sub2[which(sub2$s>0),] # note! nebutina visu imti, tik ten kur >0
        
        # sujungiame kandidato balsus ir jo partijos balsus
        mrg = merge(sub1,sub2,by="apylinke",all=F) # F: turetu nebelikti NA
        mrg  = mrg[,c("apylinke","n","s")]
        
        # warning flag
        if( (any(is.na(mrg$s)))|(any(is.na(mrg$n))) ) warning("found missing values! (row 43)")
        
        # tikrinam kiekviena kandidata
        ps = rs = ns = NULL
        for (i in 1:nrow(mrg)) {
                p = binom.test(mrg$n[i],sum(mrg$n),p=mrg$s[i]/sum(mrg$s),alternative = "greater")$p.value
                r = (mrg$n[i] / sum(mrg$n)) / (mrg$s[i] / sum(mrg$s)) #  "risk ratio"
                ps = c(ps,p); rm(p)
                rs = c(rs,r); rm(r) # risk ratio
                ns = c(ns,mrg$n[i])
        }
        tmp = data.frame(kand,part=sub1$partija[1],ps,rs,ns,stringsAsFactors = F)  ## needs review, Vytautas Juozapaitis has a duplication
        tmp = tmp[which(tmp$ns>3),]  ###  ARBITRARY! 3-more-than expected (0) is not a surprising event
        tmp1 = tmp[which(tmp$ps==min(tmp$ps))[1],]
        tmp2 = tmp[which(tmp$rs==max(tmp$rs))[1],]
        tmp3 = rbind(tmp1,tmp2)
        selection = rbind(selection,tmp3)
        rm(tmp,tmp1,tmp2,tmp3,sub1,sub2,mrg,ps,rs)
}
head(selection)

#hist(selection$ns,breaks=100,col="grey")
#hist(selection$fs)
#hist(selection$ps)
selection = selection[which(selection$ns>3),]
selection = selection[which((selection$ps<0.01)|(selection$rs>10)),]
selection = selection[which(!duplicated(selection$kand)),]


# most extreme candidates
kand_heroes = selection$kand # [which(selection$ps<0.001)]
#kand_heroes = kand_heroes[which(kand_heroes != "VYTAUTAS JUOZAPAITIS")]  # two persons with the same name!
length(kand_heroes)

# apylinkes
df = as.data.frame(group_by(dat,apylinke) %>% summarise(s=sum(n)) %>% ungroup())
df = df[which(df$s>=25),]
df = df[order(df$s,decreasing = T),]
apylinkes = df$apylinke[order(df$s,decreasing = T)]

# visualize
n_kand = length(unique(kand_heroes))
n_apyl = length(unique(df$apylinke))
p = matrix(NA,nr=n_kand,nc=n_apyl); row.names(p) = kand_heroes; colnames(p) = apylinkes
r = n = f = p
for (i in 1:n_kand) {
        
        # kandidato partija:
        jo_partija = dat$partija[which(dat$vardas_partija==kand_heroes[i])[1]]
        
        # shios partijos foninis dazhnis (balsu gausa ish dipl.atstovybiu)
        pfon = fon[which(fon$partija==jo_partija),] # atstovybiu foninis dazhnis shiai partijai
        pfon = pfon[which(pfon$s>0),] 
        
        # kandidato duomenys
        knd = dat[which(dat$vardas_partija==kand_heroes[i]),c("partija","vardas","apylinke","n","vardas_partija")]
        
        # sujungiam kandidata su tos partijos fonu
        mrg = merge(knd,pfon,by="apylinke",all=F) # F!
        
        # warning flag
        if( (any(is.na(mrg$s)))|(any(is.na(mrg$n))) ) warning("found missing values! (row 99)")
        
        # tikrinam kiekviena apylinke
        for (j in 1:n_apyl) {
                tmp = mrg[which(mrg$apylinke==apylinkes[j]),]
                if (nrow(tmp)>0) {
                p[i,j] = binom.test(tmp$n,sum(mrg$n),p=tmp$s/sum(mrg$s),alternative = "greater")$p.value
                r[i,j] = (tmp$n / sum(mrg$n)) / (tmp$s / sum(mrg$s)) #  "risk ratio"
                f[i,j] = tmp$n / sum(mrg$n)  # fraction of votes comming from this embassy
                n[i,j] = tmp$n # observed votes
                } else {
                        p[i,j] = 1
                        r[i,j] = 1
                        f[i,j] = 0
                        n[i,j] = 0
                }
                rm(tmp)
        }
        rm(jo_partija,pfon,knd,mrg)
}
log_n = log(n,10); log_n[which(log_n==(-Inf))] = 0
log_p = -log(p,10); log_p[which(log_p==(Inf))] = 0
#corrplot(p,is.corr = F,method = "square",tl.col = "royalblue4",tl.cex=0.7)
#corrplot(log_p,is.corr = F,method = "square",tl.col = "royalblue4",tl.cex=0.7)
log_p_trim = log_p; log_p_trim[which(log_p_trim>5)] = 5
corrplot(log_p_trim,is.corr = F,method = "square",tl.col = "royalblue4",tl.cex=0.7)
#corrplot(r,is.corr = F,method = "square",tl.col = "royalblue4",tl.cex=0.7)
corrplot(f,is.corr = F,method = "square",tl.col = "royalblue4",tl.cex=0.7)
#corrplot(n,is.corr = F,method = "square",tl.col = "royalblue4",tl.cex=0.7)
corrplot(log_n,is.corr = F,method = "square",tl.col = "royalblue4",tl.cex=0.7)
#corrplot(r,is.corr = F,method = "square",tl.col = "royalblue4",tl.cex=0.7,p.mat = 1/n,sig.level =1/2 ,insig="blank")

# Relative Risk:  excluding (dimming) extremes
rr = log(r,2.3)
#rr = r; rr[which(r>5)] = 5; 
rr[which(r<1.1)] = NA; rr[which(n<4)] = 1; rr[which(p>0.0005)] = 1
rr = rr[which(apply(rr,1,max)>1),]
rr = rr[,which(apply(rr,2,max)>1)]
prt=NULL; for(i in 1:nrow(rr)) prt=c(prt,unlist(strsplit(row.names(rr)[i],"[[:punct:]]"))[2])
rr = rr[order(prt,decreasing = T),]
corrplot(rr,is.corr = F,method = "square",tl.col = "royalblue4",tl.cex=0.7,cl.length = 10,cl.align.text="l")
dim(rr)

# NUMBER OF VOTES:  excluding (dimming) extremes
nn = log(n,2.3)
#nn[which(r>5)] = 5
nn[which(log(r,2.3)<1.1)] = 1; nn[which(n<4)] = 1; nn[which(p>0.0005)] = 1
rr = log(r,2.3); rr[which(r<1.1)] = 1; rr[which(n<4)] = 1; rr[which(p>0.0005)] = 1
nn = nn[which(apply(rr,1,max)>1),]
nn = nn[,which(apply(rr,2,max)>1)]
prt=NULL; for(i in 1:nrow(nn)) prt=c(prt,unlist(strsplit(row.names(nn)[i],"[[:punct:]]"))[2])
nn = nn[order(prt,decreasing = T),]
corrplot(nn,is.corr = F,method = "square",tl.col = "royalblue4",tl.cex=0.7,cl.length = 10,cl.align.text="l") #,p.mat = p,sig.level =0.001 ,insig="blank")
dim(nn)
