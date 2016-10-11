
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
transl1 = read.table("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/partiju_zodynas.txt",sep="\t",h=T,stringsAsFactors = F)
for (i in 1:nrow(transl1)) {
        cum$partija[grep(transl1[i,"pavadinimo_fragmentas"],cum$part)] = transl1[i,"trumpinys"]
}
cum = cum[,-2]
head(cum)
table(cum$partija,useNA="a")

#####  assign shorter consulate names
cum$apylinke = NA # to be filles using translator
transl2 = read.table("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/konsulatu_zodynas.txt",sep="\t",h=T,stringsAsFactors = F)
for (i in 1:nrow(transl2)) {
        cum$apylinke[grep(transl2[i,"lokacijos_fragmentas"],cum$apyl)] = transl2[i,"trumpinys"]
}
cum = cum[,-1]
head(cum)
table(cum$apylinke,useNA="a")

### doublecheck
cum[which(cum$vardas=="GABRIELIUS LANDSBERGIS"),]

# force numerics
cum$n = as.numeric(cum$n)
cum$post = as.numeric(cum$post)
cum$pre = as.numeric(cum$pre)

# add political party info to the name (blame Vytautas Juozapaitis x2)
dat = merge(cum,transl1,by.x="partija",by.y="trumpinys",all=T)
dat$vardas_partija = paste(dat$vardas," (",dat$abbrv,")",sep="")
head(dat)

# save for future reference
save(list="dat",file="~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/konsulines_daugiamandates_pirmumo_balsai_20161011-0800.RData")



