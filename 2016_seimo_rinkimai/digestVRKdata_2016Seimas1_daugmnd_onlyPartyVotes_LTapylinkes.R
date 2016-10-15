
#  GOAL: digest and format extracted VRK data
#  ONLY party level votes on apylinkes-level
#  NOTE: apskaichiuota ne ish pirmumo balsu, o butent partijoms skirtu balsai daugiamandatese

# 2016 Oct 14, by JonasB


# automatic grabbing from web is not 100% successful, thus I run couple of grabs

### all available raw data files from VRK
director = "~/Biostuff/POLITIKA_LT/2016/raw_data/"
ls_files = list.files(director,pattern = "2016_vrk_raw_data_LTapylinkes_onlyPartyVotes")


#####################   LIETUVOS APYLINKES   ########################

master = NULL  # all results from multiple grabs (many will be duplicated)

for (i in 1:length(ls_files)) {
        print(i)
        # load VRK data
        a = readLines(paste(director,ls_files[i],sep=""))
        
        # freshness (VRK freeze date-time)
        unique(a[grep("Puslapis atnaujintas",a)])  # maybe use it in results later ***
        if(length(unique(a[grep("Puslapis atnaujintas",a)])[1])>1) warning("multiple VRK freezes")
        
        
        ### identify the positions in the data that contain relevant information
        vers = grep("^Puslapis atnaujintas",a)  + 0  # VRK freeze
        apyg = grep("^Puslapis atnaujintas",a)  + 2  # apygarda
        apyl = grep("^Puslapis atnaujintas",a)  + 4  # apylinke
        from = grep("^balsai",a) + 1
        till = grep("^Iš viso",a) - 1
        
        ### doublecheck for inconsistencies (warning flags)
        if( !all(length(from)==length(till),length(apyl)==length(apyg),
                 length(from)==length(apyl),length(vers)==length(apyl))) {
                warning("nesutampa start/end indikatoriu ilgiai")
        }
        
        ### create the document map (index)
        map = data.frame(vers=vers,apyg=apyg,apyl=apyl,from = from,till=till,stringsAsFactors = F)
        #head(map); dim(map)
        n_part = map$till - map$from + 1;  table(n_part)  # are all parties present ? should be 14
        if (length(table(n_part))>1) warning("some parties are missing")
        
        ### extract only relevant data in "a" object
        cum = NULL
        for (j in 1:nrow(map)) { # kiekvienai apylinkei
                
                # extract essential meta
                apygarda = a[map[j,"apyg"]]
                apylinke = a[map[j,"apyl"]]
                vrkfreez = a[map[j,"vers"]]
                
                rows = (map[j,"from"]):(map[j,"till"])
                dat = NULL
                for (k in rows) { # kiekvienam kandidatui ishraukti surinktus pirmumo balsus
                        xtr = unlist(strsplit(a[k],"\t"))
                        tmp = data.frame(partija=xtr[2],
                                         v_apyl = as.integer(xtr[4]),
                                         v_post = as.integer(xtr[5]),
                                         v_totl = as.integer(xtr[6]),
                                         prc_of_valid = as.numeric(gsub(",", ".", xtr[7])),
                                         prc_of_valid_LT = as.numeric(gsub(",", ".", xtr[8])),
                                         stringsAsFactors = F) # gauti pirmumo balsai
                        dat = rbind(dat,tmp)
                        rm(xtr,tmp)
                } # end of cycling through parties
                
                # add voting station data
                dat$apygarda = apygarda
                dat$apylinke = apylinke
                
                # add vrk freeze date
                dat$freeze = paste(unlist(strsplit(vrkfreez," "))[3:4],collapse="_")
                
                cum = rbind(cum,dat)
                rm(dat,apygarda,apylinke,vrkfreez,rows)
                
        } # end of cycling through apylinkes
        
        master = rbind(master,cum)
        rm(a,cum,map,vers,apyg,apyl,from,till)
        
} # end of cycling through grabs


## assign short merging index for apylinke
master$apygapyl = paste(master$apygarda,master$apylinke,sep="___")        
fun = function(x) digest(master$apygapyl[x],algo="xxhash32",serialize = F) # hashed apygarda-apylinke name
master$apygapyl_hash = unlist(lapply(1:nrow(master),fun))

# doublecheck the fullness of the grabbed data (should be 1996)
length(unique(master$apygapyl_hash))

# test time stamp (freeze)
unique(master$freeze)  # should be only one

# doublecheck the consistency between the duplicated grabs
master$tst = paste(master$partija,master$v_apyl,master$v_post,master$v_totl,master$prc_of_valid,master$apygapyl_hash,sep="_")
length(unique(master$tst)) # should be 1996*14 = 27944  !!! ***

# if above tests passed - delete dublicated entries (due to multiple grabs)
master = master[which(!duplicated(master$tst)),]
master = master[,-grep("tst",colnames(master))]

# cleanup
rm(list = ls()[which(ls()!="master")])


############   ADD PARTY META INFO   #################
# load party names translator
transl1 = read.table("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/partiju_zodynas.txt",
                     sep="\t",h=T,stringsAsFactors = F)
transl1 = transl1[order(transl1$ordr),]

dat = merge(master,transl1,by="partija",by.y="full_name",all=T)
dat = dat[,c("abbrv","trumpinys","v_apyl","v_post","v_totl","prc_of_valid",
             "prc_of_valid_LT","apygarda","apylinke","apygapyl_hash","freeze")]
colnames(dat)[grep("abbrv",colnames(dat))] = "acron"
colnames(dat)[grep("freeze",colnames(dat))] = "vrk_freeze"
head(dat)

## add total votes per apylinke
df = group_by(dat,apygapyl_hash) %>% summarise(apyl_total=sum(v_totl)) %>% ungroup()
df = as.data.frame(df)
hist(df$apyl_total); sort(df$apyl_total)[1:10]

dat = merge(dat,df,by="apygapyl_hash",all=T)
dat = dat[order(-dat$prc_of_valid_LT),]
head(dat)


############   ADD VOTING STATION INFO   #################
transl3 = read.table("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/apylinkiu_zodynas.txt",h=T,sep="\t",stringsAsFactors = F)
head(transl3)

# warning falgs
if (!all(transl3$apygapyl_hash %in% dat$apygapyl_hash)) warning("nesutampa hashai konsulatu!")
if (!all(dat$apygapyl_hash %in% transl3$apygapyl_hash)) warning("nesutampa hashai konsulatu!")
dat = merge(dat,transl3,by="apygapyl_hash",all=T)

# cleanup and save
rm(list = ls()[which(ls()!="dat")])
save(list="dat",file="~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/VRK_2016Seim1_daugmand_onlyPartyVotes_apylink.RData")



# for further analysis load:
load("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/VRK_2016Seim1_daugmand_onlyPartyVotes_apylink.RData")
# or
load("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/VRK_2016Seim1_daugmand_onlyPartyVotes_ApylAmbas.RData")


