# seimo rinkimai 2016
# pirmumo balsai kandidatams (daugiamandates): 
# kiekvienam kandidatui ish kiekvienos ambasados ir apylinkes

# goal: extract full voting data from VRK

#### 1) run Mac Automator to extract data from VRK website
#### 2) run this script to extract and format the web-to-text file data

library(digest)

########################################
## LOAD ALL UNIQUE PARTY NAMES (translator file: partiju zhodynas)
transl1 = read.table("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/partiju_zodynas.txt",sep="\t",h=T,stringsAsFactors = F)
transl1 = transl1[order(transl1$ordr),]
# create archiving index
transl1$from = 1 + (1:14 - 1) * 145  # we will assume that all parties can have (and do have) max 145 candidates
transl1$till = 0 + (1:14    ) * 145  # we will also assume that there are (and there are) 14 parties

########################################
## get all UNIQUE apygard-apylink NAMES 
tmp1 = read.table("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/apylinkiu_zodynas.txt",h=T,stringsAsFactors = F,sep="\t")
tmp1 = tmp1[,c("apyg","apyl","apygapyl_hash","apyg_nr","apyl_nr","apgapl_abbrev")]
tmp2 = read.table("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/konsulatu_zodynas.txt",h=T,stringsAsFactors = F,sep="\t")
tmp2 = tmp2[,c("apyg","apyl","apygapyl_hash","apyg_nr","apyl_nr","apgapl_abbrev")]
transl3 = rbind(tmp1,tmp2)
transl3 = transl3[order(transl3$apyg_nr,transl3$apyl_nr),]
transl3$ordr = seq(nrow(transl3))
head(transl3)

###################################################################################
##  create matrix and metadataframes where results will be accumulated
m_vote = matrix(NA,nr=14*145,nc=nrow(transl3)) # columns=apylinkes, rows = candidates
v_vers = m_vote # saving the version
i_meta = data.frame(partija=rep(NA,145*14),  # per individual  (could be replaced by transl1)
                    vardas=NA,pre=NA,post=NA,stringsAsFactors = F) # vers=NA, 
                    #n_scan1=0,n_scan2=0,n_scan3=0,) # rows = candidates
a_meta = data.frame(transl3,stringsAsFactors=F) # rows= voting stations # stringsAsFactors = F


####################################################################################
############################### AMBASADOS  #########################################

### all available raw data files from VRK
director = "~/Biostuff/POLITIKA_LT/2016/raw_data/"
ls_files = list.files(director,pattern = "2016_vrk_raw_data_LTambasados_ratingVotes")

# exceptions: no vrk data for: (doublecheck!)
#Garbės generalinis konsulatas Toronte (Nr.2302) rinkimų apylinkė
#Punktas Lemonte (JAV) -   Punktas Lemonte (JAV) (Nr.4502) rinkimų apylinkė

##############################################
######  DATA GRABBING, FORMATING AND ARCHIVING 

for (i in 1:length(ls_files)) {
        
        print(paste("running scan ",i,sep="")) # report
        
        # load VRK data
        a = readLines(paste(director,ls_files[i],sep=""))
        
        # unique(a[grep("Puslapis atnaujintas",a)]) # check the freshness of the data

        ### identify the positions in the data that contain relevant information
        from = grep("^balsai$",a) + 1
        till = grep("^Pastaba. Daugiamandatėje apygardoje",a) - 1
        
        vers = grep("^Puslapis atnaujintas",a)  + 0  # VRK freeze
        apyg = grep("^Puslapis atnaujintas",a)  + 2 
        apyl = grep("^Puslapis atnaujintas",a)  + 4
        part = grep("^Puslapis atnaujintas",a)  + 6
        
        ### doublecheck for inconsistencies (warning flags)
        if( !all(length(from)==length(till),length(apyl)==length(till),
                 length(vers)==length(till),
                 length(part)==length(till),length(apyg)==length(till) )) {
                warning("nesutampa start/end indikatoriu ilgiai")
        }
        
        ### create the document map (index)
        map = data.frame(vers=vers,apyg=apyg,apyl=apyl,part=part,from = from,till=till,stringsAsFactors = F)
        #head(map); dim(map)
        n_part = map$till - map$from + 1;  table(n_part)  # ar visos apylinkes? turetu buti 49-iu kartotiniai visi
        #if (!all(as.numeric(table(n_part)) %in% c(49,49*2))) warning("kai kuriu apylinkiu truksta!")
        #if (!all(as.numeric(names(table(n_part))) %in% c(32,38,39,64,96,108,136,137,138,139,140,141))) warning("kai kuriu partiju truksta!")
        if (!all(map$till>map$from)) warning("no data for some parties! till<from will be deleted from map")
        map = map[which(map$till>map$from),]
        
        ### extract only relevant data in THIS a object
        for (j in 1:nrow(map)) { # kiekvienai apylinkei
               
                # report progress
                #print(paste(j," / ",nrow(map)," _ ",i,sep=""))
                
                # extract essential meta
                apygarda = "Diplomatinė" # a[map[j,"apyg"]]
                apylinke = a[map[j,"apyl"]]
                partija = a[map[j,"part"]]
                partija_short = transl1[which(transl1$full_name == partija),"trumpinys"]
                freeze = paste(unlist(strsplit(a[map[j,"vers"]]," "))[3:4],collapse="_") # vrk freeze date
                
                rows = (map[j,"from"]):(map[j,"till"])
                dat = NULL
                for (k in rows) { # kiekvienam kandidatui ishraukti surinktus pirmumo balsus
                        xtr = unlist(strsplit(a[k],"\t"))
                        tmp = data.frame(vardas=xtr[2],post=xtr[1],pre=xtr[3], # numeris sarashe priesh ir po rinkimu
                                         n=xtr[4],stringsAsFactors = F) # gauti pirmumo balsai
                        dat = rbind(dat,tmp)
                        rm(xtr,tmp)
                }
                dat$pre =        as.integer(dat$pre)
                dat$post =      as.integer(dat$post)
                dat$n =         as.integer(dat$n)
                dat$n[which(is.na(dat$n))] = 0 # assign zero's where NA's are found
                
                # flag warning
                if(nrow(dat)>145) warning("rasti daugiau nei 145 kandidatai!")
                
                ## make sure that the format is consistent with hdf5 structure        
                filler_ixs = which(!seq(145) %in% dat$pre)
                filler_df = data.frame(vardas="missing",post=-1,pre=filler_ixs,n=-1,stringsAsFactors = F) # I decided to put zero here ***
                dat = rbind(dat,filler_df)
                dat = dat[order(dat$pre),]
                dat$vers = freeze
                
                #########  idea: check for party members that got lost!
                
                ###  output matrix column indexes
                apygapyl = paste(apygarda,apylinke,sep="___")
                apygapyl_hash = digest(apygapyl,algo = "xxhash32",serialize = F) # hashed apygarda-apylinke name
                col_indx = transl3[which(transl3$apygapyl_hash == apygapyl_hash),"ordr"]
                
                ###  output matrix row indexes
                row_from = transl1[which(transl1$full_name == partija),"from"]
                row_till = transl1[which(transl1$full_name == partija),"till"]
                
                # save results 
                m_vote[row_from:row_till,col_indx] = dat$n
                v_vers[row_from:row_till,col_indx] = dat$vers
                #.. I do not include a safety check to ensure that previous data is not overwritten with bad new one coz a new 
                #.. replacement is always valid: if voting station was not scanned - it will not generate a replacement
                
                # save meta-data (will be constantly overwritten)
                i_meta[row_from:row_till,c("partija","vardas","pre","post")] = 
                        data.frame(partija=partija_short,vardas=dat$vardas,
                                   pre=dat$pre,post=dat$post,stringsAsFactors = F)
               
                # quality check: how many times data was overwriten or added
                #i_meta[row_from:row_till,grep("n_scan",colnames(i_meta))[i]] = dat$n_scan # updated values
                # quality check: how many times data was overwriten or added
                #a_meta[col_indx,grep("n_scan",colnames(a_meta))[i]] = rep(1,length(col_indx))
                
                rm(dat,rows,apylinke,apygarda,partija,apygapyl,apygapyl_hash,
                   col_indx,row_from,row_till,filler_ixs,filler_df,freeze,partija_short)
        }
        rm(a,map,from,till,apyl,apyg,part)
}

######################################################################
###################  QUALITY CHECK FOR DATA GRABBING  ################

### check how many missing values are there remaining
i_meta[1:10,]
a_meta[1:10,]
unique(as.character(v_vers)) # check the assortment of versions. should be one
ww = m_vote[,which(a_meta$apyg=="Diplomatinė")]
sum(is.na(ww)) # must be zero NAs after 1-2 cycles

table(apply(m_vote,1,function(x) sum(is.na(x)))) # should be 2030 with value 1996
table(apply(m_vote,2,function(x) sum(is.na(x)))) # should be 49 with value "0" and 1996 with value "2030"

# exceptions: no vrk data for:
#Garbės generalinis konsulatas Toronte (Nr.2302) rinkimų apylinkė
#Punktas Lemonte (JAV) -   Punktas Lemonte (JAV) (Nr.4502) rinkimų apylinkė






#####################################################################
########################     APYLINKES     ##########################

### all available raw data files from VRK
director = "~/Biostuff/POLITIKA_LT/2016/raw_data/vrk_freeze_20161016_1448/"
ls_files = list.files(director,pattern = "2016_vrk_raw_data_LTapylinkes_ratingVotes")


######################################################################
###################  QUALITY CHECK FOR DATA GRABBING  ################


# prescan  - first see if anything is missing
aap = aa = NULL
for (i in 1:length(ls_files)) {
        print(ls_files[i]) 
        a = readLines(paste(director,ls_files[i],sep=""))
        apyg = grep("^Puslapis atnaujintas",a)  + 2 
        apyl = grep("^Puslapis atnaujintas",a)  + 4
        part = grep("^Puslapis atnaujintas",a)  + 6
        aa = c(aa, paste(a[apyg],a[apyl],sep="___"))
        aap =c(aap, paste(a[apyg],a[apyl],a[part],sep="___"))
        rm(a,apyg,apyl,part)
}
aap = unique(aap); aa = unique(aa)
# assign apyg-apyl hashes 
aa_hsh=NULL; for (i in 1:length(aa)) aa_hsh = c(aa_hsh,digest(aa[i],algo = "xxhash32",serialize = F))

length(aa)  # there are (apyg-apyl)
length(aa_hsh)
length(unique(transl3$apygapyl_hash[which(transl3$apyg!="Diplomatinė")])) # there should be
transl3[which(!transl3$apygapyl_hash %in% aa_hsh),]

table(apply(m_vote,2,function(x) sum(is.na(x))))

# what fraction of candidate-apyl values is covered by these grabs
length(aap) / (length(aa)*14)
# how many apylinke-party combinations are not grabbed ? 
(length(aa)*14) - length(aap)
# what are the missing combinations? 
aap_full = NULL
for(p in transl1$full_name) { 
        for (a in unique(aa)) {
        aap_full = c(aap_full,paste(a,p,sep="___"))
        }
}

### which precisely are missing ? 
aap_full[which(!aap_full %in% aap)]
unique(substr(aap_full[which(!aap_full %in% aap)],1,30))
unique(substr(aap_full[which(!aap_full %in% aap)],1,70))


#0.03^2 * 14 * 1996 / 5 # how many expected mistakes there should be after two runs (3% standard loss in one grab)
#0.03^3 * 14 * 1996 / 5 # how many expected mistakes there should be after three runs



##############################################
######  DATA GRABBING, FORMATING AND ARCHIVING 

# and now the real scan
for (i in 1:length(ls_files)) {
        
        print(ls_files[i]) # print(paste("running scan ",i,sep="")) # report
        
        # load VRK data
        a = readLines(paste(director,ls_files[i],sep=""))
        
        ### identify the positions in the data that contain relevant information
        from = grep("^balsai$",a) + 1
        till = grep("^Pastaba. Daugiamandatėje apygardoje",a) - 1
        
        vers = grep("^Puslapis atnaujintas",a)  + 0  # VRK freeze
        apyg = grep("^Puslapis atnaujintas",a)  + 2 
        apyl = grep("^Puslapis atnaujintas",a)  + 4
        part = grep("^Puslapis atnaujintas",a)  + 6
        
        ### doublecheck for inconsistencies (warning flags)
        if( !all(length(from)==length(till),length(apyl)==length(till),
                 length(vers)==length(till),
                 length(part)==length(till),length(apyg)==length(till) )) {
                warning("nesutampa start/end indikatoriu ilgiai")
        }
        
        ### create the document map (index)
        map = data.frame(vers=vers,apyg=apyg,apyl=apyl,part=part,from = from,till=till,stringsAsFactors = F)
        #head(map); dim(map)
        
        #n_part = map$till - map$from + 1;  table(n_part)  # aptikti kandidatu skaichiai partijose
        if (!all(map$till>map$from)) warning("no data for some parties! till<from will be deleted from map")
        map = map[which(map$till>map$from),]
        
        ### extract only relevant data in THIS a object
        for (j in 1:nrow(map)) { # kiekvienai apylinkei
        
                # extract essential meta
                apygarda = a[map[j,"apyg"]]
                apylinke = a[map[j,"apyl"]]
                partija = a[map[j,"part"]]
                partija_short = transl1[which(transl1$full_name == partija),"trumpinys"]
                freeze = paste(unlist(strsplit(a[map[j,"vers"]]," "))[3:4],collapse="_") # vrk freeze date
                
                rows = (map[j,"from"]):(map[j,"till"])
                dat = NULL
                for (k in rows) { # kiekvienam kandidatui ishraukti surinktus pirmumo balsus
                        xtr = unlist(strsplit(a[k],"\t"))
                        tmp = data.frame(vardas=xtr[2],post=xtr[1],pre=xtr[3], # numeris sarashe priesh ir po rinkimu
                                         n=xtr[4],stringsAsFactors = F) # gauti pirmumo balsai
                        dat = rbind(dat,tmp)
                        rm(xtr,tmp)
                }
                dat$pre =        as.integer(dat$pre)
                dat$post =      as.integer(dat$post)
                dat$n =         as.integer(dat$n)
                dat$n[which(is.na(dat$n))] = 0 # assign zero's where NA's are found
                
                # flag warning
                if(nrow(dat)>145) warning("rasti daugiau nei 145 kandidatai!")
                
                ## make sure that the format is consistent with hdf5 structure        
                filler_ixs = which(!seq(145) %in% dat$pre)
                filler_df = data.frame(vardas="missing",post=-1,pre=filler_ixs,n=-1,stringsAsFactors = F) # I decided to put zero here ***
                dat = rbind(dat,filler_df)
                dat = dat[order(dat$pre),]
                dat$vers = freeze
                
                ###  output matrix column indexes
                apygapyl = paste(apygarda,apylinke,sep="___")
                apygapyl_hash = digest(apygapyl,algo = "xxhash32",serialize = F) # hashed apygarda-apylinke name
                col_indx = transl3[which(transl3$apygapyl_hash == apygapyl_hash),"ordr"]
                if(length(col_indx)==0) warning("apygapyl_hash did not match anything in the translator file!")
                
                ###  output matrix row indexes
                row_from = transl1[which(transl1$full_name == partija),"from"]
                row_till = transl1[which(transl1$full_name == partija),"till"]
                
                # save results 
                m_vote[row_from:row_till,col_indx] = dat$n
                v_vers[row_from:row_till,col_indx] = dat$vers
                #.. I do not include a safety check to ensure that previous data is not overwritten with bad new one coz a new 
                #.. replacement is always valid: if voting station was not scanned - it will not generate a replacement
                
                # save meta-data (will be constantly overwritten)
                i_meta[row_from:row_till,c("partija","vardas","pre","post")] = 
                        data.frame(partija=partija_short,vardas=dat$vardas,
                                   pre=dat$pre,post=dat$post,stringsAsFactors = F)
                # cleanup
                rm(dat,rows,apylinke,apygarda,partija,apygapyl,apygapyl_hash,
                   col_indx,row_from,row_till,filler_ixs,filler_df,freeze,partija_short)
        }
        # cleanup
        rm(a,map,from,till,apyl,apyg,part)
        # report the quality
        print(table(apply(m_vote,2,function(x) sum(is.na(x))))) # kiek apylinkiu neturi duomenu apie kiek kandidatu
        
}

##############################################
##############################  QUALITY CHECK

# check the vrk freeze versions for every voting station
table(unlist(apply(v_vers,2,function(x) unique(x)))) 

# estimate missing values per voting station
table(apply(m_vote,2,function(x) sum(is.na(x)))) # kiek apylinkiu neturi duomenu apie kiek kandidatu

# estimate "-1" values per voting station
table(apply(m_vote,2,function(x) sum(x==(-1)))) # kiek apylinkiu neturi duomenu apie kiek kandidatu
table(apply(m_vote,1,function(x) sum(x==(-1)))) # kiek apylinkiu neturi duomenu apie kiek kandidatu

# preview output
tail(i_meta)
tail(a_meta)

# correct VRK habbit to put "(V)" to candidates name
i_meta$vardas = gsub(" \\(V\\)$","",i_meta$vardas)

good_rows = which(i_meta$vardas!="missing")
i_meta = i_meta[good_rows,]
m_vote = m_vote[good_rows,]
v_vers = v_vers[good_rows,]
rm(good_rows)

# add index, rename index column
i_meta$rix = seq(nrow(i_meta))
colnames(a_meta)[grep("ordr",colnames(a_meta))] = "cix"

direc = "~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/"
save(list = c("m_vote","v_vers","i_meta","a_meta"),
     file=paste(direc,"VRK_2016Seim1_daugmand_PrimeVotes_ApylAmbas.RData",sep=""))

########################################################################

# for analyses use:
direc = "~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/"
load(paste(direc,"VRK_2016Seim1_daugmand_PrimeVotes_ApylAmbas.RData",sep=""))



######  export data if needed
#partija = "Liberalai"
#sub = m_vote[which(i_meta$partija==partija),]
#ind = i_meta[which(i_meta$partija==partija),]
#apl = a_meta$apgapl_abbrev
#nms=NULL
#for(i in 1:nrow(ind)) nms = c(nms, paste(unlist(strsplit(ind$vardas[i]," ")),collapse="_"))
#ind$vardas = nms
#write.table(sub,"~/Downloads/sub.txt",row.names = F,col.names = F,sep="\t",quote=F)
#write.table(ind[,1:4],"~/Downloads/ind.txt",row.names = F,col.names = F,sep="\t",quote=F)
#write.table(apl,"~/Downloads/apl.txt",row.names = F,col.names = F,sep="\t",quote=F)


