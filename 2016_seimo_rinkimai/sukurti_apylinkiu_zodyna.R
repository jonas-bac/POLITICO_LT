

### sukurti apylinkiu zodyna (kartu su ju apygardom), o taip pat HDF5 archyvavimo indeksa


# use data generated in digestVRKdata_2016Seimas1_daugm..R

load("~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/VRK_2016Seim1_daugmand_onlyPartyVotes_apylink.RData")
#should be 1996 apylinkes



#### palyginimui - didzhiausios ir mazhiausios apylinkes
dat = dat[which(!duplicated(dat$apygapyl_hash)),] # remove party data
dat = dat[order(dat$apyg_nr,dat$apyl_nr,dat$apyl_total),]
dat$sq = seq(nrow(dat))
head(dat)

# for unique identification and hashing
dat$apygarda_apylinke = paste(dat$apygarda,dat$apylinke,sep="___")

transl3 = NULL
for (i in 1:nrow(dat)) {
                
                # short names for apygardos ir apylinkes
                apyg_short = unlist(strsplit(dat$apygarda[i],"[[:punct:]]|\\s"))[1]
                frgm = unlist(strsplit(dat$apylinke[i],"[[:punct:]]|\\s"))
                apyl_short = frgm[min(which(nchar(frgm)>=3))]; rm(frgm)
                # two nested exceptions for apylinkes titled in Name Surname of a famous person
                if (nchar(apyl_short)==1) apyl_short = unlist(strsplit(dat$apygarda[i],"[[:punct:]]|\\s"))[3]
                if (nchar(apyl_short)==0) apyl_short = unlist(strsplit(dat$apylinke[i],"[[:punct:]]|\\s"))[2]
                
                # official numbers for apygardos ir apylinkes (not unique!)
                apyg_numbr = as.numeric(unlist(strsplit(dat$apygarda[i]," \\(Nr\\.|\\) "))[2])
                apyl_numbr = as.numeric(unlist(strsplit(dat$apylinke[i]," \\(Nr\\.|\\) "))[2])
                
                abbrev = paste(apyg_short,"(",apyg_numbr,")-",apyl_short,"(",apyl_numbr,")",sep="")
                apygapyl_hash = digest(dat$apygarda_apylinke[i],algo = "xxhash32",serialize = F)

                # temporary results
                tmp = data.frame(
                        apyg = apyg_short, apyl = apyl_short, 
                        apyg_nr=apyg_numbr, apyl_nr=apyl_numbr,
                        apgapl_abbrev=abbrev,apygapyl_hash = apygapyl_hash,stringsAsFactors = F)
                transl3 = rbind(transl3,tmp)
                rm(tmp)
                
}
head(transl3)

# doublecheck:
sum(duplicated(transl3$apygapyl_hashed)) # should be 0

write.table(transl3,"~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/apylinkiu_zodynas.txt",
            row.names = F,col.names = T,sep="\t",quote=F)


....
... continue ....
... add embassies  (or leaveit as separate script)
....

....  below  - not reviewed !
... maybe delete overall..        
        
        

        
        
        
        
        
        
        
# evaluate capture quality        
ss = tr[which( ! tr$apygarda %in% transl3$apygarda),]
unique(ss$apygarda)
        
ss = tr[which( ! tr$apylinke %in% transl3$apylinke),]
unique(ss$apygarda)
  
ss = tr[which( ! tr$apygapyl_hashed %in% transl3$apygapyl_hashed),]
unique(ss$apygarda)
unique(ss$abbrevi_name)
table(ss$apygarda)

head(transl3)
dim(transl3)
sort(unique(transl3$apygarda))
# Lietuvos diplomatines atstovybes (dalins Naujamieschio apygardos)
a2 = readLines("~/Biostuff/POLITIKA_LT/2016/raw_data/2016_vrk_raw_data_LTapylinkes_onlyPartyVotes.txt")






# doublecheck:
length(unique(transl3$apygapyl_hashed))   ###  are there any APYLINKES missing?
length(unique(transl3$apyg_nr))  ### are there any apygardos missing ? 
##### SHOUD BE:
# Apylinkių skaičius – 1996 (duomenys iš 1996 apylinkių) 
# Apygardų skaičius – 71 (duomenys iš 71 apygardų) 

## PROBLEM!  some apygNR-apylNR combinations are not unique! soviet hell!
sum(duplicated(transl3$abbrevi_numb))
exmpl = transl3$abbrevi_numb[which(duplicated(transl3$abbrevi_numb))[1:3]]
transl3[which(transl3$abbrevi_numb %in% exmpl),]  # shiaip tai chia skandalas, sakychiau

# doublecheck whether first word of apygarda title uniquelly matched apygarda number
mtr = as.matrix(table(transl3$apygarda,transl3$apyg_nr))
as.numeric(apply(mtr,1,function(x) sum(x !=0)))  # should be only 1's: 1 1 1 1 1 1 1 ...

# fun thing: check most popular:
#df = group_by(transl3,apylinke) %>% summarise(n=n()) %>% ungroup()
#df = df[order(-df$n),]; df[1:15,]; length(unique(transl3$apygapyl_concat))

# check for duplicated apygapyl abbrevi names
sum(duplicated(transl3$abbrevi_name))  
dupl_nms = transl3[which(duplicated(transl3$abbrevi_name)),"abbrevi_name"]
transl3[which(transl3$abbrevi_name %in% dupl_nms[1:3]),]  # there are some, but I prefer to keep it that way

# doublecheck hash duplication (by design)
sum(duplicated(transl3$apygapyl_hashed))

# make sure that all are ordered based on apygardos NR and then apylinkes hash (APYLINKES NR is not unique in apygarda!!!)
transl3 = transl3[order(transl3$apyg_nr,transl3$apygapyl_hashed),]
transl3$ordr = seq(nrow(transl3))
transl3[1:20,]

##### for HDF5 storage and compression reasons, need to create an index coordinates
tmp = as.data.frame(group_by(transl3,apyg_nr) %>% summarise(n_apyl=n()) %>% ungroup())
max(tmp$n_apyl) # max_number of apylinkes (60)
nrow(tmp) # max_number of apygardos (71)
which(! 1:71 %in% tmp$apyg_nr)  #  IMPORTANT TO CHECK THIS !!!  should be all 71


z = 1:max(tmp$apyg_nr) # max number of apygardos (71)
tmp$from = 1 + (z-1) * max(tmp$n_apyl)  # first instance index in storage matrix (max number of apylinkes)
tmp$till = 0 + (z  ) * max(tmp$n_apyl)   # last instance index in storage matrix (max number of apylinkes)
transl3 = merge(transl3,tmp,by="apyg_nr",all=T)
transl3 = as.data.frame(group_by(transl3,apyg_nr) %>% mutate(hash_rank=rank(apygapyl_hashed)) %>% ungroup())

## assign the final position in HDF5 file (assuming 60*71 columns)
transl3$poz = transl3$from + transl3$hash_rank - 1
transl3[1:100,]

write.table(transl3,
            "~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/apylinkiu_zodynas.txt",
            row.names = F,col.names = T,sep="\t",quote=F)
