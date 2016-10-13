

### sukurti apylinkiu zodyna (kartu su ju apygardom), o taip pat HDF5 archyvavimo indeksa

a = readLines("~/Biostuff/POLITIKA_LT/2016/raw_data/2016_vrk_raw_data_apyl_onlyParty.txt")

## get all voting station names
apyg = grep("Puslapis atnaujintas",a) + 2
apyl = grep("^Puslapis atnaujintas",a)  + 4
map = data.frame(apyl=apyl,apyg=apyg,stringsAsFactors = F)

transl3 = NULL # translator file for apygarda-apylinke
for (i in 1:nrow(map)) {

apygapyl_long = paste(a[map[i,"apyg"]],a[map[i,"apyl"]],sep="___")
apygapyl_hash = digest(apygapyl_long,algo = "xxhash32")

apyg_short = unlist(strsplit(a[map[i,"apyg"]],"[[:punct:]]|\\s"))[1]
apyl_short = unlist(strsplit(a[map[i,"apyl"]],"[[:punct:]]|\\s"))[1]
# two nested exceptions for apylinkes titled in Name Surname of a famous person
if (nchar(apyl_short)==1) apyl_short = unlist(strsplit(a[map[i,"apyl"]],"[[:punct:]]|\\s"))[3]
if (nchar(apyl_short)==0) apyl_short = unlist(strsplit(a[map[i,"apyl"]],"[[:punct:]]|\\s"))[2]

apyg_numbr = as.numeric(unlist(strsplit(a[map[i,"apyg"]]," \\(Nr\\.|\\) "))[2])
apyl_numbr = as.numeric(unlist(strsplit(a[map[i,"apyl"]]," \\(Nr\\.|\\) "))[2])

apygapyl_name = paste(apyg_short,apyl_short,sep="")
apygapyl_numb = paste(apyg_numbr,apyl_numbr,sep="_")

# temporary results
tmp = data.frame(
        apygarda = apyg_short,
        apylinke = apyl_short,
        abbrevi_name = apygapyl_name,
        apyg_nr = apyg_numbr,
        apyl_nr = apyl_numbr,
        abbrevi_numb = apygapyl_numb,
        apygapyl_hashed = apygapyl_hash,
        apygapyl_concat = apygapyl_long,
        stringsAsFactors = F
        )
transl3 = rbind(transl3,tmp)
rm(tmp)

}

## PROBLEM!  some apygNR-apylNR combinations are not unique! soviet hell!
sum(duplicated(transl3$abbrevi_numb))
exmpl = transl3$abbrevi_numb[which(duplicated(transl3$abbrevi_numb))[1:3]]
transl3[which(transl3$abbrevi_numb %in% exmpl),]  # shiaip tai chia skandalas, sakychiau

# doublecheck whether first word of apygarda title uniquelly matched apygarda number
mtr = as.matrix(table(transl3$apygarda,transl3$apyg_nr))
as.numeric(apply(mtr,1,function(x) sum(x !=0)))  # should be only 1's

# fun thing: check most popular:
#df = group_by(transl3,apylinke) %>% summarise(n=n()) %>% ungroup()
#df = df[order(-df$n),]; df[1:15,]; length(unique(transl3$apygapyl_concat))

# check for duplicated apygapyl abbrevi names
sum(duplicated(transl3$abbrevi_name))  
dupl_nms = transl3[which(duplicated(transl3$abbrevi_name)),"abbrevi_name"]
transl3[which(transl3$abbrevi_name %in% dupl_nms[1:3]),]  # there are some, but I prefer to keep it that way

# doublecheck hash duplication
sum(duplicated(transl3$apygapyl_hashed))

# make sure that all are ordered based on apygardos NR and then apylinkes NR
transl3 = transl3[order(transl3$apyg_nr,transl3$apygapyl_hashed),]
transl3$ordr = seq(nrow(transl3))
transl3[1:20,]

##### for HDF5 storage and compression reasons, need to create an index coordinates
tmp = as.data.frame(group_by(transl3,apyg_nr) %>% summarise(n_apyl=n()) %>% ungroup())
max(tmp$n_apyl) # max_number of apylinkes (60)
nrow(tmp) # max_number of apygardos (71)
which(!tmp$apyg_nr %in% 1:71)  # any missing values (curiousity)

z = 1:max(tmp$apyg_nr) # max number of apygardos (71)
tmp$from = 1 + (z-1)*max(tmp$n_apyl)  # first instance index in storage matrix (max number of apylinkes)
tmp$till = 0 + (z )*max(tmp$n_apyl)   # last instance index in storage matrix (max number of apylinkes)
transl3 = merge(transl3,tmp,by="apyg_nr",all=T)
transl3 = as.data.frame(group_by(transl3,apyg_nr) %>% mutate(hash_rank=rank(apygapyl_hashed)) %>% ungroup())

## assign the final position in HDF5 file (assuming 60*71 columns)
transl3$poz = transl3$from + transl3$hash_rank - 1
transl3[1:100,]

write.table(transl3,"~/Dropbox/GIT/POLITIKA_LT/2016_seimo_rinkimai/apylinkiu_zodynas.txt",
            row.names = F,col.names = T,sep="\t",quote=F)
