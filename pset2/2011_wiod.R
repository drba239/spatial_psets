wiod <- read_excel("C:/Users/tyler/Downloads/WIOTS_in_EXCEL_v2/WIOT11_ROW_Sep12.xlsx",
                  range="B5:BKG1449")

fn <- function(a) {
  return(ifelse(is.na(a),0,a))
}

sectors <- list( # from readme file in replication package
  "c3",
  c("c4","c5"),
  c("c6","c7"),
  "c8",
  "c9",
  "c10",
  "c11",
  "c12",
  "c13",
  "c14",
  "c15",
  "c16",
  "c18",
  c("c19","c20","c21"),
  c("c23","c24","c25","c26"),
  "c27",
  "c28",
  c("c29","c30"),
  "c32",
  "c33",
  "c22",
  "c34"
)

countries <- unique(wiod$...2)
countries <- countries[2:(length(countries)-1)] # drop NA

N <- length(countries)
J <- length(sectors)

M <- matrix(0,N*J,N)

for (i in 1:J) { # sector
  print(i)
  sector <- sectors[[i]]
  wiod_sector <- wiod[wiod$...3 %in% sector&!is.na(wiod$...3),]
  for (j in 1:N) { # source country (row in WIOD, column in output)
    wiod_sector_c1 <- wiod_sector[wiod_sector$...2==countries[j],]
    for (k in 1:N) { # destination country (column in WIOD, row in output)
      wiod_sector_c2 <- wiod_sector_c1[,substr(colnames(wiod_sector_c1),
                                               1,3)==countries[k]]
      M[N*(i-1)+k,j] <- sum(sum(fn(as.numeric(as.matrix(wiod_sector_c2)))))
    }
  }
}

M <- as.data.frame(M)
colnames(M)[1:41] <- countries 
M$destination <- rep(countries,22)
M %<>% mutate(sector = ((row_number()-1) %/% N) + 1)
M$RoW <- M$RoW + M$LUX + M$MLT + M$LVA
M <- M[,!(colnames(M) %in% c("LUX","MLT","LVA"))]
M %<>% mutate(destination = ifelse(destination %in% c("LUX","MLT","LVA"),
                                   "RoW",destination))
M %<>% group_by(destination,sector) %>% summarise(across(AUS:RoW, sum))

M_US <- M %>% filter(destination=="USA") %>% select(sector,USA)
M_US_dest <- M %>% filter(destination=="USA") %>%
  pivot_longer(cols = AUS:RoW,names_to="origin",values_to="total")
M_US_origin <- M %>% select(destination,sector,USA) %>% 
  mutate(origin = "USA") %>% rename(total=USA)

rm(wiod)
gc()
  

cfs <- read.csv("C:/Users/tyler/Downloads/CFS 2017 PUF CSV/CFS 2017 PUF CSV.csv")

sectors_cfs <- list(
  c(311,312),
  c(313,314,315,316),
  c(321,322,323),
  324,
  325,
  326,
  327,
  c(331,332),
  333,
  c(334,335),
  336,
  c(337,338,339),
  c(481,482,483,484,
    485,486,487,488),
  c(511,512,513,514,
    515,516,517,518),
  c(521,522,523,524,
    525),
  c(531,532,533),
  61,
  c(621,622,623,624),
  c(721,722),
  c(493,541,55,561,
    562,711,712,713,
    811,812,813,814),
  c(42,43,44,45),
  23
)

cfs %<>% filter(ORIG_STATE!=0,ORIG_STATE!=11)
cfs %<>% filter(DEST_STATE!=0,DEST_STATE!=11)

N <- 50
J <- length(sectors_cfs)



cfs %<>% mutate(sector = NA)
cfs %<>% mutate(NAICS = as.character(NAICS))

for (i in 1:J) {
  for (s in sectors_cfs[[i]]) {
    s_str <- as.character(s)
    cfs %<>% mutate(sector =
                      ifelse(substr(NAICS,1,nchar(s_str))==s_str,
                             i,sector))
  }
}
cfs %<>% filter(!is.na(sector))

cfs_agg <- cfs %>% group_by(ORIG_STATE,DEST_STATE,sector,.drop=FALSE) %>%
  summarise(total_shipmt = sum(SHIPMT_VALUE),.groups = "drop") %>%
  complete(ORIG_STATE,DEST_STATE,sector,fill = list(total_shipmt=0)) %>%
  group_by(sector) %>%
  mutate(total_sector = sum(total_shipmt)) %>%
  as.data.frame %>%
  mutate(share = total_shipmt/total_sector) %>%
  select(ORIG_STATE,DEST_STATE,sector,share) %>%
  inner_join(M_US,by="sector") %>%
  mutate(total = share*USA) %>%
  select(-destination) %>%
  rename(origin=ORIG_STATE,destination=DEST_STATE) %>%
  select(origin,destination,sector,total)

places <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
            "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
            "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
            "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
            "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
            "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming",
            "Australia","Austria","Belgium","Bulgaria","Brazil","Canada","China","Cyprus","Czech Republic",
            "Denmark","Estonia","Finland","France","Germany","Greece","Hungary","India","Indonesia","Italy",
            "Ireland","Japan","Lithuania","Mexico","Netherlands","Poland","Portugal","Romania","Russia","Spain",
            "Slovakia","Slovenia","South Korea","Sweden","Taiwan","Turkey","UK","RoW")
states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
            "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
            "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
            "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
            "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
            "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

xbilat <- read.delim("C:/Users/tyler/Downloads/Base_Year/Base_Year/xbilat.txt",
                     header = F)
colnames(xbilat) <- places
xbilat %<>% mutate(destination = rep(places,22))
xbilat %<>% mutate(sector = ((row_number()-1) %/% 87) + 1)

xbilat_long <- xbilat %>% pivot_longer(Alabama:RoW,names_to="origin",
                                       values_to="total")

M_long <- M %>% pivot_longer(AUS:RoW,names_to="origin",
                             values_to="total")

countries2 <- c("Australia","Austria","Belgium","Bulgaria","Brazil","Canada","China","Cyprus","Czech Republic","Germany",
               "Denmark","Spain","Estonia","Finland","France","UK","Greece","Hungary","Indonesia","India","Ireland","Italy",
               "Japan","South Korea","Lithuania","Mexico","Netherlands","Poland","Portugal","Romania","Russia",
               "Slovakia","Slovenia","Sweden","Turkey","Taiwan","USA","RoW")

country_xwalk <- data.frame(names = countries2,abbrev = colnames(M)[3:40])

origin_shares <- xbilat_long %>% 
  filter(origin %in% states,!(destination %in% states)) %>%
  group_by(sector,origin) %>%
  mutate(total_so = sum(total)) %>%
  ungroup %>%
  group_by(sector) %>%
  mutate(total_s = sum(total_so)) %>%
  ungroup %>%
  mutate(share = total_so / total_s) %>%
  select(destination,sector,origin,share)

destination_shares <- xbilat_long %>% 
  filter(destination %in% states,!(origin %in% states)) %>%
  group_by(sector,destination) %>%
  mutate(total_so = sum(total)) %>%
  ungroup %>%
  group_by(sector) %>%
  mutate(total_s = sum(total_so)) %>%
  ungroup %>%
  mutate(share = total_so / total_s) %>%
  select(destination,sector,origin,share)

origin_totals <- M_long %>% filter(origin=="USA") %>%
  ungroup %>% select(destination,sector,total) %>%
  inner_join(country_xwalk,by = join_by(destination==abbrev)) %>%
  select(-destination) %>% rename(destination=names)

destination_totals <- M_long %>% filter(destination=="USA") %>%
  ungroup %>% select(origin,sector,total) %>%
  inner_join(country_xwalk,by = join_by(origin==abbrev)) %>%
  select(-origin) %>% rename(origin=names)

origin_shares %<>% inner_join(origin_totals,by=c("destination","sector")) %>%
  mutate(total = share*total)
origin_shares %<>% select(-share)

destination_shares %<>% inner_join(destination_totals,by=c("origin","sector")) %>%
  mutate(total = share*total)
destination_shares %<>% select(-share)

usa_sectors <- unique(cfs_agg$sector)

sector_shares <- xbilat_long %>% 
  filter(destination %in% states,(origin %in% states)) %>%
  filter(!(sector %in% usa_sectors)) %>%
  group_by(sector) %>%
  mutate(total_s = sum(total)) %>%
  ungroup %>%
  mutate(share = total / total_s) %>%
  select(origin,destination,sector,share)

sector_totals <- M_long %>% ungroup %>%
  filter(origin=="USA",destination=="USA") %>%
  select(sector,total)

sector_shares %<>% inner_join(sector_totals,by="sector") %>%
  mutate(total = share * total) %>% select(-share)

M_long %<>% ungroup %>% inner_join(country_xwalk,by = join_by(destination==abbrev)) %>%
  select(-destination) %>% rename(destination=names) %>%
  inner_join(country_xwalk,by = join_by(origin==abbrev)) %>%
  select(-origin) %>% rename(origin=names)

statefips <- unique(cfs_agg$origin)
state_xwalk <- data.frame(fips = statefips, name=states)

cfs_agg %<>% inner_join(state_xwalk,by=join_by(destination==fips)) %>%
  select(-destination) %>% rename(destination=name) %>%
  inner_join(state_xwalk,by=join_by(origin==fips)) %>%
  select(-origin) %>% rename(origin=name)

xbilat_new_long <- M_long %>% filter(origin!="USA",destination!="USA") %>%
  rbind(cfs_agg) %>% rbind(destination_shares) %>%
  rbind(origin_shares) %>% rbind(sector_shares)

placenum <- 1:87
places <- data.frame(place=placenum,destination=places)

xbilat_new_long %<>% inner_join(places,by="destination") %>%
  rename(place_d = place) %>%
  inner_join(places,by=join_by(origin==destination)) %>%
  rename(place_o = place)

xbilat_new_long %<>% arrange(sector,place_d,place_o)

xbilat_new <- xbilat_new_long %>% select(-place_d,-place_o) %>%
  mutate(total = ifelse(is.na(total),0,total)) %>%
  mutate(total = ifelse(total<0,0,total)) %>%
  pivot_wider(id_cols=c(destination,sector),names_from=origin,
              values_from=total)
  

output <- xbilat_new %>% select(-destination,-sector)

write.table(output,file="C:/Users/tyler/Downloads/xbilat_new.txt",
            sep="\t",col.names=FALSE,row.names=FALSE)

GO_new <- xbilat_new %>% group_by(sector) %>% summarise(across(Alabama:RoW,sum))
GO_new %<>% select(-sector) %>% as.matrix %>% t %>% as.data.frame

write.table(GO_new,file="C:/Users/tyler/Downloads/GO_new.txt",
            sep="\t",col.names=FALSE,row.names=FALSE)

wiod <- read_excel("C:/Users/tyler/Downloads/WIOTS_in_EXCEL_v2/WIOT11_ROW_Sep12.xlsx",
                   range="B5:BKG1449")

countries <- unique(wiod$...2)
countries <- countries[2:(length(countries)-1)] # drop NA

N <- length(countries)
J <- 22
M <- matrix(0,N*J,J)

sectors <- list( # from readme file in replication package
  "c3",
  c("c4","c5"),
  c("c6","c7"),
  "c8",
  "c9",
  "c10",
  "c11",
  "c12",
  "c13",
  "c14",
  "c15",
  "c16",
  "c18",
  c("c19","c20","c21"),
  c("c23","c24","c25","c26"),
  "c27",
  "c28",
  c("c29","c30"),
  "c32",
  "c33",
  "c22",
  "c34"
)



for (i in 1:J) { # sector
  print(i)
  sector <- sectors[[i]]
  wiod_sector <- wiod[1,] %>% 
    rbind(wiod[(wiod$...3 %in% sector&!is.na(wiod$...3)),])
  for (j in 1:N) { # source country (row in WIOD, column in output)
    wiod_sector_c1 <- wiod_sector[,substr(colnames(wiod_sector),
                                          1,3)==countries[j]]
    for (k in 1:J) {
      sector <- sectors[[k]]
      wiod_sector2 <- wiod_sector_c1[,as.vector(
        wiod_sector_c1[1,] %in% sector&!is.na(wiod_sector_c1[1,]))] %>%
        filter(row_number()!=1)
      M[(j-1)*J+i,k] <- sum(sum(fn(as.numeric(as.matrix(wiod_sector2)))))
    }
  }
  
}

countries3 <- c("Australia","Austria","Belgium","Bulgaria","Brazil","Canada","China","Cyprus","Czech Republic","Germany",
                "Denmark","Spain","Estonia","Finland","France","UK","Greece","Hungary","Indonesia","India","Ireland","Italy",
                "Japan","South Korea","Lithuania","Luxembourg","Latvia","Mexico","Malta","Netherlands","Poland","Portugal","Romania","Russia",
                "Slovakia","Slovenia","Sweden","Turkey","Taiwan","USA","RoW")

countries4 <- sort(rep(countries3,22))

M %<>% as.data.frame %>% mutate(country = countries4)
M %<>% mutate(country = ifelse(country %in% c("Luxembourg","Latvia","Malta"),
                                      "RoW",country)) %>%
  mutate(sector = row_number() %% 22) %>%
  mutate(sector = ifelse(sector==0,22,sector)) %>%
  group_by(country,sector) %>% summarise(across(V1:V22,sum)) %>% ungroup %>%
  left_join(places,by=join_by(country==destination)) %>%
  mutate(place = ifelse(country=="USA",1,place)) %>%
  arrange(place,sector) %>%
  select(-country,-sector,-place) %>% as.matrix



IO_table <- matrix(0,(N-3)*J,J)
for (i in 1:((N-3)*J)) {
  ctry <- ((i-1) %/% 22)
  for (j in 1:J) {
    IO_table[i,j] <- M[i,j] / sum(M[(ctry*J+1):((ctry+1)*J),j])
  }
}

IO_table_new <- ifelse(is.na(IO_table),1/22,IO_table)
write.table(IO_table_new,file="C:/Users/tyler/Downloads/IO_table_new.txt",
            sep="\t",col.names=FALSE,row.names=FALSE)


