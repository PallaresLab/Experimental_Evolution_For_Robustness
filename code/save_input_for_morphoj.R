
library(stringr)
library(readr)
library(geomorph)
library(gplots)
library(ggplot2)
library(borealis)
#ca <- commandArgs()
ca_wings = read_csv("~/Downloads/Phen1108_B1_R7.csv")

ca_wings$condition = "hgSg_fs"
#ca_wings$condition = "hgSg_ns"
#ca_wings$condition = "ctrl_fs"
#ca_wings$condition = "ctrl_ns"

#complete = rbind(phen1108_B2_R12,phen1808_B1_R1,phen2308_B1_R4)
#complete$symetry = ifelse(str_detect(complete$id, "R"), print("R"), print("L"))
#complete$id = paste(complete$id, complete$condition, complete$symetry, sep = "_")

complete = ca_wings
complete$symetry = ifelse(str_detect(complete$id, "R"), print("R"), print("L"))
complete$id = paste(complete$id, complete$condition, complete$symetry, sep = "_")


#now I just need to make format ok for MorphoJ
table(nchar(complete$id))
complete = complete[!nchar(complete$id)==19,]
complete = complete[!nchar(complete$id)==16,]
complete$id= ifelse(nchar(complete$id)<18, paste("0",complete$id, sep = ""), complete$id)
complete$box_id = NULL
complete$box_top = NULL
complete$box_left = NULL
complete$box_width = NULL
complete$box_height = NULL
complete$symetry = NULL
complete$condition = NULL
colnames(complete)
complete = na.omit(complete)

write.table(complete, "~/Desktop/microscopy_data/Phen1108_B1_R7_clean.txt", quote = F, sep = ",", row.names = F)

#once you fixed the wings with issues import using this 
#saveRDS(colomns, "~/Desktop/microscopy_data/colomns.RDS") # object with colomn names
colomns = readRDS("~/Desktop/microscopy_data/colomns.RDS")
output_morphoj= read_delim("~/Desktop/microscopy_data/Phen1108_B1_R7_clean_RO.txt")
colnames(output_morphoj) = colomns
output_morphoj$pair = stringr::str_extract(output_morphoj$id, "^.{3}")
output_morphoj$ind = str_split_i(output_morphoj$id, "_",1)
output_morphoj$symetry = ifelse(str_detect(output_morphoj$id, "R"), print("R"), print("L"))
output_morphoj$diet = str_split_i(output_morphoj$id, "_",2)
output_morphoj$condition = "hgSg_fs"
head(output_morphoj)
dim(output_morphoj) #should be 37 x xxx
#link to other objects
NEX_wings = rbind(output_morphoj, NEX_wings)
NEX_wings$pair = paste(NEX_wings$condition, NEX_wings$pair, sep = "_")
