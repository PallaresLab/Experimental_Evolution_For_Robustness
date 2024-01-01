library(readr)
library(ggpubr)
library(stringr)
library(readr)
library(geomorph)
library(gplots)
library(ggplot2)
library(borealis)
####### last task for today
#combine files from coords, IDs, and height values.

setwd("~/Desktop/flight_assay/G5")
Results_G5R2_sticker_ID_1gabsent <- read_csv("Results_G5R2_sticker_ID_1gabsent.csv")
View(Results_G5R2_sticker_ID_1gabsent)
Results_G5R2_sticker_ID_1gabsent$V1 = Results_G5R2_sticker_ID_1gabsent$...1

G5R2_ids = read.table("G5R2_idvalues")
G5R2_coords = read.table("coord_phen1608_modeltraining_part2.txt")

Results_G5R2_with_IDS =merge(Results_G5R2_sticker_ID_1gabsent, G5R2_ids,by="V1")

dim(Results_G5R2_sticker_ID_1gabsent)
dim(Results_G5R2_with_IDS)
dim(G5R2_complete)

Results_G5R2_with_IDS$ids= ifelse(nchar(Results_G5R2_with_IDS$V2)<3, paste("0",Results_G5R2_with_IDS$V2, sep = ""), Results_G5R2_with_IDS$V2)

Results_G5R2_with_IDS = Results_G5R2_with_IDS[,c(8,10)]
G5R2_coords$V1= ifelse(nchar(G5R2_coords$V1)<4, paste("0",G5R2_coords$V1, sep = ""), G5R2_coords$V1)
G5R2_coords$ids = substr(G5R2_coords$V1, 1, 3)  
G5R2_coords$ids_full = G5R2_coords$V1
G5R2_complete =merge(G5R2_coords, Results_G5R2_with_IDS,by="ids")
#G5R2_complete$Y = format(round(G5R2_complete$Y, 2), nsmall = 2)
#G5R2_complete$Y= ifelse(nchar(G5R2_complete$Y)<5, paste("0",G5R2_complete$Y, sep = ""), G5R2_complete$Y)
G5R2_complete$V1 = paste0(G5R2_complete$V1,G5R2_complete$Y)
G5R2_complete$ids = NULL
G5R2_complete$Y = NULL
colnames(G5R2_complete) = NULL
G5R2_complete[,1] = str_replace(G5R2_complete[,1]," ","0")
write.table(G5R2_complete, "G5R2_complete_coords.txt", row.names = F,sep="\t", quote = FALSE)

G5R2_data =read.table("r2data.txt", header = T)

ggscatter(G5R2_data, x = "Centroid_Size", y = "height",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(G5R2_data, x = "Log_Centroid_Size", y = "height",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson")

G5R2_complete = read_delim("G5R2_complete_coords.txt", col_names = F)

write.table(G5R2_data_subset, "G5R2_complete_covariated.txt", row.names = F,sep="\t", quote = FALSE)
G5R2_data_subset$group= "class"
G5R2_data_subset$group= ifelse(G5R2_data_subset$height<15, paste("strong_flight"), G5R2_data_subset$group)
G5R2_data_subset$group= ifelse(G5R2_data_subset$height>15, paste("robust_flight"), G5R2_data_subset$group)
G5R2_data_subset$group= ifelse(G5R2_data_subset$height>25, paste("weak_flight"), G5R2_data_subset$group)
table(G5R2_data_subset$group)

repeatability_landmarks = read.table("manual_annotation_repeated_coord_phen1608_73wings.txt")
repeatability_landmarks
G5R2_coords = read.table("coord_phen1608_modeltraining_part2.txt")
G5R2_coords_rep = G5R2_coords[G5R2_coords$V1 %in% repeatability_landmarks$V1,]
dim(G5R2_coords_rep)

G5R2_coords_rep$V1 = paste0(G5R2_coords_rep$V1, "annot_one")
repeatability_landmarks$V1 = paste0(repeatability_landmarks$V1, "annot_two")

combined_repeatability= rbind(G5R2_coords_rep,repeatability_landmarks)
dim(combined_repeatability)
head(combined_repeatability)

write.table(combined_repeatability, "combined_repeatabilitytxt", row.names = F,sep="\t", quote = FALSE)
)

#lets integrate in geomorph
cord <- as.matrix(G5R2_complete[,3:32])
shape <- arrayspecs(cord, 15, 2)

shape_fix = estimate.missing(shape )
myGPA<-gpagen(shape)
shapes_aligned <- align.reflect(myGPA, top.pt = 7, links = fw.links,provenance = NULL )
wing.pca <- gm.prcomp(shapes_aligned$coords)
wing.pca$condition = NEX_wings$condition
plot(wing.pca, col = as.factor(NEX_wings$condition))
ggGMMplot(wing.pca, group = NEX_wings$condition, 
          group.title = 'Condition', 
          convex.hulls = TRUE, include.legend = TRUE, label.groups = F)


ratings.split <- split(G5_final2$Y, G5_final2$ReplicaID)
Reploca.mean <- sapply(Replica.split, mean)


########################################################################################################################################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
########################################################
#same code but in G5R4, G5R7, and G5R12. Let's make it more robust
#it should provide a loop with 3 things: ids, flight scores, and morphology scores
#the loop will find an overlap


G5R2_ids = read.table("G5R2_idvalues")
G5R2_coords = read.table("coord_phen1608_modeltraining_part2.txt")
wings = Nex_wings_AV


G5R4_flight = read.csv("~/Desktop/flight_assay/G5/Results_G5R4.csv")
G5R4_values = read_table("~/Desktop/flight_assay/G5/G5R4_values", col_names = F)
G5R4_values$X1= ifelse(nchar(G5R4_values$X1)<3, paste("0",G5R4_values$X1, sep = ""), G5R4_values$X1)
G5R4_values$X1 = paste("ctrl_ns",G5R4_values$X1, sep = "_")

G5R12_flight = read.csv("~/Desktop/flight_assay/G5/Results_G5R12_with_IDs.csv")
G5R12_values = read_table("~/Desktop/flight_assay/G5/G5R12values.txt", col_names = F)
G5R12_values$X1 = G5R12_values$X2
G5R12_values$X1= ifelse(nchar(G5R12_values$X1)<3, paste("0",G5R12_values$X1, sep = ""), G5R12_values$X1)
G5R12_values$X1 = paste("hgSg_ns",G5R12_values$X1, sep = "_")

G5R7_flight = read.csv("~/Desktop/flight_assay/G5/Results_g5r7_with_ids.csv")
G5R7_values = read_table("~/Desktop/flight_assay/G5/G5R7values.txt", col_names = F)
G5R7_values$X1= ifelse(nchar(G5R7_values$X1)<3, paste("0",G5R7_values$X1, sep = ""), G5R7_values$X1)
G5R7_values$X1 = paste("hgSg_fs",G5R7_values$X1, sep = "_")



integrate_morphology_and_flight <- function(ids, flight, wings ) {
  row.names(flight) = flight$...1 #flight data always in this shape
  results_with_ids = merge(ids, flight, by = 0) #here we use numbers = the rownames to merge
  
  results_with_ids$pair = results_with_ids$X1 #after that I should clean up
  #results_with_ids = results_with_ids[,c(1)]
  integrated = merge(results_with_ids, wings, by.y  = "pair",) #here we use ids to merge
  return(integrated)
}
g5r4_integrated = integrate_morphology_and_flight(flight =G5R4_flight, ids = G5R4_values, wings = wings )
g5r7_integrated = integrate_morphology_and_flight(flight =G5R7_flight, ids = G5R7_values, wings = wings )
g5r12_integrated = integrate_morphology_and_flight(flight =G5R12_flight, ids = G5R12_values, wings = wings )

g5r12_integrated$X2 = NULL
complete_integr = rbind(g5r4_integrated,g5r12_integrated,g5r7_integrated)
ggscatter(complete_integr, x = "size", y = "Y", add = "reg.line", conf.int = TRUE,cor.coef = TRUE, cor.method = "pearson")

ggscatter(g5r4_integrated, x = "size", y = "Y", add = "reg.line", conf.int = TRUE,cor.coef = TRUE, cor.method = "pearson")
ggscatter(g5r12_integrated, x = "size", y = "Y", add = "reg.line", conf.int = TRUE,cor.coef = TRUE, cor.method = "pearson")
ggscatter(g5r7_integrated, x = "size", y = "Y", add = "reg.line", conf.int = TRUE,cor.coef = TRUE, cor.method = "pearson")

AI_NEX$pair =str_split_i(rownames(AI_NEX), "ind",2)
AI_NEX$ai = AI_NEX$`bs$signed.AI`
dim(AI_NEX)
AI_NEX_merges = merge(complete_integr, AI_NEX, by.y  = "pair",) #here we use ids to merge
dim(complete_integr)
dim(AI_NEX_merges)
table(complete_integr$pair %in% AI_NEX$pair)
AI_NEX_merges_na = na.omit(AI_NEX_merges)
dim(AI_NEX_merges_na)


ggscatter(AI_NEX_merges_na, x = "ai", y = "Y", add = "reg.line", conf.int = TRUE,cor.coef = TRUE, cor.method = "pearson")

AI_NEX_merges = merge(g5r4_integrated, AI_NEX, by.y  = "pair",) #here we use ids to merge
AI_NEX_merges = merge(g5r7_integrated, AI_NEX, by.y  = "pair",) #here we use ids to merge
AI_NEX_merges = merge(g5r12_integrated, AI_NEX, by.y  = "pair",) #here we use ids to merge


AI_NEX_merges_na = na.omit(AI_NEX_merges)
ggscatter(AI_NEX_merges_na, x = "ai", y = "Y", add = "reg.line", conf.int = TRUE,cor.coef = TRUE, cor.method = "pearson")

#the last correlations I need to have is for the flight scores!
#now I just need to make a GPA object and go with analysis
AI_NEX_merges_na
cord = as.matrix(AI_NEX_merges_na[,11:40])
shape <- arrayspecs(cord, 15, 2)
nex_mph <- geomorph.data.frame(shape = shape, size = AI_NEX_merges_na$size, id =AI_NEX_merges_na$pair, condition = AI_NEX_merges_na$condition,
                               flight = AI_NEX_merges_na$Y)

wing.pca <- gm.prcomp(shape)
wing.pca$condition = AI_NEX_merges_na$condition
wing.pca$flight = nex_mph$flight

shape.space(wing.pca, group = wing.pca$flight, color = wes_palette("BottleRocket1", type = "continuous"),
            group.title = 'Condition', convex.hulls = F,
            backtransform.examples = TRUE,
            ref.shape = mshape(shape_fx),
            shape.method = "points",
            bt.shape.mag = 4, hull.alpha = 0.1,ref.pt.size = 0.5, target.pt.size = 0.5, lm.labels = F,
            bt.links = fw.links,pt.size = 1, label.groups = F)
################################################################################################################
########################################################
########################################################
'''
#there are two things I need to figure out:
#how to make an average of two wings
#use the output for regressions
how to merge the data for asymetry, so using both wings
First - I need to seperate a dataframe into symmetrical and non-symetrical
Second - assign average value for two wings
Third merge dataframe using ID variable. Lets go!
'''

df = as.matrix(two.d.array(shapes_aligned$coords))
df = as.data.frame(df)
df$pair = myGPA$pairs
df$size = myGPA$Csize
NEX_wings_sm = df[duplicated(df$pair) | duplicated(df$pair, fromLast=TRUE), ]
NEX_wings_as = df[!df$pair %in% NEX_wings_sm$pair,]
dim(NEX_wings)
dim(NEX_wings_sm)
dim(NEX_wings_as)
#NEX_wings_as_average <- aggregate(NEX_wings_sm, by=NEX_wings_sm["pair"], mean )
#minus <- function(x) {x[1] - x[2]} # I used this to find difference in right versus left
#NEX_wings_as_average <- aggregate(NEX_wings_sm$size, by=NEX_wings_sm["pair"], minus )

NEX_wings_as_average <- aggregate(NEX_wings_sm, by=NEX_wings_sm["pair"], mean )

dim(NEX_wings_as_average)
NEX_wings_as_average[,32] = NULL
Nex_wings_AV = rbind(NEX_wings_as_average,NEX_wings_as)
#Nex_wings_AV = NEX_wings_as_average #if you want just one 

dim(Nex_wings_AV)
Nex_wings_AV$condition = stringr::str_extract(Nex_wings_AV$pair, "^.{7}")
Nex_wings_AV$logsize=  log10(Nex_wings_AV$size)
Nex_wings_AV$condition =  ifelse(Nex_wings_AV$condition == "ctrl_ns", paste("Control diet + no selection"), Nex_wings_AV$condition )
Nex_wings_AV$condition =  ifelse(Nex_wings_AV$condition == "ctrl_fs", paste("Control diet + flight selection"), Nex_wings_AV$condition )
Nex_wings_AV$condition =  ifelse(Nex_wings_AV$condition == "hgSg_ns", paste("High Sugar diet + no selection"), Nex_wings_AV$condition )
Nex_wings_AV$condition =  ifelse(Nex_wings_AV$condition == "hgSg_fs", paste("High Sugar diet + flight selection"), Nex_wings_AV$condition )

ggbetweenstats(
  data  = Nex_wings_AV,
  x     = condition,
  y     = logsize,
  title = "Wing size distribution per group", pairwise.display = "significant", centrality.plotting = F, p.adjust.method = "bonferroni",xlab = "Groups",results.subtitle = FALSE, ylab = "Log 10 -Wing Centrod Size", palette = "Accent")

#######Now we can try integrating flight scores
#Nex_wings_AV_sp = Nex_wings_AV[Nex_wings_AV$condition == "High Sugar diet + flight selection" |Nex_wings_AV$condition == "High Sugar diet + no selection", ]
cord_fx <- as.matrix(Nex_wings_AV[,2:31])
shape_fx <- arrayspecs(cord_fx, 15, 2)
#myGPA<-gpagen(shape)
nex_mph <- geomorph.data.frame(shape = shape_fx, size = Nex_wings_AV$size, leftright= Left_vs_Right_Difference$x,
                               id =Nex_wings_AV$pair, condition = Nex_wings_AV$condition)



wing.pca <- gm.prcomp(shape_fx)
wing.pca$condition = Nex_wings_AV$condition
shape.space(wing.pca, group = wing.pca$condition, 
            group.title = 'Condition', convex.hulls = F,
            backtransform.examples = TRUE,
            ref.shape = mshape(shape_fx),
            shape.method = "points",
            bt.shape.mag = 4, hull.alpha = 0.1,ref.pt.size = 0.5, target.pt.size = 0.5, lm.labels = F,
            bt.links = fw.links,pt.size = 1, label.groups = F)
size.model <- procD.lm(cord_fx ~ log(Csize), data =nex_mph, iter = 1000) 
plot

anova(size.model)
allometry.corrected.pca <- gm.prcomp(size.model$residuals)
shape.space(allometry.corrected.pca, group = nex_mph$condition, 
          group.title = 'condition', convex.hulls = TRUE,
          include.legend = TRUE)
shape_fx_ex <- arrayspecs(size.model$data[,1], 15, 2)

shape.space(allometry.corrected.pca, group = nex_mph$condition, 
            group.title = 'Condition', convex.hulls = F,
            backtransform.examples = F, hull.alpha = 0.1,
            shape.method = "points",
            ref.shape = mshape(shape_fx_ex),
            bt.shape.mag = 4,
            bt.links = fw.links,pt.size = 1, label.groups = F)





