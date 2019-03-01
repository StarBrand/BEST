# Color palette
seba_palette <- c("#4c9e4a", "#124211", "#336931", "#68d565", "#a1fb9e",
                  "#083121", "#2e624d", "#499d7b", "#10784e", "#6cd1a8",
                  "#5964e5", "#333ec2", "#394093", "#0a0c26", "#202453")
seba_palette2 <- c("#124212", "#336830", "#4c9e4a", "#5bb959", "#69d565",
                   "#a1fa9e", "#ffffff", "#5964e4", "#4a51c3",
                   "#393f93", "#2b3070", "#202453", "#13153b")

pal <- c(seba_palette[1], seba_palette[15])
pal <- setNames(pal, c("Mutant", "Wild Type"))

# Attributtes
at <- c("mw", "ic50", "kc", "ki", "km", "pho", "phr", "pi", "sa", "to", "tr", "ton")
nat <- c("Molecular_Weight", "IC50", "Kcat/Km",
         "Ki", "Km", "pH_Optimum", "pH_Range",
         "pI", "Specific_Activity", "Temperature_Optimum",
         "Temperature_Range", "Turnover_Number")
nat_to_show <- unlist(lapply(nat, gsub, pattern = "_", replacement = " ")) 
files_name <- unlist(lapply(nat_to_show, gsub, pattern = "/", replacement = "_"))
files_name <- paste(files_name, ".txt", sep ="")
molList <- c("null", "Inhibitor", "Substrate", "Inhibitor", "Substrate", "null",
             "null", "null", "null", "null", "null", "Substrate")
bool_mol <- molList != "null"

linealprotein <- c(4.101506, 0.002124 + 0.002)
linealpdb <- c(5.497487, 0.001329 + 0.001)
linealparameters <- c(6.877541, 0.007142 + 0.01)
fastatime <- 44

idLineal <- c(5.9175667, 0.3621877 + 0.07)
taxaLineal <- c(-0.9112722, 0.4452462 + 0.07)
treeLineal <- c(-12.6334140, 0.1493507 + 0.07)