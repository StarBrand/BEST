# Color palette
seba_palette <- c("#4c9e4a", "#124211", "#336931", "#68d565", "#a1fb9e",
                  "#083121", "#2e624d", "#499d7b", "#10784e", "#6cd1a8",
                  "#5964e5", "#333ec2", "#394093", "#0a0c26", "#202453")
seba_palette2 <- c("#124212", "#336830", "#4c9e4a", "#5bb959", "#69d565",
                   "#a1fa9e", "#ffffff", "#5964e4", "#4a51c3",
                   "#393f93", "#2b3070", "#202453", "#13153b")

pal <- c(seba_palette[1], seba_palette[15])
pal <- setNames(pal, c("Mutant", "Wild Type"))
palDist <- seba_palette[c(1, 2, 3, 5, 6, 7, 13, 9, 12, 8, 14, 15)]

# Attributes
at <- c("Molecular Weight" = "mw",
        "IC50" = "ic50",
        "Kcat/Km" = "kc",
        "Ki" = "ki",
        "Km" = "km",
        "pH Optimum" = "pho",
        "pH Range" = "phr",
        "pI" = "pi",
        "Specific Activity" = "sa",
        "Temperature Optimum" = "to",
        "Temperature Range" = "tr",
        "Turnover Number" = "ton")
nat <- c("Molecular_Weight", "IC50", "Kcat/Km",
         "Ki", "Km", "pH_Optimum", "pH_Range",
         "pI", "Specific_Activity", "Temperature_Optimum",
         "Temperature_Range", "Turnover_Number")
units <- c("Da", "umol/min/mg", "mM/s", "mM", "mM", "",
           "", "", "umol/min/mg", "Celsius degrees", "Celsius degrees", "1/s")
units <- paste("[", units, "]", sep = "")
nat_to_show <- unlist(lapply(nat, gsub, pattern = "_", replacement = " ")) 
nat_axis <- paste(nat_to_show, units)
table_name <- c("molecular_weight", "ic50", "kcat_km", "ki", "km", "ph_optima",
                "ph_range", "pi", "specific_activity", "temperature_optima",
                "temperature_range", "turnover_number")
molList <- c("null", "Inhibitor", "Substrate", "Inhibitor", "Substrate", "null",
             "null", "null", "null", "null", "null", "Substrate")
bool_mol <- molList != "null"

palDist <- setNames(palDist, nat_to_show)

linealprotein <- c(4.101506, 0.002124 + 0.002)
linealpdb <- c(5.497487, 0.001329 + 0.001)
linealparameters <- c(6.877541, 0.007142 + 0.01)
fastatime <- 44
