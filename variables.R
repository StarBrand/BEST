# Color palette
seba_palette <- c("#4c9e4a", "#124211", "#336931", "#68d565", "#a1fb9e",
                  "#083121", "#2e624d", "#499d7b", "#10784e", "#6cd1a8",
                  "#5964e5", "#333ec2", "#394093", "#0a0c26", "#202453")

# Attributtes
at <- c("mw", "ic50", "kc", "ki", "km", "pho", "phr", "pi", "sa", "to", "tr", "ton")
nat <- c("Molecular_Weight", "IC50", "Kcat/Km",
         "Ki", "Km", "pH_Optimum", "pH_Range",
         "pI", "Specific_Activity", "Temperature_Optimum",
         "Temperature_Range", "Turnover_Number")
molList <- c("null", "Inhibitor", "Substrate", "Inhibitor", "Substrate", "null",
             "null", "null", "null", "null", "null", "Substrate")
bool_mol <- molList != "null"

idLineal <- c(5.9175667, 0.3621877 + 0.07)
taxaLineal <- c(-0.9112722, 0.4452462 + 0.07)
treeLineal <- c(-12.6334140, 0.1493507 + 0.07)