# Initial values
# Flags
nullFlags <- list(
  isThereUser=FALSE,
  anonymousUser=FALSE,
  proteinSearch=FALSE,
  phylogenySearch=FALSE,
  addEcNumber=FALSE,
  pdbSearch=FALSE,
  fastaSearch=FALSE,
  paramSearch=FALSE,
  attributeFound=list(
    mw=FALSE,
    ic50=FALSE,
    kc=FALSE,
    ki=FALSE,
    km=FALSE,
    pho=FALSE,
    phr=FALSE,
    pi=FALSE,
    sa=FALSE,
    to=FALSE,
    tr=FALSE,
    ton=FALSE
  ),
  merged=FALSE
)

# Saved
nullSaved <- list(
  proteinSaved=FALSE,
  pdbSaved=FALSE,
  fastaSaved=FALSE,
  paramSaved=FALSE
)

# Tables
nullTables <- list(
  # Saved Tables
  summaryTable=NULL,
  proteinTable=NULL,
  # Phylogenetic Table
  phylogenyTable=NULL,
  # Fasta Table
  fastaTable=NULL,
  # PDB Table
  pdbTable=NULL,
  # Parameter Table
  parameterTable=NULL,
  # Summary Table
  infoTable=NULL,
  summaryTable=NULL,
  # Subclass Table
  subclassTable=NULL,
  # Synonyms Table
  synonymsTable=NULL,
  # Parameters Table
  attrTable=list(
    mw=NULL,
    ic50=NULL,
    kc=NULL,
    ki=NULL,
    km=NULL,
    pho=NULL,
    phr=NULL,
    pi=NULL,
    sa=NULL,
    to=NULL,
    tr=NULL,
    ton=NULL
  ),
  # Filtered (f_)
  fattrTable=list(
    mw=NULL,
    ic50=NULL,
    kc=NULL,
    ki=NULL,
    km=NULL,
    pho=NULL,
    phr=NULL,
    pi=NULL,
    sa=NULL,
    to=NULL,
    tr=NULL,
    ton=NULL
  ),
  fparameterTable=NULL,
  # Merge tables
  groupMerging=list(NULL),
  # Clusterized
  kmeansTable=NULL,
  dbscanTable=NULL,
  dbscanSaveTable=NULL
)

# Plots
nullSavedPlots <- list(
  # Image enzyme tab
  imageWordCloud=list(
    src=NULL,
    contentType=NULL
  ),
  # Histogram
  histPlot=NULL,
  # Distribution
  distributionPlot=NULL,
  # Correlation
  correlationPlot=NULL,
  correlationPlotScatter=NULL,
  # Clustering: K-means
  elbowPlot=NULL,
  kPlot=NULL,
  # DBSCAN
  distDBSCAN=NULL,
  dbscanPlot=NULL
)
