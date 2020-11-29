d <- apply_model(path = "INFORMATION/FILES/ubicaciones.csv", result = "output")

e <- apply_model(path = "INFORMATION/FILES/ubicaciones.csv", result = "table")
e[,.N,by = "LABEL"]

D_labels <- glue("D{1:6}")

s <- read.csv(file = "INFORMATION/FILES/ubicaciones2.csv",
              header = TRUE,
              na.strings = "NA") %>% as.data.table() %>%
  setnames(old = "Id_Cliente", "id_cliente") %>%
  setnames(toupper(names(.)))

walk(str_subset(string = names(df), "(?:ID|LAT|LON).*"),
     ~df[,(.x):=as.character(get(.x))])

a <- data.table(
  id = d[,"Id_Cliente"],
  mio = d[,rowSums(.SD), .SDcols = D_labels]
)
a[,ellos:=s[,FRECUENCIA]]
a[,diff:=ellos-mio]
a[diff!=0]
################################################################################
e[,.N, by = "LABEL"]
e[,.(VOL = sum(VOL_ENTREGA)), by = "LABEL"]

  
#ï..Id_Cliente  