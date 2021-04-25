library(pandas);



res <- read.csv("speaker-model.csv");
res$probability = as.numeric(as.character(res$probability));

res4COARSE <- subset(res, res$cardinality == 4 & res$precision == "COARSE")[3:5];
shaped <- reshape(res4COARSE, direction = "wide", idvar = "description", timevar = "granularity");
row.names(shaped) <- shaped$description
colnames(shaped) <- str_replace(colnames(shaped), "probability.", "")
shaped <- shaped[-1]
shaped[is.na(shaped)] <- 0
shaped <- t(shaped);
heatmap(as.matrix(shaped), scale="none", col=heat.colors(256), xlab="utterance", ylab="granularity", main="Cardinality 4 Coarse Speaker Outputs")
