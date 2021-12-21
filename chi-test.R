library(readxl)

chi_order <- read_excel("Desktop/WW_data/chi-order.xlsx")
chi <- chi_order[,-1]

chisq <- chisq.test(chi)
chisq




#random things
library(gplots)

dt <- as.table(as.matrix(chi_order))

dt2 <- dt[,-1]
rownames(dt2) <- dt[,1]

b <- balloonplot(t(dt2), main ="orders", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

png("b-orders.png", units="in", width=8, height=5, res=300)
b
dev.off()