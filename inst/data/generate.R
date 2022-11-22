
# Create demo output for playing around (Reto)
set.seed(100)
d <- data.frame("Zeitraum-ab" = sprintf("1.10.11 00:%02d", seq(0, 55, by = 5)))
n <- nrow(d)
d <- transform(d, "T-WZ-W1"   = round(runif(nrow(d), 21, 23), 1),
                  "rH-WZ-W1"  = round(runif(nrow(d), 55, 56), 1),
                  "CO2-WZ-W1" = round(runif(nrow(d), 910, 930)),
                  "T-WZ-W2"   = round(runif(nrow(d), 21, 23), 1),
                  "rH-WZ-W2"  = round(runif(nrow(d), 50, 53), 1),
                  "CO2-WZ-W2" = round(runif(nrow(d), 500, 800)))
print(d)
write.table(d, "demo_01.csv", sep = "\t", quote = FALSE, row.names = TRUE)

