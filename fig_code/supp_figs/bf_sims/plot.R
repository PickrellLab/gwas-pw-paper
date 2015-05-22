

pdf("bfs_sim.pdf")
d = read.table("bfs_split1", as.is = T)
plot(density(d[,8]), xlim = c(-10, 50), xlab = "ln(BF) [true model versus null]", main = "")
d = read.table("bfs_split2", as.is = T)
points(density(d[,9]), col = "red", type = "l")
d = read.table("bfs_split3", as.is = T)
points(density(d[,10]), col = "blue", type = "l")
d = read.table("bfs_split4", as.is = T)
points(density(d[,11]), col = "green", type = "l")
legend("topright", legend = c("model 1", "model 2", "model 3", "model 4"), lty = 1, col = c("black", "red", "blue", "green"))
dev.off()




d = read.table("bfs_nosplit1", as.is = T)
plot(density(d[,8]), xlim = c(-10, 90), ylim = c(0, 0.04), xlab = "ln(BF)", main = "")
d = read.table("bfs_nosplit2", as.is = T)
points(density(d[,9]), col = "red", type = "l")
d = read.table("bfs_nosplit3", as.is = T)
points(density(d[,10]), col = "blue", type = "l")
d = read.table("bfs_nosplit4", as.is = T)
points(density(d[,11]), col = "green", type = "l")
legend("topright", legend = c("model 1", "model 2", "model 3", "model 4"), lty = 1, col = c("black", "red", "blue", "green"))





