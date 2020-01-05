#Meta Analysis
readRDS(SM BDAM58F.RDS)
readRDS(SM BDAM58L.RDS)
readRDS(SM BDAM77F.RDS)
readRDS(SM BDAM77L.RDS)
meta<-siena08(ans58F,ans58L,ans77F,ans77L)