result.group.IDN1 <- results %>% filter(trt == "IDN_1")
result.group.IDN2 <- results %>% filter(trt == "IDN_2")
result.group.IDN1[is.na(result.group.IDN1)] <- 0
result.group.IDN2[is.na(result.group.IDN2)] <- 0

tha.1 <- IP.data %>% filter(groups == "THA_1")
tha.2 <- IP.data %>% filter(groups == "THA_2")

comp.2.cc.fdr(data1 = as.data.frame(t(tha.1[1:47, -1])), data2 =  as.data.frame(t(tha.2[1:47, -1])), 
              method="spearman", threshold=0.05)

get.common.networks
