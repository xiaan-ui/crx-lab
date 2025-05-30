rm(list=ls())
##堆叠直方图
library(ggplot2)
library(ggprism)
library(dplyr)
library(plyr)
library(ggpubr)
library (reshape2)
library(tidyverse)
library(ggsci)####配色

###数据准备：
#①不同处理土壤样品的微生物组成数据，包括【物种丰度信息，分类单元信息】
#②样本分组数据
###读入数据
####物种组成数据
sep <- read.csv("C:/Users/hp/Desktop/daotian/all_bin_coverm.csv", header=T, 
                row.names = 1, stringsAsFactors = TRUE,comment.char="",quote = "")
tax <- read.csv("C:/Users/hp/Desktop/daotian/all_bin_gtdbtk_tax_7904.csv", header=T,
                row.names = 1,comment.char="",stringsAsFactors = TRUE,quote = "")

#tax1 <- str_split(tax$Phylum,"__")
#tax$Phylum <- sapply(tax1,FUN = function(x)x[2])

#检查数据格式
head(sep)
head(tax)
#配色
col <- pal_d3("category20")(20)
col2 <- pal_d3("category20",alpha = 0.5)(20)
mypal <- c(col,col2[-8])
my_col <- c("#d2da93","#5196d5","#00ceff","#ff630d","#35978b",
            "#e5acd7","#77aecd","#ec8181","#dfc6a5","#E15759",
            "#d27e43","#8a4984","#fe5094","#8d342e","#f94e54",
            "#ffad00","#36999d","#00fc8d","#b64aa0","#9b82e1")

my_col1 <- c('#5196d5', '#F28E2F', '#E15759', '#76B7B2',  # 主类
             '#59A14F', '#BAB0AC', '#FF9DA7', '#9C755F',  
             '#B07AA1', '#86BCB6', '#79706E', '#D37295',  # 次类
             '#A0D6EC', '#D4A6C8', '#8CD17D', '#EDC948'   # 含纹理的次类
             )

###门水平上物种分度统计
phy <- sep %>%
  group_by(tax$Phylum) %>% #用于将数据框按照一个或多个列进行分组
  summarise_all(sum) #计算每个分组的总和
colnames(phy)[1] <- "Phylum" #更改列的名称
#计算所有行的总和
row_sums <- rowSums(phy[,-1])
#将总和作为新列添加到数据框的最后一列
phy <- cbind(phy, Total = row_sums)
#按照总和大小进行排序
phy <- phy[order(phy$Total,decreasing=TRUE),]
#write_csv(phy,"C:/Users/hp/Desktop/NEW/all_bin_phy_coverm.csv")#写入新的csv文件
#删除最后一列
phy <- phy[,-ncol(phy)]
#取丰度top15的门，其余求和记为"Others"
other <- phy[16:nrow(phy),]
rownames(other) <- other[,1]
other <- other[,-1]
col_sum <- colSums(other)
other <- rbind(other,Others = col_sum)
others <- cbind(Phylum = "Others",other[nrow(other),])
phy1 <- rbind(phy[c(1:15),],others)
phy_LONG <- phy1 %>%
  gather(key="Samples",value = "abun",-Phylum)#将数据宽格式改为长格式

####相对丰度堆叠图
#pdf("rel_stack.pdf",width = 18,height = 15,family="Times")
ggplot()+
  geom_bar(data = phy_LONG,aes(x = Samples ,weight = abun,fill = reorder(Phylum,-abun)),
           #ggplot2会自己计算相对丰度，无需自己计算！所以，在绝对分度对应的这部分注释中所用的是“fill”
           position = "fill",
           width = 0.5)+
  #scale_fill_manual(values = col[-c(1:1)])+#颜色可以和相对丰度的保持一致
  scale_fill_manual(values = my_col1)+
  scale_y_continuous(expand = c(0,0),#设置横坐标轴紧挨柱状图，x轴底部有无空缺的空间
                     name = "Relative abundance(%)",
                     limits = c(0,1),breaks = seq(0,1,0.20),labels = paste(seq(0,100,20),"%"))+
  #图例的形式，标题为“phylum”，列数为1，更改图例的文字可以在数据中直接修改
  guides(fill = guide_legend(title = "Phylum",ncol = 1))+
  theme(legend.position = "right",#图例的位置为图形的右侧
        axis.title = element_text(face = "bold",size = 12,color = "black"),#坐标轴标题
        axis.text = element_text(face = "bold",size = 10,color ="black"),#坐标轴标签
        axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 0.5,
                                   margin = margin(t = 3)),#x轴坐标标签
        strip.text.x =  element_text(face = "bold",size = 12,color ="black"))+
  theme(panel.grid = element_blank(),
        legend.title = element_text(face = "bold",size = 12,color = "black"),
        legend.text = element_text(face = "bold",size = 10,color = "black"))
#dev.off()