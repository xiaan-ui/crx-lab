library(tidyr)
library(ggplot2)

dat<-read.csv("enzyme data.csv",header=T)
colnames(dat)

#Ligninase
p1<-ggplot(data = dat,aes(x=Ligninase,y=SOC))+
  theme_bw()+
  geom_point(size=4,color = "#1E97F3",alpha = 0.6)+ #,alpha = 0.5
  geom_smooth(method="lm", se = T,color = "#1E97F3",fill="#1E97F3",alpha=0.1)+ #,aes(fill = Depth)，要置信区间se=T
  ylab("SOC")+
  xlab("Ligninase (nmol g-1 h-1)")+
  # scale_color_manual(values = c("#F7A049","#1E97F3","#22B5AF"),labels=c("Cropland","Shrub","Forest"))+ #"red","skyblue"
  theme(panel.grid = element_blank(),strip.text.x = element_text(size = 14), #设置分页标签字体大小
        legend.title = element_blank(),
        legend.position = "top",
        legend.text=element_text(size=14),axis.text=element_text(size=14,color = "black"),axis.title=element_text(size=16))+
  facet_wrap(~Region,scales = "free")
p1

#Cellulase
p2<-ggplot(data = dat,aes(x=Cellulase,y=SOC))+
  theme_bw()+
  geom_point(size=4,color = "#3FC4BF",alpha = 0.6)+ #,alpha = 0.5
  geom_smooth(method="lm", se = T,color = "#3FC4BF",fill="#3FC4BF",alpha=0.1)+ #,aes(fill = Depth)
  ylab("SOC")+
  xlab("Cellulase (nmol g-1 h-1)")+
  #scale_color_manual(values = c("#F7A049","#1E97F3","#22B5AF"),labels=c("Cropland","Shrub","Forest"))+ #"red","skyblue"
  theme(panel.grid = element_blank(),strip.text.x = element_text(size = 14), #设置分页标签字体大小
        legend.title = element_blank(),
        legend.position = "top",
        legend.text=element_text(size=14),axis.text=element_text(size=14,color = "black"),axis.title=element_text(size=16))+
  facet_wrap(~Region,scales = "free")
p2

#Ligninase:Cellulase
p3<-ggplot(data = dat,aes(x=Ligninase.Cellulase,y=SOC))+
  theme_bw()+
  geom_point(size=4,color = "Orange",alpha = 0.6)+ #,alpha = 0.5
  geom_smooth(method="lm", se = T,color = "Orange",fill="Orange",alpha=0.1)+ #,aes(fill = Depth)
  ylab("SOC")+
  xlab("Ligninase.Cellulase")+
  #scale_color_manual(values = c("#F7A049","#1E97F3","#22B5AF"),labels=c("Cropland","Shrub","Forest"))+ #"red","skyblue"
  theme(panel.grid = element_blank(),strip.text.x = element_text(size = 14), #设置分页标签字体大小
        legend.title = element_blank(),
        legend.position = "top",
        legend.text=element_text(size=14),axis.text=element_text(size=14,color = "black"),axis.title=element_text(size=16))+
  facet_wrap(~Region,scales = "free")
p3

library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 1)


# 分组拟合并查看结果
library(dplyr)
library(broom)
library(tidyr)

fit_results <- dat %>%
  group_by(Region) %>%
  do(fit = lm(SOC ~ Ligninase, data = .)) %>%
  mutate(glance_fit = list(broom::glance(fit)))

# 显示拟合结果
fit_results %>%
  select(Region, glance_fit) %>%
  unnest(glance_fit)

