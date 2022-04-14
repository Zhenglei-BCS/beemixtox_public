
# # Plot by combo/class only for selected chemical classes or AIs
#
# # By combo.class.name
# #Only P450
# Plot4<-ggplot(subset(Data, P450==1),aes(x=combo.class.name,y=log10Q))+
#   geom_boxplot(fill="grey75", outlier.shape = NA)+
#   geom_jitter(width = 0.2, alpha=0.7, aes(colour = Exposure.Type)) +
#   # geom_boxplot(aes(fill=combo.class.name))+
#   # geom_boxplot(aes(fill=combo.Type.name))+
#   geom_hline(yintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
#   annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.5), ymax=log10(2), alpha=0.2) +
#   annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.2), ymax=log10(5), alpha=0.2) +
#   theme_classic()+
#   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
#         axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
#         legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
#   labs(x=NULL,y="log10(MDR)", title= "P450 - All contact & oral by combo class")+
#   coord_flip(ylim = c(min(Data$log10Q, na.rm = T), max(Data$log10Q, na.rm = T)))
# Plot4
# ggsave(paste0("Bee.CA.Boxplots_ByComboClass_P450.pdf"),width=10, height = 7.19, units = "in")
#
# ## Only combinations including NEO
# Plot5<-ggplot(subset(Data, NEO==1),aes(x=combo.class.name,y=log10Q))+
#   geom_boxplot(fill="grey75", outlier.shape = NA)+
#   geom_jitter(width = 0.2, alpha=0.7, aes(colour = Exposure.Type)) +
#   # geom_boxplot(aes(fill=combo.class.name))+
#   # geom_boxplot(aes(fill=combo.Type.name))+
#   geom_hline(yintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
#   annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.5), ymax=log10(2), alpha=0.2) +
#   annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.2), ymax=log10(5), alpha=0.2) +
#   theme_classic()+
#   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
#         axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
#         legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
#   labs(x=NULL,y="log10(MDR)", title= "NEONIC - All contact & oral by combo class")+
#   coord_flip(ylim = c(min(Data$log10Q, na.rm = T), max(Data$log10Q, na.rm = T)))
# Plot5
# ggsave(paste0("Bee.CA.Boxplots_ByComboClass_NEO.pdf"),width=10, height = 7.19, units = "in")


# By combo.name
# Only combinations including P450

NEO.P450.list = list()
Plot6<-ggplot(subset(Data, P450==1),aes(x=combo.name,y=log10Q))+
  geom_boxplot(fill="grey75", outlier.shape = NA)+
  geom_jitter(width = 0.2, alpha=0.7, aes(colour = Exposure.Type)) +
  # geom_boxplot(aes(fill=combo.class.name))+
  # geom_boxplot(aes(fill=combo.Type.name))+
  geom_hline(yintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.5), ymax=log10(2), alpha=0.2) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.2), ymax=log10(5), alpha=0.2) +
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.5,0.3,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12), axis.title.x=element_text(size=14), axis.title.y=element_text(size=14), legend.text=element_text(size=14), legend.title=element_text(size=14),
        # legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
        legend.position="right")+
  labs(x=NULL,y="log10(MDR)")+
  coord_flip(ylim = c(min(Data$log10Q, na.rm = T), max(Data$log10Q, na.rm = T)))
Plot6
NEO.P450.list[[1]] = Plot6
# ggsave(paste0("Bee.CA.Boxplots_ByComboName_P450-INS_ColorbyExp.pdf"),width=7.5, height = 7, units = "in")
# ggsave(paste0("Bee.CA.Boxplots_ByComboName_P450-INS_ColorbyExp.png"),width=7.5, height = 7, units = "in")

# USE. Only NEONIC combinations
Plot7<-ggplot(subset(Data, NEO==1),aes(x=combo.name,y=log10Q))+
  geom_boxplot(fill="grey75", outlier.shape = NA)+
  geom_jitter(width = 0.2, alpha=0.7, aes(colour = Exposure.Type)) +
  # geom_boxplot(aes(fill=combo.class.name))+
  # geom_boxplot(aes(fill=combo.Type.name))+
  geom_hline(yintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.5), ymax=log10(2), alpha=0.2) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.2), ymax=log10(5), alpha=0.2) +
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.5,0.3,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=12), legend.title=element_text(size=12),
        # legend.key.size=unit(1, "cm"), legend.position="top", legend.box.background = element_rect(size=1,fill=NA))+
        legend.position="none")+
  labs(x=NULL,y="log10(MDR)")+
  coord_flip(ylim = c(min(Data$log10Q, na.rm = T), max(Data$log10Q, na.rm = T)))
Plot7
NEO.P450.list[[2]] = Plot7
# ggsave(paste0("Bee.CA.Boxplots_ByComboName_NEO.ColorbyExp.pdf"),width=6.5, height = 6.5, units = "in")
# ggsave(paste0("Bee.CA.Boxplots_ByComboName_NEO.ColorbyExp.png"),width=6.5, height = 6.5, units = "in")

#Combine
# Arrange the plots using patchworks syntax
NEO.P450.list[[1]] + NEO.P450.list[[2]] +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')
ggsave("Paper_SM_NEO_P450.pdf", width=14, height = 5, units = "in")
ggsave("Paper_SM_NEO_P450.png", width=14, height = 5, units = "in")
