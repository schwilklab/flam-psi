## requires read-data and read-hobos.R
## TODO: organize scripts to minimize dependencies and clarify order.

library(ggplot2)

source("./scripts/ggplot_theme.R")

short_ignite <- filter(alldata, ignition_delay <= 20)

fig1 <- ggplot(alldata, aes(wp, 100*fmc, color=display_name)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("Moisture content (%)") +
  scale_color_manual(values = schwilkcolors) +
  pubtheme +
  theme(legend.position = c(0.28,0.85),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize))


ggsave("./results/fig1.pdf", plot=fig1, width=col1, height=col1, units="cm")


fig2 <- ggplot(alldata, aes(wp, heat_release_j, color=display_name)) +
  dws_point + bestfit +
  xlab("Water potential (MPa)") +
  ylab("Heat release (J)") +
  scale_color_manual(values = schwilkcolors) +
  pubtheme +
  theme(legend.position = c(0.28,0.85),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize))
ggsave("./results/fig2.pdf", plot=fig2, width=col1, height=col1, units="cm")

fig3 <- ggplot(alldata, aes(100*fmc, heat_release_j, color=display_name)) +
  dws_point + bestfit +
  xlab("Moisture content (%)") +
  ylab("Heat transfer (J)") +
  scale_color_manual(values = schwilkcolors) +
  pubtheme +
  theme(legend.position = c(0.7,0.85),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize))
ggsave("./results/fig3.pdf", plot=fig3, width=col1, height=col1, units="cm")
ggsave("./results/fig3.png", plot=fig3, width=col1, height=col1, units="cm")


fig4 <- ggplot(alldata, aes(100*fmc, PC1, color=display_name)) +
  dws_point + bestfit +
  xlab("Moisture content (%)") +
  ylab("Flammability (PC1)") +
  scale_color_manual(values = schwilkcolors) +
  pubtheme +
  theme(legend.position = c(0.7,0.85),
        legend.text = element_text(face="italic"),
        legend.title = element_blank(),
        axis.text = element_text(size = smsize))
ggsave("./results/fig4.pdf", plot=fig4, width=col1, height=col1, units="cm")
ggsave("./results/fig4.png", plot=fig4, width=col1, height=col1, units="cm")




ggplot(alldata, aes(fmc, flame_duration, color=spcode)) + geom_point() + geom_smooth(method="lm")
ggplot(short_ignite, aes(fmc, flame_duration, color=spcode)) + geom_point() + geom_smooth(method="lm")
ggplot(short_ignite, aes(wp, flame_duration, color=spcode)) + geom_point() + geom_smooth(method="lm")


ggplot(alldata, aes(wp, degsec_100, color=spcode)) + geom_point() + geom_smooth(method="lm")
ggplot(short_ignite, aes(wp, degsec_100, color=spcode)) + geom_point() + geom_smooth(method="lm")


ggplot(alldata, aes(wp, flame_duration, color=spcode)) + geom_point() + geom_smooth(method="lm")

ggplot(alldata, aes(fmc, vol_burned, color=spcode)) + geom_point() + geom_smooth(method="lm")

ggplot(filter(alldata, spcode!= "JUPI"), aes(wp, flame_duration, color=spcode)) + geom_point() + geom_smooth(method="lm")

ggplot(alldata, aes(mass_pre, ignition_delay, color=spcode)) + geom_point()


