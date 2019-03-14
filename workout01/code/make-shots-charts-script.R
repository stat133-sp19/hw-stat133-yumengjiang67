#Write code in the R script make-shot-charts-script.R to create shot charts (with court
#backgrounds) for each player, and save the plots in PDF format, with dimensions width =
# 6.5 and height = 5 inches, inside the folder images/

library(ggplot2)
library(jpeg)
library(grid)
# court image (to be used as background of plot)
court_file <- "./images/nba-court.jpg"
# create raste object
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))
#andre-iguodala-shot-chart
iguodala_shot_chart <- ggplot(data=iguodala)+
  annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+ggtitle('Shot Chart: Adrew Iguodala (2016 season)')+theme_minimal()
ggsave("./images/andre-iguodala-shot-chart.pdf",width=6.5,height=5)

#draymond-green-shot-chart
green_shot_chart <- ggplot(data=green)+
  annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+ggtitle('Shot Chart:Draymond Green (2016 season)')+theme_minimal()
ggsave("./images/draymond-green-shot-chart.pdf",width=6.5,height=5)

#kevin-durant-shot-chart
durant_shot_chart <- ggplot(data=durant)+
  annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+ggtitle('Shot Chart:Kevin Durant (2016 season)')+theme_minimal()
ggsave("./images/kevin-durant-shot-chart.pdf",width=6.5,height=5)

#klay-thompson-shot-chart
thompson_shot_chart <- ggplot(data=thompson)+
  annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+ggtitle('Shot Chart:Klay Thompson (2016 season)')+theme_minimal()
ggsave("./images/klay-thompson-shot-chart.pdf",width=6.5,height=5)

#stephen-curry-shot-chart
curry_shot_chart <- ggplot(data=curry)+
  annotation_custom(court_image, -250, 250, -50, 420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+ggtitle('Shot Chart:Stephen Curry (2016 season)')+theme_minimal()
ggsave("./stephen-curry-shot-chart.pdf",width=6.5,height=5)

#4.2) Facetted Shot Chart
ggplot(data=gsw)+annotation_custom(court_image, -250, 250, -50, 420)+
         geom_point(aes(x=x,y=y,color=shot_made_flag))+ylim(-50,420)+ggtitle('Shot Chart:GSW (2016 season)')+theme_minimal()+facet_wrap(.~name)
ggsave("./images/gsw-shot-charts.pdf",width=8,height=7)
ggsave("./images/gsw-shot-charts.png",width=8,height=7)
