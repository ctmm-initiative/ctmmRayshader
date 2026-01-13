source("ctmmRayshader.r")


df=read.csv("movebank_data/Movement patterns of seed dispersing spotted nutcrackers (Nucifraga caryocatactes).csv")


plot_rayshader(individuals=c("K94648","K125874"),
               df=df,
               sim_path=FALSE,
               AKDE=FALSE,
               AKDE_color_list = c("Reds 3","Blues 2"),
               show_data = TRUE,
               padding =.05,
               animate_shadows=FALSE,
               spin_animation=FALSE, 
               season="all",
               shadow_intesity=.3,
               stretch=c(.005,995),
               animation_duration = 15,
               Upscale_factor = 1)


df = read.csv("movebank_data/3D flights of European free-tailed bats.csv")
DATA=as.telemetry(df)
plot(DATA$Bat2_3D6001852B95D)
plot(DATA$Bat4_3D6001852B980,add=TRUE)

plot_rayshader(individuals=c("Bat2_3D6001852B95D","Bat4_3D6001852B980"),
               df=df,
               sim_path=TRUE,
               AKDE=FALSE,
               AKDE_color_list = c("Blues 2","Greens"),
               show_data = FALSE,
               padding =.01,
               animate_shadows=FALSE,
               spin_animation=FALSE, 
               path_animation=TRUE,
               season="all",
               shadow_intesity=.3,
               stretch=c(.005,995),
               animation_duration = 15,
               Upscale_factor = 1)




# av::av_encode_video(sprintf("animation/landsat_hillshade%d.png",seq(1,450*3,by=1)), framerate = 30*3,
#                     output = "Rayshader_spin_animation.mp4")
# system("ffmpeg -framerate 30 -i animation/landsat_hillshade%d.png -pix_fmt yuv420p Rayshader_spin_animation.mp4")



df=read.csv("movebank_data/Hornbill telemetry in northeast India.csv")

load("fit_models/3_mogambo.rda")
FIT_mogambo=FIT
load("fit_models/6_tkbhai.rda")
FIT_tkbhai=FIT

#FIT_list=list(FIT_mogambo,FIT_tkbhai)
plot_rayshader(individuals=c("3_mogambo","6_tkbhai"),
               df=df,
               FIT_list=list(FIT_mogambo,FIT_tkbhai),
               sim_path=TRUE,
               AKDE=FALSE,
               AKDE_color_list = c("Blues 2","Greens"),
               show_data = FALSE,
               padding =.01,
               animate_shadows=FALSE,
               spin_animation=FALSE, 
               path_animation=FALSE,
               season="all",
               shadow_intesity=.3,
               stretch=c(.005,995),
               animation_duration = 15,
               Upscale_factor = 1)






