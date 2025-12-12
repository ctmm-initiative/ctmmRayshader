source("ctmmRayshader.r")


df=read.csv("movebank_data/Movement patterns of seed dispersing spotted nutcrackers (Nucifraga caryocatactes).csv")


landsat_obj=plot_rayshader(individuals=c("K94648","K125874"),
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







