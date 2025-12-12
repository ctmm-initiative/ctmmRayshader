# ctmmRayshader


* This repository contains the ctmmRayshader function and supporting files. 

* ctmmRayshader_example.R is a short demo of the ctmmRayshader

* [Examples of animations](https://www.youtube.com/watch?v=QAbZr7BFZTU&list=PLoobivMaZpiePlmIviWiu34xIHp30BGkc&index=7)

* ctmmRayshader function arugments
  * df:**Animal** movement data formatted as a movebank csv or a ctmm telematry object
  * **individuals**: list of animals that will be plotted
  * **FIT_list**(optional):a list of fit ctmm models that the user provides. If no fit models are provided the function will fit ctmm models using ctmm::guess and ctmm::select if the visualization require a fit model.
  * **sim_path**:
  * __AKDE__:
  * __AKDE_color_list__: 
  * __show_data__:
  * __padding__:
  * __animate_shadows__:
  * __spin_animation__:
  * __sim_animation__:
  * __season__:
  * __shadow_intesity__:
  * __stretch__:
  * __animation_duration__
  * __Upscale_factor__:
