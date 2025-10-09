Code to create chord diagrams of global migration patterns, including: 

* `plot-ims2024/time` - UN DESA IMS2024 data over time, with the sector axes fixed to the maximum turnover during the period.
* `plot-ims2024/sex` - UN DESA IMS2024 data in 2024 for females and males, with the sector axes fixed to the maximum migration volumes over the sexes.
* `plot-ims2024/pop` - UN DESA IMS2024 data in 2024, with sector axes alternating between migration and population totals. 

All plots use the [`mig_chord()`](https://guyabel.github.io/migest/reference/mig_chord.html) R function as their basis. 
See [https://guyabel.com/post/global-migrant-chord-diagrams/](https://guyabel.com/post/global-migrant-chord-diagrams/) for more plots.

Note for reading plots: The arrowhead indicates the direction of the migration. The width of the arrow at its base indicates the size of the migrant population. Numbers on the outer section axis indicate the size of the migration population in millions.

![Global Migration GIF](https://raw.githubusercontent.com/guyabel/chord-global-migration/main/plot-ims2024/time.gif)
