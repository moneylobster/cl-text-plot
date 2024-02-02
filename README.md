## cl-text-plot
A plotting library for Common Lisp that prints plots on the REPL using Unicode braille characters. Code quality/style may not be the best.

Can be loaded as "textplot" with ASDF/Quicklisp (as a local project)

### Usage
For some reason the plots don't show up nicely on Github's markdown renderer, so the outputs look wonkier than they actually are.
#### Normal Plotting
```lisp
(plot '((1 2) (2 6) (3 4)) :title "Cool plot")
=>

          Cool plot
6⡏⠉⠉⠉⠉⠉⠉⠉⠉⣹⠛⢍⠉⠉⠉⠉⠉⠉⠉⢹
 ⡇⠀⠀⠀⠀⠀⠀⠀ ⣰⠃⠀ ⠀⠑⢄ ⠀⠀⠀⠀⠀⢸
 ⡇⠀⠀⠀⠀⠀⠀ ⣰⠃⠀⠀  ⠀⠀⠀⠑⢄⠀⠀⠀⢸
 ⡇⠀⠀⠀⠀⠀ ⣰⠃⠀⠀⠀  ⠀⠀⠀⠀⠀⠑⢄⠀⢸
 ⡇⠀⠀⠀⠀ ⣰⠃⠀⠀⠀⠀  ⠀⠀⠀⠀⠀⠀⠀⠑⢼
 ⡇⠀⠀⠀ ⣰⠃⠀⠀⠀⠀⠀  ⠀⠀⠀⠀⠀⠀⠀⠀⢸
 ⡇⠀⠀ ⣰⠃⠀⠀⠀⠀⠀⠀  ⠀⠀⠀⠀⠀⠀⠀⠀⢸
 ⡇⠀ ⣰⠃⠀⠀⠀⠀⠀⠀⠀  ⠀⠀⠀⠀⠀⠀⠀⠀⢸
 ⡇ ⣰⠃⠀⠀⠀⠀⠀⠀⠀⠀  ⠀⠀⠀⠀⠀⠀⠀⠀⢸
2⣷⣃⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸
 1                          3
```

#### Plotting Functions
```lisp
(plot-fun #'sin -4 4 0.1)
=>
               0.99957365⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢉⠏⠙⢏⠉⠉⠉⢹
                         ⣇⠀⠀⠀ ⠀⠀⠀⠀⠀ ⠀⠀⠀⡎  ⠀⠘⡆⠀⠀⢸
                         ⡟⡄⠀⠀ ⠀⠀⠀⠀⠀ ⠀⠀⡸⠀  ⠀⠀⢹⠀⠀⢸
                         ⡇⢳⠀⠀ ⠀⠀⠀⠀⠀⠀⢀⠇  ⠀⠀⠀⠀⠀⡇⠀⢸
                         ⡇⠈⡆⠀ ⠀⠀⠀⠀⠀⠀⡼⠀  ⠀⠀⠀⠀⠀⢸⠀⢸
#<FUNCTION SIN>          ⡇⠀⢱⠀⠀ ⠀⠀⠀⠀⢠⠇⠀  ⠀⠀⠀⠀⠀⠀⡇⢸
                         ⡇⠀⠈⡆⠀ ⠀⠀⠀⠀⡜⠀ ⠀ ⠀⠀⠀⠀⠀⠀⢸⣸
                         ⡇⠀⠀⢱⠀⠀ ⠀⠀⢠⠃ ⠀⠀ ⠀⠀⠀⠀⠀⠀⠀⢿
                         ⡇⠀⠀⠈⣇⠀ ⠀⠀⡎⠀ ⠀⠀ ⠀⠀⠀⠀⠀⠀⠀⢸
               -0.9995735⣇⣀⣀⣀⣘⣄⣀⣜⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸
                         -4.0               3.9999971
                                      x
```

#### Scatter Plots
```lisp
(scatter '((1 2) (2 6) (3 3) (4 4)))
=>
6.5⡏⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⠉⢹
   ⡇⠀⠀⠀⠀⠀⠀⠀⡀⠀⠀⠀⠀⠀⠀⠀⠀    ⠀⠀⢸
   ⡇⠀⠀⠀⠀⠀⠀⠈⠋⠀⠀⠀⠀⠀⠀⠀⠀    ⠀⠀⢸
   ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀    ⠀⠀ ⠀⢸
   ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀    ⠀⠀⠀⠀⠀⡀⠀⠀⢸
   ⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀    ⠀⠀⠀⠀⠈⠋⠀⠀⢸
   ⡇⠀⠀⠀⠀⠀⠀⠀⠀    ⠀⠀⠠⡦⠀⠀⠀⠀⠀⠀⢸
   ⡇⠀⠀⢀⠀⠀⠀⠀⠀⠀⠀    ⠀⠀⠀⠀⠀⠀⠀⠀⢸
   ⡇⠀⠀⠙⠀⠀⠀⠀⠀⠀⠀⠀⠀    ⠀⠀⠀⠀⠀⠀⢸
1.5⣇⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣀⣸
   0.625                  4.375
```


### TODOs
* Polish api more
* Change usage example outputs with images
* histograms and bar charts

### Acknowledgement
Inspired by [UnicodePlots.jl](https://github.com/JuliaPlots/UnicodePlots.jl).
