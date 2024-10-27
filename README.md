## cl-text-plot
A plotting library for Common Lisp that prints plots on the REPL using Unicode characters. Code quality/style may not be the best.

Can be loaded as "textplot" via Ultralisp.

### Usage
For some reason the plots don't show up nicely on Github's markdown renderer, so the outputs look wonkier than they actually are.

There are two backends:
* `:blocks`  uses characters from the "Symbols for Legacy Computing" set.
* `:braille` uses Braille dots. This is intended as a fallback option in case your font of choice cannot render the `:blocks` backend.

You can use the `:as-string` key to get the output as a string instead of printing to the console.

#### Normal Plotting
```lisp
(plot '((1 2) (2 6) (3 4)) :title "Cool plot" :backend :blocks)
=>
      Cool plot
6🬕🬂🬂🬂🬂🬂🬂🬂🬂🬨🬊🬴🬂🬂🬂🬂🬂🬂🬂🬨
 ▌       🬦🬀  🬈🬱     ▐
 ▌      🬦🬀     🬈🬱   ▐
 ▌     🬦🬀        🬈🬱 ▐
 ▌    🬦🬀           🬈🬻
 ▌   🬦🬀             ▐
 ▌  🬦🬀              ▐
 ▌ 🬦🬀               ▐
 ▌🬦🬀                ▐
2🬺🬮🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬷
 1                  3
 ```

```lisp
(plot '((1 2) (2 6) (3 4)) :title "Cool plot" :backend :braille)
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
(plot-fun #'sin -4 4 0.1 :blocks)
=>
               0.99957365🬕🬂🬂🬂🬂🬂🬂🬂🬂🬂🬂🬂🬡🬆🬊🬴🬂🬂🬂🬨
                         🬲           🬔  🬁🬓  ▐
                         🬝🬓         🬘    🬨  ▐
                         ▌🬧        🬞🬄     ▌ ▐
                         ▌🬁🬓       🬘      ▐ ▐
#<FUNCTION SIN>          ▌ 🬧      🬞🬄       ▌▐
                         ▌ 🬁🬓     🬘        ▐▐
                         ▌  🬧    🬞🬄         █
                         ▌  🬁▌   🬔          ▐
               -0.9995735🬲🬭🬭🬭🬷🬱🬭🬷🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬷
                         -4.0       3.9999971
                                  x
```
```lisp
(plot-fun #'sin -4 4 0.1 :braille)
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
(scatter '((1 2) (2 6) (3 3) (4 4)) :backend :blocks)
=>
6.5🬕🬂🬂🬂🬂🬂🬂🬂🬂🬂🬂🬂🬂🬂🬂🬂🬂🬂🬂🬨
   ▌       🬏          ▐
   ▌      🬁🬆          ▐
   ▌                  ▐
   ▌               🬏  ▐
   ▌              🬁🬆  ▐
   ▌          🬇🬛      ▐
   ▌  🬞               ▐
   ▌  🬊               ▐
1.5🬲🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬭🬷
   0.625          4.375
```
```lisp
(scatter '((1 2) (2 6) (3 3) (4 4)) :backend :braille)
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

### Acknowledgement
Inspired by [UnicodePlots.jl](https://github.com/JuliaPlots/UnicodePlots.jl).

Also check out [cl-spark](https://github.com/tkych/cl-spark) and [hbook](https://github.com/eigenhombre/hbook)!
