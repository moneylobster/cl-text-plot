## cl-text-plot
A plotting library for Common Lisp that prints plots on the REPL using Unicode characters. Code quality/style may not be the best. Should work with SBCL and CCL.

Can be loaded as "textplot" via Ultralisp. `cl-unicode` is a dependency.

### Usage
For some reason the plots don't show up nicely on Github's markdown renderer, so the outputs look wonkier than they actually are.

There are three backends:
* `:blocks`  uses the 2x3 block characters from the "Symbols for Legacy Computing" set.
* `:blocks4`  uses the 2x2 block characters.
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

[vgplot](https://github.com/volkers/vgplot) can also generate text plots if you run `(vgplot:format-plot t "set terminal dumb")`.
