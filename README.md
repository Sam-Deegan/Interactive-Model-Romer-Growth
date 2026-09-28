# Interactive Model: Romer Growth

A Shiny app for teaching the Romer model of growth through ideas, in its
semi-endogenous (φ < 1) and endogenous (φ = 1) forms, with the capital
side of the exam questions added on top. Built by
[Sam Deegan](https://sam-deegan.com) for ECON42550 Macroeconomics,
University College Dublin.

**Try it in the browser (nothing to install):**
https://sam-deegan.com/toy-models/romer/

Current version: **1.0.6** (see [CHANGELOG.md](CHANGELOG.md)). The version
is shown in the app footer; releases are tagged `vX.Y.Z`.

## What it does

The stage selector adds one layer of the model at a time:

| Stage | What is added |
|---|---|
| 1 | Ideas and goods: output is A(1−a)L, so a worker moved into research costs output today |
| 2 | How technology evolves: the research equation, its two externalities (λ and φ), and the growth rate of ideas over time |
| 3 | Semi-endogenous growth: population growth, the phase diagram for g<sub>A</sub>, and where the growth rate settles |
| 4 | Endogenous growth: φ = 1, the research share sets the growth rate, and the scale effect that comes with it |
| 5 | Levels against growth: a permanent research push, the best research share, and what the evidence says about φ |
| 6 | The capital side: saving, depreciation, capital per effective worker and balanced growth |
| 7 | Level or growth: the balanced level of output against the research share, and the same push under φ < 1 and φ = 1 |

Each stage opens on a worked example (the cost of research today, ideas
getting harder to find, no population growth, a permanent research push,
capital catching up, the same push with φ < 1 and with φ = 1). Every slider
has a box beside it for an exact value, and once a slider moves the figures
draw a faded ghost of the loaded example alongside the live curve. The
Equations, Notation and In Words tabs show the model as it stands at the
chosen stage and flag what that stage changed.

## Run it locally

1. Install [R](https://cran.r-project.org/) (4.1 or later) and, ideally,
   [RStudio](https://posit.co/download/rstudio-desktop/).
2. Install the three packages once:

   ```r
   install.packages(c("shiny", "bslib", "ggplot2"))
   ```

3. Open `app.R` in RStudio and click **Run App**, or from R in this folder:

   ```r
   shiny::runApp()
   ```

Equations are typeset with MathJax from a CDN, so they need an internet
connection; everything else runs offline.

## Files

```
app.R          the app: settings and text (section B), figures (D),
               interface (E), server (F)
R/model.R      the model: the technology block (C_01) and the capital
               block (C_02). Sources on its own, so slides can reuse it.
R/toolkit.R    layout and helpers shared with the other toy-model apps
www/           QR code
README.md      this file
CHANGELOG.md   version history
CONVENTIONS.md how the figures and worked examples are laid out
LICENSE        CC BY-NC-ND 4.0
```

All text on screen (worked examples, prompts, equations, notation) is in
section `B_03` of `app.R`, so it can be edited without touching the rest.

## The model

The Romer model is the workhorse model of growth through ideas: output is
made by workers using a stock of non-rival ideas, and new ideas are made by
the workers set aside to look for them. This app uses the reduced form of
the Part 3 exam questions, in which the research share `a` is given rather
than chosen by firms, and follows Romer's *Advanced Macroeconomics* (2019,
chapter 3): the model without capital (section 3.2) at stages 1 to 5, and
the model with capital (section 3.3) at stages 6 and 7. Jones (1995) is the
source of the semi-endogenous case. Periods are years.

```
Goods:       Y = A (1 − a) L                                  (stages 1 to 5)
             Y = K^α (A L_Y)^(1−α),   L_Y = (1 − a) L          (stages 6 and 7)
Research:    Ȧ = θ (aL)^λ A^φ
Population:  L̇ / L = n
Capital:     K̇ = s Y − δ K
```

**Final goods** are made by the workers who are not in research, and each
of them is more productive the more ideas exist. From stage 6 goods also use
capital, with `α` the share of income going to capital.

**Research** turns researchers `aL` and the existing stock of ideas `A` into
new ideas. `θ` is the productivity of research; `λ ≤ 1` allows for
duplication (two researchers find less than twice as much); `φ` is how much
past ideas help, positive for standing on shoulders and negative for fishing
out.

**Population** grows at `n`, so the number of researchers grows with it even
at a constant research share.

**Capital** accumulates out of a fixed share `s` of output and wears out at
rate `δ`: the Solow accumulation equation, unchanged.

Dividing the research equation by `A` gives the growth rate of technology,
and differentiating that gives a differential equation in `g_A` alone:

```
g_A = θ (aL)^λ A^(φ−1)
ġ_A = g_A [ λn + (φ − 1) g_A ]
```

With `φ < 1` the second term is negative, so the growth rate is
self-correcting and settles at

```
g_A* = λn / (1 − φ)
```

which has population growth in it and the research share not at all. The
research share shows up in the *level* of technology, `A* ∝ a^{λ/(1−φ)}`,
so output per worker `(1 − a) A*` is humped in `a` and peaks at
`a† = κ / (1 + κ)` with `κ = λ / (1 − φ)`. With `φ = 1` the stock of ideas
drops out and `g_A = θ (aL)^λ`: the research share sets the growth rate
(endogenous growth), and so does the size of the workforce (the scale
effect).

Written per effective worker, `k̃ = K/(AL)` and `ỹ = Y/(AL)`, the capital
side is one more differential equation:

```
ỹ  = k̃^α (1 − a)^(1−α)
k̃̇  = s k̃^α (1 − a)^(1−α) − (n + g_A + δ) k̃
k̃* = [ s (1 − a)^(1−α) / (n + g_A* + δ) ]^{1/(1−α)}
ỹ* = [ s / (n + g_A* + δ) ]^{α/(1−α)} (1 − a)
```

The technology block is stepped forward in years; the capital block
integrates the two differential equations with a fourth-order Runge-Kutta
step, twenty steps a year, so capital lands on its balanced stock cleanly.
On the balanced path `k̃` is constant, so output per worker and capital per
worker both grow at `g_A` and at nothing else.

**What the seven stages show with it**

- *1* Output per worker today is `A(1 − a)`: raising the research share
  lowers it one for one, and nothing at this stage pays it back.
- *2* The growth rate of ideas over time against the stock of ideas: with
  `φ < 1` the stock keeps rising while its growth rate fades, because each
  new idea is a smaller share of a bigger stock.
- *3* The phase diagram `ġ_A` against `g_A`. Doubling population growth
  doubles the steady rate; doubling the research share does not move it.
  With `n = 0` the only resting point is zero.
- *4* At `φ = 1` the phase line lies flat, the growth rate rests wherever it
  starts, and the research share sets it; turning population growth back on
  makes the growth rate itself rise for ever, which is the scale effect the
  evidence does not support.
- *5* A permanent research push: growth jumps and returns to the same
  resting point while the stock of ideas stays permanently higher. A level
  effect and a growth effect look alike for about twenty years. The best
  research share `a†` maximises the level, not growth.
- *6* Capital per effective worker walks to `k̃*` from wherever it starts,
  and the growth rates of output per worker, capital per worker and
  technology converge on `g_A`. Saving more raises the level and not the
  growth rate.
- *7* The balanced level of output against the research share slopes plainly
  down when `φ < 1` while the growth rate beside it is flat: research costs
  level through `(1 − a)` and buys no growth. At `φ = 1` the same push makes
  the path permanently steeper. Same policy, same cost, opposite verdict.

**Where it departs from the textbook.** The research share is a parameter
set by the slider, not the outcome of firms' decisions under monopoly rents,
so the app is the reduced form of the Romer model rather than the full
model with patents and mark-ups. The technology block runs in discrete
yearly steps while the equations are written in continuous time. The
warnings for `φ > 1` (technology explodes in finite time) and for `φ = 1`
with `n > 0` (no balanced path) are the model's own corner cases, not
additions to it. The model has no human capital, no trade and no policy
block: it says what a research push does to the level and the growth rate,
not how a government would bring one about.

## References

- Romer, D. (2019). *Advanced Macroeconomics*, 5th ed. Chapter 3.
- Romer, P. M. (1990). Endogenous technological change. *Journal of
  Political Economy* 98(5).
- Jones, C. I. (1995). R&D-based models of economic growth. *Journal of
  Political Economy* 103(4).
- Gordon, R. J. (2016). *The Rise and Fall of American Growth*. Princeton
  University Press. Source of the six headwinds named in the slide figure
  `D_07_01`.

## Licence

© Sam Deegan. Released under
[CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/):
free to use and share for teaching with attribution; not for commercial use
or redistribution in modified form.
