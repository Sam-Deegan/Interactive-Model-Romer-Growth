################################################################################
## Project: ECON42550 Macroeconomics                                          ##
## Romer Endogenous Growth: Interactive Shiny App                             ##
################################################################################

## Author:      Sam Deegan
## Affiliation: University College Dublin
## Email:       sam.deegan@ucdconnect.ie

## Usage:
##   Open app.R in RStudio and click Run App, or from this folder:
##     shiny::runApp()
##   Needs R 4.1 or later with shiny, bslib and ggplot2 installed. A hosted
##   copy runs in the browser at https://sam-deegan.com/toy-models/romer/
##   The stage selector adds one layer of the model at a time:
##     1  ideas and goods: what moving a worker into research costs today
##     2  how technology evolves, and the two externalities in it
##     3  semi-endogenous growth: phi < 1, and where the growth rate settles
##     4  endogenous growth: phi = 1, and the scale effect that comes with it
##     5  levels against growth, and what the evidence says
##     6  the capital side: saving, depreciation and balanced growth
##     7  the research share: does it buy growth, or only cost level?
##   Periods are years. All text (scenarios, prompts, equations, notation)
##   lives in B_03.
##
## Inputs:
##   R/model.R (the model) and R/toolkit.R (shared layout and helpers),
##   both sourced automatically by Shiny. www/ holds the QR code.
##
## Outputs:
##   None. Figures for the slides are drawn by the same builders.
##
## Packages:
##   shiny, bslib, ggplot2.
##
## Version:
##   B_03_15_version_chr; history in CHANGELOG.md; git tag vX.Y.Z.
##
## References:
##   Romer, D. (2019). Advanced Macroeconomics, 5th ed. Ch. 3, sections 3.2
##     (the model without capital) and 3.3 (the model with capital).
##   Romer, P. (1990). Endogenous Technological Change. JPE 98(5).
##   Jones, C. (1995). R&D-Based Models of Economic Growth. JPE 103(4).
##   Gordon, R. J. (2016). The Rise and Fall of American Growth. Princeton
##     University Press, for the six headwinds in B_03_13.

#-------------------------------- Script Begin --------------------------------#

################################################################################
## A: Table of Contents ########################################################
################################################################################
# Note: C (the model) is in R/model.R and T (the toolkit) in R/toolkit.R.
#
#   B: Setup
#     B_01  Packages
#     B_02  Settings
#     B_03  Soft-coded objects
#     B_04  Paths
#   C: Model (R/model.R)
#   T: Toolkit (R/toolkit.R)
#   D: Plots
#     D_01  Technology over time and its own dynamics
#     D_02  The research share
#     D_03  Raising it
#     D_04  Capital and balanced growth
#     D_05  Level against growth
#     D_06  What research costs today (stage 1)
#     D_07  The headwinds (slide figure; not mounted in the UI)
#   E: User Interface
#   F: Server
#   G: Run

################################################################################
## B: Setup ####################################################################
################################################################################
# Note: Packages, options and every soft-coded value.

#### B_01: Packages ############################################################
# Note: Shiny for the app, bslib for the look, ggplot2 for the figures.

###### B_01_01: Load Packages ##################################################
# Note: All three run under shinylive.

library(shiny)
library(bslib)
library(ggplot2)

###### B_01_02: Load the Model #################################################
# Note: Shiny sources R/ itself; this covers sourcing app.R by hand.

if (!exists("C_01_02_steady_fn")) {
  source(file.path("R", "model.R"))
}

###### B_01_03: Load the Toolkit ###############################################
# Note: The shared palette, plot theme, CSS and builders.

if (!exists("T_01_01_palette_vec")) {
  source(file.path("R", "toolkit.R"))
}

#### B_02: Settings ############################################################
# Note: Standard options.

###### B_02_01: Global Options #################################################
# Note: No scientific notation; three digits in the console.

options(scipen = 999, digits = 3)

###### B_02_02: Seed ###########################################################
# Note: Nothing here is random; kept for consistency.

set.seed(42)

#### B_03: Soft-Coded Objects ##################################################
# Note: Calibration, stage list, scenarios, controls and text.

###### B_03_01: Input Defaults #################################################
# Note: Starting value of every control; Reset returns here. At these values
#   the steady growth rate of technology is lambda n / (1 - phi) = 1.25%.

B_03_01_defaults_lst <- list(
  theta     = 0.05,   # productivity of the research sector
  lambda    = 0.75,   # returns to research effort; below 1 means duplication
  phi       = 0.4,    # how much the existing stock of ideas helps
  a_res     = 0.10,   # share of the workforce in research
  n         = 0.01,   # population growth
  a_start   = 1,      # technology at the start
  l_start   = 1,      # workforce at the start
  n_periods = 250,    # years drawn
  shift_at  = 100,    # year the research share is raised
  shift_by  = 0.10,   # by how much
  alpha     = 0.33,   # share of income going to capital
  saving    = 0.25,   # share of output saved and invested
  delta     = 0.05,   # share of the capital stock wearing out each year
  k_start   = 1       # capital per effective worker at the start
)

###### B_03_02: Stages #########################################################
# Note: One layer of the model each. Stages 1 to 5 are the technology block
#   of lecture 3.3; 6 and 7 add the capital block of the exam questions.

B_03_02_stages_vec <- c(
  "Stage 1: Ideas and Goods"           = "1",
  "Stage 2: How Technology Evolves"    = "2",
  "Stage 3: Semi-Endogenous Growth"    = "3",
  "Stage 4: Endogenous Growth"         = "4",
  "Stage 5: Levels, Growth, Evidence"  = "5",
  "Stage 6: Capital and Output"        = "6",
  "Stage 7: Level or Growth?"          = "7"
)

###### B_03_03: Scenarios ######################################################
# Note: Worked examples. Each belongs to a stage and overrides some defaults;
#   unlisted controls return to B_03_01. Wording follows CONVENTIONS.md 3 to 5.

B_03_03_scenarios_lst <- list(
  tradeoff = list(
    label  = "The Cost of Research Today",
    stage  = "1",
    values = list(a_res = 0.3),
    story  = paste(
      "Three workers in ten are moved out of making goods and into finding",
      "them: the research share (a) is set to 0.30, so the part of the",
      "workforce still making things, (1−a)L, shrinks by the same three",
      "tenths. The economy slides down and to the right along the line: a",
      "rises on the horizontal axis and output per worker today, A(1−a),",
      "falls on the vertical one, by exactly the share of the workforce that",
      "has stopped producing. Nothing at this stage pays that back, because",
      "the stock of ideas (A) is still a fixed number, so the whole effect is",
      "the cost and a alone decides how large it is."
    ),
    prompt = paste(
      "Slide the research share up and watch output per worker fall. Nothing",
      "in this stage pays it back yet: that is what the next stage is for."
    )
  ),
  hard = list(
    label  = "Ideas Getting Harder to Find",
    stage  = "2",
    values = list(phi = 0, a_res = 0.1),
    story  = paste(
      "The ideas exponent (φ) is set to 0, so the existing stock of ideas is",
      "no help at all in finding the next one and the research equation",
      "Ȧ = θ(aL)<sup>λ</sup>A<sup>φ</sup> delivers a constant number of new",
      "ideas each year. Both figures run in years and their vertical axes go",
      "opposite ways: the stock of ideas (A) keeps climbing, while the growth",
      "rate of ideas (g<sub>A</sub> = θ(aL)<sup>λ</sup>A<sup>φ−1</sup>) falls",
      "towards zero, because a constant annual addition is a smaller and",
      "smaller proportion of a stock that is bigger every year. How fast it",
      "fades is set by φ through the A<sup>φ−1</sup> term; the research share",
      "(a = 0.10), research productivity (θ) and duplication (λ) scale the",
      "flow of ideas without changing that."
    ),
    prompt = paste(
      "Raise phi towards one and watch the growth rate stop falling. What is",
      "the economic claim being made when you set phi = 1?"
    )
  ),
  semi = list(
    label  = "Where the Growth Rate Settles",
    stage  = "3",
    values = list(phi = 0.4, n = 0.01, a_res = 0.1),
    story  = paste(
      "Population growth is switched on (n = 0.01) and the ideas exponent is",
      "below one (φ = 0.4), which gives the growth rate of ideas",
      "(g<sub>A</sub>) dynamics of its own: ġ<sub>A</sub> = g<sub>A</sub>(λn",
      "+ (φ−1)g<sub>A</sub>), the arch drawn on the phase diagram. Read both",
      "axes of it. Where g<sub>A</sub> on the horizontal axis is below",
      "λn/(1−φ), the change in the growth rate on the vertical axis is",
      "positive and g<sub>A</sub> is pushed right; where it is above, the",
      "vertical axis is negative and g<sub>A</sub> is pushed back left. The",
      "crossing g<sub>A</sub>* = λn/(1−φ) is a resting point, not an",
      "optimum, and it is set by population growth (n), duplication (λ) and",
      "φ — the research share (a) appears nowhere in it."
    ),
    prompt = paste(
      "Double population growth and watch the steady rate double. Then",
      "double the research share and watch it not move at all. That is the",
      "answer to part (c) of the exam question."
    )
  ),
  nopop = list(
    label  = "No Population Growth",
    stage  = "3",
    values = list(n = 0, phi = 0.4),
    story  = paste(
      "Population growth is set to zero (n = 0), so the workforce (L) stops",
      "growing and the number of researchers (aL) is fixed for ever. On the",
      "phase diagram the λn term vanishes and the arch is pulled down below",
      "the axis: at every positive growth rate of ideas (g<sub>A</sub>) on",
      "the horizontal axis the change in the growth rate on the vertical axis",
      "is negative, so the only crossing left is the origin. g<sub>A</sub>",
      "therefore falls to zero while the stock of ideas (A) goes on rising —",
      "a fixed number of researchers must keep raising a stock that is bigger",
      "every year, and with the ideas exponent (φ = 0.4) below one they",
      "eventually cannot. How quickly it dies away is set by φ."
    ),
    prompt = paste(
      "This is the uncomfortable prediction of semi-endogenous growth: with",
      "falling birth rates, what happens to long-run growth? Raise phi to 1",
      "to see the alternative."
    )
  ),
  endog = list(
    label  = "Fully Endogenous Growth",
    stage  = "4",
    values = list(phi = 1, n = 0, a_res = 0.1),
    story  = paste(
      "The ideas exponent is set to one (φ = 1): a proportional rise in the",
      "stock of ideas (A) is exactly as easy to produce as the last one, so",
      "A<sup>φ−1</sup> = 1 and A drops out of the growth rate altogether.",
      "With population growth off (n = 0) the phase line lies flat along the",
      "zero line: the change in the growth rate on the vertical axis is zero",
      "at every g<sub>A</sub> on the horizontal one, so wherever the growth",
      "rate of ideas starts it stays, and there is no resting point pulling",
      "it anywhere. What sets it instead is g<sub>A</sub> = θ(aL)<sup>λ</sup>",
      "— the research share (a = 0.10), research productivity (θ) and the",
      "size of the workforce (L), which is why the line beside it now slopes",
      "up in a. That last term is the scale effect: a bigger population",
      "should mean permanently faster growth, and the world's did not deliver",
      "it."
    ),
    prompt = paste(
      "Now double the research share and watch growth rise permanently. Then",
      "set population growth above zero and read the warning: the same",
      "assumption that makes policy powerful also predicts that growth",
      "should have accelerated as the world's population grew."
    )
  ),
  shift = list(
    label  = "A Permanent Research Push",
    stage  = "5",
    values = list(shift_at = 80, shift_by = 0.15, phi = 0.4, n = 0.01),
    story  = paste(
      "In year 80 the research share (a) is raised by 0.15 and held there,",
      "so the annual flow of new ideas jumps while the ideas exponent stays",
      "at φ = 0.4. The phase diagram itself does not move, because",
      "ġ<sub>A</sub> = g<sub>A</sub>(λn + (φ−1)g<sub>A</sub>) has no a in it:",
      "the economy is knocked rightwards along the horizontal axis to a",
      "higher growth rate of ideas (g<sub>A</sub>), finds the vertical axis",
      "negative there, and is pushed back to the same resting point",
      "g<sub>A</sub>* = λn/(1−φ). The two figures beside it say the same",
      "thing twice: the growth line jumps and returns to the path it was",
      "already on, while the level of the stock of ideas (A) stays",
      "permanently above the old one. This is a level effect, not a growth",
      "effect — how long the jump lasts is set by φ, and how much level it",
      "buys by λ/(1−φ)."
    ),
    prompt = paste(
      "Watch the growth line return to its old level while the technology",
      "line stays above the old path for ever. Being able to tell those two",
      "apart is most of what this lecture is for."
    )
  ),
  catchup = list(
    label  = "Capital Catching Up",
    stage  = "6",
    values = list(k_start = 1, phi = 0.4, n = 0.01, a_res = 0.1),
    story  = paste(
      "The economy starts with capital per effective worker (k̃<sub>0</sub> =",
      "1) well below the level the balanced path calls for, so investment",
      "sk̃<sup>α</sup>(1−a)<sup>1−α</sup> runs above the line",
      "(n + g<sub>A</sub> + δ)k̃ that just keeps up with wear, extra workers",
      "and better ideas. On the first figure k̃ climbs year by year towards",
      "its resting point k̃*; on the second, the growth rate of output per",
      "worker sits above the growth rate of ideas (g<sub>A</sub>) for as long",
      "as that climb lasts and then settles onto it. How long is set by the",
      "capital share (α) and by (n + g<sub>A</sub>* + δ): capital deepening",
      "is a transition, and in the long run output per worker and capital per",
      "worker grow at g<sub>A</sub> and at nothing else."
    ),
    prompt = paste(
      "Watch the three growth lines come together. Capital deepening is a",
      "transition, not a source of growth: the only thing still growing in",
      "the long run is the stock of ideas."
    )
  ),
  saveup = list(
    label  = "Saving More",
    stage  = "6",
    values = list(saving = 0.4, k_start = 1, phi = 0.4, n = 0.01),
    story  = paste(
      "The saving rate (s) is raised to 0.40, which lifts the investment",
      "curve sk̃<sup>α</sup>(1−a)<sup>1−α</sup> above the replacement line",
      "(n + g<sub>A</sub> + δ)k̃ at the old crossing. Capital per effective",
      "worker (k̃) therefore climbs to a much higher resting point k̃*, and",
      "output per effective worker (ỹ = k̃<sup>α</sup>(1−a)<sup>1−α</sup>)",
      "with it; on the convergence figure the growth rates rise above the",
      "growth rate of ideas (g<sub>A</sub>) while the climb is on and come",
      "back to it exactly when k̃ stops moving. The long-run rate is still",
      "λn/(1−φ), which has no s in it: saving is a level effect and not a",
      "growth effect, and how much level it buys is set by the capital share",
      "through α/(1−α)."
    ),
    prompt = paste(
      "Compare the balanced stock of capital with saving at 0.25 and at",
      "0.40, then look at where the growth lines settle. Two very different",
      "levels, one growth rate."
    )
  ),
  cost = list(
    label  = "A Research Push with φ < 1",
    stage  = "7",
    values = list(phi = 0.4, n = 0.01, a_res = 0.1, shift_by = 0.25,
                  n_periods = 500),
    story  = paste(
      "A quarter of the workforce is moved into research (the push is Δa =",
      "0.25) while the ideas exponent stays below one (φ = 0.4). The phase",
      "diagram does not move, because ġ<sub>A</sub> = g<sub>A</sub>(λn +",
      "(φ−1)g<sub>A</sub>) has no a in it: the growth rate of ideas",
      "(g<sub>A</sub>) is knocked to the right on the horizontal axis, meets",
      "a negative change on the vertical axis, and is drawn back to the same",
      "resting point g<sub>A</sub>* = λn/(1−φ). On the log-scale push figure",
      "that is two lines ending parallel — a permanently higher level of",
      "output per worker and an unchanged slope — which takes the 500 years",
      "drawn here to be visible. What the push is worth is therefore a level",
      "question: the stock of ideas rises with a<sup>λ/(1−φ)</sup> while",
      "production loses (1−a), and the balanced level of output per effective",
      "worker (ỹ*) slopes plainly down in a."
    ),
    prompt = paste(
      "Read the level line: it slopes down and the growth line is flat. The",
      "push still leaves the economy richer through a higher stock of ideas,",
      "but it buys no extra growth at all. This is the central result of",
      "semi-endogenous growth."
    )
  ),
  buy = list(
    label  = "The Same Push with φ = 1",
    stage  = "7",
    values = list(phi = 1, n = 0, a_res = 0.1, shift_by = 0.25,
                  n_periods = 500),
    story  = paste(
      "The identical push (Δa = 0.25) under the endogenous assumption, φ = 1",
      "and n = 0. The stock of ideas (A) has dropped out of g<sub>A</sub> =",
      "θ(aL)<sup>λ</sup>, so the phase line lies flat on zero: the change in",
      "the growth rate on the vertical axis is nil at every g<sub>A</sub> on",
      "the horizontal one, the economy simply stays wherever the research",
      "share (a) puts it, and raising a moves it permanently right. The level",
      "still falls on impact by the same (1−a) — those workers are still gone",
      "from production — so on the log-scale figure the pushed line starts",
      "below the old one and is permanently steeper, and it crosses only",
      "after enough years have passed. Same policy and same cost as the",
      "example above; a growth effect here, a level effect there, and the",
      "verdict turns entirely on whether φ is one or a little below it."
    ),
    prompt = paste(
      "Compare this with the scenario above it. Same push, same cost, and",
      "the two models disagree completely about whether it was worth doing.",
      "Everything turns on whether φ is one or a little below it."
    )
  )
)

###### B_03_04: Controls #######################################################
# Note: One entry per numeric control: label, range, step and the stage from
#   which it appears.

B_03_04_controls_lst <- list(
  a_res     = list(label = "Share of Workers in Research (a)",
                   min = 0.01, max = 0.8, step = 0.01, from = 1),
  theta     = list(label = "Productivity of Research (θ)",
                   min = 0.01, max = 0.3, step = 0.01, from = 2),
  lambda    = list(label = "Returns to Research Effort (λ)",
                   min = 0.2, max = 1.2, step = 0.05, from = 2),
  phi       = list(label = "How Much Past Ideas Help (φ)",
                   min = -0.5, max = 1, step = 0.05, from = 2),
  n         = list(label = "Population Growth (n)",
                   min = 0, max = 0.04, step = 0.01, from = 3),
  a_start   = list(label = "Technology at the Start (A<sub>0</sub>)",
                   min = 0.05, max = 5, step = 0.05, from = 2),
  l_start   = list(label = "Workforce at the Start (L<sub>0</sub>)",
                   min = 0.2, max = 5, step = 0.1, from = 1),
  n_periods = list(label = "Years Drawn",
                   min = 50, max = 500, step = 25, from = 2),
  shift_at  = list(label = "Year the Share Is Raised",
                   min = 10, max = 300, step = 10, from = 5),
  shift_by  = list(label = "Raised By",
                   min = 0, max = 0.4, step = 0.05, from = 5),
  alpha     = list(label = "Share of Income to Capital (α)",
                   min = 0.1, max = 0.6, step = 0.01, from = 6),
  saving    = list(label = "Share of Output Saved (s)",
                   min = 0.05, max = 0.6, step = 0.01, from = 6),
  delta     = list(label = "Depreciation (δ)",
                   min = 0.01, max = 0.15, step = 0.01, from = 6),
  k_start   = list(label = "Capital at the Start (k&#771;<sub>0</sub>)",
                   min = 0.1, max = 20, step = 0.1, from = 6)
)

###### B_03_05: Parameter Explanations #########################################
# Note: Tooltip text: what each control is and what raising it does.

B_03_05_help_lst <- list(
  a_res = paste(
    "The share of the workforce doing research rather than making things.",
    "It costs output today. Whether it buys growth or only a higher level",
    "depends entirely on φ, which is the point of the model."
  ),
  theta = paste(
    "How productive researchers are. It scales the growth rate in the",
    "endogenous case and only the level in the semi-endogenous one."
  ),
  lambda = paste(
    "Returns to research effort. Below one means duplication: two",
    "researchers produce less than twice as much as one, because they trip",
    "over each other's work."
  ),
  phi = paste(
    "How much the existing stock of ideas helps in producing new ones.",
    "Positive is standing on the shoulders of giants; negative is fishing",
    "out. The single most important parameter here: below one growth is",
    "semi-endogenous, at one it is endogenous, above one it explodes."
  ),
  n = paste(
    "Population growth. In the semi-endogenous case it is the ONLY source of",
    "long-run growth, because it is the only thing that keeps adding",
    "researchers."
  ),
  a_start = "The stock of ideas at the start of the simulation.",
  l_start = "The size of the workforce at the start.",
  n_periods = "How many years of the path are drawn.",
  shift_at = "The year in which the research share is permanently raised.",
  shift_by = paste(
    "How much the research share is raised by. In the semi-endogenous case",
    "this is a level effect dressed up as a growth effect, and telling the",
    "two apart is the exercise."
  ),
  alpha = paste(
    "The share of income going to capital rather than to labour. It is also",
    "how fast the returns to piling up more capital run out: the higher it",
    "is, the further capital deepening can carry output before it stalls.",
    "A third is the usual number for an advanced economy."
  ),
  saving = paste(
    "The share of output saved and turned into new capital. Raising it",
    "raises the level of output on the balanced path and does nothing at all",
    "to the long-run growth rate — the Solow result, which survives having",
    "the growth of technology explained rather than assumed."
  ),
  delta = paste(
    "How much of the capital stock wears out each year. It joins population",
    "growth and the growth of technology in (n + g<sub>A</sub> + δ), the",
    "rate at which investment has to run just to keep capital per effective",
    "worker where it is."
  ),
  k_start = paste(
    "Capital per effective worker at the start of the simulation. Start it",
    "below the balanced level and the economy grows faster than technology",
    "while it catches up; start it above and slower. Either way the",
    "difference is a transition, not growth."
  )
)

###### B_03_06: Prompts ########################################################
# Note: One "what to try" prompt per stage, shown above the figures.

B_03_06_prompts_lst <- list(
  "1" = paste(
    "Output is A(1−a)L. A researcher is a worker not making anything, so",
    "raising the research share lowers output today, one for one. The rest",
    "of the model is about what you get back."
  ),
  "2" = paste(
    "Technology grows at θ(aL)^λ A^(φ−1). Move φ and watch what happens as",
    "the stock of ideas grows: below one, growth fades as A rises; at one it",
    "does not fade at all."
  ),
  "3" = paste(
    "The growth rate has dynamics of its own: ġ = g(λn + (φ−1)g). It settles",
    "where that is zero, at λn/(1−φ). Change the research share and watch",
    "the steady rate not move."
  ),
  "4" = paste(
    "Set φ to 1 and population growth to 0. Now the research share sets the",
    "growth rate, and policy can raise it for ever. Then turn population",
    "growth back on and read the warning about scale effects."
  ),
  "5" = paste(
    "Raise the research share part-way through the simulation. Growth jumps",
    "and then returns; technology stays permanently higher. A level effect",
    "and a growth effect look identical for about twenty years, which is",
    "why they are so easily confused."
  ),
  "6" = paste(
    "Output is now K^α(AL_Y)^(1−α) and capital accumulates out of saving.",
    "Written per effective worker the model is two differential equations:",
    "one in g_A, one in k̃. Move the starting capital stock and watch k̃",
    "walk to the same place either way, with output per worker and capital",
    "per worker both settling at growth of g_A."
  ),
  "7" = paste(
    "Now put the two halves together. Raising the research share costs",
    "output through (1−a) and, with φ<1, buys nothing in growth, because",
    "λn/(1−φ) has no a in it. Set φ to 1 and the growth line tilts up.",
    "Same policy, same cost, opposite verdict."
  )
)

###### B_03_07: The Model, Stage by Stage ######################################
# Note: The equations panel. "versions" maps the stage a form first applies
#   from to its LaTeX; "notes" holds the In Words text for the same stages.

B_03_07_equations_lst <- list(

  # --- The model's equations --------------------------------------------------
  list(
    group = "model", label = "Final Goods",
    versions = list(
      "1" = "Y = A\\,(1 - a)\\,L",
      "6" = "Y = K^{\\alpha}\\,(A\\,L_Y)^{1-\\alpha}"
    ),
    notes = list(
      "1" = paste("Output uses the workers who are not in research, and each",
                  "of them is more productive the more ideas exist."),
      "6" = paste("Goods are now made with capital as well as effective",
                  "labour, and L_Y = (1−a)L is the part of the workforce",
                  "still making them. Setting α = 0 gives back the version",
                  "stages 1 to 5 used.")
    )
  ),
  list(
    group = "model", label = "Capital",
    versions = list("6" = "\\dot{K} = s\\,Y - \\delta\\,K"),
    notes = list(
      "6" = paste("A fixed share of output is saved and invested, and a",
                  "fixed share of the existing stock wears out. This is the",
                  "Solow accumulation equation, unchanged.")
    )
  ),
  list(
    group = "model", label = "Research",
    versions = list("2" = paste0("\\dot{A} = \\theta\\,(a L)^{\\lambda}",
                                 "\\,A^{\\phi}")),
    notes = list(
      "2" = paste("New ideas come from researchers and from the ideas that",
                  "already exist. λ and φ are the two externalities, and",
                  "everything in the model turns on them.")
    )
  ),
  list(
    group = "model", label = "Population",
    versions = list("3" = "\\dot{L}/L = n"),
    notes = list(
      "3" = paste("The workforce grows, so the number of researchers grows",
                  "with it even at a constant research share.")
    )
  ),

  # --- Assumptions ------------------------------------------------------------
  list(
    group = "assumption", label = "Ideas Are Non-Rival",
    versions = list("1" = "A \\text{ is used by every worker at once}"),
    notes = list(
      "1" = paste("The central idea of the paper. A blueprint used by one",
                  "firm is not used up, so there are increasing returns to",
                  "ideas and labour together, and the competitive model has",
                  "to be abandoned.")
    )
  ),
  list(
    group = "assumption", label = "Duplication",
    versions = list("2" = "\\lambda \\le 1"),
    notes = list(
      "2" = paste("Two researchers find less than twice as much, because",
                  "some of the time they are finding the same thing.")
    )
  ),
  list(
    group = "assumption", label = "Standing on Shoulders",
    versions = list("2" = "\\phi \\lessgtr 0"),
    notes = list(
      "2" = paste("Positive φ: past ideas make new ones easier. Negative:",
                  "the easy things have been found. The sign is an empirical",
                  "question and the model's answer depends on it entirely.")
    )
  ),
  list(
    group = "assumption", label = "A Fixed Research Share",
    versions = list("1" = "a \\text{ given}"),
    notes = list(
      "1" = paste("Not chosen by anyone here. In the full Romer model firms",
                  "choose it, and patents give them the monopoly rent that",
                  "makes it worth doing.")
    )
  ),
  list(
    group = "assumption", label = "Where the Workers Go",
    versions = list("6" = "L = L_A + L_Y,\\quad L_A = a\\,L"),
    notes = list(
      "6" = paste("Every worker is either finding ideas or making goods.",
                  "Nothing is lost between the two, which is why the cost of",
                  "research is exactly the (1−a) in the production function.")
    )
  ),
  list(
    group = "assumption", label = "Constant Returns in K and AL",
    versions = list("6" = "\\alpha \\in (0, 1)"),
    notes = list(
      "6" = paste("Double the capital and the effective labour together and",
                  "output doubles. That is what lets the model be written per",
                  "effective worker at all, and what makes capital deepening",
                  "run out of road on its own.")
    )
  ),

  # --- Solved forms -----------------------------------------------------------
  list(
    group = "solved", label = "Growth of Technology",
    versions = list("2" = paste0("g_A = \\frac{\\dot{A}}{A} = \\theta\\,",
                                 "(aL)^{\\lambda} A^{\\phi - 1}")),
    notes = list(
      "2" = paste("Part (a) of the exam question: divide the research",
                  "equation by A. Note the A^(φ−1) term, which is the whole",
                  "story.")
    )
  ),
  list(
    group = "solved", label = "Its Own Dynamics",
    versions = list("3" = paste0("\\frac{\\dot{g_A}}{g_A} = \\lambda n +",
                                 " (\\phi - 1) g_A")),
    notes = list(
      "3" = paste("Part (b). Differentiate the growth rate and use",
                  "L̇/L = n. With φ < 1 the second term is negative, so the",
                  "growth rate is self-correcting.")
    )
  ),
  list(
    group = "solved", label = "The Steady Growth Rate",
    versions = list("3" = "g_A^* = \\frac{\\lambda n}{1 - \\phi}"),
    notes = list(
      "3" = paste("Part (c). Population growth matters; the research share",
                  "does not. This is semi-endogenous growth, and it is Jones'",
                  "correction to Romer.")
    )
  ),
  list(
    group = "solved", label = "The Level of Technology",
    versions = list("3" = paste0("A^* \\propto a^{\\lambda/(1-\\phi)}")),
    notes = list(
      "3" = paste("Where the research share does show up. More research",
                  "raises the LEVEL of technology permanently, which is not",
                  "nothing — it just is not growth.")
    )
  ),
  list(
    group = "solved", label = "The Endogenous Case",
    versions = list("4" = "\\phi = 1:\\quad g_A = \\theta (aL)^{\\lambda}"),
    notes = list(
      "4" = paste("The stock of ideas drops out, so the growth rate depends",
                  "on the research share and can be raised by policy for",
                  "ever. It also depends on L, which is the scale effect.")
    )
  ),
  list(
    group = "solved", label = "The Best Research Share",
    versions = list("5" = paste0("a^{\\dagger} = \\frac{\\kappa}{1+\\kappa},",
                                 "\\quad \\kappa = \\frac{\\lambda}",
                                 "{1-\\phi}")),
    notes = list(
      "5" = paste("Maximising the LEVEL of output per worker on the balanced",
                  "path. Too few researchers and there are no ideas; too",
                  "many and nobody is making anything.")
    )
  ),
  list(
    group = "solved", label = "Output per Effective Worker",
    versions = list("6" = paste0("\\tilde{y} = \\tilde{k}^{\\alpha}",
                                 "\\,(1 - a)^{1-\\alpha}")),
    notes = list(
      "6" = paste("Dividing output by AL. Only two things are left: how much",
                  "capital there is per effective worker, and how much of",
                  "the workforce is making goods with it.")
    )
  ),
  list(
    group = "solved", label = "Capital per Effective Worker",
    versions = list("6" = paste0("\\dot{\\tilde{k}} = s\\,\\tilde{k}^{",
                                 "\\alpha}(1-a)^{1-\\alpha} - (n + g_A +",
                                 " \\delta)\\,\\tilde{k}")),
    notes = list(
      "6" = paste("The capital equation written per effective worker.",
                  "Investment on the left of the minus sign, and on the",
                  "right everything that has to be kept up with: wear, more",
                  "workers, and better technology.")
    )
  ),
  list(
    group = "solved", label = "The Balanced Stock of Capital",
    versions = list("6" = paste0("\\tilde{k}^* = \\left[\\frac{s\\,",
                                 "(1-a)^{1-\\alpha}}{n + g_A^* + \\delta}",
                                 "\\right]^{1/(1-\\alpha)}")),
    notes = list(
      "6" = paste("Where capital per effective worker settles. Once it is",
                  "there, output per worker and capital per worker both grow",
                  "at exactly g_A: capital adds no growth of its own.")
    )
  ),
  list(
    group = "solved", label = "The Balanced Level of Output",
    versions = list("7" = paste0("\\tilde{y}^* = \\left(\\frac{s}{n + g_A^*",
                                 " + \\delta}\\right)^{\\alpha/(1-\\alpha)}",
                                 "(1 - a)")),
    notes = list(
      "7" = paste("Capital deepening turns out to be a constant multiplier,",
                  "so the research share's cost is the plain (1−a) it always",
                  "was. With φ < 1 the bracket has no a in it either, so the",
                  "whole effect of research on the level is that minus sign.")
    )
  ),

  # --- Descriptors ------------------------------------------------------------
  list(
    group = "descriptor", label = "Level Against Growth",
    versions = list("5" = paste0("\\Delta a: \\ \\text{level} \\uparrow,",
                                 "\\ \\text{growth}^* \\text{ unchanged}")),
    notes = list(
      "5" = paste("In the semi-endogenous case a research push raises",
                  "growth for a couple of decades and the level for ever.",
                  "Over a sample of twenty years the two are almost",
                  "impossible to tell apart, which is why the debate",
                  "persists.")
    )
  ),
  list(
    group = "descriptor", label = "The Scale Effect",
    versions = list("4" = "\\phi = 1:\\ \\partial g_A / \\partial L > 0"),
    notes = list(
      "4" = paste("A bigger population should mean faster growth. The world",
                  "population has risen enormously and growth in the frontier",
                  "economies has not accelerated, which is the evidence",
                  "against φ = 1 and the reason for the semi-endogenous",
                  "version.")
    )
  ),
  list(
    group = "descriptor", label = "Ideas Are Getting Harder to Find",
    versions = list("5" = "\\phi < 1 \\text{ in the data}"),
    notes = list(
      "5" = paste("Research effort has risen by orders of magnitude while",
                  "growth has been flat or falling, in aggregate and in",
                  "individual industries. That pattern is exactly what",
                  "φ < 1 predicts.")
    )
  ),
  list(
    group = "descriptor", label = "Balanced Growth",
    versions = list("6" = paste0("g_{Y/L} = g_{K/L} = g_A \\quad",
                                 "(\\tilde{k} \\text{ constant})")),
    notes = list(
      "6" = paste("The answer to the sample question: once capital per",
                  "effective worker has stopped moving, output per worker",
                  "and capital per worker grow at the growth rate of",
                  "technology and at nothing else.")
    )
  ),
  list(
    group = "descriptor", label = "Saving Is a Level Effect",
    versions = list("6" = paste0("\\partial \\tilde{y}^*/\\partial s > 0,",
                                 "\\quad \\partial g_A^*/\\partial s = 0")),
    notes = list(
      "6" = paste("Saving more raises the balanced stock of capital and the",
                  "level of output that goes with it, and leaves the growth",
                  "rate alone. Solow's result, carried over intact.")
    )
  ),
  list(
    group = "descriptor", label = "Research: Level or Growth",
    versions = list("7" = paste0("\\phi < 1:\\ \\partial \\tilde{y}^*/",
                                 "\\partial a < 0,\\ \\partial g_A^*/",
                                 "\\partial a = 0")),
    notes = list(
      "7" = paste("With φ below one a research push costs level and buys no",
                  "growth whatsoever. With φ = 1 it costs the same level and",
                  "raises the growth rate for ever. The policy is identical;",
                  "the verdict depends entirely on one parameter.")
    )
  )
)

###### B_03_08: Equation Group Titles ##########################################
# Note: Group headings in the equations tabs.

B_03_08_groups_vec <- c(
  model      = "Model Equations",
  assumption = "Assumptions",
  solved     = "Solved Forms",
  descriptor = "Descriptors"
)

###### B_03_09: Notation Key ###################################################
# Note: Notation tab. Groups: var, par, flw (results); "from" is the first
#   stage the symbol appears at.

B_03_09_notation_lst <- list(
  list(grp = "var", sym = "Y", txt = "output", from = 1),
  list(grp = "var", sym = "A", txt = "the stock of ideas", from = 1),
  list(grp = "var", sym = "L", txt = "the workforce", from = 1),
  list(grp = "par", sym = "a", txt = "share of workers in research", from = 1),
  list(grp = "par", sym = "\\theta", txt = "productivity of research",
       from = 2),
  list(grp = "par", sym = "\\lambda", txt = "returns to research effort",
       from = 2),
  list(grp = "par", sym = "\\phi", txt = "how much past ideas help", from = 2),
  list(grp = "var", sym = "g_A", txt = "growth rate of technology", from = 2),
  list(grp = "par", sym = "n", txt = "population growth", from = 3),
  list(grp = "flw", sym = "g_A^*", txt = "the steady growth rate", from = 3),
  list(grp = "flw", sym = "\\kappa", txt = "lambda over one minus phi",
       from = 5),
  list(grp = "flw", sym = "a^{\\dagger}", txt = "the best research share",
       from = 5),
  list(grp = "var", sym = "K", txt = "the capital stock", from = 6),
  list(grp = "var", sym = "L_Y", txt = "workers making goods", from = 6),
  list(grp = "var", sym = "L_A", txt = "workers finding ideas", from = 6),
  list(grp = "var", sym = "\\tilde{k}", txt = "capital per effective worker",
       from = 6),
  list(grp = "var", sym = "\\tilde{y}", txt = "output per effective worker",
       from = 6),
  list(grp = "par", sym = "\\alpha", txt = "share of income to capital",
       from = 6),
  list(grp = "par", sym = "s", txt = "share of output saved", from = 6),
  list(grp = "par", sym = "\\delta", txt = "depreciation", from = 6),
  list(grp = "flw", sym = "\\tilde{k}^*", txt = "the balanced stock of capital",
       from = 6),
  list(grp = "flw", sym = "\\tilde{y}^*", txt = "the balanced level of output",
       from = 7)
)

###### B_03_10: Notation Columns ###############################################
# Note: How the notation tab is split into columns.

B_03_10_nota_cols_lst <- list(
  "Variables"  = "var",
  "Parameters" = "par",
  "Results"    = "flw"
)

###### B_03_11: Figure Heights #################################################
# Note: Height of the main figures in the browser.

B_03_11_tall_chr <- "410px"

###### B_03_12: Recalculation Delay ############################################
# Note: Milliseconds to wait for further changes before recalculating.

B_03_12_debounce_ms_int <- 250L

###### B_03_13: Gordon's Headwinds #############################################
# Note: The six headwinds Gordon (2016) names against growth in US output per
#   head, in his order. The names are his; the sizes are not in this app, so
#   D_07_01 divides B_03_14 of the balanced rate equally between them and says
#   so on its face. For Gordon's own magnitudes, replace this with a named
#   numeric vector of percentage points and give D_07_01 the figures to draw.

B_03_13_headwinds_vec <- c(
  "Demography",
  "Education",
  "Inequality",
  "Globalisation",
  "Energy and the environment",
  "Consumer and government debt"
)

###### B_03_14: How Much the Headwinds Take ####################################
# Note: The share of the starting growth rate the headwinds remove between
#   them in D_07_01. A drawing choice, not an estimate.

B_03_14_headwind_cut_num <- 0.5

###### B_03_15: Version ########################################################
# Note: Semantic version, shown in the footer; CHANGELOG.md has the history.

B_03_15_version_chr <- "1.0.8"

###### B_03_16: Source Repository ##############################################
# Note: The GitHub repo, linked from the footer.

B_03_16_repo_chr <- paste0("https://github.com/Sam-Deegan/",
                        "Interactive-Model-Romer-Growth")

#### B_04: Paths ###############################################################
# Note: The QR code for the credit.

###### B_04_01: QR Code Source #################################################
# Note: The QR image: www/ if present, else the toolkit's own copy.

B_04_01_qr_src_chr <- T_07_04_qr_fn()

################################################################################
## D: Plots ####################################################################
################################################################################
# Note: Builders only; each returns a ggplot for the server to draw. Figure
#   conventions are CONVENTIONS.md 6.

#### D_01: Technology Over Time ################################################
# Note: The path of ideas, and the dynamics of the growth rate itself.

###### D_01_01: The Growth Rate Over Time ######################################
# Note: The growth rate of technology along the simulated path, against the
#   steady rate it is heading for.

D_01_01_growth_fn <- function(par, stage, ref = NULL) {
  path <- C_01_04_path_fn(par)
  path <- path[!is.na(path$g_tech), ]
  g_ss <- C_01_02_steady_fn(par)

  rests <- !is.na(g_ss) && g_ss > 0

  # Ghost: the same path at the worked example's settings, drawn first
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_df <- C_01_04_path_fn(ref)
    g_df <- g_df[!is.na(g_df$g_tech), ]
    T_02_03a_ghost_line_fn(g_df, aes(x = period, y = g_tech),
                           colour = T_01_02_series_vec[["main"]],
                           linewidth = 1.1)
  }

  p <- ggplot(path, aes(x = period, y = g_tech)) +
    T_02_02_zero_fn(v = FALSE) +
    (if (rests) T_02_02_rest_fn(h = g_ss)) +
    ghost_lyr +
    geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.1) +
    # The steady rate is named on the right-hand axis; see CONVENTIONS.md 6
    (if (rests) T_02_02_mark_y_fn(g_ss, expression(g[A]^"*"))) +
    labs(
      title = if (stage >= 4 && par$phi >= 1) {
        "The Growth Rate of Ideas Does Not Fade"
      } else if (isTRUE(g_ss <= 0)) {
        "The Growth Rate of Ideas Fades Away to Zero"
      } else {
        paste0("The Growth Rate of Ideas Settles at ",
               if (is.na(g_ss)) "No Finite Rate" else T_02_06_pct_fn(g_ss, 2))
      },
      x = expression(bold("Year (" * t * ")")),
      y = expression(bold("Growth rate of ideas (" * g[A] * ")")),
      caption = paste(
        "As the stock of ideas grows, each new idea is a smaller proportional",
        "addition. Whether that fades to zero or to something positive is",
        "what phi decides."
      )
    ) +
    T_02_01_theme_fn()

  p
}

###### D_01_02: The Dynamics of the Growth Rate ################################
# Note: The phase line from part (b), gdot_A = g_A (lambda n + (phi - 1) g_A),
#   with zero named on the right-hand axis and the crossing g_A* along the top.

D_01_02_phase_fn <- function(par, ref = NULL) {
  g_ss <- C_01_02_steady_fn(par)
  dyn  <- C_01_03_dynamics_fn(par, max(g_ss * 1.8, 0.02, na.rm = TRUE))
  rests <- !is.na(g_ss) && g_ss > 0

  # Ghost: the locus and its crossing at the worked example's settings
  ghost_ss  <- if (is.null(ref)) NA_real_ else C_01_02_steady_fn(ref)
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    list(
      T_02_03a_ghost_line_fn(
        C_01_03_dynamics_fn(ref, max(ghost_ss * 1.8, 0.02, na.rm = TRUE)),
        aes(x = g_a, y = gdot),
        colour = T_01_02_series_vec[["main"]], linewidth = 1.1),
      if (!is.na(ghost_ss) && ghost_ss > 0) {
        T_02_03a_ghost_point_fn(ghost_ss, 0)
      }
    )
  }

  ggplot(dyn, aes(x = g_a, y = gdot)) +
    T_02_02_zero_fn(v = FALSE) +
    (if (rests) T_02_02_rest_fn(v = g_ss)) +
    ghost_lyr +
    geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.1) +
    (if (rests) T_02_03_point_fn(g_ss, 0)) +
    T_02_02_mark_y_fn(0, expression(dot(g)[A] == 0)) +
    (if (rests) T_02_02_mark_x_fn(g_ss, expression(g[A]^"*"))) +
    labs(
      title = if (isTRUE(par$phi >= 1) && isTRUE(par$n <= 0)) {
        "The Growth Rate of Ideas Rests Wherever It Starts"
      } else if (isTRUE(par$phi >= 1)) {
        "The Growth Rate of Ideas Never Stops Rising"
      } else if (is.na(g_ss) || g_ss <= 0) {
        "The Growth Rate of Ideas Is Pulled Towards Zero"
      } else {
        paste0("The Growth Rate of Ideas Is Pulled Towards ",
               T_02_06_pct_fn(g_ss, 2))
      },
      x = expression(bold("Growth rate of ideas (" * g[A] * ")")),
      y = expression(bold("Change in the growth rate (" * dot(g)[A] * ")")),
      caption = paste(
        "Above zero the growth rate is rising, below zero it is falling.",
        "The crossing is where it settles."
      )
    ) +
    T_02_01_theme_fn(grid = "none")
}

###### D_01_03: The Stock of Ideas Over Time ###################################
# Note: The stock of ideas on a log scale: it keeps rising while the growth
#   rate in D_01_01 falls, and the two are read together at stage 2.

D_01_03_tech_fn <- function(par, ref = NULL) {
  path <- C_01_04_path_fn(par)
  g_ss <- C_01_02_steady_fn(par)
  n_t  <- nrow(path)

  # Log axis, so the ghost keeps strictly positive values only
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_df <- C_01_04_path_fn(ref)
    g_df <- g_df[is.finite(g_df$tech) & g_df$tech > 0, ]
    g_n  <- nrow(g_df)
    if (g_n == 0) NULL else list(
      T_02_03a_ghost_line_fn(g_df, aes(x = period, y = tech),
                             colour = T_01_02_series_vec[["main"]],
                             linewidth = 1.1),
      T_02_03a_ghost_point_fn(g_df$period[g_n], g_df$tech[g_n])
    )
  }

  # A log axis has no zero, so no zero cross here
  ggplot(path, aes(x = period, y = tech)) +
    ghost_lyr +
    geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.1) +
    scale_y_log10(expand = expansion(mult = c(0.05, 0.09))) +
    T_02_03_point_fn(path$period[n_t], path$tech[n_t]) +
    labs(
      title = paste0("The Stock of Ideas Reaches ",
                     T_02_05_num_fn(path$tech[n_t], 1), " After ", n_t,
                     " Years"),
      x = expression(bold("Year (" * t * ")")),
      y = expression(bold("Stock of ideas (" * A * "), log scale")),
      caption = if (isTRUE(g_ss <= 0)) {
        paste("Ideas keep arriving and the stock keeps rising. What falls is",
              "the growth RATE, because each new idea is a smaller share of a",
              "stock that is bigger every year. On a log scale a fading",
              "growth rate is a line that keeps climbing and keeps flattening.")
      } else {
        paste("On a log scale a constant growth rate is a straight line. The",
              "path bends until the growth rate has settled and is straight",
              "afterwards.")
      }
    ) +
    T_02_01_theme_fn()
}

#### D_02: The Research Share ##################################################
# Note: What raising it does, to growth and to the level.

###### D_02_01: Growth Against the Research Share ##############################
# Note: The figure that separates semi-endogenous from endogenous growth. A
#   flat line means policy cannot touch the growth rate.

D_02_01_share_growth_fn <- function(par, ref = NULL) {
  now_fn <- function(p) {
    if (p$phi >= 1) {
      p$theta * (p$a_res * p$l_start)^p$lambda
    } else {
      C_01_02_steady_fn(p)
    }
  }
  df  <- C_01_06_share_fn(par, 200)
  now <- now_fn(par)
  flat <- diff(range(df$growth)) < 1e-12

  # Ghost: the locus and the operating point at the worked example's settings
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_df  <- C_01_06_share_fn(ref, 200)
    g_now <- now_fn(ref)
    list(
      T_02_03a_ghost_line_fn(g_df, aes(x = a_res, y = growth),
                             colour = T_01_02_series_vec[["main"]],
                             linewidth = 1.1),
      if (is.finite(g_now)) T_02_03a_ghost_point_fn(ref$a_res, g_now)
    )
  }

  # A flat series has no range of its own, so set the window by hand
  span <- if (flat) c(0, max(df$growth, 1e-4) * 2.2) else
    c(0, max(df$growth) * 1.1)

  x_lim <- c(0, 0.85)

  ggplot(df, aes(x = a_res, y = growth)) +
    T_02_02_zero_fn() +
    T_02_02_rest_fn(v = par$a_res, h = if (flat) NULL else now) +
    ghost_lyr +
    geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.1) +
    T_02_03_point_fn(par$a_res, now) +
    coord_cartesian(xlim = x_lim, ylim = span, expand = FALSE) +
    T_02_02_mark_y_fn(now, expression(g[A]^"*")) +
    T_02_02_mark_x_fn(par$a_res, expression(a)) +
    # A flat line could pass for a reference level, so it carries its name
    (if (flat) {
      annotate("text", x = x_lim[2], y = now,
               label = "'Balanced growth  '*g[A]^'*'",
               parse = TRUE, size = 3.2, hjust = 1, vjust = -1.15,
               colour = T_01_01_palette_vec[["muted"]])
    }) +
    labs(
      title = if (flat) {
        "The Long-Run Growth Rate of Ideas Does Not Move"
      } else {
        "The Long-Run Growth Rate of Ideas Rises with a"
      },
      x = expression(bold("Share of workers in research (" * a * ")")),
      y = expression(bold("Long-run growth rate of ideas (" *
                            g[A]^"*" * ")")),
      caption = if (flat) {
        paste("With phi below one the steady rate is lambda n / (1 - phi),",
              "which has no a in it. Policy can change the level and not the",
              "slope.")
      } else {
        paste("With phi = 1 the stock of ideas drops out of the growth rate,",
              "so the research share sets it. This is what makes the model",
              "endogenous.")
      }
    ) +
    T_02_01_theme_fn()
}

###### D_02_02: The Level Against the Research Share ###########################
# Note: Output per worker on the balanced path. Humped, because researchers
#   are workers who are not making anything.

D_02_02_share_level_fn <- function(par, ref = NULL) {
  df <- C_01_06_share_fn(par, 200)
  df <- df[is.finite(df$y_worker), ]
  if (nrow(df) == 0) {
    return(T_02_02_placeholder_fn(paste(
      "No balanced path at these parameters.\nSet phi below one and",
      "population growth above zero.")))
  }
  best <- C_01_07_best_fn(par)
  now  <- df$y_worker[which.min(abs(df$a_res - par$a_res))]

  # Ghost: the hump and the point on it at the worked example's settings
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_df <- C_01_06_share_fn(ref, 200)
    g_df <- g_df[is.finite(g_df$y_worker), ]
    if (nrow(g_df) == 0) NULL else list(
      T_02_03a_ghost_line_fn(g_df, aes(x = a_res, y = y_worker),
                             colour = T_01_02_series_vec[["main"]],
                             linewidth = 1.1),
      T_02_03a_ghost_point_fn(
        ref$a_res, g_df$y_worker[which.min(abs(g_df$a_res - ref$a_res))])
    )
  }

  # a_best is a choice, not a resting point; see CONVENTIONS.md 5
  ggplot(df, aes(x = a_res, y = y_worker)) +
    T_02_02_zero_fn() +
    (if (!is.na(best$share)) T_02_02_rest_fn(v = best$share)) +
    ghost_lyr +
    geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.1) +
    T_02_03_point_fn(par$a_res, now) +
    (if (!is.na(best$share)) {
      T_02_02_mark_x_fn(best$share, expression(a["best"]))
    }) +
    labs(
      title = "The Level of Output per Worker on the Balanced Path",
      x = expression(bold("Share of workers in research (" * a * ")")),
      y = expression(bold("Output per worker (" * Y / L * ")")),
      caption = paste0(
        "Rises with a through the stock of ideas, falls with a through the",
        " workers lost to production. The peak is at kappa/(1 + kappa) with",
        " kappa = lambda/(1 - phi) = ", T_02_05_num_fn(best$kappa, 2), "."
      )
    ) +
    T_02_01_theme_fn()
}

#### D_03: Raising the Research Share ##########################################
# Note: The experiment that separates a level effect from a growth effect.

###### D_03_01: The Research Push ##############################################
# Note: Growth jumps and comes back; technology stays permanently higher.

D_03_01_shift_fn <- function(par, what, ref = NULL) {
  df   <- C_01_08_shift_fn(par)
  base <- C_01_04_path_fn(par)
  when <- min(df$period[df$shifted])

  # Two ghosts, one per series; the log level panel keeps positive values
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_push <- C_01_08_shift_fn(ref)
    g_base <- C_01_04_path_fn(ref)
    g_var  <- if (what == "growth") "g_tech" else "tech"
    keep_fn <- function(d) {
      v <- d[[g_var]]
      d <- d[is.finite(v) & (what == "growth" | v > 0), ]
      d$value <- d[[g_var]]
      d
    }
    g_push <- keep_fn(g_push)
    g_base <- keep_fn(g_base)
    list(
      if (nrow(g_base) > 0) {
        T_02_03a_ghost_line_fn(g_base, aes(x = period, y = value),
                               colour = T_01_02_series_vec[["compare"]],
                               linewidth = 1.1)
      },
      if (nrow(g_push) > 0) {
        T_02_03a_ghost_line_fn(g_push, aes(x = period, y = value),
                               colour = T_01_02_series_vec[["main"]],
                               linewidth = 1.1)
      }
    )
  }

  long <- if (what == "growth") {
    rbind(
      data.frame(period = df$period, value = df$g_tech,
                 line = "With the Research Push"),
      data.frame(period = base$period, value = base$g_tech,
                 line = "Without It")
    )
  } else {
    rbind(
      data.frame(period = df$period, value = df$tech,
                 line = "With the Research Push"),
      data.frame(period = base$period, value = base$tech,
                 line = "Without It")
    )
  }
  long <- long[!is.na(long$value), ]
  long$line <- factor(long$line,
                      levels = c("With the Research Push", "Without It"))

  g_ss  <- C_01_02_steady_fn(par)
  rests <- what == "growth" && !is.na(g_ss) && g_ss > 0

  p <- ggplot(long, aes(x = period, y = value, colour = line,
                        linetype = line)) +
    (if (what == "growth") T_02_02_zero_fn(v = FALSE)) +
    T_02_02_rest_fn(v = when, h = if (rests) g_ss else NULL) +
    ghost_lyr +
    geom_line(linewidth = 1.1) +
    scale_colour_manual(values = c(
      "With the Research Push" = T_01_02_series_vec[["main"]],
      "Without It"             = T_01_02_series_vec[["compare"]]
    )) +
    scale_linetype_manual(values = c("With the Research Push" = "solid",
                                     "Without It" = "22")) +
    # The year of the push is named along the top axis
    T_02_02_mark_x_fn(when, expression(t["push"])) +
    T_02_01_theme_fn()

  if (what == "growth") {
    p +
      (if (rests) T_02_02_mark_y_fn(g_ss, expression(g[A]^"*"))) +
      labs(
        title = if (isTRUE(par$phi >= 1)) {
          "The Growth Rate of Ideas Jumps and Stays Up"
        } else {
          "The Growth Rate of Ideas Jumps, Then Comes Back"
        },
        x = expression(bold("Year (" * t * ")")),
        y = expression(bold("Growth rate of ideas (" * g[A] * ")")),
        caption = paste(
          "The push raises growth for a couple of decades. Then the stock of",
          "ideas has caught up with the extra effort and growth returns to",
          "exactly where it was."
        )
      )
  } else {
    p + scale_y_log10() +
      labs(
        title = "But the Stock of Ideas Is Permanently Higher",
        x = expression(bold("Year (" * t * ")")),
        y = expression(bold("Stock of ideas (" * A * "), log scale")),
        caption = paste(
          "The two lines never converge again. On a log scale they are",
          "parallel: same slope, different level. That is a level effect."
        )
      )
  }
}

#### D_04: Capital and Balanced Growth #########################################
# Note: The transition of capital per effective worker, and the three growth
#   rates collapsing onto the growth rate of technology.

###### D_04_01: Capital per Effective Worker ###################################
# Note: k tilde walking to k tilde star. Where it starts does not matter,
#   which is the Solow convergence result inside the Romer model.

D_04_01_ktilde_fn <- function(par, ref = NULL) {
  path <- C_02_07_capital_fn(par)
  k_ss <- C_02_03_kstar_fn(par)

  # Ghost: the same transition at the worked example's settings
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_df <- C_02_07_capital_fn(ref)
    g_df <- g_df[is.finite(g_df$k_tilde), ]
    if (nrow(g_df) == 0) NULL else list(
      T_02_03a_ghost_line_fn(g_df, aes(x = period, y = k_tilde),
                             colour = T_01_02_series_vec[["main"]],
                             linewidth = 1.1),
      T_02_03a_ghost_point_fn(g_df$period[1], g_df$k_tilde[1])
    )
  }

  p <- ggplot(path, aes(x = period, y = k_tilde)) +
    T_02_02_zero_fn(v = FALSE) +
    (if (!is.na(k_ss)) T_02_02_rest_fn(h = k_ss)) +
    ghost_lyr +
    geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.1) +
    (if (!is.na(k_ss)) T_02_02_mark_y_fn(k_ss, expression(tilde(k)^"*"))) +
    labs(
      title = if (is.na(k_ss)) {
        "Capital per Effective Worker Has No Resting Point"
      } else {
        paste0("Capital per Effective Worker Settles at ",
               T_02_05_num_fn(k_ss, 2))
      },
      x = expression(bold("Year (" * t * ")")),
      y = expression(bold("Capital per effective worker (" *
                            tilde(k) * ")")),
      caption = paste(
        "Investment is s k^alpha (1-a)^(1-alpha); what has to be kept up",
        "with is (n + g_A + delta) k. Capital rises while the first is",
        "bigger than the second, and stops when they are equal."
      )
    ) +
    T_02_01_theme_fn()

  if (!is.na(k_ss)) {
    p <- p + T_02_03_point_fn(path$period[1], path$k_tilde[1])
  }
  p
}

###### D_04_02: The Growth Rates Converging ####################################
# Note: Growth in output per worker, capital per worker and technology; all
#   three sit on g_A once capital has arrived at its balanced stock.

D_04_02_converge_fn <- function(par, ref = NULL) {
  path <- C_02_07_capital_fn(par)
  g_ss <- C_02_01_glong_fn(par)

  # Three ghosts, one per series, each in its series colour
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_df <- C_02_07_capital_fn(ref)
    one_fn <- function(col, colour) {
      d <- data.frame(period = g_df$period, value = g_df[[col]])
      d <- d[is.finite(d$value), ]
      if (nrow(d) == 0) NULL else {
        T_02_03a_ghost_line_fn(d, aes(x = period, y = value),
                               colour = colour, linewidth = 1.1)
      }
    }
    list(
      one_fn("g_y",    T_01_02_series_vec[["main"]]),
      one_fn("g_kw",   T_01_02_series_vec[["third"]]),
      one_fn("g_tech", T_01_02_series_vec[["compare"]])
    )
  }

  long <- rbind(
    data.frame(period = path$period, value = path$g_y,
               line = "Output per Worker"),
    data.frame(period = path$period, value = path$g_kw,
               line = "Capital per Worker"),
    data.frame(period = path$period, value = path$g_tech,
               line = "Technology")
  )
  long <- long[is.finite(long$value), ]
  if (nrow(long) == 0) {
    return(T_02_02_placeholder_fn(
      "No growth path to draw at these parameters."))
  }
  long$line <- factor(long$line, levels = c("Output per Worker",
                                            "Capital per Worker",
                                            "Technology"))

  # Window at the ninetieth percentile: capital's growth starts very high
  top <- max(stats::quantile(long$value, 0.90, na.rm = TRUE),
             g_ss * 2.5, 0.06, na.rm = TRUE)
  bot <- min(0, max(min(long$value, na.rm = TRUE), -top))

  rests <- !is.na(g_ss) && g_ss > 0

  p <- ggplot(long, aes(x = period, y = value, colour = line,
                        linetype = line)) +
    T_02_02_zero_fn(v = FALSE) +
    (if (rests) T_02_02_rest_fn(h = g_ss)) +
    ghost_lyr +
    geom_line(linewidth = 1.1) +
    coord_cartesian(ylim = c(bot, top)) +
    scale_colour_manual(values = c(
      "Output per Worker"  = T_01_02_series_vec[["main"]],
      "Capital per Worker" = T_01_02_series_vec[["third"]],
      "Technology"         = T_01_02_series_vec[["compare"]]
    )) +
    scale_linetype_manual(values = c("Output per Worker"  = "solid",
                                     "Capital per Worker" = "22",
                                     "Technology"         = "solid")) +
    (if (rests) T_02_02_mark_y_fn(g_ss, expression(g[A]^"*"))) +
    labs(
      title = if (is.na(g_ss)) {
        "The Three Growth Rates Have No Rate to Meet At"
      } else {
        paste0("Output, Capital and Ideas All Grow at ",
               T_02_06_pct_fn(g_ss, 2))
      },
      x = expression(bold("Year (" * t * ")")),
      y = expression(bold("Growth rate (" * g * ")")),
      caption = paste(
        "Output per worker grows at g_A + alpha k-dot/k and capital per",
        "worker at g_A + k-dot/k. Capital deepening is the gap, and it",
        "closes. Only the stock of ideas is still growing at the end."
      )
    ) +
    T_02_01_theme_fn()

  p
}

#### D_05: Level Against Growth ################################################
# Note: The level-against-growth trade-off, now that there is a level to
#   trade against.

###### D_05_01: The Balanced Level Against the Research Share ##################
# Note: y tilde star against a. With phi < 1 it is a straight line sloping
#   down while the growth rate beside it is flat.

D_05_01_level_fn <- function(par, ref = NULL) {
  df <- C_02_08_share_fn(par, 200)
  df <- df[is.finite(df$y_tilde), ]
  if (nrow(df) == 0) {
    return(T_02_02_placeholder_fn(paste(
      "No balanced path at these parameters.\nSet phi at or below one and",
      "depreciation above zero.")))
  }
  now  <- df$y_tilde[which.min(abs(df$a_res - par$a_res))]
  flat <- diff(range(df$growth, na.rm = TRUE)) < 1e-12

  # Ghost: the locus and the operating point at the worked example's settings
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_df <- C_02_08_share_fn(ref, 200)
    g_df <- g_df[is.finite(g_df$y_tilde), ]
    if (nrow(g_df) == 0) NULL else list(
      T_02_03a_ghost_line_fn(g_df, aes(x = a_res, y = y_tilde),
                             colour = T_01_02_series_vec[["main"]],
                             linewidth = 1.1),
      T_02_03a_ghost_point_fn(
        ref$a_res, g_df$y_tilde[which.min(abs(g_df$a_res - ref$a_res))])
    )
  }

  # The line carries its name, hung from the x axis; see CONVENTIONS.md 6
  x_lim <- c(0, 0.85)
  y_lim <- c(0, max(df$y_tilde, na.rm = TRUE) * 1.1)
  i_end <- which.max(df$a_res)

  ggplot(df, aes(x = a_res, y = y_tilde)) +
    T_02_02_zero_fn() +
    T_02_02_rest_fn(v = par$a_res, h = now) +
    ghost_lyr +
    geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.1) +
    T_02_03_point_fn(par$a_res, now) +
    coord_cartesian(xlim = x_lim, ylim = y_lim, expand = FALSE) +
    T_02_02_mark_x_fn(par$a_res, expression(a)) +
    T_02_02_mark_y_fn(now, expression(tilde(y)^"*")) +
    (if (flat) {
      annotate("text", x = df$a_res[i_end], y = y_lim[1],
               label = "'Balanced level  '*tilde(y)^'*'",
               parse = TRUE, size = 3.2, hjust = 1, vjust = -0.7,
               colour = T_01_01_palette_vec[["muted"]])
    }) +
    labs(
      title = if (flat) {
        "Research Costs Level and Buys No Growth"
      } else {
        "Research Costs Level and Buys Growth"
      },
      x = expression(bold("Share of workers in research (" * a * ")")),
      y = expression(bold("Output per effective worker (" *
                            tilde(y)^"*" * ")")),
      caption = if (flat) {
        paste("With phi below one the bracket in y* has no a in it, so the",
              "whole effect of research on the level is the (1-a): a worker",
              "in a laboratory is a worker not making anything.")
      } else {
        paste("With phi = 1 more research also raises g_A, so capital has",
              "more to keep up with and the level falls faster still. The",
              "growth figure beside this one is what pays for it.")
      }
    ) +
    T_02_01_theme_fn() +
    theme(aspect.ratio = 2 / 3)
}

###### D_05_02: A Research Push, Level and Slope ###############################
# Note: Output per worker with the research share as it is and raised by the
#   push, on a log scale: parallel with phi < 1, steeper with phi = 1.

D_05_02_push_fn <- function(par, ref = NULL) {
  raised <- min(max(par$a_res + par$shift_by, 0.01), 0.9)
  base   <- C_02_07_capital_fn(par)
  push   <- C_02_07_capital_fn(par, a_res = raised)

  # Both runs ghosted in their series colours; log axis keeps v > 0
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    g_raised <- min(max(ref$a_res + ref$shift_by, 0.01), 0.9)
    one_fn <- function(d, colour) {
      d <- data.frame(period = d$period, value = d$y_worker)
      d <- d[is.finite(d$value) & d$value > 0, ]
      if (nrow(d) == 0) NULL else {
        T_02_03a_ghost_line_fn(d, aes(x = period, y = value),
                               colour = colour, linewidth = 1.1)
      }
    }
    list(
      one_fn(C_02_07_capital_fn(ref), T_01_02_series_vec[["compare"]]),
      one_fn(C_02_07_capital_fn(ref, a_res = g_raised),
             T_01_02_series_vec[["main"]])
    )
  }

  long <- rbind(
    data.frame(period = push$period, value = push$y_worker,
               line = "With the Research Push"),
    data.frame(period = base$period, value = base$y_worker,
               line = "As It Is")
  )
  long <- long[is.finite(long$value) & long$value > 0, ]
  if (nrow(long) == 0) {
    return(T_02_02_placeholder_fn(
      "No output path to draw at these parameters."))
  }
  long$line <- factor(long$line,
                      levels = c("With the Research Push", "As It Is"))

  n_t   <- nrow(base)
  ratio <- push$y_worker[n_t] / base$y_worker[n_t]
  g_ss  <- C_02_01_glong_fn(par)

  # Whether the two growth rates have met yet; the title says if not
  met <- isTRUE(par$phi < 1) && isTRUE(g_ss > 0) &&
    abs(push$g_tech[n_t] - base$g_tech[n_t]) < 0.03 * g_ss

  ggplot(long, aes(x = period, y = value, colour = line, linetype = line)) +
    ghost_lyr +
    geom_line(linewidth = 1.1) +
    scale_y_log10() +
    scale_colour_manual(values = c(
      "With the Research Push" = T_01_02_series_vec[["main"]],
      "As It Is"               = T_01_02_series_vec[["compare"]]
    )) +
    scale_linetype_manual(values = c("With the Research Push" = "solid",
                                     "As It Is" = "22")) +
    labs(
      title = if (isTRUE(par$phi >= 1)) {
        "Output per Worker on a Permanently Steeper Path"
      } else if (met) {
        "Output per Worker: Different Level, Same Slope"
      } else {
        "Output per Worker Is Still Catching Up: the Slopes Have Not Met"
      },
      x = expression(bold("Year (" * t * ")")),
      y = expression(bold("Output per worker (" * Y / L * "), log scale")),
      caption = paste0(
        "The research share is raised from ", T_02_06_pct_fn(par$a_res, 0),
        " to ", T_02_06_pct_fn(raised, 0), ". After ", n_t, " years output",
        " per worker is ", T_02_05_num_fn(ratio, 2), " times what it would",
        " have been. ",
        if (isTRUE(par$phi >= 1)) {
          paste("With phi = 1 the pushed line is permanently steeper, so",
                "that ratio keeps growing for ever.")
        } else if (met) {
          paste("With phi below one the lines have gone parallel: same",
                "slope, different level, and the ratio has stopped moving.")
        } else {
          paste("With phi below one they will end parallel, but the growth",
                "rates have not met yet. Draw more years to see the ratio",
                "settle down.")
        }
      )
    ) +
    T_02_01_theme_fn()
}

#### D_06: What Research Costs Today ###########################################
# Note: Stage 1 has only the goods equation Y = A (1 - a) L, so its figures
#   are about the workforce and what it makes this year.

###### D_06_01: Output Today Against the Research Share ########################
# Note: Output per worker A (1 - a) against the research share: a straight
#   line falling one for one, with the marker where the slider sits.

D_06_01_cost_today_fn <- function(par, ref = NULL) {
  grid <- seq(0, 0.85, length.out = 200)
  df   <- data.frame(a_res = grid, y_worker = par$a_start * (1 - grid))
  now  <- par$a_start * (1 - par$a_res)

  x_lim <- c(0, 0.85)
  y_lim <- c(0, par$a_start * 1.15)

  # Ghost: the same line and point at the worked example's settings
  ghost_lyr <- if (T_02_03b_ghost_off_fn(par, ref)) NULL else {
    list(
      T_02_03a_ghost_line_fn(
        data.frame(a_res = grid, y_worker = ref$a_start * (1 - grid)),
        aes(x = a_res, y = y_worker),
        colour = T_01_02_series_vec[["main"]], linewidth = 1.1),
      T_02_03a_ghost_point_fn(ref$a_res, ref$a_start * (1 - ref$a_res))
    )
  }

  ggplot(df, aes(x = a_res, y = y_worker)) +
    T_02_02_zero_fn() +
    T_02_02_rest_fn(v = par$a_res, h = now) +
    ghost_lyr +
    geom_line(colour = T_01_02_series_vec[["main"]], linewidth = 1.1) +
    T_02_03_point_fn(par$a_res, now) +
    coord_cartesian(xlim = x_lim, ylim = y_lim, expand = FALSE) +
    T_02_02_mark_x_fn(par$a_res, expression(a)) +
    T_02_02_mark_y_fn(now, expression(A * (1 - a))) +
    # The line carries its name, hung from the x axis; see CONVENTIONS.md 6
    annotate("text", x = x_lim[2], y = y_lim[1],
             label = "'Output per worker  '*Y/L",
             parse = TRUE, size = 3.2, hjust = 1, vjust = -0.7,
             colour = T_01_01_palette_vec[["muted"]]) +
    labs(
      title = paste0("Output per Worker Today Is ",
                     T_02_06_pct_fn(1 - par$a_res, 0),
                     " of Capacity"),
      x = expression(bold("Share of workers in research (" * a * ")")),
      y = expression(bold("Output per worker today (" * Y / L * ")")),
      caption = paste(
        "Researchers are workers who are not making anything, so the cost of",
        "research today is exactly one for one. Nothing in this stage pays it",
        "back: the rest of the model is about what the ideas are worth."
      )
    ) +
    T_02_01_theme_fn(grid = "none") +
    theme(aspect.ratio = 2 / 3)
}

###### D_06_02: Where the Workforce Goes #######################################
# Note: One bar of workers, split into those making goods and those finding
#   ideas, so the (1 - a) in the production function has a visible size.

D_06_02_split_fn <- function(par) {
  df <- data.frame(
    job    = factor(c("Finding Ideas", "Making Goods"),
                    levels = c("Finding Ideas", "Making Goods")),
    people = c(par$a_res * par$l_start, (1 - par$a_res) * par$l_start),
    share  = c(par$a_res, 1 - par$a_res)
  )

  # One discrete category, so only the horizontal half of the zero cross
  ggplot(df, aes(x = "The Workforce", y = people, fill = job)) +
    T_02_02_zero_fn(v = FALSE) +
    T_02_02_rest_fn(h = (1 - par$a_res) * par$l_start) +
    geom_col(width = 0.45) +
    geom_text(aes(label = paste0(job, "\n", round(share * 100), "%")),
              position = position_stack(vjust = 0.5), size = 4,
              fontface = "bold", colour = T_01_01_palette_vec[["ground"]]) +
    scale_fill_manual(values = c(
      "Finding Ideas" = T_01_02_series_vec[["compare"]],
      "Making Goods"  = T_01_02_series_vec[["main"]]
    )) +
    T_02_02_mark_y_fn((1 - par$a_res) * par$l_start,
                      expression(L[Y] == (1 - a) * L)) +
    guides(fill = "none") +
    labs(
      title = "Every Worker Is Either Finding Ideas or Making Goods",
      x = NULL, y = expression(bold("Workers (" * L * ")")),
      caption = paste(
        "L = L_A + L_Y with L_A = aL. Output is made by the green slice's",
        "complement alone, which is why the goods equation carries a (1-a)",
        "and why moving the slider costs output the moment you move it."
      )
    ) +
    T_02_01_theme_fn(grid = "none")
}

#### D_07: The Headwinds #######################################################
# Note: Gordon's (2016) headwinds subtracting from the model's own balanced
#   growth rate. A slide figure, illustrative, and it says so in its subtitle.

###### D_07_01: The Headwinds Waterfall ########################################
# Note: A step-down: the balanced rate as a column, one floating bar per
#   headwind, the residual as a column. Horizontal, because the names are long.

D_07_01_headwinds_fn <- function(par,
                                 start     = NULL,
                                 headwinds = B_03_13_headwinds_vec,
                                 cut       = B_03_14_headwind_cut_num) {

  g_start <- if (is.null(start)) C_02_01_glong_fn(par) else start

  if (!is.finite(g_start) || g_start <= 0) {
    return(T_02_02_placeholder_fn(
      "No balanced growth rate at these settings, so nothing to subtract from."
    ))
  }

  n_head <- length(headwinds)
  step   <- g_start * cut / n_head
  after  <- g_start - step * seq_len(n_head)
  g_end  <- g_start - g_start * cut

  # The two ends are levels from zero; the headwinds float between levels
  lab_chr <- c("Balanced rate, g*", headwinds, "What is left")

  df <- data.frame(
    step_chr = lab_chr,
    lo       = c(0, after, 0),
    hi       = c(g_start, c(g_start, after[-n_head]), g_end),
    kind     = c("level", rep("headwind", n_head), "level"),
    stringsAsFactors = FALSE
  )
  df$step_chr <- factor(df$step_chr, levels = rev(lab_chr))
  df$kind     <- factor(df$kind, levels = c("level", "headwind"))

  # Ledger lines carry each bar's foot across the gap to the next bar's head
  carry_num <- c(g_start, after)
  link_df   <- data.frame(
    x    = seq_len(n_head + 1) + 0.4,
    xend = seq_len(n_head + 1) + 1.4,
    y    = carry_num
  )
  # The factor is reversed for coord_flip, so positions count from the top
  link_df$x    <- (n_head + 2) - link_df$x + 1
  link_df$xend <- (n_head + 2) - link_df$xend + 1

  # A linerange takes its colour from colour, not fill
  ggplot(df, aes(x = step_chr, ymin = lo, ymax = hi, colour = kind)) +
    T_02_02_zero_fn(v = FALSE) +
    geom_segment(data = link_df, inherit.aes = FALSE,
                 aes(x = x, xend = xend, y = y, yend = y),
                 linetype = "dotted", linewidth = 0.4,
                 colour = T_01_01_palette_vec[["muted"]]) +
    # Thin enough to leave a gap for the ledger lines between the bars
    geom_linerange(linewidth = 6) +
    scale_colour_manual(values = c(level    = T_01_02_series_vec[["main"]],
                                   headwind = T_01_02_series_vec[["band"]])) +
    scale_x_discrete(expand = expansion(add = 0.6)) +
    scale_y_continuous(labels = function(v) T_02_06_pct_fn(v, 1)) +
    guides(colour = "none") +
    coord_flip() +
    labs(
      title = "The Headwinds Subtract from Growth in Output per Head",
      # Folded narrow: ggplot2 never wraps a subtitle
      subtitle = T_02_01b_fold_fn(paste0(
        "Illustrative. The headwinds are Gordon's; the equal steps are not ",
        "his estimates. They are drawn taking ", T_02_06_pct_fn(cut, 0),
        " of this model's own balanced rate, ",
        T_02_06_pct_fn(g_start, 2), "."), 76),
      x = NULL,
      y = expression(bold("Growth in output per head (" * g[A] * ")")),
      caption = paste(
        "A starting rate, a list of subtractions, a residual. The argument",
        "is about the list and about how big each item on it is; the model",
        "in this app has nothing to say about either, which is the point of",
        "putting the figure at the end of the lecture rather than inside it."
      )
    ) +
    T_02_01_theme_fn(grid = "v")
}

################################################################################
## E: User Interface ###########################################################
################################################################################
# Note: bslib page: controls in a sidebar, figures in cards.

#### E_01: Sidebar #############################################################
# Note: Stage selector, then the controls. The sidebar chooses the model;
#   the presets in the main window (E_01_02) choose what to run in it.

###### E_01_01: Control Shorthand ##############################################
# Note: T_03_01_control_fn with this app's three lists filled in.

E_01_01_ctl_fn <- function(id) {
  T_03_01_control_fn(id, B_03_04_controls_lst, B_03_05_help_lst,
                     B_03_01_defaults_lst)
}

###### E_01_02: Worked-Example Presets #########################################
# Note: Preset card for the main window, showing the stage's own scenarios;
#   the machinery is in the toolkit (T_05_04 to T_05_07).

E_01_02_presets_lst <- T_05_04_presets_fn(
  B_03_03_scenarios_lst, B_03_02_stages_vec, stage_word = "Stage"
)

###### E_01_03: Sidebar ########################################################
# Note: conditionalPanel reveals controls as the stages add layers.

E_01_03_sidebar_lst <- sidebar(
  width = 380,
  radioButtons("stage", "Stage of the Model",
               choices = B_03_02_stages_vec, selected = "1"),
  T_03_05_note_fn(paste(
    "Each stage adds one piece to the model and leaves the rest",
    "alone. Start at the top; the equations panel marks what is new.")),
  accordion(
    open = c("Research"),
    accordion_panel(
      "Research",
      E_01_01_ctl_fn("a_res"),
      conditionalPanel(
        "parseFloat(input.stage) >= 2",
        E_01_01_ctl_fn("phi"),
        E_01_01_ctl_fn("lambda"),
        E_01_01_ctl_fn("theta")
      ),
      conditionalPanel("parseFloat(input.stage) < 2",
                       tags$p(class = "stat-caption",
                              "The research equation appears at stage 2."))
    ),
    accordion_panel(
      "Population and the Start",
      conditionalPanel("parseFloat(input.stage) >= 3",
                       E_01_01_ctl_fn("n")),
      E_01_01_ctl_fn("l_start"),
      conditionalPanel("parseFloat(input.stage) >= 2",
                       E_01_01_ctl_fn("a_start"),
                       E_01_01_ctl_fn("n_periods"))
    ),
    accordion_panel(
      "The Research Push",
      conditionalPanel(
        "parseFloat(input.stage) >= 5",
        E_01_01_ctl_fn("shift_by"),
        E_01_01_ctl_fn("shift_at")
      ),
      conditionalPanel("parseFloat(input.stage) < 5",
                       tags$p(class = "stat-caption",
                              "The experiment appears at stage 5."))
    ),
    accordion_panel(
      "Capital and Saving",
      conditionalPanel(
        "parseFloat(input.stage) >= 6",
        E_01_01_ctl_fn("alpha"),
        E_01_01_ctl_fn("saving"),
        E_01_01_ctl_fn("delta"),
        E_01_01_ctl_fn("k_start")
      ),
      conditionalPanel("parseFloat(input.stage) < 6",
                       tags$p(class = "stat-caption",
                              "The capital side appears at stage 6."))
    )
  ),
  actionButton("reset", "Reset Everything",
               class = "btn-outline-secondary btn-sm w-100"),
  T_07_10b_sidebarqr_fn(B_04_01_qr_src_chr)
)

#### E_02: Main Panel ##########################################################
# Note: Equations card, presets, prompt, readouts, then the figures.

###### E_02_01: Page ###########################################################
# Note: The UI passed to shinyApp().

E_02_01_app_ui_lst <- tagList(
  T_07_08b_nav_fn(),
  page_sidebar(
  title        = T_07_09_title_fn("Growth Model: Romer “Endogenous”",
                                  B_04_01_qr_src_chr),
  window_title = paste("Romer: Endogenous ·", T_07_01_author_chr),
  fillable     = FALSE,
  theme        = T_07_05_theme_fn(),
  sidebar      = E_01_03_sidebar_lst,
  T_07_08_head_fn(),
  tags$head(
    tags$style(HTML(T_05_07_preset_css_chr)),
    tags$script(HTML(T_05_05_preset_js_chr))
  ),
  navset_card_tab(
    title = textOutput("eq_title", inline = TRUE),
    nav_panel("Equations", uiOutput("eq_model")),
    nav_panel("Notation", uiOutput("eq_notation")),
    nav_panel("In Words", uiOutput("eq_explain"))
  ),
  E_01_02_presets_lst,
  uiOutput("prompt"),
  uiOutput("problems"),
  uiOutput("tiles"),
  conditionalPanel(
    "parseFloat(input.stage) < 2",
    layout_columns(
      col_widths = breakpoints(sm = 12, xl = c(6, 6)),
      T_07_07c_figcard_fn("cost_today",
                          "Output Today Against the Research Share",
                          B_03_11_tall_chr),
      T_07_07c_figcard_fn("cost_split", "Where the Workforce Goes",
                          B_03_11_tall_chr)
    )
  ),
  conditionalPanel(
    "parseFloat(input.stage) >= 2",
    layout_columns(
      col_widths = breakpoints(sm = 12, xl = c(6, 6)),
      T_07_07c_figcard_fn("growth", "Growth in Technology Over Time",
                          B_03_11_tall_chr),
      tags$div(
        conditionalPanel(
          "parseFloat(input.stage) < 3",
          T_07_07c_figcard_fn("tech", "The Stock of Ideas Over Time",
                              B_03_11_tall_chr)
        ),
        conditionalPanel(
          "parseFloat(input.stage) >= 3",
          T_07_07c_figcard_fn("phase", "The Growth Rate's Own Dynamics",
                              B_03_11_tall_chr)
        )
      )
    )
  ),
  conditionalPanel(
    "parseFloat(input.stage) >= 3",
    layout_columns(
      col_widths = breakpoints(sm = 12, xl = c(6, 6)),
      T_07_07c_figcard_fn("share_growth", "Growth Against the Research Share",
                          B_03_11_tall_chr),
      T_07_07c_figcard_fn("share_level", "The Level Against the Research Share",
                          B_03_11_tall_chr)
    )
  ),
  conditionalPanel(
    "parseFloat(input.stage) >= 5",
    layout_columns(
      col_widths = breakpoints(sm = 12, xl = c(6, 6)),
      T_07_07c_figcard_fn("shift_growth", "A Research Push: the Growth Rate",
                          B_03_11_tall_chr),
      T_07_07c_figcard_fn("shift_level", "A Research Push: the Level",
                          B_03_11_tall_chr)
    ),
    uiOutput("evidence_note")
  ),
  conditionalPanel(
    "parseFloat(input.stage) >= 6",
    layout_columns(
      col_widths = breakpoints(sm = 12, xl = c(6, 6)),
      T_07_07c_figcard_fn("ktilde", "Capital per Effective Worker",
                          B_03_11_tall_chr),
      T_07_07c_figcard_fn("converge", "The Growth Rates Converging",
                          B_03_11_tall_chr)
    )
  ),
  conditionalPanel(
    "parseFloat(input.stage) >= 7",
    layout_columns(
      col_widths = breakpoints(sm = 12, xl = c(6, 6)),
      T_07_07c_figcard_fn("trade_level", "The Balanced Level of Output",
                          B_03_11_tall_chr),
      T_07_07c_figcard_fn("trade_push", "A Research Push: Level and Slope",
                          B_03_11_tall_chr)
    ),
    uiOutput("capital_note")
  ),
  T_07_11_footer_fn(paste0("Notation follows the Part 3 exam questions.",
                           " Version ", B_03_15_version_chr, "."),
                    repo = B_03_16_repo_chr),
))

################################################################################
## F: Server ###################################################################
################################################################################
# Note: Assembles the stage's parameters, solves the model, draws.

#### F_01: Server Function #####################################################
# Note: Everything reactive lives here.

###### F_01_01: Server #########################################################
# Note: Local objects are plain snake_case.

F_01_01_app_server_fn <- function(input, output, session) {

  # --- Figure captions --------------------------------------------------------
  # T_02_01c_draw_fn lifts each caption out of the device; this prints it
  T_07_07d_cap_fn(output)

  # --- Stage as a number ------------------------------------------------------
  stage_num <- reactive(as.numeric(input$stage))

  # --- Controls ---------------------------------------------------------------
  val <- function(id) T_03_04_val_fn(input, id)
  T_03_02_sync_fn(input, session, B_03_04_controls_lst)

  set_control <- function(id, value) {
    T_03_03_set_fn(session, B_03_04_controls_lst, id, value)
  }

  # --- Worked-example presets -------------------------------------------------
  # A preset sets the controls, not the stage. One guarded lookup of the
  # loaded scenario serves the card header, the story and the prompt.
  scenario <- reactiveVal(names(B_03_03_scenarios_lst)[1])

  scn_now <- reactive({
    k <- scenario()
    if (is.null(k) || !k %in% names(B_03_03_scenarios_lst)) NULL
    else B_03_03_scenarios_lst[[k]]
  })

  set_scenario_fn <- function(key) {
    scenario(if (is.null(key)) "custom" else key)
    session$sendCustomMessage("dgPreset", if (is.null(key)) "" else key)
    invisible(NULL)
  }

  load_preset_fn <- function(key) {
    if (is.null(key) || !key %in% names(B_03_03_scenarios_lst)) {
      return(invisible(NULL))
    }
    scn <- B_03_03_scenarios_lst[[key]]
    set_scenario_fn(key)
    for (id in names(B_03_04_controls_lst)) {
      set_control(id, if (is.null(scn$values[[id]])) {
        B_03_01_defaults_lst[[id]]
      } else {
        scn$values[[id]]
      })
    }
    invisible(NULL)
  }

  lapply(names(B_03_03_scenarios_lst), function(key) {
    observeEvent(input[[paste0("preset_", key)]],
                 load_preset_fn(key), ignoreInit = TRUE)
  })

  # First scenario of a stage; NULL when the stage has none
  first_preset_fn <- function(stage) {
    hits <- names(B_03_03_scenarios_lst)[vapply(
      B_03_03_scenarios_lst, function(x) identical(x$stage, stage), TRUE)]
    if (length(hits) == 0L) NULL else hits[[1L]]
  }

  # Every stage opens on its own first example; the sliders stay live
  observeEvent(input$stage, {
    first <- first_preset_fn(input$stage)
    if (is.null(first)) set_scenario_fn(NULL) else load_preset_fn(first)
  })

  # The stage names in B_03_02 begin with "Stage n:", so stage_word is empty
  output$preset_title <- renderUI({
    T_05_06_preset_title_fn(scn_now(), input$stage, B_03_02_stages_vec,
                            stage_word = "")
  })

  # --- Reset ------------------------------------------------------------------
  observeEvent(input$reset, {
    for (id in names(B_03_04_controls_lst)) {
      set_control(id, B_03_01_defaults_lst[[id]])
    }
    set_scenario_fn(NULL)
  })

  # --- Parameters in force at this stage --------------------------------------
  # A pure function of a list of control values and the stage, so it runs
  # once over the sliders and once over the loaded example for the ghost
  assemble_fn <- function(v, s) {
    list(
      theta     = if (s >= 2) v$theta else B_03_01_defaults_lst$theta,
      lambda    = if (s >= 2) v$lambda else B_03_01_defaults_lst$lambda,
      phi       = if (s >= 2) v$phi else B_03_01_defaults_lst$phi,
      a_res     = v$a_res,
      n         = if (s >= 3) v$n else 0,
      a_start   = if (s >= 2) v$a_start else 1,
      l_start   = v$l_start,
      n_periods = if (s >= 2) round(v$n_periods) else 100,
      shift_at  = round(v$shift_at),
      shift_by  = if (s >= 5) v$shift_by else 0,
      alpha     = if (s >= 6) v$alpha else B_03_01_defaults_lst$alpha,
      saving    = if (s >= 6) v$saving else B_03_01_defaults_lst$saving,
      delta     = if (s >= 6) v$delta else B_03_01_defaults_lst$delta,
      k_start   = if (s >= 6) v$k_start else B_03_01_defaults_lst$k_start
    )
  }

  par_raw <- reactive({
    req(!is.null(input$a_res))
    vals <- stats::setNames(lapply(names(B_03_01_defaults_lst), val),
                            names(B_03_01_defaults_lst))
    assemble_fn(vals, stage_num())
  })

  par_now  <- debounce(par_raw, B_03_12_debounce_ms_int)

  # --- The ghost: every figure at the worked example's own settings -----------
  # The reference is the loaded example's values, or the defaults after Reset.
  # It goes through assemble_fn like the sliders; when the two lists agree the
  # ghost is NULL and every builder skips its ghost layer.
  ref_vals <- reactive({
    key <- scenario()
    if (is.null(key) || !key %in% names(B_03_03_scenarios_lst)) {
      return(B_03_01_defaults_lst)
    }
    utils::modifyList(B_03_01_defaults_lst,
                      B_03_03_scenarios_lst[[key]]$values)
  })

  ref_par <- reactive(assemble_fn(ref_vals(), stage_num()))

  ghost_par <- reactive({
    ref <- ref_par()
    if (T_02_03b_ghost_off_fn(par_now(), ref)) NULL else ref
  })

  # The note about n only speaks once the n slider is on screen (stage 3)
  diag_now <- reactive({
    C_01_09_diagnostics_fn(par_now(), n_live = stage_num() >= 3)
  })

  # The capital block is solved only once it is on screen
  cap_now <- reactive({
    req(stage_num() >= 6)
    C_02_09_capdiag_fn(par_now())
  })

  prob_now <- reactive({
    c(diag_now()$problems,
      if (stage_num() >= 6) C_02_10_problems_fn(par_now()) else character(0))
  })

  # Figures are held back only when C_01_11 or C_02_11 finds no path to draw
  ok_now <- reactive({
    length(c(C_01_11_fatal_fn(par_now()),
             if (stage_num() >= 6) {
               C_02_11_fatal_fn(par_now())
             } else {
               character(0)
             })) == 0
  })

  # --- Scenario story ---------------------------------------------------------
  output$scenario_story <- renderUI({
    T_05_02_story_fn(scn_now(), B_03_04_controls_lst, B_03_05_help_lst)
  })

  # --- The model so far -------------------------------------------------------
  output$eq_title <- renderText({
    T_05_04_stage_name_fn(B_03_02_stages_vec, input$stage)
  })

  eq_items <- reactive(T_06_03_items_fn(B_03_07_equations_lst, stage_num()))

  output$eq_model <- renderUI({
    T_06_04_model_fn(eq_items(), B_03_08_groups_vec,
                     "These appear as the later stages add to the model.")
  })

  output$eq_notation <- renderUI({
    T_06_05_notation_fn(B_03_09_notation_lst, stage_num(),
                        B_03_10_nota_cols_lst, first_stage = 1)
  })

  output$eq_explain <- renderUI({
    T_06_06_explain_fn(eq_items(), B_03_08_groups_vec)
  })

  # --- Prompt and problems ----------------------------------------------------
  output$prompt <- renderUI({
    T_07_12_prompt_fn(scn_now(), input$stage, B_03_06_prompts_lst)
  })

  output$problems <- renderUI(T_07_13_problems_fn(prob_now()))

  # --- Readouts ---------------------------------------------------------------
  output$tiles <- renderUI({
    d <- diag_now()
    s <- stage_num()
    p <- par_now()
    T_04_03_row_fn(
      T_04_01_tile_fn(
        "Workers in research", T_02_06_pct_fn(p$a_res, 0),
        paste0("Output today is ", T_02_06_pct_fn(1 - p$a_res, 0),
               " of what it could be")
      ),
      if (s >= 2) {
        T_04_01_tile_fn(
          "Growth in technology now", T_02_06_pct_fn(d$g_now, 2),
          paste0("After ", p$n_periods, " years: ",
                 T_02_06_pct_fn(d$g_end, 2))
        )
      },
      if (s >= 3) {
        T_04_01_tile_fn(
          "The steady growth rate",
          if (is.na(d$g_steady)) "—" else T_02_06_pct_fn(d$g_steady, 2),
          if (d$endogenous) "phi = 1: set by the research share"
          else "λn/(1−φ): set by population growth",
          class = if (isTRUE(d$g_steady > 0)) "good" else "bad"
        )
      },
      if (s >= 4) {
        T_04_01_tile_fn(
          "Can policy raise growth?",
          if (d$endogenous) "Yes" else "No",
          if (d$endogenous) "Growth is endogenous at phi = 1"
          else "Only the level, not the growth rate",
          class = if (d$endogenous) "good" else "bad"
        )
      },
      if (s >= 5) {
        T_04_01_tile_fn(
          "Best research share",
          if (is.na(d$best_share)) "—" else T_02_06_pct_fn(d$best_share, 0),
          if (isTRUE(d$at_best)) "You are at it" else
            "For the level of output per worker",
          class = if (isTRUE(d$at_best)) "good" else ""
        )
      },
      if (s >= 6) {
        k <- cap_now()
        T_04_01_tile_fn(
          "Capital per effective worker",
          T_02_05_num_fn(k$k_end, 2),
          if (is.na(k$k_star)) "No balanced stock at these parameters" else
            paste0("Heading for ", T_02_05_num_fn(k$k_star, 2),
                   if (isTRUE(k$settled)) "; it is there" else
                     "; still catching up"),
          class = if (isTRUE(k$settled)) "good" else ""
        )
      },
      if (s >= 6) {
        k <- cap_now()
        T_04_01_tile_fn(
          "Growth in output per worker",
          T_02_06_pct_fn(k$g_y_end, 2),
          paste0("Technology grows at ", T_02_06_pct_fn(k$g_tech_end, 2),
                 "; on the balanced path the two are the same")
        )
      },
      if (s >= 7) {
        k <- cap_now()
        T_04_01_tile_fn(
          "Output per effective worker",
          if (is.na(k$y_star)) "—" else T_02_05_num_fn(k$y_star, 2),
          if (is.na(k$y_star) || is.na(k$y_none)) {
            "No balanced level at these parameters"
          } else {
            paste0("With nobody in research it would be ",
                   T_02_05_num_fn(k$y_none, 2),
                   ": that gap is what research costs")
          },
          class = "bad"
        )
      }
    )
  })

  # --- Figures ----------------------------------------------------------------
  output$cost_today <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() < 2)
    D_06_01_cost_today_fn(par_now(), ref = ghost_par())
  }) })

  # A stacked bar takes no ghost: two translucent bars read as a third colour
  output$cost_split <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() < 2)
    D_06_02_split_fn(par_now())
  }) })

  output$growth <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() >= 2)
    D_01_01_growth_fn(par_now(), stage_num(), ref = ghost_par())
  }) })

  output$tech <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() >= 2, stage_num() < 3)
    D_01_03_tech_fn(par_now(), ref = ghost_par())
  }) })

  output$phase <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() >= 3)
    D_01_02_phase_fn(par_now(), ref = ghost_par())
  }) })

  output$share_growth <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() >= 3)
    D_02_01_share_growth_fn(par_now(), ref = ghost_par())
  }) })

  output$share_level <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() >= 3)
    D_02_02_share_level_fn(par_now(), ref = ghost_par())
  }) })

  output$shift_growth <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() >= 5)
    D_03_01_shift_fn(par_now(), "growth", ref = ghost_par())
  }) })

  output$shift_level <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() >= 5)
    D_03_01_shift_fn(par_now(), "level", ref = ghost_par())
  }) })

  output$ktilde <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() >= 6)
    D_04_01_ktilde_fn(par_now(), ref = ghost_par())
  }) })

  output$converge <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() >= 6)
    D_04_02_converge_fn(par_now(), ref = ghost_par())
  }) })

  output$trade_level <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() >= 7)
    D_05_01_level_fn(par_now(), ref = ghost_par())
  }) })

  output$trade_push <- renderPlot({ T_02_01c_draw_fn({
    req(ok_now(), stage_num() >= 7)
    D_05_02_push_fn(par_now(), ref = ghost_par())
  }) })

  output$evidence_note <- renderUI({
    req(stage_num() >= 5)
    tags$div(
      class = "narrative",
      tags$div(class = "nar-head", "What the Evidence Says About phi"),
      tags$p(HTML(paste(
        "If &phi; were one, a larger research workforce would mean faster",
        "growth. The number of researchers in the advanced economies has",
        "risen by orders of magnitude since the 1950s and growth in output",
        "per worker has not accelerated. The same pattern holds industry by",
        "industry: far more researchers are needed to sustain the same rate",
        "of improvement, so research productivity,",
        "&theta;A<sup>&phi;&minus;1</sup> here, is falling. That fits",
        "&phi; below one, not &phi; = 1."
      ))),
      tags$p(HTML(paste(
        "Research is still worth doing. In the semi-endogenous case a",
        "research push raises the level of income permanently, and the",
        "figure on the left shows the country is well short of the share",
        "that would maximise it. Several per cent of income for ever is a",
        "serious policy prize; it is a level effect, not a growth effect.",
        "What decides how hard firms look for ideas is a question about",
        "competition, entry and rents, which is where the Schumpeterian",
        "model of lecture 3.4 starts."
      )))
    )
  })

  output$capital_note <- renderUI({
    req(stage_num() >= 7)
    p <- par_now()
    tags$div(
      class = "narrative",
      tags$div(class = "nar-head",
               "Reading the Two Halves of the Model Together"),
      tags$p(HTML(paste(
        "On the balanced path k&#771; is constant, so output per worker",
        "and capital per worker both grow at g<sub>A</sub> and nothing else.",
        "Saving more, or starting with more capital, moves the level and",
        "leaves the slope where it was. That is Solow's result, and it",
        "survives having the growth of technology explained rather than",
        "assumed."
      ))),
      tags$p(HTML(paste(
        "Output per effective worker on the balanced path is",
        "(s/(n+g*+&delta;))<sup>&alpha;/(1&minus;&alpha;)</sup>",
        "times (1&minus;a). The capital term is a constant, so the research",
        "share acts on the level through (1&minus;a) alone: a worker in a",
        "laboratory is a worker not making anything, and capital",
        "accumulation cannot buy that worker back."
      ))),
      tags$p(HTML(if (isTRUE(p$phi >= 1)) paste(
        "At &phi; = 1 the cost is worth paying. The growth rate is",
        "&theta;(aL)<sup>&lambda;</sup>, which rises with the research",
        "share, so the pushed path is permanently steeper. Set &phi; below",
        "one and the verdict reverses. Part (b) of the exam question asks",
        "for the capital equation and the balanced growth result; the rest",
        "asks what a change in the research share does, and the answer",
        "depends on which case you are in."
      ) else paste(
        "At &phi; &lt; 1 the push buys no growth. The long-run rate is",
        "&lambda;n/(1&minus;&phi;), which has no a in it. A higher research",
        "share lowers output per effective worker and raises the level of",
        "technology, which is why a best share exists, but growth is set by",
        "population growth and by how hard ideas are to find. Set &phi; to 1",
        "and the verdict reverses. Part (b) of the exam question asks for",
        "the capital equation and the balanced growth result; the rest asks",
        "what a change in the research share does, and the answer depends",
        "on which case you are in."
      )))
    )
  })
}

################################################################################
## G: Run ######################################################################
################################################################################
# Note: Launch.

#### G_01: Launch ##############################################################
# Note: Returns the app object.

###### G_01_01: The App ########################################################
# Note: UI from E, server from F.

G_01_01_app_lst <- shinyApp(E_02_01_app_ui_lst, F_01_01_app_server_fn)

G_01_01_app_lst
