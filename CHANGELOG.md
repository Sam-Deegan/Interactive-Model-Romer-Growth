# Changelog

All notable changes to this app. Versions follow [Semantic Versioning](https://semver.org/):
MAJOR for a change to the model or its notation, MINOR for new features
(a stage, a worked example, a figure), PATCH for fixes and wording.
Each release is tagged in git as `vX.Y.Z` and shown in the app footer.

## [1.0.1] - 2026-09-28

### App
- The In Words tab lays out its three columns at fixed widths, so an
  equation no longer collapses to one term per line beside its note.
- The preset card no longer doubles the word "Stage" in front of a stage
  name that already carries it.

## [1.0.0] - 2026-09-28

First public release as a standalone repository.

### Model
- Technology block in the notation of the Part 3 exam questions (Romer 2019,
  ch. 3): the research equation, the growth rate of ideas and its own
  dynamics, the semi-endogenous steady rate λn/(1−φ), the balanced level of
  technology and the research share that maximises output per worker.
- Capital block: output with capital and effective labour, capital per
  effective worker, its balanced stock and the balanced level of output,
  integrated with a fourth-order Runge-Kutta step.
- Calibration notes and no-path checks for φ > 1, φ = 1 with n > 0, no
  population growth, and a capital share outside (0, 1).

### App
- Seven stages (1 to 7) that add one layer of the model at a time.
- Ten worked examples, one card of presets per stage.
- Equations, Notation and In Words tabs that track the model at each stage.
- Readout tiles: research share, growth now and at the end, the steady rate,
  whether policy can raise growth, the best research share, capital per
  effective worker, growth in output per worker, the balanced level of output.
- Ghost curves showing the loaded worked example alongside the live sliders.
- Gordon's headwinds waterfall (D_07_01) for the slides; not mounted in the
  UI.
