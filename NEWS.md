
# CDSim 0.1.2

## Major improvements

- Revised climate simulation engine (`simulate_climate_series()`):
  - Replaced the previous implementation with a more physically consistent stochastic framework.
  - Improved representation of seasonality, temporal persistence (AR(1)), and long-term trends.
  - Introduced bounded temperature generation using truncated normal (Tmin) and gamma-based perturbation (Tmax).
  - Enhanced rainfall modelling using a first-order Markov chain for occurrence and gamma-distributed intensity.
  - Added rainfall–temperature coupling, ensuring realistic negative dependence between precipitation and maximum temperature.
  - Enforced diurnal consistency constraint (Tmax > Tmin) after rounding.

## Enhancements

- Improved statistical realism of simulated outputs:
  - More realistic skewness in rainfall distributions.
  - Better alignment of autocorrelation structure with climatological expectations.
  - Strengthened cross-variable relationships (temperature–rainfall coupling).

- Updated internal validation workflow:
  - `validate_climate_internal()` now performs more robust checks on:
    - physical constraints
    - distributional properties
    - temporal autocorrelation
    - trend behavior
    - seasonal patterns
