# Package index

## Data summaries

- [`sum_bilat()`](sum_bilat.md) : Summary of bilateral flows,
  counter-flow and net migration flow
- [`sum_region()`](sum_region.md) [`sum_country()`](sum_region.md)
  [`sum_unilat()`](sum_region.md) [`sum_unilateral()`](sum_region.md) :
  Unilateral summaries of in-, out-, turnover and net-migration totals
  from an origin-destination migration flow matrix or data frame.
- [`sum_lump()`](sum_lump.md) : Sum and lump together small flows into a
  "other" category
- [`sum_expand()`](sum_expand.md) : Sum bilateral data to include
  aggregate bilateral totals for origin and destination meta areas
- [`multi_comp()`](multi_comp.md) : Multiplicative component description
  of origin-destination migration flow tables
- [`multi_comp2()`](multi_comp2.md) : Multiplicative component
  descriptions of origin-destination flow tables based on total
  reference coding system.

## Visualisation

- [`mig_chord()`](mig_chord.md) : Chord diagram for directional
  origin-destination data
- [`str_wrap_n()`](str_wrap_n.md) : Wrap character string to fit a
  target number of lines

## Indices

- [`index_age()`](index_age.md) : Summary indices of migration age
  profile
- [`index_age_rc()`](index_age_rc.md) : Summary indices of age migration
  profile based on parameters from a Rogers and Castro schedule
- [`index_connectivity()`](index_connectivity.md) : Summary indices of
  migration connectivity
- [`index_distance()`](index_distance.md) : Summary indices of migration
  distance
- [`index_impact()`](index_impact.md) : Summary indices of migration
  impact
- [`index_intensity()`](index_intensity.md) : Summary indices of
  migration intensity

## Estimation: flows from stocks

- [`ffs_demo()`](ffs_demo.md) : Estimation of bilateral migrant flows
  from bilateral migrant stocks using demographic accounting approaches
- [`ffs_diff()`](ffs_diff.md) : Estimation of bilateral migrant flows
  from bilateral migrant stocks using stock differencing approaches
- [`ffs_rates()`](ffs_rates.md) : Estimation of bilateral migrant flows
  from bilateral migrant stocks using rates approaches

## Estimation: constrained maximisation routines

- [`cm2()`](cm2.md) : Conditional maximization routine for the indirect
  estimation of origin-destination migration flow table with known
  margins
- [`cm3()`](cm3.md) : Conditional maximization routine for the indirect
  estimation of origin-destination-migrant type migration flow tables
  with known origin and destination margins.
- [`cm_net()`](cm_net.md) : Conditional maximization routine for the
  indirect estimation of origin-destination-type migration flow tables
  with known net migration totals.
- [`cm_net_tot()`](cm_net_tot.md) : Conditional maximization routine for
  the indirect estimation of origin-destination-type migration flow
  tables with known net migration and grand totals.

## Estimation: iterative proportional fitting procedures

- [`ipf2()`](ipf2.md) : Iterative proportional fitting routine for the
  indirect estimation of origin-destination migration flow table with
  known margins.
- [`ipf3()`](ipf3.md) : Iterative proportional fitting routine for the
  indirect estimation of origin-destination-migrant type migration flow
  tables with known origin and destination margins.
- [`ipf2_block()`](ipf2_block.md) : Iterative proportional fitting
  routine for the indirect estimation of origin-destination-type
  migration flow tables with known origin and destination margins and
  block diagonal elements.
- [`ipf2_stripe()`](ipf2_stripe.md) : iterative proportional fitting
  routine for the indirect estimation of origin-destination-type
  migration flow tables with known origin and destination margins and
  stripe elements.

## Estimation: net migration

- [`net_matrix_ipf()`](net_matrix_ipf.md) : Estimate Migration Flows to
  Match Net Totals via Iterative Proportional Fitting
- [`net_sr()`](net_sr.md) : Estimate net migration from survival ratios
  applied to lifetime migration data
- [`net_vs()`](net_vs.md) : Estimate net migration from vital statistics

## Rescaling functions

- [`rescale_net()`](rescale_net.md) : Rescale net migration total to a
  global zero sum
- [`rescale_integer_sum()`](rescale_integer_sum.md) : Rescale integer
  vector to a set sum

## Rogers Castro model age schedule parameters

- [`rc_model_fund`](rc_model_fund.md) : Fundamental parameters for
  Rogers-Castro migration schedule
- [`rc_model_un`](rc_model_un.md) : Model parameters for six
  Rogers-Castro migration schedules proposed by UN DESA

## Creating special matrices

- [`block_matrix()`](block_matrix.md) : Create a block matrix with
  non-uniform block sizes.
- [`stripe_matrix()`](stripe_matrix.md) : Create a stripped matrix with
  non-uniform block sizes.

## Miscellaneous

- [`quadratic_eqn()`](quadratic_eqn.md) : Solutions from the quadratic
  equation
- [`dict_ims`](dict_ims.md) : Dictionary to look up region geographies
  based on countries used in UN DESA International Migrant Stock.

## Data

- [`alabama_1970`](alabama_1970.md) : Alabama population totals in 1960
  and 1970 by age, sex and race
- [`bombay_1951`](bombay_1951.md) : Bombay population totals in 1941 and
  1951 by age
- [`indian_sub`](indian_sub.md) : Lifetime migration totals for states
  and zones in the Indian 1901 to 1931
- [`ipumsi_age`](ipumsi_age.md) : Age specific migration and population
  counts from two IPUMSI samples
- [`italy_area`](italy_area.md) : Single year age-specific origin
  destination migration flows between Italian NUTS1 areas
- [`korea_gravity`](korea_gravity.md) : Annual origin destination
  migration flows between Korean regions alongside selected geographic,
  economic and demographic variables.
- [`manila_1970`](manila_1970.md) : Manila female population 1970 by age
- [`new_england_1960`](new_england_1960.md) : New England male
  white-native population totals in 1950 and 1960 by place of birth and
  age
- [`uar_1960`](uar_1960.md) : Lifetime migration data for Governorates
  of United Arab Republic in 1960
- [`usa_1960`](usa_1960.md) : US population totals in 1950 and 1960 by
  place of birth, age, sex and race
