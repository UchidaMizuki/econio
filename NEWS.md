# econio (development version)

* Added a `NEWS.md` file to track changes to the package.
* `io_check_axes()` is now called by `io_leontief_inverse()` and `io_ghosh_inverse()` to validate that input and output industry axes match before matrix inversion, preventing errors from mismatched tables.
* `io_check_totals()` now validates `total_tolerance` with a clearer error message.
* `io_downstreamness()` returns the column sums of the Leontief inverse, i.e. the output multiplier (backward linkage), with a `normalize` option to obtain the power of dispersion.
* `io_input_coef()`, `io_output_coef()`, `io_leontief_inverse()`, and `io_ghosh_inverse()` now report invalid `open_economy` values against the calling function with a clearer error message.
* `io_reclass()` now validates `from_col`, `to_col`, `weight_col`, `weight_tolerance`, and `check_axes`.
* `io_spectral_embedding()` is now marked as experimental.
* `io_spectral_embedding()` returns the Laplacian spectral embedding (Fiedler vector) of the domestic production network, minimizing the graph Dirichlet energy; pair it with `io_trophic_level()` for a directed supply-chain map.
* `io_spectral_embedding()`, `io_trophic_incoherence()` and `io_trophic_level()` now inform when they implicitly convert a competitive import type table to noncompetitive import type before building the industry network.
* `io_spectral_embedding()`, `io_trophic_incoherence()` and `io_trophic_level()` now inform and zero out negative domestic intermediate transactions (e.g. from byproduct treatment such as the Stone method) before building the industry network, instead of producing an ill-defined result.
* `io_spectral_embedding()` now reports an invalid `dims` argument with a clearer error message.
* `io_table_regional()` and `io_table_multiregional()` now validate `check_axes`, `competitive_import`, and `total_tolerance`.
* `io_table_to_competitive_import()`, `io_table_to_noncompetitive_import()`, and `io_table_to_regional()` now validate their sector-name and tolerance arguments.
* `io_total_input()` and `io_total_output()` now validate `same_region`.
* `io_trophic_incoherence()` returns the trophic incoherence of the domestic production network.
* `io_trophic_level()` returns the directed-network trophic level of each industry, following MacKay, Johnson and Sansom (2020).
* `io_upstreamness()` returns the row sums of the Ghosh inverse, i.e. the forward linkage.
