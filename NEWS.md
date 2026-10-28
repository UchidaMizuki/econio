# econio (development version)

* Added a `NEWS.md` file to track changes to the package.
* `io_check_axes()` is now called by `io_leontief_inverse()` and `io_ghosh_inverse()` to validate that input and output industry axes match before matrix inversion, preventing errors from mismatched tables.
* `io_check_axes()`, `io_check_totals()`, `io_table_to_competitive_import()`, and `io_table_to_noncompetitive_import()` now label mismatched sectors of multi-regional tables with their region (e.g. `region_1/industry_1`) in error messages.
* `io_check_totals()` now validates `total_tolerance` with a clearer error message.
* `io_downstreamness()` returns the column sums of the Leontief inverse, i.e. the output multiplier (backward linkage), with a `normalize` option to obtain the power of dispersion.
* `io_input_coef()`, `io_output_coef()`, `io_leontief_inverse()`, and `io_ghosh_inverse()` now report invalid `open_economy` values against the calling function with a clearer error message.
* `io_reclass()` now validates `from_col`, `to_col`, `weight_col`, `weight_tolerance`, and `check_axes`.
* `io_spectral_embedding()` is now marked as experimental.
* `io_spectral_embedding()` returns the Laplacian spectral embedding (Fiedler vector) of the domestic production network, minimizing the graph Dirichlet energy; pair it with `io_trophic_level()` for a directed supply-chain map.
* `io_spectral_embedding()`, `io_trophic_incoherence()` and `io_trophic_level()` now inform when they implicitly convert a competitive import type table to noncompetitive import type before building the industry network.
* `io_spectral_embedding()`, `io_trophic_incoherence()` and `io_trophic_level()` now abort with a clear error when negative domestic intermediate transactions (e.g. from byproduct treatment such as the Stone method) are found before building the industry network, pointing to `io_table_to_byproduct_transfer()`, instead of silently zeroing them out.
* `io_spectral_embedding()` now reports an invalid `dims` argument with a clearer error message.
* `io_spectral_embedding()` gains a `normalized` argument and now uses the normalized Laplacian by default, solving `Λh = λ diag(u) h` so that the embedding no longer concentrates on industries with little intermediate trade; use `normalized = FALSE` for the previous behavior. This changes the default node order of `econiochart::io_chart_sankey()`. It now also aborts when the domestic production network is not connected, where the embedding would be arbitrary (#29).
* `io_spectral_embedding()`, `io_trophic_incoherence()` and `io_trophic_level()` gain an experimental `paths` argument; `paths = "all"` weights the network by all direct and indirect transactions, `(L - I) diag(x)`, with each edge spanning its average propagation length (#31).
* `io_table_regional()` and `io_table_multiregional()` now validate `check_axes`, `import_type`, and `total_tolerance`; the former `competitive_import` logical argument is replaced by `import_type`, taking `"competitive_import"` or `"noncompetitive_import"`.
* `io_table_to_byproduct_transfer()` is a new function that converts negative domestic intermediate transactions (e.g. from byproduct treatment such as the Stone method) to the transfer method, adding the absolute value of a negative transaction to its reverse-direction cell.
* `io_table_to_competitive_import()`, `io_table_to_noncompetitive_import()`, and `io_table_to_regional()` now validate their sector-name and tolerance arguments.
* `io_table_to_competitive_import()` and `io_table_to_noncompetitive_import()` now report mismatched `data_import` totals against the calling function and include the region of each mismatched sector in multiregional tables (#10).
* `io_table_to_noncompetitive_import()` no longer leaves a leftover zero-valued `import` sector on the output axis when converting from a competitive import type table.
* `io_table_to_regional()` no longer drops imports to final demand when splitting a noncompetitive import type multi-regional table, and now errors clearly when `data` is not a multi-regional table.
* `io_total_input()` and `io_total_output()` now validate `same_region`.
* `io_trophic_incoherence()` returns the trophic incoherence of the domestic production network.
* `io_trophic_level()` returns the directed-network trophic level of each industry, following MacKay, Johnson and Sansom (2020), centered so that the mean weighted by each industry's total intermediate transactions (in plus out) is 0. It and `io_trophic_incoherence()` abort with an informative error when the domestic production network is not connected, since levels in separate components are not comparable (#30).
* `io_upstreamness()` returns the row sums of the Ghosh inverse, i.e. the forward linkage.
