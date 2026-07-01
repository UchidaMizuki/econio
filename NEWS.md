# econio (development version)

* Added a `NEWS.md` file to track changes to the package.
* `io_check_axes()` is now called by `io_leontief_inverse()` and `io_ghosh_inverse()` to validate that input and output industry axes match before matrix inversion, preventing errors from mismatched tables.
* `io_downstreamness()` returns the column sums of the Leontief inverse, i.e. the output multiplier (backward linkage), with a `normalize` option to obtain the power of dispersion.
* `io_forward_flow()` zeros out domestic intermediate transactions that run backward or between same-level industries relative to a supplied per-industry ordering, such as `io_trophic_level()`.
* `io_spectral_embedding()` returns the Laplacian spectral embedding (Fiedler vector) of the domestic production network, minimizing the graph Dirichlet energy; pair it with `io_trophic_level()` for a directed supply-chain map.
* `io_trophic_incoherence()` returns the trophic incoherence of the domestic production network.
* `io_trophic_level()` returns the directed-network trophic level of each industry, following MacKay, Johnson and Sansom (2020).
* `io_upstreamness()` returns the row sums of the Ghosh inverse, i.e. the forward linkage.
