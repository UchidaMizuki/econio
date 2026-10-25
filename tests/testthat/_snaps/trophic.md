# io_trophic_level aborts on an industry with no links to others

    Code
      io_trophic_level(iotable)
    Condition
      Error in `io_trophic_level()`:
      ! The domestic production network must be connected, but it has 2 separate components.
      i Trophic levels in separate components are not comparable.
      i 1 industry has no transactions with other industries: "industry_1".
      i Component sizes: 1 and 2.
      i Remove or merge disconnected industries before computing trophic levels.

---

    Code
      io_trophic_incoherence(iotable)
    Condition
      Error in `io_trophic_incoherence()`:
      ! The domestic production network must be connected, but it has 2 separate components.
      i Trophic levels in separate components are not comparable.
      i 1 industry has no transactions with other industries: "industry_1".
      i Component sizes: 1 and 2.
      i Remove or merge disconnected industries before computing trophic levels.

# io_check_connected aborts on separate components

    Code
      io_check_connected(laplacian)
    Condition
      Error:
      ! The domestic production network must be connected, but it has 2 separate components.
      i Trophic levels in separate components are not comparable.
      i Component sizes: 2 and 2.
      i Remove or merge disconnected industries before computing trophic levels.

