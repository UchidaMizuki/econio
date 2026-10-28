# io_trophic_level aborts on an industry with no links to others

    Code
      io_trophic_level(iotable)
    Condition
      Error in `io_trophic_level()`:
      ! Can't compute trophic levels because the domestic production network is not connected.
      i Trophic levels in separate components are not comparable.
      i Remove or merge industries that have no transactions with the others.

---

    Code
      io_trophic_incoherence(iotable)
    Condition
      Error in `io_trophic_incoherence()`:
      ! Can't compute trophic levels because the domestic production network is not connected.
      i Trophic levels in separate components are not comparable.
      i Remove or merge industries that have no transactions with the others.

# io_trophic aborts on separate components

    Code
      io_trophic(laplacian)
    Condition
      Error:
      ! Can't compute trophic levels because the domestic production network is not connected.
      i Trophic levels in separate components are not comparable.
      i Remove or merge industries that have no transactions with the others.

