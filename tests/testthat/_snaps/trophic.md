# io_trophic_level aborts on an industry with no links to others

    Code
      io_trophic_level(iotable)
    Condition
      Error in `io_trophic_level()`:
      ! The domestic production network must be connected.
      i Results for separate parts of the network are not comparable.
      i Remove or merge industries that are not linked to the others.

---

    Code
      io_trophic_incoherence(iotable)
    Condition
      Error in `io_trophic_incoherence()`:
      ! The domestic production network must be connected.
      i Results for separate parts of the network are not comparable.
      i Remove or merge industries that are not linked to the others.

# io_trophic aborts on separate components

    Code
      io_trophic(network)
    Condition
      Error:
      ! The domestic production network must be connected.
      i Results for separate parts of the network are not comparable.
      i Remove or merge industries that are not linked to the others.

# io_trophic_level validates paths

    Code
      io_trophic_level(iotable, paths = "indirect")
    Condition
      Error in `io_trophic_level()`:
      ! `paths` must be one of "direct" or "all", not "indirect".
      i Did you mean "direct"?

