# io_trophic_level returns NA for an industry with no links to others

    Code
      trophic_level <- io_trophic_level(iotable)
    Message
      Returning `NA` for 1 industry without domestic intermediate transactions with other industries: "industry_1".

# io_trophic_level aborts when no industries have links to others

    Code
      io_trophic_level(iotable)
    Condition
      Error in `io_trophic_level()`:
      ! No industries have domestic intermediate transactions with other industries.

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

