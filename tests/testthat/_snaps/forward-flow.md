# io_forward_flow validates order

    Code
      io_forward_flow(iotable, dibble::dibble(1:3, .dim_names = list(x = 1:3)))
    Condition
      Error in `io_industry_order_values()`:
      ! `order` must be a dibble with a single industry dimension.

