CLASS zcl_msq_tdd_calculator DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS add
      IMPORTING number_1      TYPE zmind2_flight_price
                number_2      TYPE zmind2_flight_price
      RETURNING VALUE(result) TYPE zmind2_flight_price.

    METHODS divide
      IMPORTING number_1      TYPE zmind2_flight_price
                number_2      TYPE zmind2_flight_price
      RETURNING VALUE(result) TYPE zmind2_flight_price
      RAISING   cx_sy_zerodivide.
ENDCLASS.


CLASS zcl_msq_tdd_calculator IMPLEMENTATION.
  METHOD add.
    result = number_1 + number_2.
  ENDMETHOD.

  METHOD divide.
    IF number_2 = 0.
      RAISE EXCEPTION NEW cx_sy_zerodivide( ).
    ENDIF.

    result = number_1 / number_2.
  ENDMETHOD.
ENDCLASS.
