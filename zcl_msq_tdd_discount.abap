CLASS zcl_msq_tdd_discount DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_msq_tdd_discount .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_msq_tdd_discount IMPLEMENTATION.


  METHOD zif_msq_tdd_discount~get_max_discount.
    result = 0.
  ENDMETHOD.
ENDCLASS.
