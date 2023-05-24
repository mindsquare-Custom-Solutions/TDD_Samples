# Begleitmaterialien zur Test Driven Development (TDD) Schulung

## Beispielcode

### zcl_aunit_calculator

```abap
CLASS zcl_aunit_calculator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS add
      IMPORTING number_1      TYPE i
                number_2      TYPE i
      RETURNING VALUE(result) TYPE i.
    METHODS divide
      IMPORTING number_1      TYPE i
                number_2      TYPE i
      RETURNING VALUE(result) TYPE i
      RAISING   cx_sy_zerodivide.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aunit_calculator IMPLEMENTATION.
  METHOD add.
    result = number_1 + number_2.
  ENDMETHOD.

  METHOD divide.
    IF number_2 = 0.
      RAISE EXCEPTION TYPE cx_sy_zerodivide.
    ENDIF.

    result = number_1 / number_2.
  ENDMETHOD.
ENDCLASS.
```

### zcl_aunit_converter

```abap
CLASS zcl_aunit_converter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: add_and_convert
      IMPORTING number_1      TYPE i
                number_2      TYPE i
                currency      TYPE waers
                bukrs         TYPE bukrs
      RETURNING VALUE(result) TYPE char20.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aunit_converter IMPLEMENTATION.
  METHOD add_and_convert.
    data: sum_in_target_currency TYPE f.

    select single waers from t001 where bukrs = @bukrs into @data(target_currency).
    if sy-subrc <> 0.
        result = 'invalid bukrs'.
        return.
    endif.

    data(calculator) = new zcl_aunit_calculator(  ).

    data(sum) = calculator->add( number_1 = number_1 number_2 = number_2 ).

    call function 'CONVERT_TO_LOCAL_CURRENCY'
        EXPORTING
            date = sy-datum
            foreign_amount = sum
            foreign_currency = currency
            local_currency = target_currency
        IMPORTING
            local_amount = sum_in_target_currency
        EXCEPTIONS
            NO_RATE_FOUND = 1
            OVERFLOW = 2
            NO_FACTORS_FOUND = 3
            NO_SPREAD_FOUND = 4
            derived_2_times = 5
            OTHERS = 6.

    result = |{ sum_in_target_currency } { target_currency }|.
  ENDMETHOD.

ENDCLASS.
```
