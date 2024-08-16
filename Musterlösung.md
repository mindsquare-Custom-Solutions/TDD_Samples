# Lösungsvorschläge für Übungsaufgaben

## Erstellung eines ABAP Unit Tests

Lokale Testklasse unter der Calculator Klasse
```abap
CLASS ltcl_calculator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      add_positive_numbers FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_calculator IMPLEMENTATION.

  METHOD add_positive_numbers.
    "GIVEN
    DATA(number1) = 1.
    DATA(number2) = 2.
    DATA(expected) = 3.

    DATA(calculator) = NEW zcl_aunit_calculator( ).

    "WHEN
    DATA(actual) = calculator->add( number_1 = number1 number_2 = number2 ).

    "THEN
    cl_abap_unit_assert=>assert_equals( act = actual exp = expected ).
  ENDMETHOD.

ENDCLASS.
```

##  Hinzufügen einer neuen Methode nach TDD-Ansatz

Geänderte Calculator Klasse
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
    METHODS absolute
      IMPORTING
        number        TYPE i
      RETURNING
        VALUE(result) TYPE i.
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

  METHOD absolute.
    result = COND #( WHEN number < 0 THEN number * -1
                     ELSE number ).
  ENDMETHOD.

ENDCLASS.
```

Lokale Testklasse unter der Calculator Klasse
```abap
CLASS ltcl_absolute DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      absolute_negative_number FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_absolute IMPLEMENTATION.

  METHOD absolute_negative_number.
    "GIVEN
    DATA(number) = -1.
    DATA(expected) = 1.

    DATA(calculator) = NEW zcl_aunit_calculator( ).

    "WHEN
    DATA(actual) = calculator->absolute( number = number ).

    "THEN
    cl_abap_unit_assert=>assert_equals( act = actual exp = expected ).
  ENDMETHOD.

ENDCLASS.
```

## Auslagern der Methoden in Interfaces

Calculator Interface
```abap
interface ZIF_AUNIT_CALCULATOR
  public .
  METHODS add
    IMPORTING number_1      TYPE i
              number_2      TYPE i
    RETURNING VALUE(result) TYPE i.
  METHODS divide
    IMPORTING number_1      TYPE i
              number_2      TYPE i
    RETURNING value(result) TYPE i
    RAISING   cx_sy_zerodivide.
  METHODS absolute
    IMPORTING
      number        TYPE i
    RETURNING
      value(result) TYPE i.

endinterface.
```

Calculator Klasse
```abap
CLASS zcl_aunit_calculator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_aunit_calculator.
    ALIASES: add FOR zif_aunit_calculator~add,
             divide FOR zif_aunit_calculator~divide,
             absolute FOR zif_aunit_calculator~absolute.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_aunit_calculator IMPLEMENTATION.
  METHOD zif_aunit_calculator~add.
    result = number_1 + number_2.
  ENDMETHOD.

  METHOD zif_aunit_calculator~divide.
    IF number_2 = 0.
      RAISE EXCEPTION TYPE cx_sy_zerodivide.
    ENDIF.

    result = number_1 / number_2.
  ENDMETHOD.

  METHOD zif_aunit_calculator~absolute.
    result = COND #( WHEN number < 0 THEN number * -1
                     ELSE number ).
  ENDMETHOD.

ENDCLASS.
```

## Dependency Injection

### Konstruktor Injection

Converter Klasse
```abap
CLASS zcl_aunit_converter_ki DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING calculator TYPE REF TO zif_aunit_calculator OPTIONAL,
      add_and_convert
        IMPORTING number_1      TYPE i
                  number_2      TYPE i
                  currency      TYPE waers
                  bukrs         TYPE bukrs
        RETURNING VALUE(result) TYPE char20.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: calculator TYPE REF TO zif_aunit_calculator.
ENDCLASS.



CLASS zcl_aunit_converter_ki IMPLEMENTATION.

  METHOD constructor.
    me->calculator = COND #( WHEN calculator IS BOUND THEN calculator
                             ELSE NEW zcl_aunit_calculator( ) ).
  ENDMETHOD.

  METHOD add_and_convert.
    DATA: sum_in_target_currency TYPE f.

    SELECT SINGLE waers FROM t001 WHERE bukrs = @bukrs INTO @DATA(target_currency).
    IF sy-subrc <> 0.
      result = 'invalid bukrs'.
      RETURN.
    ENDIF.

    DATA(sum) = calculator->add( number_1 = number_1 number_2 = number_2 ).

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
        date             = sy-datum
        foreign_amount   = sum
        foreign_currency = currency
        local_currency   = target_currency
      IMPORTING
        local_amount     = sum_in_target_currency
      EXCEPTIONS
        no_rate_found    = 1
        overflow         = 2
        no_factors_found = 3
        no_spread_found  = 4
        derived_2_times  = 5
        OTHERS           = 6.

    result = |{ sum_in_target_currency } { target_currency }|.
  ENDMETHOD.
ENDCLASS.
```

Testklasse 
```abap
CLASS lcl_calculator_dummy DEFINITION CREATE PUBLIC FOR TESTING.

  PUBLIC SECTION.
    INTERFACES: zif_aunit_calculator PARTIALLY IMPLEMENTED.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_calculator_dummy IMPLEMENTATION.
  METHOD zif_aunit_calculator~add.
    " Implementierung des Dummies
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_constructor_injection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      converter_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_constructor_injection IMPLEMENTATION.

  METHOD converter_test.
    " GIVEN
    DATA: calculator TYPE REF TO zif_aunit_calculator.
    calculator = NEW lcl_calculator_dummy( ).
    data(converter) = new zcl_aunit_converter_ki( calculator = calculator ).

    " WHEN
    data(result) = converter->add_and_convert( bukrs = '1000' currency = 'EUR' number_1 = 1 number_2 = 2 ).

    " THEN
    " Testimplementierung
  ENDMETHOD.

ENDCLASS.
```

### Setter Injection

Converter Klasse 
```abap
CLASS zcl_aunit_converter_si DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor,
      set_calculator
        IMPORTING calculator TYPE REF TO zif_aunit_calculator,
      add_and_convert
        IMPORTING number_1      TYPE i
                  number_2      TYPE i
                  currency      TYPE waers
                  bukrs         TYPE bukrs
        RETURNING VALUE(result) TYPE char20.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: calculator TYPE REF TO zif_aunit_calculator.
ENDCLASS.

CLASS zcl_aunit_converter_si IMPLEMENTATION.

  METHOD constructor.
    me->calculator = NEW zcl_aunit_calculator( ).
  ENDMETHOD.

  METHOD set_calculator.
    me->calculator = calculator.
  ENDMETHOD.

  METHOD add_and_convert.
    DATA: sum_in_target_currency TYPE f.

    SELECT SINGLE waers FROM t001 WHERE bukrs = @bukrs INTO @DATA(target_currency).
    IF sy-subrc <> 0.
      result = 'invalid bukrs'.
      RETURN.
    ENDIF.

    DATA(sum) = calculator->add( number_1 = number_1 number_2 = number_2 ).

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
        date             = sy-datum
        foreign_amount   = sum
        foreign_currency = currency
        local_currency   = target_currency
      IMPORTING
        local_amount     = sum_in_target_currency
      EXCEPTIONS
        no_rate_found    = 1
        overflow         = 2
        no_factors_found = 3
        no_spread_found  = 4
        derived_2_times  = 5
        OTHERS           = 6.

    result = |{ sum_in_target_currency } { target_currency }|.
  ENDMETHOD.
ENDCLASS.
```

Testklasse
```abap
CLASS lcl_calculator_dummy DEFINITION CREATE PUBLIC FOR TESTING.

  PUBLIC SECTION.
    INTERFACES: zif_aunit_calculator PARTIALLY IMPLEMENTED.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_calculator_dummy IMPLEMENTATION.
  METHOD zif_aunit_calculator~add.
    " Implementierung des Dummies
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_setter_injection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      converter_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_setter_injection IMPLEMENTATION.

  METHOD converter_test.
    " GIVEN
    DATA: calculator TYPE REF TO zif_aunit_calculator.
    calculator = NEW lcl_calculator_dummy( ).
    DATA(converter) = NEW zcl_aunit_converter_si( ).
    converter->set_calculator( calculator ).

    " WHEN
    DATA(result) = converter->add_and_convert( bukrs = '1000' currency = 'EUR' number_1 = 1 number_2 = 2 ).

    " THEN
    " Testimplementierung
  ENDMETHOD.

ENDCLASS.
```

### Parameter Injection

Converter Klasse
```abap
CLASS zcl_aunit_converter_pi DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      add_and_convert
        IMPORTING number_1      TYPE i
                  number_2      TYPE i
                  currency      TYPE waers
                  bukrs         TYPE bukrs
                  calculator    TYPE REF TO zif_aunit_calculator
        RETURNING VALUE(result) TYPE char20.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: calculator TYPE REF TO zif_aunit_calculator.
ENDCLASS.

CLASS zcl_aunit_converter_pi IMPLEMENTATION.

  METHOD add_and_convert.
    DATA: sum_in_target_currency TYPE f,
          calculator_interface   TYPE REF TO zif_aunit_calculator.

    calculator_interface = COND #( WHEN calculator IS BOUND THEN calculator
                                   ELSE NEW zcl_aunit_calculator( ) ).

    SELECT SINGLE waers FROM t001 WHERE bukrs = @bukrs INTO @DATA(target_currency).
    IF sy-subrc <> 0.
      result = 'invalid bukrs'.
      RETURN.
    ENDIF.

    DATA(sum) = calculator_interface->add( number_1 = number_1 number_2 = number_2 ).

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
        date             = sy-datum
        foreign_amount   = sum
        foreign_currency = currency
        local_currency   = target_currency
      IMPORTING
        local_amount     = sum_in_target_currency
      EXCEPTIONS
        no_rate_found    = 1
        overflow         = 2
        no_factors_found = 3
        no_spread_found  = 4
        derived_2_times  = 5
        OTHERS           = 6.

    result = |{ sum_in_target_currency } { target_currency }|.
  ENDMETHOD.
ENDCLASS.
```

Testklasse
```abap
CLASS lcl_calculator_dummy DEFINITION CREATE PUBLIC FOR TESTING.

  PUBLIC SECTION.
    INTERFACES: zif_aunit_calculator PARTIALLY IMPLEMENTED.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_calculator_dummy IMPLEMENTATION.
  METHOD zif_aunit_calculator~add.
    " Implementierung des Dummies
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_parameter_injection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      converter_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_parameter_injection IMPLEMENTATION.

  METHOD converter_test.
    " GIVEN
    DATA: calculator TYPE REF TO zif_aunit_calculator.
    calculator = NEW lcl_calculator_dummy( ).
    DATA(converter) = NEW zcl_aunit_converter_pi( ).

    " WHEN
    DATA(result) = converter->add_and_convert( bukrs = '1000' currency = 'EUR' number_1 = 1 number_2 = 2 calculator = calculator ).

    " THEN
    " Testimplementierung
  ENDMETHOD.

ENDCLASS.
```

### Backdoor Injection

Converter Klasse
```abap
CLASS zcl_aunit_converter_bi DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor,
      add_and_convert
        IMPORTING number_1      TYPE i
                  number_2      TYPE i
                  currency      TYPE waers
                  bukrs         TYPE bukrs
        RETURNING VALUE(result) TYPE char20.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: calculator TYPE REF TO zif_aunit_calculator.
ENDCLASS.

CLASS zcl_aunit_converter_bi IMPLEMENTATION.

  METHOD constructor.
    me->calculator = NEW zcl_aunit_calculator( ).
  ENDMETHOD.

  METHOD add_and_convert.
    DATA: sum_in_target_currency TYPE f.

    SELECT SINGLE waers FROM t001 WHERE bukrs = @bukrs INTO @DATA(target_currency).
    IF sy-subrc <> 0.
      result = 'invalid bukrs'.
      RETURN.
    ENDIF.

    DATA(sum) = calculator->add( number_1 = number_1 number_2 = number_2 ).

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
        date             = sy-datum
        foreign_amount   = sum
        foreign_currency = currency
        local_currency   = target_currency
      IMPORTING
        local_amount     = sum_in_target_currency
      EXCEPTIONS
        no_rate_found    = 1
        overflow         = 2
        no_factors_found = 3
        no_spread_found  = 4
        derived_2_times  = 5
        OTHERS           = 6.

    result = |{ sum_in_target_currency } { target_currency }|.
  ENDMETHOD.
ENDCLASS.
```

Testklasse
```abap
class ltcl_backdoor_injection DEFINITION DEFERRED.
class zcl_aunit_converter_bi DEFINITION local FRIENDS ltcl_backdoor_injection.

CLASS lcl_calculator_dummy DEFINITION CREATE PUBLIC FOR TESTING.

  PUBLIC SECTION.
    INTERFACES: zif_aunit_calculator PARTIALLY IMPLEMENTED.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_calculator_dummy IMPLEMENTATION.
  METHOD zif_aunit_calculator~add.
    " Implementierung des Dummies
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_backdoor_injection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      converter_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_backdoor_injection IMPLEMENTATION.

  METHOD converter_test.
    " GIVEN
    DATA: calculator TYPE REF TO zif_aunit_calculator.
    calculator = NEW lcl_calculator_dummy( ).
    DATA(converter) = NEW zcl_aunit_converter_bi( ).
    converter->calculator = calculator.

    " WHEN
    DATA(result) = converter->add_and_convert( bukrs = '1000' currency = 'EUR' number_1 = 1 number_2 = 2 ).

    " THEN
    " Testimplementierung
  ENDMETHOD.

ENDCLASS.
```

### Factory Injection

Calculator Klasse
```abap
CLASS zcl_aunit_calculator_fi DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE GLOBAL FRIENDS zcl_aunit_calculator_factory.

  PUBLIC SECTION.
    INTERFACES: zif_aunit_calculator.
    ALIASES: add FOR zif_aunit_calculator~add,
             divide FOR zif_aunit_calculator~divide,
             absolute FOR zif_aunit_calculator~absolute.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_aunit_calculator_fi IMPLEMENTATION.
  METHOD zif_aunit_calculator~add.
    result = number_1 + number_2.
  ENDMETHOD.

  METHOD zif_aunit_calculator~divide.
    IF number_2 = 0.
      RAISE EXCEPTION TYPE cx_sy_zerodivide.
    ENDIF.

    result = number_1 / number_2.
  ENDMETHOD.

  METHOD zif_aunit_calculator~absolute.
    result = COND #( WHEN number < 0 THEN number * -1
                     ELSE number ).
  ENDMETHOD.

ENDCLASS.
```

Calculator Factory
```abap
CLASS zcl_aunit_calculator_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE GLOBAL FRIENDS zcl_aunit_calculator_injector.

  PUBLIC SECTION.
    CLASS-METHODS:
      get_calculator
        RETURNING VALUE(result) TYPE REF TO zif_aunit_calculator.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: calculator TYPE REF TO zif_aunit_calculator.
ENDCLASS.

CLASS zcl_aunit_calculator_factory IMPLEMENTATION.
  METHOD get_calculator.
    result = COND #( WHEN calculator IS BOUND THEN calculator
                     ELSE NEW zcl_aunit_calculator_fi( ) ).
  ENDMETHOD.

ENDCLASS.
```

Calculator Injetor
```abap
CLASS zcl_aunit_calculator_injector DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      inject
        IMPORTING
          calculator TYPE REF TO zif_aunit_calculator.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_aunit_calculator_injector IMPLEMENTATION.
  METHOD inject.
    zcl_aunit_calculator_factory=>calculator = calculator.
  ENDMETHOD.

ENDCLASS.
```

Converter Klasse
```abap
CLASS zcl_aunit_converter_fi DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      add_and_convert
        IMPORTING number_1      TYPE i
                  number_2      TYPE i
                  currency      TYPE waers
                  bukrs         TYPE bukrs
        RETURNING VALUE(result) TYPE char20.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_aunit_converter_fi IMPLEMENTATION.
  METHOD add_and_convert.
    DATA: sum_in_target_currency TYPE f.

    SELECT SINGLE waers FROM t001 WHERE bukrs = @bukrs INTO @DATA(target_currency).
    IF sy-subrc <> 0.
      result = 'invalid bukrs'.
      RETURN.
    ENDIF.

    DATA(calculator) = zcl_aunit_calculator_factory=>get_calculator( ).
    DATA(sum) = calculator->add( number_1 = number_1 number_2 = number_2 ).

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
        date             = sy-datum
        foreign_amount   = sum
        foreign_currency = currency
        local_currency   = target_currency
      IMPORTING
        local_amount     = sum_in_target_currency
      EXCEPTIONS
        no_rate_found    = 1
        overflow         = 2
        no_factors_found = 3
        no_spread_found  = 4
        derived_2_times  = 5
        OTHERS           = 6.

    result = |{ sum_in_target_currency } { target_currency }|.
  ENDMETHOD.
ENDCLASS.
```

Testklasse
```abap
*"* use this source file for your ABAP unit test classes
CLASS lcl_calculator_dummy DEFINITION CREATE PUBLIC FOR TESTING.

  PUBLIC SECTION.
    INTERFACES: zif_aunit_calculator PARTIALLY IMPLEMENTED.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_calculator_dummy IMPLEMENTATION.
  METHOD zif_aunit_calculator~add.
    " Implementierung des Dummies
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_constructor_injection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      converter_test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_constructor_injection IMPLEMENTATION.

  METHOD converter_test.
    " GIVEN
    DATA: calculator TYPE REF TO zif_aunit_calculator.
    calculator = NEW lcl_calculator_dummy( ).
    zcl_aunit_calculator_injector=>inject( calculator ).

    DATA(converter) = NEW zcl_aunit_converter_fi( ).

    " WHEN
    DATA(result) = converter->add_and_convert( bukrs = '1000' currency = 'EUR' number_1 = 1 number_2 = 2 ).

    " THEN
    " Testimplementierung
  ENDMETHOD.

ENDCLASS.
```

## Test Seams

Converter Klasse
```abap
CLASS zcl_aunit_converter_ts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      add_and_convert
        IMPORTING number_1      TYPE i
                  number_2      TYPE i
                  currency      TYPE waers
                  bukrs         TYPE bukrs
        RETURNING VALUE(result) TYPE char20.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_aunit_converter_ts IMPLEMENTATION.
  METHOD add_and_convert.
    DATA: sum_in_target_currency TYPE f.

    SELECT SINGLE waers FROM t001 WHERE bukrs = @bukrs INTO @DATA(target_currency).
    IF sy-subrc <> 0.
      result = 'invalid bukrs'.
      RETURN.
    ENDIF.

    DATA(calculator) = NEW zcl_aunit_calculator( ).
    DATA(sum) = calculator->add( number_1 = number_1 number_2 = number_2 ).

    TEST-SEAM fm_ctlc.
      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
        EXPORTING
          date             = sy-datum
          foreign_amount   = sum
          foreign_currency = currency
          local_currency   = target_currency
        IMPORTING
          local_amount     = sum_in_target_currency
        EXCEPTIONS
          no_rate_found    = 1
          overflow         = 2
          no_factors_found = 3
          no_spread_found  = 4
          derived_2_times  = 5
          OTHERS           = 6.
    end-TEST-SEAM.

    result = |{ sum_in_target_currency } { target_currency }|.
  ENDMETHOD.
ENDCLASS.
```

Testklasse
```abap
CLASS lcl_calculator_dummy DEFINITION CREATE PUBLIC FOR TESTING.

  PUBLIC SECTION.
    INTERFACES: zif_aunit_calculator PARTIALLY IMPLEMENTED.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_calculator_dummy IMPLEMENTATION.
  METHOD zif_aunit_calculator~add.
    " Implementierung des Dummies
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_backdoor_injection DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      converter_test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_backdoor_injection IMPLEMENTATION.

  METHOD converter_test.
    " GIVEN
    TEST-INJECTION fm_ctlc.
      sum_in_target_currency = 10.
    END-TEST-INJECTION.

    DATA(converter) = NEW zcl_aunit_converter_ts( ).

    " WHEN
    DATA(result) = converter->add_and_convert( bukrs = '1000' currency = 'EUR' number_1 = 1 number_2 = 2 ).

    " THEN
    " Testimplementierung
  ENDMETHOD.

ENDCLASS.
```

## Test Doubles

### Stub

Stub Klasse
```abap
CLASS lcl_calculator_stub DEFINITION CREATE PUBLIC FOR TESTING.

  PUBLIC SECTION.
    INTERFACES: zif_aunit_calculator PARTIALLY IMPLEMENTED.
    data: add_result TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_calculator_stub IMPLEMENTATION.
  METHOD zif_aunit_calculator~add.
    result = add_result.
  ENDMETHOD.
ENDCLASS.
```

Testklasse
```abap
CLASS ltcl_constructor_inject_stub DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      converter_test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_constructor_inject_stub IMPLEMENTATION.

  METHOD converter_test.
    " GIVEN
    DATA(calculator) = NEW lcl_calculator_stub( ).
    calculator->add_result = 3.

    DATA(converter) = NEW zcl_aunit_converter_ki( calculator = calculator ).

    " WHEN
    DATA(result) = converter->add_and_convert( bukrs = '1000' currency = 'EUR' number_1 = 1 number_2 = 2 ).

    " THEN
    " Testimplementierung
  ENDMETHOD.

ENDCLASS.
```

### Spy

Spy Klasse
```abap
CLASS lcl_calculator_spy DEFINITION CREATE PUBLIC FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: zif_aunit_calculator PARTIALLY IMPLEMENTED.
    data: add_number_1 TYPE i,
          add_number_2 TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_calculator_spy IMPLEMENTATION.
  METHOD zif_aunit_calculator~add.
    add_number_1 = number_1.
    add_number_2 = number_2.
  ENDMETHOD.
ENDCLASS.
```

Testklasse
```abap
CLASS ltcl_constructor_inject_spy DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      converter_test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_constructor_inject_spy IMPLEMENTATION.
  METHOD converter_test.
    " GIVEN
    DATA(calculator) = NEW lcl_calculator_spy( ).
    DATA(converter) = NEW zcl_aunit_converter_ki( calculator = calculator ).

    " WHEN
    DATA(result) = converter->add_and_convert( bukrs = '1000' currency = 'EUR' number_1 = 1 number_2 = 2 ).

    " THEN
    cl_abap_unit_assert=>assert_equals( act = calculator->add_number_1 exp = 1 ).
    cl_abap_unit_assert=>assert_equals( act = calculator->add_number_2 exp = 2 ).
  ENDMETHOD.
ENDCLASS.
```

### OO Test Double Framework

Testklasse
```abap
CLASS ltcl_constructor_inject_ootdf DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      converter_test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_constructor_inject_ootdf IMPLEMENTATION.
  METHOD converter_test.
    " GIVEN
    DATA(calculator) = CAST zif_aunit_calculator( cl_abap_testdouble=>create( 'ZIF_AUNIT_CALCULATOR' ) ).
    cl_abap_testdouble=>configure_call( calculator )->returning( 3 )->and_expect( )->is_called_once( ).
    calculator->add( number_1 = 1 number_2 = 2 ).

    DATA(converter) = NEW zcl_aunit_converter_ki( calculator = calculator ).

    " WHEN
    DATA(result) = converter->add_and_convert( bukrs = '1000' currency = 'EUR' number_1 = 1 number_2 = 2 ).

    " THEN
    cl_abap_testdouble=>verify_expectations( calculator ).
  ENDMETHOD.
ENDCLASS.
```

### Authorization Test Double Framework

Testklasse
```abap

CLASS ltc_filter_unauthorized DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS unauthorized FOR TESTING RAISING cx_static_check.
    METHODS teardown.
    METHODS setup.

    METHODS prepare_test_data IMPORTING variant         TYPE etvar_id
                              RETURNING VALUE(bookings) TYPE ztttdd_booking.

    CLASS-METHODS class_setup.

    CLASS-DATA auth_controller TYPE REF TO if_aunit_auth_check_controller.

    DATA cut TYPE REF TO zcl_msq_tdd_authorization.
ENDCLASS.


CLASS ltc_filter_unauthorized IMPLEMENTATION.
  METHOD unauthorized.
    " given
    DATA(bookings) = prepare_test_data( 'UNAUTHORIZED' ).

    DATA(role_carrier_unauthorized) = VALUE cl_aunit_auth_check_types_def=>role_auth_objects(
        ( object         = 'Z_CARRIER'
          authorizations = VALUE #( ( VALUE #(
            ( fieldname = 'ACTVT' fieldvalues = VALUE #( ( lower_value = '03' ) ) )
            ( fieldname = 'ZMSQ_CARRI' fieldvalues = VALUE #( ) )
          ) ) ) ) ).

    DATA(user_role) = VALUE cl_aunit_auth_check_types_def=>user_role_authorizations(
                                ( role_authorizations = role_carrier_unauthorized ) ).
    DATA(auth_object_set) = cl_aunit_authority_check=>create_auth_object_set( user_role ).
    auth_controller->restrict_authorizations_to( auth_object_set ).

    " when
    DATA(result) = cut->filter_unauthorized_bookings( bookings ).

    " then
    auth_controller->get_auth_check_execution_log( )->get_execution_status( IMPORTING failed_execution = DATA(failed)
                                                                                      passed_execution = DATA(passed) ).

    cl_abap_unit_assert=>assert_initial( passed ).
    cl_abap_unit_assert=>assert_not_initial( failed ).

    cl_abap_unit_assert=>assert_initial(
        act = result
        msg = 'Es wurden Daten zurückgegeben, obwohl keine Berechtigungen erteilt sind' ).
  ENDMETHOD.

  METHOD class_setup.
    auth_controller = cl_aunit_authority_check=>get_controller( ).
  ENDMETHOD.

  METHOD teardown.
    auth_controller->reset( ).
  ENDMETHOD.

  METHOD prepare_test_data.
    TRY.
        DATA(tdc_api) = cl_apl_ecatt_tdc_api=>get_instance( 'ZTDC_MSQ_AUTHORIZATION' ).
        tdc_api->get_value( EXPORTING i_param_name   = 'BOOKINGS'
                                      i_variant_name = variant
                            CHANGING  e_param_value  = bookings ).
      CATCH cx_ecatt_tdc_access INTO DATA(cx).
        cl_abap_unit_assert=>fail( msg = cx->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.
ENDCLASS.
```

### Function Module Test Double Framework

Testklasse
```abap
CLASS ltc_convert_2_currency DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS one_booking_usd_2_eur FOR TESTING RAISING cx_static_check.
    METHODS one_booking_conversion_failed FOR TESTING RAISING cx_static_check.
    METHODS setup.
    CLASS-METHODS class_setup.

    CONSTANTS _test_data_container TYPE etobj_name VALUE 'ZTDC_MSQ_TDD_FM_CONV_2_CURR'.
    CONSTANTS _fixture_param       TYPE etpar_name VALUE 'FIXTURE'.
    CONSTANTS _expected_param      TYPE etpar_name VALUE 'EXPECTED'.

    DATA _cut                TYPE REF TO zcl_msq_tdd_fm.
    CLASS-DATA _fm_test_enviroment TYPE REF TO if_function_test_environment .

    METHODS get_fixture
      IMPORTING i_variant     TYPE etvar_id
      RETURNING VALUE(result) TYPE ztdc_msq_tdd_fm_conv_2_curr_fix
      RAISING   cx_ecatt_tdc_access.

    METHODS get_expected
      IMPORTING i_variant     TYPE etvar_id
      RETURNING VALUE(result) TYPE ztdc_msq_tdd_fm_conv_2_curr_exp
      RAISING   cx_ecatt_tdc_access.
ENDCLASS.


CLASS ltc_convert_2_currency IMPLEMENTATION.

  METHOD setup.
    _cut = NEW zcl_msq_tdd_fm( ).

    _fm_test_enviroment->clear_doubles( ).
  ENDMETHOD.

  METHOD one_booking_conversion_failed.
    TRY.
        DATA(fixtures) = get_fixture( 'ONE_BOOKING_CONVERSION_FAILED' ).
        DATA(expected) = get_expected( 'ONE_BOOKING_CONVERSION_FAILED' ).
      CATCH cx_ecatt_tdc_access.
        cl_abap_unit_assert=>fail( 'Testdata not accessible' ).
    ENDTRY.

    " given


    " when
    DATA(converted_bookings) = _cut->convert_2_currency( bookings         = fixtures-bookings
                                                         display_currency = fixtures-display_currency ).

    " then
    cl_abap_unit_assert=>assert_equals( exp = expected-bookings
                                        act = converted_bookings
                                        msg = 'Currency conversion wrong' ).
  ENDMETHOD.

  METHOD one_booking_usd_2_eur.
    TRY.
        DATA(fixtures) = get_fixture( 'ONE_BOOKING_USD_2_EUR' ).
        DATA(expected) = get_expected( 'ONE_BOOKING_USD_2_EUR' ).
      CATCH cx_ecatt_tdc_access.
        cl_abap_unit_assert=>fail( 'Testdata not accessible' ).
    ENDTRY.

    " given


    " when
    DATA(converted_bookings) = _cut->convert_2_currency( bookings         = fixtures-bookings
                                                         display_currency = fixtures-display_currency ).

    " then
    cl_abap_unit_assert=>assert_equals( exp = expected-bookings
                                        act = converted_bookings
                                        msg = 'Currency conversion wrong' ).
  ENDMETHOD.

  METHOD get_expected.
    DATA(tdc_ref) = cl_apl_ecatt_tdc_api=>get_instance( _test_data_container ).

    tdc_ref->get_value( EXPORTING i_param_name   = _expected_param
                                  i_variant_name = i_variant
                        CHANGING  e_param_value  = result ).
  ENDMETHOD.

  METHOD get_fixture.
    DATA(tdc_ref) = cl_apl_ecatt_tdc_api=>get_instance( _test_data_container ).

    tdc_ref->get_value( EXPORTING i_param_name   = _fixture_param
                                  i_variant_name = i_variant
                        CHANGING  e_param_value  = result ).
  ENDMETHOD.

  METHOD class_setup.
    _fm_test_enviroment = cl_function_test_environment=>create( function_modules = VALUE #( ( 'Z_TDD_CONV_CURR' ) ) ).
  ENDMETHOD.

ENDCLASS.
```

### CDS Test Double Framework

Testklasse
```abap
"!@testing ZC_MSQ_FLIGHTUTILISATION
CLASS ltc_ZC_MSQ_FLIGHTUTILISATION
DEFINITION FINAL FOR TESTING
DURATION SHORT
RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    CLASS-DATA:
      environment TYPE REF TO if_cds_test_environment.

    CLASS-METHODS:
      "! In CLASS_SETUP, corresponding doubles and clone(s) for the CDS view under test and its dependencies are created.
      class_setup RAISING cx_static_check,
      "! In CLASS_TEARDOWN, Generated database entities (doubles & clones) should be deleted at the end of test class execution.
      class_teardown.

    DATA:
      act_results                   TYPE STANDARD TABLE OF zc_msq_flightutilisation WITH EMPTY KEY,
      lt_zmind2e_i_carrier          TYPE STANDARD TABLE OF zmind2e_i_carrier WITH EMPTY KEY,
      lt_zmind2e_i_flight           TYPE STANDARD TABLE OF zmind2e_i_flight WITH EMPTY KEY,
      lt_zmind2e_i_connection       TYPE STANDARD TABLE OF zmind2e_i_connection WITH EMPTY KEY,
      lt_zc_msq_flightutilbookingsu TYPE STANDARD TABLE OF zc_msq_flightutilbookingsum WITH EMPTY KEY,
      lt_zi_msq_flightutilbookingsu TYPE STANDARD TABLE OF zi_msq_flightutilbookingsum WITH EMPTY KEY.

    METHODS:
      "! SETUP method creates a common start state for each test method,
      "! clear_doubles clears the test data for all the doubles used in the test method before each test method execution.
      setup RAISING cx_static_check,
      prepare_testdata_set,
      "!  In this method test data is inserted into the generated double(s) and the test is executed and
      "!  the results should be asserted with the actuals.
      none_authroized FOR TESTING RAISING cx_static_check,
    authorized_only_aa FOR TESTING RAISING cx_static_check,
    authorized_only_fra FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_ZC_MSQ_FLIGHTUTILISATION IMPLEMENTATION.
  METHOD class_setup.
    environment = cl_cds_test_environment=>create( i_for_entity = 'ZC_MSQ_FLIGHTUTILISATION' ).
    environment->enable_double_redirection( ).
  ENDMETHOD.

  METHOD setup.
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD authorized_only_fra.
    DATA expected TYPE STANDARD TABLE OF zc_msq_flightutilisation WITH EMPTY KEY.

    prepare_testdata_set( ).

    DATA(acm_data_only_aa) = cl_cds_test_data=>create_access_control_data(
        i_role_authorizations = VALUE #( ( object         = 'Z_AIRPORT'
                                           authorizations = VALUE #(
                                               ( VALUE #( ( fieldname   = 'ZMSQ_AIRP'
                                                            fieldvalues = VALUE #( ( lower_value = 'FRA' ) ) ) ) ) ) ) ) ).
    environment->get_access_control_double( )->enable_access_control( i_access_control_data = acm_data_only_aa ).

    expected = VALUE #( ( CarrierId    = 'AA'
                          ConnectionId = '1'
                          DepartureAirport = 'FRA'
                          DestinationAirport = 'NYC'
                          FlightDate   = '20230101' ) ).

    SELECT * FROM zc_msq_flightutilisation INTO TABLE @act_results.

    cl_abap_unit_assert=>assert_equals( exp = expected
                                        act = act_results
                                        msg = 'Unauthorized data accessible' ).
  ENDMETHOD.

  METHOD authorized_only_aa.
    DATA expected TYPE STANDARD TABLE OF zc_msq_flightutilisation WITH EMPTY KEY.

    prepare_testdata_set( ).

    DATA(acm_data_only_aa) = cl_cds_test_data=>create_access_control_data(
        i_role_authorizations = VALUE #( ( object         = 'Z_CARRIER'
                                           authorizations = VALUE #(
                                               ( VALUE #( ( fieldname   = 'ZMSQ_CARRI'
                                                            fieldvalues = VALUE #( ( lower_value = 'AA' ) ) )
                                                          ( fieldname   = 'ACTVT'
                                                            fieldvalues = VALUE #( ( lower_value = '03' ) ) ) ) ) ) ) ) ).
    environment->get_access_control_double( )->enable_access_control( i_access_control_data = acm_data_only_aa ).

    expected = VALUE #( ( CarrierId    = 'AA'
                          ConnectionId = '1'
                          DepartureAirport = 'FRA'
                          DestinationAirport = 'NYC'
                          FlightDate   = '20230101' ) ).

    SELECT * FROM zc_msq_flightutilisation INTO TABLE @act_results.

    cl_abap_unit_assert=>assert_equals( exp = expected
                                        act = act_results
                                        msg = 'Unauthorized data accessible' ).
  ENDMETHOD.

  METHOD none_authroized.
    prepare_testdata_set( ).

    DATA(acm_data_no_authorizations) = cl_cds_test_data=>create_access_control_data(
      i_role_authorizations = VALUE #( ) ).
    environment->get_access_control_double( )->enable_access_control( i_access_control_data = acm_data_no_authorizations ).

    SELECT * FROM zc_msq_flightutilisation INTO TABLE @act_results.

    cl_abap_unit_assert=>assert_initial( act = act_results
                                         msg = 'Unauthorized data accessible' ).
  ENDMETHOD.

  METHOD prepare_testdata_set.
    " Prepare test data for 'zmind2e_i_flight'
    lt_zmind2e_i_flight = VALUE #( FlightDate = '20230101'
                                   ( CarrierId    = 'AA'
                                     ConnectionId = '1' )
                                   ( CarrierId    = 'LH'
                                     ConnectionId = '2' ) ).
    environment->insert_test_data( i_data = lt_zmind2e_i_flight ).

    " Prepare test data for 'zmind2e_i_connection'
    lt_zmind2e_i_connection = VALUE #( ( ConnectionId       = '1'
                                         CarrierId          = 'AA'
                                         DepartureAirport   = 'FRA'
                                         DestinationAirport = 'NYC' )
                                       ( ConnectionId       = '2'
                                         CarrierId          = 'LH'
                                         DepartureAirport   = 'SIN'
                                         DestinationAirport = 'MUN' ) ).
    environment->insert_test_data( i_data = lt_zmind2e_i_connection ).

    " Prepare test data for 'zc_msq_flightutilbookingsum'
    " TODO: Provide the test data here
    lt_zc_msq_flightutilbookingsu = VALUE #( ( ) ).
    environment->insert_test_data( i_data = lt_zc_msq_flightutilbookingsu ).

    " Prepare test data for 'zi_msq_flightutilbookingsum'
    " TODO: Provide the test data here
    lt_zi_msq_flightutilbookingsu = VALUE #( ( ) ).
    environment->insert_test_data( i_data = lt_zi_msq_flightutilbookingsu ).
  ENDMETHOD.

ENDCLASS.
```
