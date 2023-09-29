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