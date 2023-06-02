<img src="https://mindsquare.de/files/logo-mindsquare-176x781.png" alt="mindsquare Logo" title="mindsquare AG" align="right">

# Begleitmaterialien zur Test Driven Development (TDD) Schulung

Codebeispiele für die [mindsquare Test Driven Development Schulung](https://mindsquare.de/schulung/test-driven-development-mit-abap-unit/)

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

## Injektionstechniken

### Constructor Injection

Produktivcode

```abap
CLASS … DEFINITION … .
  PUBLIC SECTION.
    METHODS 
      constructor 
        IMPORTING 
          i_cash_provider TYPE REF TO if_cash_provider OPTIONAL.
  
  PRIVATE SECTION.
    DATA m_cash_provider TYPE REF TO if_cash_provider.
  …
ENDCLASS.

CLASS … IMPLEMENTATION.
  METHOD constructor.
    m_cash_provider = COND #( WHEN i_cash_provider IS BOUND
                                THEN i_cash_provider
                              ELSE NEW cl_cash_provider( ) ).
  ENDMETHOD.
  
  …
ENDCLASS.
```

### Setter Injection

Produktivcode

```abap
CLASS … DEFINITION … .
  PUBLIC SECTION.
    METHODS 
      set_cash_provider 
        IMPORTING 
          i_cash_provider TYPE REF TO if_cash_provider.
  
  PRIVATE SECTION.
    DATA m_cash_provider TYPE REF TO if_cash_provider.
  …
ENDCLASS.

CLASS … IMPLEMENTATION.
  METHOD set_cash_provider.
    m_cash_provider = i_cash_provider.
  ENDMETHOD.
  …
ENDCLASS.
```

### Parameter Injection

Produktivcode

```abap
CLASS … DEFINITION … .
  PUBLIC SECTION.
  METHODS 
    get_amount_in_coins
      IMPORTING
        i_amount TYPE i
        i_cash_provider TYPE REF TO if_cash_provider OPTIONAL
      RETURNING VALUE(r_value) TYPE i.

  PRIVATE SECTION.
    DATA m_cash_provider TYPE REF TO if_cash_provider.
  …
ENDCLASS.

CLASS … IMPLEMENTATION.
  METHOD get_amount_in_coins.
    DATA(cash_provider) = CAST if_cash_provider( COND #( 
      WHEN i_cash_provider IS BOUND
        THEN i_cash_provider
      ELSE NEW cl_cash_provider( ) ) ).
    …
  ENDMETHOD.
  …
ENDCLASS.
```

### Backdoor Injection

Produktivcode

```abap
CLASS … DEFINITION … .
  PRIVATE SECTION.
    DATA m_cash_provider TYPE REF TO if_cash_provider.
  …
ENDCLASS
```

Testcode

```abap
CLASS ltc_get_amount_in_coins DEFINITION DEFERRED.
CLASS cl_money_machine DEFINITION LOCAL FRIENDS ltc_get_amount_in_coins.

CLASS ltc_get_amount_in_coins DEFINITION FOR TESTING … .
  …
ENDCLASS.

CLASS ltc_get_amount_in_coins IMPLEMENTATION.
  METHOD setup.
    "given
    m_cut = NEW #( ).
    m_cut->m_cash_provider = NEW ltd_cash_provider( ).
  ENDMETHOD.
ENDCLASS.
```

## Test Double Terminologie

### Stub

Ermöglicht das Setzen des indirekten Inputs aus der Testklasse heraus.

Ablauf:

1. Test setzt gewünschten Rückgabewert in Stub
2. Stub gibt den gewünschten Wert an den zu testenden Code

```abap
CLASS ltd_stub DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES if_cash_provider PARTIALLY IMPLEMENTED.
    
    DATA m_notes TYPE if_cash_provider=>tt_change.
ENDCLASS.

CLASS ltd_stub IMPLEMENTATION.
  METHOD if_cash_provider~get_notes.
    r_notes = m_notes.
  ENDMETHOD.
ENDCLASS.
```

### Spy

Ermöglicht das Logging der vom Produktivcode an die Dependent On Komponente übergebenen Parameter.

Ablauf:

- Spy protokolliert den vom Code under Test übergebenen Wert
- Der Test verifiziert den aufgezeichneten Wert

```abap
CLASS ltd_spy DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES if_cash_provider PARTIALLY IMPLEMENTED.

    DATA m_currency TYPE string.
ENDCLASS.

CLASS ltd_spy IMPLEMENTATION.
  METHOD if_cash_provider~get_notes.
    m_currency = i_currency.
  ENDMETHOD.
ENDCLASS.
```

### Fake

Stellt eine einfache Implementierung der Dependent On Komponente dar.

### Mock

Erweiterung der Konzepte Stub, Spy und Fake. Verifiziert je nach Anforderung den indirekten Output und/oder stellt indirekten Input zu Verfügung.

Ablauf:

- Test setzt gewünschten Wert im Mock
- Spy protokolliert den vom Code under Test übergebenen Wert
- Stub gibt den gewünschten Wert an den Code under Test zurück
- Der Test verifiziert den aufgezeichneten Wert

```abap
CLASS ltd_mock DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES if_cash_provider PARTIALLY IMPLEMENTED.
  
    DATA m_currency TYPE string.
    DATA m_currency_exp TYPE string.
    DATA m_notes TYPE if_cash_provider=>tt_change.
ENDCLASS.

CLASS ltd_mock IMPLEMENTATION.
  METHOD if_cash_provider~get_notes.
    m_currency = i_currency.
    r_notes = m_notes.
  ENDMETHOD.

  METHOD assert_expectations.
    cl_abap_unit_assert=>assert_equals( act = m_currency
                                        exp = m_currency_exp ).
  ENDMETHOD.
ENDCLASS.
```

### Dummy

Ein Dummy erfüllt die syntaktischen Anforderungen ohne weitere Logik.

## Test Double Frameworks

[Übersicht über die verschiedenen Test Double Frameworks](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/04a2d0fc9cd940db8aedf3fa29e5f07e.html?locale=en-US)

### ABAP OO Test Double Framework

> Verfügbar ab:
>
> - globale Interfaces: SAP NetWeaver 7.4 SP9
> - globale Klassen: AS ABAP 7.53

- [ABAP Test Double Framework – An Introduction](https://blogs.sap.com/2015/01/05/abap-test-double-framework-an-introduction/)
- [Short examples of CL_ABAP_TESTDOUBLE](https://blogs.sap.com/2018/04/03/short-examples-of-cl_abap_testdouble/)
- [Beispielcode auf GitHub](https://github.com/sandraros/Demo-ATD/blob/master/src/z_atd_demo.prog.abap)

### ABAP SQL Test Double Framework

> Verfügbar ab: AS ABAP 7.52

- Beispielcode: `SABP_UNIT_DOUBLE_OSQL_DEMO`
- Dokumentation: [ABAP SQL Test Double Framework](https://help.sap.com/docs/btp/sap-abap-development-user-guide/abap-sql-test-double-framework?locale=en-US)

### ABAP CDS Test Double Framework

> Verfügbar ab: AS ABAP 7.51

- Beispielcode: `SABP_UNIT_DOUBLE_CDS_DEMO`
- Dokumentation: [ABAP CDS Test Double Framework](https://help.sap.com/docs/btp/sap-abap-development-user-guide/abap-cds-test-double-framework?locale=en-US)

### ABAP Function Module Test Double Framework

> Verfügbar ab: AS ABAP 7.56

- Beispielcode: `SABP_UNIT_DOUBLE_FUNCTION_DEMO`
- Dokumentation: [Managing Dependencies on ABAP Function Modules with ABAP Unit](https://help.sap.com/docs/ABAP_PLATFORM_NEW/c238d694b825421f940829321ffa326a/75964f284aa9435da40c4d82e111f276.html?locale=en-US)

### ABAP Authority Check Test Helper API

> Verfügbar ab: AS ABAP 7.55

- Beispielcode: `SABP_UNIT_AUTHORITY_DEMO`
- Dokumentation: [Managing Dependencies on ABAP Authority Checks with ABAP Unit](https://help.sap.com/docs/btp/sap-abap-development-user-guide/managing-dependencies-on-abap-authority-checks-with-abap-unit?locale=en-US)

### Test Seams

> Verfügbar ab: SAP NetWeaver 7.5

- [SAP Help](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abentest_seams.htm)
- [Working effectively with ABAP legacy code – ABAP Test Seams are your friend](https://blogs.sap.com/2016/02/06/working-effectively-with-abap-legacy-code-abap-test-seams-are-your-friend/)
- [ABAP Test Seam for Unit Test with external dependencies – Personal guideline](https://blogs.sap.com/2018/06/08/abap-test-seam-for-unit-test-with-external-dependencies-personal-guideline/)
