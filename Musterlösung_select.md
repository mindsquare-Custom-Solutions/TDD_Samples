## Lösungvorschlag für Übungsaufgaben
### Erstellen einer ZCL_<Kürzel>_SELECT Klasse
#### Global Class

```
CLASS zcl_se_select DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES ZIF_MSQ_ML_SELECT.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_se_select IMPLEMENTATION.
  METHOD zif_msq_ml_select~select.
    DATA bookings      TYPE ztttdd_booking.
    SELECT
        FROM zmind2_booking AS booking
               LEFT OUTER JOIN
                 zmind2_conn AS connection ON booking~carrier_id = connection~carrier_id AND booking~connection_id = connection~connection_id
        FIELDS booking~travel_id,
               booking~booking_id,
               booking~carrier_id,
               booking~connection_id,
               booking~flight_date,
               connection~airport_from_id,
               connection~airport_to_id,
               booking~booking_date,
               booking~booking_status,
               booking~flight_price,
               booking~currency_code
        WHERE booking~carrier_id IN @carriers AND booking~flight_date IN @flight_dates
        INTO CORRESPONDING FIELDS OF TABLE @bookings.

      " Authorization
      LOOP AT bookings REFERENCE INTO DATA(carrier_id) GROUP BY carrier_id->carrier_id.
        AUTHORITY-CHECK OBJECT 'Z_CARRIER'
                        ID 'ZMSQ_CARRI' FIELD carrier_id->carrier_id
                        ID 'ACTVT' FIELD '03'.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        LOOP AT GROUP carrier_id REFERENCE INTO DATA(booking).
          APPEND booking->* TO result.
        ENDLOOP.
      ENDLOOP.

  ENDMETHOD.

ENDCLASS.
```

#### Test Class
```
CLASS ltc_select DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS authorized_for_ua FOR TESTING RAISING cx_static_check.
    METHODS full_select       FOR TESTING RAISING cx_static_check.
    METHODS unauthorized      FOR TESTING RAISING cx_static_check.
    METHODS setup.
    METHODS teardown.

    DATA cut          TYPE REF TO zcl_se_select.
    DATA carriers     TYPE zif_msq_ml_select=>ttr_carrier.
    DATA flight_dates TYPE zif_msq_ml_select=>ttr_flight_date.
    DATA bookings     TYPE ztttdd_booking.

ENDCLASS.

CLASS ltc_select IMPLEMENTATION.

    METHOD setup.
        cut = NEW zcl_se_select( ).
    ENDMETHOD.

    METHOD teardown.
        FREE: cut,
              carriers,
              flight_dates,
              bookings.
    ENDMETHOD.

  METHOD authorized_for_ua.
    " Given

    " when
    bookings = cut->zif_msq_ml_select~select( carriers     = carriers
                                              flight_dates = flight_dates ).

    " then
    cl_aunit_assert=>assert_equals( exp = 4
                                    act = lines( bookings )
                                    msg = |Statt 4 wurden für UA { lines( bookings ) } zurückgegeben| ).
  ENDMETHOD.

  METHOD full_select.
    " Given

    " when
    bookings = cut->zif_msq_ml_select~select( carriers     = carriers
                                              flight_dates = flight_dates ).

    " then
    cl_aunit_assert=>assert_equals( exp = 10
                                    act = lines( bookings )
                                    msg = |Statt 10 wurden { lines( bookings ) } zurückgegeben| ).
  ENDMETHOD.

  METHOD unauthorized.
    " Given

    " when
    bookings = cut->zif_msq_ml_select~select( carriers     = carriers
                                              flight_dates = flight_dates ).

    " then
    cl_aunit_assert=>assert_initial( act = bookings
                                     msg = |Rückgabe nicht leer wie erwartet| ).
  ENDMETHOD.

ENDCLASS.
```

### Datenbankaufrufe auslagern
DAO Klasse anlegen
#### Global Class ZCL_<Kürzel>_SELECT_DAO
```
CLASS zcl_se_select_dao DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_msq_ml_select_dao.
ENDCLASS.

CLASS zcl_se_select_dao IMPLEMENTATION.
  METHOD zif_msq_ml_select_dao~select_bookings.
    SELECT
      FROM zmind2_booking AS booking
             LEFT OUTER JOIN
               zmind2_conn AS connection ON  booking~carrier_id    = connection~carrier_id
                                         AND booking~connection_id = connection~connection_id
      FIELDS booking~travel_id,
             booking~booking_id,
             booking~carrier_id,
             booking~connection_id,
             booking~flight_date,
             connection~airport_from_id,
             connection~airport_to_id,
             booking~booking_date,
             booking~booking_status,
             booking~flight_price,
             booking~currency_code
      WHERE booking~carrier_id IN @carriers AND booking~flight_date IN @flight_dates
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.

ENDCLASS.
```

#### Global Class ZCL_<Kürzel>_SELECT
```
CLASS zcl_se_select DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES ZIF_MSQ_ML_SELECT.

     METHODS constructor.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA bookings_dao TYPE REF TO zif_msq_ml_select_dao.
ENDCLASS.



CLASS zcl_se_select IMPLEMENTATION.
  METHOD constructor.
    me->bookings_dao = NEW zcl_se_select_dao( ).
  ENDMETHOD.

  METHOD zif_msq_ml_select~select.
    DATA bookings      TYPE ztttdd_booking.

    bookings = bookings_dao->select_bookings(
                 carriers     =  carriers
                 flight_dates =  flight_dates
               ).

      " Authorization
      LOOP AT bookings REFERENCE INTO DATA(carrier_id) GROUP BY carrier_id->carrier_id.
        AUTHORITY-CHECK OBJECT 'Z_CARRIER'
                        ID 'ZMSQ_CARRI' FIELD carrier_id->carrier_id
                        ID 'ACTVT' FIELD '03'.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        LOOP AT GROUP carrier_id REFERENCE INTO DATA(booking).
          APPEND booking->* TO result.
        ENDLOOP.
      ENDLOOP.

  ENDMETHOD.

ENDCLASS.
```

### Konstruktorinjektion
Abhängigkeiten im Testfall austauschen
#### Global Class ZCL_<Kürzel>_SELECT
```
CLASS zcl_se_select DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES ZIF_MSQ_ML_SELECT.

     METHODS constructor  IMPORTING select_dao TYPE REF TO zif_msq_ml_select_dao OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA bookings_dao TYPE REF TO zif_msq_ml_select_dao.
ENDCLASS.



CLASS zcl_se_select IMPLEMENTATION.
  METHOD constructor.
    me->bookings_dao = COND #( WHEN bookings_dao IS BOUND
                             THEN bookings_dao
                             ELSE NEW zcl_se_select_dao( ) ).
  ENDMETHOD.

  METHOD zif_msq_ml_select~select.
    DATA bookings      TYPE ztttdd_booking.

    bookings = bookings_dao->select_bookings(
                 carriers     =  carriers
                 flight_dates =  flight_dates
               ).

      " Authorization
      LOOP AT bookings REFERENCE INTO DATA(carrier_id) GROUP BY carrier_id->carrier_id.
        AUTHORITY-CHECK OBJECT 'Z_CARRIER'
                        ID 'ZMSQ_CARRI' FIELD carrier_id->carrier_id
                        ID 'ACTVT' FIELD '03'.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        LOOP AT GROUP carrier_id REFERENCE INTO DATA(booking).
          APPEND booking->* TO result.
        ENDLOOP.
      ENDLOOP.

  ENDMETHOD.

ENDCLASS.
```

### Erstellen von Test Doubles Stud
#### Test Class ZCL_<Kürzel>_SELECT

```
CLASS lcl_dao_stub DEFINITION CREATE PUBLIC FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_msq_ml_select_dao.

    DATA bookings TYPE ztttdd_booking.
ENDCLASS.


CLASS lcl_dao_stub IMPLEMENTATION.
  METHOD zif_msq_ml_select_dao~select_bookings.
    result = me->bookings.
  ENDMETHOD.

ENDCLASS.
```

#### Test Class ZCL_<Kürzel>_SELECT
```
CLASS ltc_select IMPLEMENTATION.

    METHOD setup.

        DATA(dao_stub) = new lcl_dao_stub(  ).

        dao_stub->bookings = VALUE #( (
            travel_id = '0000001'
            booking_id = '0001'
            carrier_id = 'LH'
            connection_id = '1001'
            flight_date = '20240101'
            airport_to_id = 'JFK'
            booking_status = 'O'
            flight_price = '500.00'
            total_price = '550.00'
            currency_code = 'EUR'
            max_discount = '0.1000'
         ) (
            travel_id = '00000002'
            booking_id = '0002'
            total_price = '550.00'
            currency_code = 'HKD'
            max_discount = '0.1300'
         ) ).


        cut = NEW zcl_se_select( dao_stub ).
    ENDMETHOD.

    METHOD teardown.
        FREE: cut,
              carriers,
              flight_dates,
              bookings.
    ENDMETHOD.

  METHOD authorized_for_ua.
  ...
ENDCLASS.
```
### ABAP OO TDF
Erstelle ein Test Double (über das Framework) für die DAO Klasse. Wir ersetzen unser Stub durch das ABAP OO Test Double. Testfälle schlagen fehl da Berechtigungsobjekte noch fehlen (siehe in nächsten Aufgaben).
#### Test Class ZCL_<Kürzel>_SELECT
```
CLASS ltc_select DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS authorized_for_ua FOR TESTING RAISING cx_static_check.
    METHODS full_select       FOR TESTING RAISING cx_static_check.
    METHODS unauthorized      FOR TESTING RAISING cx_static_check.
    METHODS _create_test_double_dao.
    METHODS setup.
    METHODS teardown.

    DATA cut          TYPE REF TO zcl_se_select.
    DATA carriers     TYPE zif_msq_ml_select=>ttr_carrier.
    DATA flight_dates TYPE zif_msq_ml_select=>ttr_flight_date.
    DATA bookings     TYPE ztttdd_booking.

    DATA double TYPE REF TO zif_msq_ml_select_dao.

ENDCLASS.

CLASS ltc_select IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_se_select( double ).
    _create_test_double_dao( ).
  ENDMETHOD.

  METHOD teardown.
    FREE: cut,
          carriers,
          flight_dates,
          bookings,
          double.
  ENDMETHOD.

  METHOD _create_test_double_dao.
    DATA(double_dao) = CAST zif_msq_ml_select_dao( cl_abap_testdouble=>create( 'zif_msq_ml_select_dao' ) ).
    " Testdaten aus dem ECATT Container lesen
    " Vorbereitete Testdaten aus dem ECATT Testdatencontainer holen
    TRY.
        DATA(tdc_api) = cl_apl_ecatt_tdc_api=>get_instance( 'ZTDC_MSQ_AUTHORIZATION' ).
        tdc_api->get_value( EXPORTING i_param_name   = 'BOOKINGS'
                                      i_variant_name = 'UNAUTHORIZED'
                            CHANGING  e_param_value  = bookings ).
      CATCH cx_ecatt_tdc_access INTO DATA(cx).
        cl_abap_unit_assert=>fail( msg = cx->get_text( ) ).
    ENDTRY.
    cl_abap_testdouble=>configure_call( double_dao )->returning( bookings ).
  ENDMETHOD.



  METHOD authorized_for_ua.
    " Given

    " when
    bookings = cut->zif_msq_ml_select~select( carriers     = carriers
                                              flight_dates = flight_dates ).

    " then
    cl_aunit_assert=>assert_equals( exp = 4
                                    act = lines( bookings )
                                    msg = |Statt 4 wurden für UA { lines( bookings ) } zurückgegeben| ).
  ENDMETHOD.

  METHOD full_select.
    " Given

    " when
    bookings = cut->zif_msq_ml_select~select( carriers     = carriers
                                              flight_dates = flight_dates ).

    " then
    cl_aunit_assert=>assert_equals( exp = 10
                                    act = lines( bookings )
                                    msg = |Statt 10 wurden { lines( bookings ) } zurückgegeben| ).
  ENDMETHOD.

  METHOD unauthorized.
    " Given

    " when
    bookings = cut->zif_msq_ml_select~select( carriers     = carriers
                                              flight_dates = flight_dates ).

    " then
    cl_aunit_assert=>assert_initial( act = bookings
                                     msg = |Rückgabe nicht leer wie erwartet| ).
  ENDMETHOD.

ENDCLASS.
```

### Berechtigungsprüfung
Implementiere in den Methoden der Testklasse LTC_SELECT die jeweils benötigten Authorization Test Doubles für die durchgeführten Berechtigungsprüfungen
####  Test Class ZCL_<Kürzel>_SELECT
```
CLASS ltc_select DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS authorized_for_ua FOR TESTING RAISING cx_static_check.
    METHODS full_select       FOR TESTING RAISING cx_static_check.
    METHODS unauthorized      FOR TESTING RAISING cx_static_check.
    METHODS _create_test_double_dao.
    METHODS setup.
    METHODS teardown.

    DATA cut          TYPE REF TO zcl_se_select.
    DATA carriers     TYPE zif_msq_ml_select=>ttr_carrier.
    DATA flight_dates TYPE zif_msq_ml_select=>ttr_flight_date.
    DATA bookings     TYPE ztttdd_booking.

    DATA double TYPE REF TO zif_msq_ml_select_dao.

    CLASS-METHODS class_setup.

    CLASS-DATA auth_controller TYPE REF TO if_aunit_auth_check_controller.

ENDCLASS.

CLASS ltc_select IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_se_select( double ).
    _create_test_double_dao( ).
  ENDMETHOD.

  METHOD teardown.
    FREE: cut,
          carriers,
          flight_dates,
          bookings,
          double.
  ENDMETHOD.

  METHOD class_setup.
    auth_controller = cl_aunit_authority_check=>get_controller( ).
  ENDMETHOD.

  METHOD _create_test_double_dao.
    DATA(double_dao) = CAST zif_msq_ml_select_dao( cl_abap_testdouble=>create( 'zif_msq_ml_select_dao' ) ).
    " Testdaten aus dem ECATT Container lesen
    " Vorbereitete Testdaten aus dem ECATT Testdatencontainer holen
    TRY.
        DATA(tdc_api) = cl_apl_ecatt_tdc_api=>get_instance( 'ZTDC_MSQ_AUTHORIZATION' ).
        tdc_api->get_value( EXPORTING i_param_name   = 'BOOKINGS'
                                      i_variant_name = 'UNAUTHORIZED'
                            CHANGING  e_param_value  = bookings ).
      CATCH cx_ecatt_tdc_access INTO DATA(cx).
        cl_abap_unit_assert=>fail( msg = cx->get_text( ) ).
    ENDTRY.
    cl_abap_testdouble=>configure_call( double_dao )->returning( bookings ).
  ENDMETHOD.

  METHOD authorized_for_ua.
    " Given
    DATA(role_carrier_unauthorized) = VALUE cl_aunit_auth_check_types_def=>role_auth_objects(
        ( object         = 'Z_CARRIER'
          authorizations = VALUE #(
              ( VALUE #( ( fieldname = 'ACTVT' fieldvalues = VALUE #( ( lower_value = '03' ) ) )
                         ( fieldname = 'ZMSQ_CARRI' fieldvalues = VALUE #( ( lower_value = 'UA' ) ) ) ) ) ) ) ).

    DATA(user_role) = VALUE cl_aunit_auth_check_types_def=>user_role_authorizations(
                                ( role_authorizations = role_carrier_unauthorized ) ).

    DATA(auth_object_set) = cl_aunit_authority_check=>create_auth_object_set( user_role ).
    auth_controller->restrict_authorizations_to( auth_object_set ).

    " when
    bookings = cut->zif_msq_ml_select~select( carriers     = carriers
                                              flight_dates = flight_dates ).

    " then
    cl_aunit_assert=>assert_equals( exp = 975
                                    act = lines( bookings )
                                    msg = |Statt 975 wurden für UA { lines( bookings ) } zurückgegeben| ).
  ENDMETHOD.

  METHOD full_select.
    " Given
    carriers = VALUE #( ( sign   = 'I'
                          option = 'EQ'
                          low    = 'UA' ) ).
    flight_dates = VALUE #( ( sign   = 'I'
                              option = 'EQ'
                              low    = '01012000'
                              high   = '01012025' ) ).

    " when
    bookings = cut->zif_msq_ml_select~select( carriers     = carriers
                                              flight_dates = flight_dates ).

    " then
    cl_aunit_assert=>assert_equals( exp = 10
                                    act = lines( bookings )
                                    msg = |Statt 10 wurden { lines( bookings ) } zurückgegeben| ).
  ENDMETHOD.

  METHOD unauthorized.
    " Given

    " when
    bookings = cut->zif_msq_ml_select~select( carriers     = carriers
                                              flight_dates = flight_dates ).

    " then
    cl_aunit_assert=>assert_initial( act = bookings
                                     msg = |Rückgabe nicht leer wie erwartet| ).
  ENDMETHOD.

ENDCLASS.
```

### OpenSQL
Nutzung des SQL Test Double Frameworks
####  Test Class ZCL_<Kürzel>_SELECT
```
CLASS ltc_select DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    CLASS-DATA auth_controller TYPE REF TO if_aunit_auth_check_controller.
    CLASS-DATA zmind2_booking_double TYPE REF TO if_osql_test_environment.

    METHODS full_select       FOR TESTING RAISING cx_static_check.
    METHODS authorized_for_ua FOR TESTING RAISING cx_static_check.
    METHODS unauthorized      FOR TESTING RAISING cx_static_check.
    METHODS setup.
    METHODS teardown.

    DATA cut          TYPE REF TO zcl_se_select.
    DATA carriers     TYPE zif_msq_ml_select=>ttr_carrier.
    DATA flight_dates TYPE zif_msq_ml_select=>ttr_flight_date.
    DATA bookings     TYPE ztttdd_booking.

    CLASS-METHODS class_setup.

    METHODS prepare_test_data IMPORTING variant TYPE etvar_id
                              RETURNING VALUE(bookings) TYPE ztttdd_booking.
ENDCLASS.

CLASS ltc_select IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_se_select( ).
    zmind2_booking_double->clear_doubles( ).
    prepare_test_data( 'UNAUTHORIZED' ).
  ENDMETHOD.

  METHOD teardown.
    FREE: cut,
          carriers,
          flight_dates,
          bookings.
  ENDMETHOD.

  METHOD class_setup.
    auth_controller = cl_aunit_authority_check=>get_controller( ).
    zmind2_booking_double = cl_osql_test_environment=>create( i_dependency_list = VALUE #( ( 'ZMIND2_BOOKING' )
                                                                                           ( 'ZMIND2_BOOK_SUPP' ) ) ).
  ENDMETHOD.

  METHOD prepare_test_data.
    DATA bookings_data TYPE TABLE OF zmind2_booking.

    " Vorbereitete Testdaten aus dem ECATT Testdatencontainer holen
    TRY.
        DATA(tdc_api) = cl_apl_ecatt_tdc_api=>get_instance( 'ZTDC_MSQ_AUTHORIZATION' ).
        tdc_api->get_value( EXPORTING i_param_name   = 'BOOKINGS'
                                      i_variant_name = variant
                            CHANGING  e_param_value  = bookings ).
      CATCH cx_ecatt_tdc_access INTO DATA(cx).
        cl_abap_unit_assert=>fail( msg = cx->get_text( ) ).
    ENDTRY.

    bookings_data = CORRESPONDING #( bookings ).
    zmind2_booking_double->insert_test_data( bookings_data ).
  ENDMETHOD.

  METHOD authorized_for_ua.
    " Given
    DATA(role_carrier_unauthorized) = VALUE cl_aunit_auth_check_types_def=>role_auth_objects(
        ( object         = 'Z_CARRIER'
          authorizations = VALUE #(
              ( VALUE #( ( fieldname = 'ACTVT' fieldvalues = VALUE #( ( lower_value = '03' ) ) )
                         ( fieldname = 'ZMSQ_CARRI' fieldvalues = VALUE #( ( lower_value = 'UA'  ) ) ) ) ) ) ) ). " VALUE #( ( lower_value = 'UA' ) ) )

    DATA(user_role) = VALUE cl_aunit_auth_check_types_def=>user_role_authorizations(
                                ( role_authorizations = role_carrier_unauthorized ) ).
    DATA(auth_object_set) = cl_aunit_authority_check=>create_auth_object_set( user_role ).
    auth_controller->restrict_authorizations_to( auth_object_set ).

    " when
    bookings = cut->zif_msq_ml_select~select( carriers     = carriers
                                              flight_dates = flight_dates ).

    " then
    cl_abap_unit_assert=>assert_equals( exp = 6
                                        act = lines( bookings )
                                        msg = |Statt 6 wurden für UA { lines( bookings ) } zurückgegeben| ).
  ENDMETHOD.

  METHOD full_select.
    " Given
    DATA(role_carrier_unauthorized) = VALUE cl_aunit_auth_check_types_def=>role_auth_objects(
        ( object         = 'Z_CARRIER'
          authorizations = VALUE #(
              ( VALUE #( ( fieldname = 'ACTVT' fieldvalues = VALUE #( ( lower_value = '03' ) ) )
                         ( fieldname = 'ZMSQ_CARRI' fieldvalues = VALUE #( ( lower_value = '*' ) ) ) ) ) ) ) ). " VALUE #( ( lower_value = 'UA' ) ) )

    DATA(user_role) = VALUE cl_aunit_auth_check_types_def=>user_role_authorizations(
                                ( role_authorizations = role_carrier_unauthorized ) ).
    DATA(auth_object_set) = cl_aunit_authority_check=>create_auth_object_set( user_role ).
    auth_controller->restrict_authorizations_to( auth_object_set ).

    " when
    bookings = cut->zif_msq_ml_select~select( carriers     = carriers
                                              flight_dates = flight_dates ).

    " then
    cl_abap_unit_assert=>assert_equals( exp = 10
                                        act = lines( bookings )
                                        msg = |Statt 10 wurden { lines( bookings ) } zurückgegeben| ).
  ENDMETHOD.

  METHOD unauthorized.
    " Given
    DATA(role_carrier_unauthorized) = VALUE cl_aunit_auth_check_types_def=>role_auth_objects(
        ( object         = 'Z_CARRIER'
          authorizations = VALUE #(
              ( VALUE #( ( fieldname = 'ACTVT' fieldvalues = VALUE #( ( lower_value = '03' ) ) )
                         ( fieldname = 'ZMSQ_CARRI' fieldvalues = VALUE #( ( lower_value = ''  ) ) ) ) ) ) ) ). " VALUE #( ( lower_value = 'UA' ) ) )

    DATA(user_role) = VALUE cl_aunit_auth_check_types_def=>user_role_authorizations(
                                ( role_authorizations = role_carrier_unauthorized ) ).
    DATA(auth_object_set) = cl_aunit_authority_check=>create_auth_object_set( user_role ).
    auth_controller->restrict_authorizations_to( auth_object_set ).

    " when
    bookings = cut->zif_msq_ml_select~select( carriers     = carriers
                                              flight_dates = flight_dates ).

    " then
    cl_abap_unit_assert=>assert_initial( act = bookings
                                         msg = |Rückgabe nicht leer wie erwartet| ).
  ENDMETHOD.
ENDCLASS.
```