*&---------------------------------------------------------------------*
*& Report ztdd_msq_select
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztdd_msq_select.

DATA airline       TYPE zmind2_carrier_id.
DATA flight_date   TYPE zmind2_flight_date.
DATA bookings      TYPE ztttdd_booking.
DATA auth_bookings TYPE ztttdd_booking.
DATA alv           TYPE REF TO cl_salv_table.
DATA alv_data      TYPE TABLE OF zstdd_booking.

SELECT-OPTIONS: s_airlin FOR airline,
                s_fldate FOR flight_date.

START-OF-SELECTION.
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
    WHERE booking~carrier_id IN @s_airlin AND booking~flight_date IN @s_fldate
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
      APPEND booking->* TO auth_bookings.
    ENDLOOP.
  ENDLOOP.

  CLEAR bookings.

  IF lines( auth_bookings ) = 0.
    MESSAGE 'No bookings found' TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  " Calculate total price
  SELECT FROM zmind2_book_supp
    FIELDS travel_id, booking_id, booking_supplement_id, price, currency_code
    FOR ALL ENTRIES IN @auth_bookings
    WHERE travel_id = @auth_bookings-travel_id AND booking_id = @auth_bookings-booking_id
    INTO TABLE @DATA(booking_supplements).

  DATA(calculator) = NEW zcl_msq_tdd_calculator( ).
  DATA(discount) = NEW zcl_msq_tdd_discount( ).


  LOOP AT auth_bookings REFERENCE INTO booking.
    booking->total_price = booking->flight_price.

    LOOP AT booking_supplements REFERENCE INTO DATA(currency) WHERE travel_id = booking->travel_id AND booking_id = booking->booking_id GROUP BY currency->currency_code.
      DATA(supplement_price) = VALUE zmind2_flight_price( ).

      LOOP AT GROUP currency REFERENCE INTO DATA(supplement).
        supplement_price = calculator->add( number_1 = supplement_price
                                            number_2 = supplement->price ).
      ENDLOOP.

      IF currency->currency_code <> booking->currency_code.
        CALL FUNCTION 'Z_TDD_CONV_CURR'
          EXPORTING
            source_amount           = supplement_price
            source_currency         = currency->currency_code
            target_currency         = booking->currency_code
            exchange_rate_date      = sy-datum
          IMPORTING
            converted_amount        = supplement_price
          EXCEPTIONS
            invalid_source_currency = 1
            invalid_target_currency = 2
            no_conversion_possible  = 3
            OTHERS                  = 4.
      ENDIF.

      booking->total_price = calculator->add( number_1 = booking->total_price
                                              number_2 = supplement_price ).
    ENDLOOP.

    booking->max_discount = discount->zif_msq_tdd_discount~get_max_discount( booking = booking->* ).
  ENDLOOP.

  alv_data = auth_bookings.

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = alv
                              CHANGING  t_table      = alv_data ).
    CATCH cx_salv_msg INTO DATA(exception).
      MESSAGE exception->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
  ENDTRY.

  alv->display( ).
