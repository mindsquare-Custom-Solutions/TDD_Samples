### Global Class
```
CLASS zcl_se_discount DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_msq_tdd_discount .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_se_discount IMPLEMENTATION.
  METHOD zif_msq_tdd_discount~get_max_discount.
    DATA lv_default_discount TYPE p LENGTH 8 DECIMALS 2 VALUE 7. " Base discount: 7%
    DATA lv_extra_discount   TYPE p LENGTH 8 DECIMALS 2 VALUE 5. " Additional discount: 5%
    DATA lv_total_discount   TYPE p LENGTH 8 DECIMALS 2.
    DATA days_difference     TYPE i.

    " Check if the booking status is 'B' (booked) or 'X' (canceled)
    IF booking-booking_status = 'B' OR booking-booking_status = 'X'.
      result = 0. " No discount if already booked or canceled
      RETURN.
    ENDIF.

    " Start with base discount
    lv_total_discount = lv_default_discount.

    " Berechne die Differenz in Tagen zwischen Buchungsdatum und Flugdatum
    days_difference = booking-flight_date - booking-booking_date.

    " Check if booking date is at least six months before the flight date
    IF days_difference >= 14.
      lv_total_discount += lv_extra_discount.
    ENDIF.

    " Return the calculated discount
    result = lv_total_discount.
  ENDMETHOD.
ENDCLASS.
```

### Test Class
```
class ltc_discount definition final for testing
  duration short
  risk level harmless.

  PRIVATE SECTION.
    METHODS base_discount             FOR TESTING RAISING cx_static_check.
    METHODS early_booking_discount    FOR TESTING RAISING cx_static_check.
    METHODS no_discount_when_canceled FOR TESTING RAISING cx_static_check.
        METHODS setup.
    METHODS teardown.

    DATA cut TYPE REF TO zcl_se_discount.
endclass.


class ltc_discount implementation.

  METHOD setup.
    cut = NEW zcl_se_discount( ).
  ENDMETHOD.

  METHOD teardown.
  ENDMETHOD.

  METHOD base_discount.
    " Test case for the base discount of 7%
    DATA lv_result  TYPE zmind2_booking_discount.
    DATA ls_booking TYPE zstdd_booking.

    " Define booking and flight dates where the booking is less than 14 days before the flight
    ls_booking-booking_date = '20241001'. " Booking date
    ls_booking-flight_date  = '20241005'. " Flight date within 14 days

    " Call the method and capture the result
    lv_result = cut->zif_msq_tdd_discount~get_max_discount( booking = ls_booking ).

    " Assert that the result is 7%
    cl_abap_unit_assert=>assert_equals( exp = 7
                                        act = lv_result
                                        msg = 'Expected base discount of 7% without early booking discount' ).
  ENDMETHOD.

  METHOD early_booking_discount.
    " Test case for an early booking discount (additional 5%) when booking is 14 or more days in advance
    DATA lv_result  TYPE zmind2_booking_discount.
    DATA ls_booking TYPE zstdd_booking.

    " Define booking and flight dates where the booking is at least 14 days before the flight
    ls_booking-booking_date = '20241001'. " Booking date
    ls_booking-flight_date  = '20241020'. " Flight date 19 days after booking

    " Call the method and capture the result
    lv_result = cut->zif_msq_tdd_discount~get_max_discount( booking = ls_booking ).

    " Assert that the result is 12% (7% base + 5% early booking)
    cl_abap_unit_assert=>assert_equals( exp = 12
                                        act = lv_result
                                        msg = 'Expected total discount of 12% for early booking' ).
  ENDMETHOD.

  METHOD no_discount_when_canceled.
    " Test case where no discount should be applied if booking status is 'X' (canceled)
    DATA lv_result  TYPE zmind2_booking_discount.
    DATA ls_booking TYPE zstdd_booking.

    ls_booking-booking_date   = '20241001'. " Any booking date
    ls_booking-flight_date    = '20241020'. " Any flight date
    ls_booking-booking_status = 'X'. " Booking status is canceled

    " Call the method and capture the result
    lv_result = cut->zif_msq_tdd_discount~get_max_discount( booking = ls_booking ).

    " Assert that the result is 0%
    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = lv_result
                                        msg = 'Expected no discount for booking status "X" (canceled)' ).
  ENDMETHOD.
ENDCLASS.
```
