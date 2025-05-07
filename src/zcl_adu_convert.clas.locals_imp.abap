CLASS lcl_time DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES zif_adu_convert_time.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS gc_day_in_sec TYPE i VALUE 86400.

ENDCLASS.



CLASS lcl_time IMPLEMENTATION.


  METHOD zif_adu_convert_time~timestamp_abap_to_java.

    " Based on CL_PCO_UTILITY=>CONVERT_ABAP_TIMESTAMP_TO_JAVA

    CONVERT TIME STAMP iv_timestamp TIME ZONE '' INTO DATE DATA(lv_current_date) TIME DATA(lv_current_time).

    " Milliseconds for the days since January 1, 1970, 00:00:00 GMT
    " one day has 86400 seconds
    DATA(lv_date)           = CONV d( '19700101' ).
    DATA(lv_days_i)         = lv_current_date - lv_date.

    " Timestamp for passed days until today in seconds
    DATA(lv_days_timestamp) = CONV timestampl( lv_days_i * gc_day_in_sec ).

    DATA(lv_sec_i)          = CONV i( lv_current_time ).

    " Timestamp for time at present day
    DATA(lv_secs_timestamp) = CONV timestampl( lv_sec_i ).

    rv_result = lv_days_timestamp + lv_secs_timestamp.

  ENDMETHOD.


  METHOD zif_adu_convert_time~timestamp_abap_to_java_msec.

    DATA lv_dummy TYPE string ##NEEDED.

    DATA(lv_timestamp) = zif_adu_convert_time~timestamp_abap_to_java( iv_timestamp ).

    lv_timestamp = lv_timestamp * 1000.

    DATA(lv_miliseconds) = ( iv_timestamp MOD 1 ) * 1000.

    SPLIT |{ lv_timestamp }| AT '.' INTO DATA(lv_java_timestamp) lv_dummy.

    rv_result = lv_java_timestamp + CONV i( lv_miliseconds ).

  ENDMETHOD.


  METHOD zif_adu_convert_time~timestamp_java_to_abap.

    " Based on CL_PCO_UTILITY=>CONVERT_JAVA_TIMESTAMP_TO_ABAP

    " IV_TIMESTAMP stores milliseconds since January 1, 1970, 00:00:00 GMT
    DATA(lv_timestamp) = SWITCH timestampl( strlen( |{ iv_timestamp }| )   " timestamp in seconds
                                            WHEN 10 THEN iv_timestamp
                                            WHEN 13 THEN iv_timestamp / 1000 ).

    " One day has 86400 seconds: Timestamp in days
    DATA(lv_days_i) = CONV i( lv_timestamp DIV gc_day_in_sec ).
    DATA(lv_date)   = CONV d( '19700101' ).
    DATA(lv_date_result) = CONV d( lv_date + lv_days_i ).

    " Rest seconds (timestamp - days)
    DATA(lv_sec_i) = CONV i( lv_timestamp MOD gc_day_in_sec ).
    DATA(lv_time_result) = CONV t( lv_sec_i ).

    " Rest sec and milli seconds
    DATA(lv_timsmsec) = CONV timestampl( lv_timestamp MOD gc_day_in_sec ).
    lv_timsmsec = lv_timsmsec - lv_sec_i.

    CONVERT DATE lv_date_result TIME lv_time_result INTO TIME STAMP rv_result TIME ZONE ''.

    rv_result = rv_result + lv_timsmsec.

  ENDMETHOD.


ENDCLASS.
