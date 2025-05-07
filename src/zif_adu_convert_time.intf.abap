INTERFACE zif_adu_convert_time
  PUBLIC.

  TYPES ty_epoch TYPE p LENGTH 13 DECIMALS 0.

  METHODS timestamp_abap_to_java
    IMPORTING iv_timestamp     TYPE timestampl
    RETURNING VALUE(rv_result) TYPE ty_epoch.

  METHODS timestamp_abap_to_java_msec
    IMPORTING iv_timestamp     TYPE timestampl
    RETURNING VALUE(rv_result) TYPE ty_epoch.

  METHODS timestamp_java_to_abap
    IMPORTING iv_timestamp     TYPE ty_epoch
    RETURNING VALUE(rv_result) TYPE timestampl.

ENDINTERFACE.
