"! <p class="shorttext synchronized" lang="en">ABAP Test Cockpit checks</p>
INTERFACE zif_adu_run_atc
  PUBLIC.

  METHODS check
    IMPORTING
      object_keys   TYPE satc_t_r3tr_keys
    RETURNING
      VALUE(result) TYPE if_satc_ci_rslt_upload_handler=>ty_result
    RAISING
      cx_adt_rest.

ENDINTERFACE.
