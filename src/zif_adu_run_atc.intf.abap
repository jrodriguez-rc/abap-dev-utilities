"! <p class="shorttext synchronized" lang="en">ABAP Test Cockpit checks</p>
INTERFACE zif_adu_run_atc
  PUBLIC .

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Results</p>
    BEGIN OF ts_result,
      findings            TYPE cl_satc_adt_ch_factory=>ty_atc_verdicts,
      has_caused_abortion TYPE abap_bool,
    END OF ts_result.

  "! <p class="shorttext synchronized" lang="en">Execute checks</p>
  "!
  "! @parameter object_keys | <p class="shorttext synchronized" lang="en">Workbench Object keys</p>
  "! @parameter result | <p class="shorttext synchronized" lang="en">Checks result</p>
  "! @raising cx_adt_rest | <p class="shorttext synchronized" lang="en">ADT Exception</p>
  METHODS check
    IMPORTING
      object_keys   TYPE satc_t_r3tr_keys
    RETURNING
      VALUE(result) TYPE zif_adu_run_atc=>ts_result
    RAISING
      cx_adt_rest.

ENDINTERFACE.
