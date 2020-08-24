"! <p class="shorttext synchronized" lang="en">Exception Dynpro information</p>
INTERFACE zif_adu_exception_dyn_info
  PUBLIC.

  DATA:
    parameter TYPE bapiret2-parameter READ-ONLY,
    row       TYPE bapiret2-row READ-ONLY,
    field     TYPE bapiret2-field READ-ONLY.

ENDINTERFACE.
