"! <p class="shorttext synchronized" lang="en">Transport Request</p>
INTERFACE zif_adu_transport_request
  PUBLIC.

  TYPES:
    tt_copy_client_log TYPE STANDARD TABLE OF cccflow WITH DEFAULT KEY.

  "! <p class="shorttext synchronized" lang="en">Change target</p>
  "!
  "! @parameter target | <p class="shorttext synchronized" lang="en">New target</p>
  "! @raising zcx_adu_transport_request | <p class="shorttext synchronized" lang="en">Exception</p>
  METHODS change_target
    IMPORTING
      target TYPE tr_target
    RAISING
      zcx_adu_transport_request.

  "! <p class="shorttext synchronized" lang="en">Copy transport request to current client</p>
  "!
  "! @parameter source_client | <p class="shorttext synchronized" lang="en">Source client, by default TR client</p>
  "! @parameter test_run | <p class="shorttext synchronized" lang="en">Test execution</p>
  "! @parameter include_tasks | <p class="shorttext synchronized" lang="en">Copy included tasks</p>
  "! @raising zcx_adu_transport_request | <p class="shorttext synchronized" lang="en">Exception</p>
  METHODS copy_to_current_client
    IMPORTING
      source_client TYPE ccmand OPTIONAL
      test_run      TYPE cctestrun OPTIONAL
      include_tasks TYPE incl_task DEFAULT abap_true
    RAISING
      zcx_adu_transport_request.

  "! <p class="shorttext synchronized" lang="en">Get copy TR log between clients</p>
  "!
  "! @parameter source | <p class="shorttext synchronized" lang="en">Source client</p>
  "! @parameter target | <p class="shorttext synchronized" lang="en">Target client</p>
  "! @parameter include_test | <p class="shorttext synchronized" lang="en">Include test log</p>
  METHODS get_copy_client_log
    IMPORTING
      source        TYPE mandt OPTIONAL
      target        TYPE mandt OPTIONAL
      include_test  TYPE abap_bool OPTIONAL
    RETURNING
      VALUE(result) TYPE zif_adu_transport_request=>tt_copy_client_log.

  "! <p class="shorttext synchronized" lang="en">Get transport request header</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en">Header</p>
  METHODS get_header
    RETURNING
      VALUE(result) TYPE trwbo_request_header.

ENDINTERFACE.
