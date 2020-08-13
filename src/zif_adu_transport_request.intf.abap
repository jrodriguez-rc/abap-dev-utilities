"! <p class="shorttext synchronized" lang="en">Transport Request</p>
INTERFACE zif_adu_transport_request
  PUBLIC.

  "! <p class="shorttext synchronized" lang="en">Get transport request header</p>
  "!
  "! @parameter request_header | <p class="shorttext synchronized" lang="en">Header</p>
  METHODS get_header
    RETURNING
      VALUE(request_header) TYPE trwbo_request_header.

  "! <p class="shorttext synchronized" lang="en">Change target</p>
  "!
  "! @parameter target | <p class="shorttext synchronized" lang="en">New target</p>
  "! @raising zcx_adu_transport_request | <p class="shorttext synchronized" lang="en">Exception</p>
  METHODS change_target
    IMPORTING
      target TYPE tr_target
    RAISING
      zcx_adu_transport_request.

ENDINTERFACE.
