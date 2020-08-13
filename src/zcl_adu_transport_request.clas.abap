"! <p class="shorttext synchronized" lang="en">Transport Request</p>
CLASS zcl_adu_transport_request DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_adu_transport_request.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    "! @parameter transport_request | <p class="shorttext synchronized" lang="en">Transport request</p>
    "! @raising zcx_adu_transport_request | <p class="shorttext synchronized" lang="en">Exception</p>
    METHODS constructor
      IMPORTING
        transport_request TYPE trkorr
      RAISING
        zcx_adu_transport_request.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      transport_request_header TYPE trwbo_request_header.

    METHODS read_transport_request_header
      IMPORTING
        transport_request     TYPE trkorr
      RETURNING
        VALUE(request_header) TYPE trwbo_request_header
      RAISING
        zcx_adu_transport_request.

ENDCLASS.



CLASS zcl_adu_transport_request IMPLEMENTATION.


  METHOD constructor.

    transport_request_header = read_transport_request_header( transport_request ).

  ENDMETHOD.


  METHOD zif_adu_transport_request~get_header.

    request_header = transport_request_header.

  ENDMETHOD.


  METHOD zif_adu_transport_request~change_target.

    IF transport_request_header-tarsystem = target.
      RETURN.
    ENDIF.

    DATA(header) = transport_request_header.

    header-tarsystem = target.

    CALL FUNCTION 'TR_REQ_CHECK_HEADER'
      EXPORTING
        is_request_header       = header
        iv_check_for_release    = abap_false
        iv_check_text           = abap_false
        iv_check_client         = abap_false
*      IMPORTING
*       ev_client_check_impossible =
*      CHANGING
*       ct_messages             =                  " Generic Table for Error Messages
      EXCEPTIONS
        invalid_request         = 1
        invalid_target          = 2
        invalid_move_parameters = 3
        wrong_call              = 4
        OTHERS                  = 5.
    IF sy-subrc <> 0.
      zcx_adu_transport_request=>raise_system( ).
    ENDIF.

    DATA(e070) = CORRESPONDING e070( transport_request_header ).

    CALL FUNCTION 'TRINT_UPDATE_COMM_HEADER'
      EXPORTING
        wi_e070            = e070
        wi_sel_e070        = abap_true
      EXCEPTIONS
        e070_update_error  = 1
        e07t_update_error  = 2
        e070c_update_error = 3
        e070m_update_error = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      zcx_adu_transport_request=>raise_system( ).
    ENDIF.

  ENDMETHOD.


  METHOD read_transport_request_header.

    request_header = VALUE #( trkorr = transport_request ).

    CALL FUNCTION 'TRINT_READ_REQUEST_HEADER'
      EXPORTING
        iv_read_e070   = abap_true
        iv_read_e07t   = abap_true
        iv_read_e070c  = abap_true
        iv_read_e070m  = abap_true
      CHANGING
        cs_request     = request_header
      EXCEPTIONS
        empty_trkorr   = 1
        not_exist_e070 = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      zcx_adu_transport_request=>raise_system( ).
    ENDIF.

  ENDMETHOD.


ENDCLASS.
