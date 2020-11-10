"! <p class="shorttext synchronized" lang="en">Transport Request</p>
CLASS zcl_adu_transport_request DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_adu_transport_request.

    CLASS-METHODS create
      IMPORTING
        transport_request TYPE trkorr
      RETURNING
        VALUE(ri_result)  TYPE REF TO zif_adu_transport_request
      RAISING
        zcx_adu_transport_request.

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


  METHOD create.

    ri_result = NEW zcl_adu_transport_request( transport_request ).

  ENDMETHOD.


  METHOD constructor.

    transport_request_header = read_transport_request_header( transport_request ).

  ENDMETHOD.


  METHOD zif_adu_transport_request~change_target.

    IF transport_request_header-tarsystem = target.
      RETURN.
    ENDIF.

    DATA(header_changed) = transport_request_header.

    header_changed-tarsystem = target.

    CALL FUNCTION 'TR_REQ_CHECK_HEADER'
      EXPORTING
        is_request_header       = header_changed
        iv_check_for_release    = abap_false
        iv_check_text           = abap_false
        iv_check_client         = abap_false
      EXCEPTIONS
        invalid_request         = 1
        invalid_target          = 2
        invalid_move_parameters = 3
        wrong_call              = 4
        OTHERS                  = 5.
    IF sy-subrc <> 0.
      zcx_adu_transport_request=>raise_system( ).
    ENDIF.

    DATA(e070) = CORRESPONDING e070( header_changed ).

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


  METHOD zif_adu_transport_request~copy_to_current_client.

    DATA:
      return_code TYPE syst_subrc.

    IF sy-mandt = '000' OR sy-mandt IS INITIAL.
      RAISE EXCEPTION TYPE zcx_adu_transport_request
        EXPORTING
          textid = zcx_adu_transport_request=>client_000_protected.
    ENDIF.

    DATA(transport_request) = zif_adu_transport_request~get_header( ).

    DATA(from_client) = COND #( WHEN source_client IS NOT INITIAL
                                    THEN source_client
                                    ELSE transport_request-client ).

    IF from_client = sy-mandt.
      RAISE EXCEPTION TYPE zcx_adu_transport_request
        EXPORTING
          textid = zcx_adu_transport_request=>source_client_same_logon.
    ENDIF.

    SELECT SINGLE *
      INTO @DATA(client_data)
      FROM t000
      WHERE mandt = @from_client.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_adu_transport_request
        EXPORTING
          textid = zcx_adu_transport_request=>source_client_not_exists
          text1  = CONV #( from_client ).
    ENDIF.

    IF client_data-cccopylock = 'L'.
      RAISE EXCEPTION TYPE zcx_adu_transport_request
        EXPORTING
          textid = zcx_adu_transport_request=>client_copy_protected
          text1  = CONV #( client_data-mandt ).
    ENDIF.

    CALL FUNCTION 'SCCR_PERFORM_SCC1'
      EXPORTING
        ccsupcopy    = test_run
        cccomfile    = transport_request-trkorr
        quellmandant = from_client
        incl_task    = include_tasks
      IMPORTING
        rcod         = return_code.
    IF return_code <> 0.
      RAISE EXCEPTION TYPE zcx_adu_transport_request
        EXPORTING
          textid = zcx_adu_transport_request=>program_ended_with_error
          text1  = CONV #( return_code ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_adu_transport_request~get_copy_client_log.

    DATA:
      source_range    TYPE RANGE OF mandt,
      target_range    TYPE RANGE OF cczmand,
      test_mode_range TYPE RANGE OF cctestmode.

    IF source IS SUPPLIED.
      source_range = VALUE #( ( sign = 'I' option = 'EQ' low = source ) ).
    ENDIF.

    IF target IS SUPPLIED.
      target_range = VALUE #( ( sign = 'I' option = 'EQ' low = target ) ).
    ENDIF.

    IF include_test = abap_true.
      test_mode_range = VALUE #( ( sign = 'I' option = 'EQ' low = include_test ) ).
    ENDIF.

    DATA(transport_request) = zif_adu_transport_request~get_header( ).

    SELECT *
      INTO TABLE @result
      FROM cccflow
      WHERE comfile     = @transport_request-trkorr
        AND mandt      IN @source_range
        AND sourcemand IN @target_range
        AND test_mode  IN @test_mode_range.
    IF sy-subrc <> 0.
      CLEAR: result.
    ENDIF.

  ENDMETHOD.


  METHOD zif_adu_transport_request~get_header.

    request_header = transport_request_header.

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
