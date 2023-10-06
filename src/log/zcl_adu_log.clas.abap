CLASS zcl_adu_log DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adu_log.

    METHODS constructor
      IMPORTING
        iv_object     TYPE balobj_d
        iv_subobject  TYPE balsubobj
        iv_extnumber  TYPE balnrext
        iv_log_handle TYPE balloghndl OPTIONAL
        iv_source     TYPE balprog OPTIONAL
        iv_date       TYPE baldate DEFAULT sy-datum
        iv_time       TYPE baltime DEFAULT sy-uzeit.

  PROTECTED SECTION.
    METHODS initialize FINAL.

    METHODS get_default_header
      RETURNING
        VALUE(rs_result) TYPE bal_s_log.

    METHODS get_log_handle FINAL
      RETURNING
        VALUE(rv_result) TYPE balloghndl.

    METHODS is_initialized FINAL
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

  PRIVATE SECTION.
    DATA ms_header TYPE bal_s_log.
    DATA mv_log_handle TYPE balloghndl.
    DATA mv_initialized TYPE abap_bool.

ENDCLASS.



CLASS zcl_adu_log IMPLEMENTATION.


  METHOD constructor.

    ms_header-aluser    = sy-uname.
    ms_header-alprog    = iv_source.
    ms_header-extnumber = iv_extnumber.
    ms_header-object    = iv_object.
    ms_header-subobject = iv_subobject.
    ms_header-aldate    = iv_date.
    ms_header-altime    = iv_time.

  ENDMETHOD.


  METHOD zif_adu_log~add_exception.

    IF get_log_handle( ) IS INITIAL OR ix_exception IS NOT BOUND.
      RETURN.
    ENDIF.

    zif_adu_log~add_exception( ix_exception->previous ).

    CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
      EXPORTING
        i_log_handle = mv_log_handle
        i_s_exc      = VALUE bal_s_exc( msgty     = 'E'
                                        probclass = '2'
                                        exception = ix_exception )
      EXCEPTIONS
        OTHERS       = 0.

  ENDMETHOD.


  METHOD zif_adu_log~add_message.

    IF get_log_handle( ) IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ls_message) = is_message.

    ls_message-probclass = SWITCH #( ls_message-msgty WHEN 'E' OR 'A' THEN '2'
                                                      WHEN 'W'        THEN '3'
                                                      WHEN 'S' OR 'I' THEN '4'
                                                      ELSE ls_message-probclass ).

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = get_log_handle( )
        i_s_msg      = ls_message
      EXCEPTIONS
        OTHERS       = 0.

  ENDMETHOD.


  METHOD zif_adu_log~save.

    IF get_log_handle( ) IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
        i_in_update_task = abap_false
        i_t_log_handle   = VALUE bal_t_logh( ( get_log_handle( ) ) )
      EXCEPTIONS
        OTHERS           = 0.

  ENDMETHOD.


  METHOD initialize.

    IF is_initialized( ).
      RETURN.
    ENDIF.

    IF mv_log_handle IS NOT INITIAL.

      CALL FUNCTION 'BAL_LOG_READ'
        EXPORTING
          i_log_handle  = mv_log_handle
        EXCEPTIONS
          log_not_found = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        CLEAR mv_log_handle.
      ENDIF.

    ENDIF.

    IF mv_log_handle IS INITIAL.

      CALL FUNCTION 'BAL_LOG_CREATE'
        EXPORTING
          i_s_log                 = get_default_header( )
        IMPORTING
          e_log_handle            = mv_log_handle
        EXCEPTIONS
          log_header_inconsistent = 1
          OTHERS                  = 9.

      IF sy-subrc <> 0.
        CLEAR mv_log_handle.
      ENDIF.

    ENDIF.

    mv_initialized = abap_true.

  ENDMETHOD.


  METHOD get_default_header.

    rs_result = ms_header.

  ENDMETHOD.


  METHOD get_log_handle.

    initialize( ).

    rv_result = mv_log_handle.

  ENDMETHOD.


  METHOD is_initialized.

    rv_result = mv_initialized.

  ENDMETHOD.


ENDCLASS.
