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
        iv_source     TYPE balprog OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mv_log_handle TYPE balloghndl.

ENDCLASS.



CLASS zcl_adu_log IMPLEMENTATION.


  METHOD constructor.

    IF iv_log_handle IS NOT INITIAL.

    ELSE.

      CALL FUNCTION 'BAL_LOG_READ'
        EXPORTING
          i_log_handle  = iv_log_handle
        EXCEPTIONS
          log_not_found = 1
          OTHERS        = 2.
      IF sy-subrc = 0.
        mv_log_handle = iv_log_handle.
      ENDIF.

    ENDIF.

    IF mv_log_handle IS INITIAL.

      CALL FUNCTION 'BAL_LOG_CREATE'
        EXPORTING
          i_s_log                 = VALUE bal_s_log( aluser    = sy-uname
                                                     alprog    = iv_source
                                                     extnumber = iv_extnumber
                                                     object    = iv_object
                                                     subobject = iv_subobject )
        IMPORTING
          e_log_handle            = mv_log_handle
        EXCEPTIONS
          log_header_inconsistent = 1
          OTHERS                  = 9.

      IF sy-subrc <> 0.
        CLEAR mv_log_handle.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_adu_log~add_exception.

    IF mv_log_handle IS INITIAL OR ix_exception IS NOT BOUND.
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

    IF mv_log_handle IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ls_message) = is_message.

    ls_message-probclass = SWITCH #( ls_message-msgty WHEN 'E' OR 'A' THEN '2'
                                                      WHEN 'W'        THEN '3'
                                                      WHEN 'S' OR 'I' THEN '4'
                                                      ELSE ls_message-probclass ).

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = mv_log_handle
        i_s_msg      = ls_message
      EXCEPTIONS
        OTHERS       = 0.

  ENDMETHOD.


  METHOD zif_adu_log~save.

    IF mv_log_handle IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client         = sy-mandt
        i_in_update_task = abap_false
        i_t_log_handle   = VALUE bal_t_logh( ( mv_log_handle ) )
      EXCEPTIONS
        OTHERS           = 0.

  ENDMETHOD.


ENDCLASS.
