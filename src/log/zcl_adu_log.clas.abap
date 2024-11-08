CLASS zcl_adu_log DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_adu_log.

    METHODS constructor
      IMPORTING iv_object     TYPE balobj_d
                iv_subobject  TYPE balsubobj
                iv_extnumber  TYPE balnrext
                iv_log_handle TYPE balloghndl OPTIONAL
                iv_source     TYPE balprog    OPTIONAL
                iv_date       TYPE baldate    OPTIONAL
                iv_time       TYPE baltime    OPTIONAL.

  PROTECTED SECTION.
    METHODS initialize FINAL.

    METHODS get_default_header
      RETURNING VALUE(rs_result) TYPE bal_s_log.

    METHODS get_log_handle FINAL
      RETURNING VALUE(rv_result) TYPE balloghndl.

    METHODS is_initialized FINAL
      RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS apply_problem_class
      IMPORTING iv_problem_class TYPE balprobcl
      RETURNING VALUE(rv_result) TYPE abap_bool.

    METHODS get_lower_problem_class
      RETURNING VALUE(rv_result) TYPE balprobcl.

    METHODS add_content
      IMPORTING iv_content        TYPE string
                iv_content_type   TYPE balpval
                is_custom_message TYPE zif_adu_log=>ty_message OPTIONAL
                is_callback       TYPE bal_s_clbk              OPTIONAL.

    METHODS content_to_params
      IMPORTING iv_content       TYPE string
      RETURNING VALUE(rt_result) TYPE bal_t_par.

  PRIVATE SECTION.
    DATA ms_header              TYPE bal_s_log.
    DATA mv_log_handle          TYPE balloghndl.
    DATA mv_initialized         TYPE abap_bool.
    DATA mv_lower_problem_class TYPE balprobcl.

    METHODS determine_severity_filter.

ENDCLASS.



CLASS zcl_adu_log IMPLEMENTATION.


  METHOD constructor.

    mv_log_handle = iv_log_handle.

    ms_header-aluser    = sy-uname.
    ms_header-alprog    = iv_source.
    ms_header-extnumber = iv_extnumber.
    ms_header-object    = iv_object.
    ms_header-subobject = iv_subobject.

    IF iv_date IS INITIAL.
      ms_header-aldate = zcl_adu_general=>get( )->get_date( ).
    ELSE.
      ms_header-aldate = iv_date.
    ENDIF.

    IF iv_time IS INITIAL.
      ms_header-altime = zcl_adu_general=>get( )->get_time( ).
    ELSE.
      ms_header-altime = iv_time.
    ENDIF.

    determine_severity_filter( ).

  ENDMETHOD.


  METHOD zif_adu_log~add_exception.

    ri_result = me.

    IF get_log_handle( ) IS INITIAL OR ix_exception IS NOT BOUND.
      RETURN.
    ENDIF.

    IF apply_problem_class( zif_adu_log=>gc_message_class-important ) = abap_false.
      RETURN.
    ENDIF.

    zif_adu_log~add_exception( ix_exception->previous ).

    CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
      EXPORTING
        i_log_handle = mv_log_handle
        i_s_exc      = VALUE bal_s_exc( msgty     = zcl_adu_messages=>severity-error
                                        probclass = zif_adu_log=>gc_message_class-important
                                        exception = ix_exception )
      EXCEPTIONS
        OTHERS       = 0.

  ENDMETHOD.


  METHOD zif_adu_log~add_message.

    ri_result = me.

    IF get_log_handle( ) IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ls_message) = is_message.

    IF ls_message-probclass IS INITIAL.
      ls_message-probclass =
        SWITCH #( ls_message-msgty
                  WHEN zcl_adu_messages=>severity-abort OR zcl_adu_messages=>severity-exception
                    THEN zif_adu_log=>gc_message_class-very_important
                  WHEN zcl_adu_messages=>severity-error
                    THEN zif_adu_log=>gc_message_class-important
                  WHEN zcl_adu_messages=>severity-warning
                    THEN zif_adu_log=>gc_message_class-medium
                  WHEN zcl_adu_messages=>severity-success OR zcl_adu_messages=>severity-information
                    THEN zif_adu_log=>gc_message_class-additinal_information
                  ELSE
                    zif_adu_log=>gc_message_class-other ).
    ENDIF.

    IF apply_problem_class( ls_message-probclass ) = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = get_log_handle( )
        i_s_msg      = ls_message
      EXCEPTIONS
        OTHERS       = 0.

  ENDMETHOD.


  METHOD zif_adu_log~add_bapiret_message.

    ri_result = me.

    zif_adu_log~add_message( VALUE #( msgty = is_message-type
                                      msgid = is_message-id
                                      msgno = is_message-number
                                      msgv1 = is_message-message_v1
                                      msgv2 = is_message-message_v2
                                      msgv3 = is_message-message_v3
                                      msgv4 = is_message-message_v4 ) ).

  ENDMETHOD.


  METHOD zif_adu_log~add_content_json.

    ri_result = me.

    add_content( iv_content        = iv_json
                 iv_content_type   = CONV #( if_rest_media_type=>gc_appl_json )
                 is_custom_message = is_custom_message ).

  ENDMETHOD.


  METHOD zif_adu_log~add_content_xml.

    ri_result = me.

    add_content( iv_content        = iv_xml
                 iv_content_type   = CONV #( if_rest_media_type=>gc_appl_xml )
                 is_custom_message = is_custom_message ).

  ENDMETHOD.


  METHOD zif_adu_log~add_text.

    ri_result = me.

    DATA(li_message) = zcl_adu_messages=>create( ).

    li_message->add_text( iv_message_type = iv_type
                          iv_text         = iv_text ).

    LOOP AT li_message->get_messages( ) ASSIGNING FIELD-SYMBOL(<ls_message>).
      zif_adu_log~add_bapiret_message( <ls_message> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_adu_log~save.

    ri_result = me.

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


  METHOD apply_problem_class.

    rv_result = xsdbool(    get_lower_problem_class( ) IS INITIAL
                         OR
                            (     iv_problem_class <= get_lower_problem_class( )
                              AND iv_problem_class IS NOT INITIAL ) ).

  ENDMETHOD.


  METHOD get_lower_problem_class.

    rv_result = mv_lower_problem_class.

  ENDMETHOD.


  METHOD add_content.

    DATA(ls_callback) =
        COND #( WHEN is_callback IS NOT INITIAL
                THEN is_callback
                ELSE VALUE #( userexitf = 'Z_ADU_LOG_DISPLAY_CONTENT'
                              userexitt = 'F' ) ).

    DATA(lv_base64) = cl_http_utility=>encode_base64( iv_content ).

    DATA(ls_parameters) =
        VALUE bal_s_parm( callback = ls_callback
                          t_par    = VALUE #( ( parname  = zif_adu_log=>gc_parameter-content_type
                                                parvalue = iv_content_type )
                                              ( LINES OF content_to_params( lv_base64 ) ) ) ).

    IF is_custom_message IS INITIAL.
      zif_adu_log~add_message( VALUE #( msgty  = zcl_adu_messages=>severity-information
                                        msgid  = zcx_adu_log=>display_content-msgid
                                        msgno  = zcx_adu_log=>display_content-msgno
                                        params = ls_parameters ) ).
    ELSE.
      zif_adu_log~add_message( VALUE #( msgty  = is_custom_message-type
                                        msgid  = is_custom_message-id
                                        msgno  = is_custom_message-number
                                        msgv1  = is_custom_message-variable1
                                        msgv2  = is_custom_message-variable2
                                        msgv3  = is_custom_message-variable3
                                        msgv4  = is_custom_message-variable4
                                        params = ls_parameters ) ).
    ENDIF.

  ENDMETHOD.


  METHOD content_to_params.

    DATA lt_parameter_values TYPE STANDARD TABLE OF balpval WITH EMPTY KEY.

    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = iv_content
      TABLES
        ftext_tab = lt_parameter_values.

    rt_result =
        VALUE #( FOR <value> IN lt_parameter_values
                 parname = zif_adu_log=>gc_parameter-content
                 ( parvalue = <value> ) ).

  ENDMETHOD.


  METHOD determine_severity_filter.

    SELECT subobject,lower_problem_class
      FROM zadu_log_severit
      WHERE object = @ms_header-object
      INTO TABLE @DATA(lt_severity).
    IF sy-subrc <> 0.
      mv_lower_problem_class = zif_adu_log=>gc_message_class-other.
      RETURN.
    ENDIF.

    TRY.
        mv_lower_problem_class = lt_severity[ subobject = ms_header-subobject ]-lower_problem_class.
      CATCH cx_sy_itab_line_not_found.
        mv_lower_problem_class = VALUE #( lt_severity[ subobject = '' ]-lower_problem_class OPTIONAL ).
    ENDTRY.

  ENDMETHOD.


ENDCLASS.
