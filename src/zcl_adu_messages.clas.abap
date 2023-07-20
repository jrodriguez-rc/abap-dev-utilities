"! <p class="shorttext synchronized" lang="en">Messages</p>
CLASS zcl_adu_messages DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_message_types TYPE STANDARD TABLE OF syst_msgty
          WITH DEFAULT KEY .
    TYPES:
      tt_messages      TYPE STANDARD TABLE OF bapiret2
          WITH DEFAULT KEY
          WITH NON-UNIQUE SORTED KEY type COMPONENTS type .

    CONSTANTS:
      BEGIN OF severity,
        error       TYPE symsgty VALUE 'E',
        warning     TYPE symsgty VALUE 'W',
        information TYPE symsgty VALUE 'I',
        success     TYPE symsgty VALUE 'S',
        exception   TYPE symsgty VALUE 'X',
        abort       TYPE symsgty VALUE 'A',
      END OF severity .
    CONSTANTS error_types TYPE string VALUE 'AEX' ##NO_TEXT.
    CLASS-DATA message_error_types TYPE tt_message_types READ-ONLY .

    CLASS-METHODS class_constructor .
    METHODS add_message
      IMPORTING
        !message_type   TYPE sy-msgty DEFAULT severity-error
        !message_id     TYPE sy-msgid
        !message_number TYPE sy-msgno
        !message_var_1  TYPE any OPTIONAL
        !message_var_2  TYPE any OPTIONAL
        !message_var_3  TYPE any OPTIONAL
        !message_var_4  TYPE any OPTIONAL
        !parameter      TYPE bapiret2-parameter OPTIONAL
        !row            TYPE bapiret2-row OPTIONAL
        !field          TYPE bapiret2-field OPTIONAL .
    METHODS add_messages
      IMPORTING
        !messages TYPE bapiret2_t .
    METHODS add_exception
      IMPORTING
        !exception TYPE REF TO cx_root
        !parameter TYPE bapiret2-parameter OPTIONAL
        !row       TYPE bapiret2-row OPTIONAL
        !field     TYPE bapiret2-field OPTIONAL .
    METHODS add_t100_message
      IMPORTING
        !message TYPE REF TO if_t100_message .
    METHODS add_text_message
      IMPORTING
        !message_type   TYPE sy-msgty DEFAULT severity-error
        !message_id     TYPE sy-msgid
        !message_number TYPE sy-msgno
        !text           TYPE string
        !parameter      TYPE bapiret2-parameter OPTIONAL
        !row            TYPE bapiret2-row OPTIONAL
        !field          TYPE bapiret2-field OPTIONAL .
    METHODS display_messages
      IMPORTING
        !initialize_after_display TYPE abap_bool DEFAULT abap_true
        !send_if_one              TYPE abap_bool DEFAULT abap_false .
    METHODS get_messages
      RETURNING
        VALUE(result) TYPE tt_messages .
    METHODS initialize .
    METHODS is_error
      RETURNING
        VALUE(result) TYPE abap_bool .
    METHODS raise_gateway
      RAISING
        /iwbep/cx_gateway .
    METHODS raise_gateway_busi_exception
      RAISING
        /iwbep/cx_mgw_busi_exception .
    METHODS raise_gateway_tech_exception
      RAISING
        /iwbep/cx_mgw_tech_exception .
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      collected_messages TYPE tt_messages.

    METHODS create_gateway_exception
      IMPORTING
        !iv_tech      TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO /iwbep/cx_mgw_base_exception.

    METHODS get_t100_attr
      IMPORTING
        !attribute    TYPE scx_attrname
        !exception    TYPE REF TO object
      RETURNING
        VALUE(result) TYPE symsgv.

    METHODS message_vars_prepare
      IMPORTING
        !message_var_1          TYPE any OPTIONAL
        !message_var_2          TYPE any OPTIONAL
        !message_var_3          TYPE any OPTIONAL
        !message_var_4          TYPE any OPTIONAL
      EXPORTING
        message_var_formatted_1 TYPE syst_msgv
        message_var_formatted_2 TYPE syst_msgv
        message_var_formatted_3 TYPE syst_msgv
        message_var_formatted_4 TYPE syst_msgv.

    METHODS message_var_prepare
      IMPORTING
        !message_var  TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE syst_msgv.

    METHODS fill_return_param
      IMPORTING
        !message_type   TYPE syst_msgty
        !message_id     TYPE syst_msgid
        !message_number TYPE syst_msgno
        !message_var_1  TYPE syst_msgv OPTIONAL
        !message_var_2  TYPE syst_msgv OPTIONAL
        !message_var_3  TYPE syst_msgv OPTIONAL
        !message_var_4  TYPE syst_msgv OPTIONAL
        !parameter      TYPE bapiret2-parameter OPTIONAL
        !row            TYPE bapiret2-row OPTIONAL
        !field          TYPE bapiret2-field OPTIONAL
      RETURNING
        VALUE(result)   TYPE bapiret2.

ENDCLASS.



CLASS zcl_adu_messages IMPLEMENTATION.


  METHOD add_exception.

    DATA:
      text_message TYPE c LENGTH 250.

    IF exception IS NOT BOUND.
      RETURN.
    ENDIF.

    add_exception( exception->previous ).

    TRY.
        DATA(dyn_info) = CAST zif_adu_exception_dyn_info( exception ).
      CATCH cx_sy_move_cast_error.
        CLEAR: dyn_info.
    ENDTRY.

    DATA(current_parameter) =
        COND #(
            WHEN parameter IS NOT INITIAL
                THEN parameter
            WHEN dyn_info IS BOUND
                THEN dyn_info->parameter ).

    DATA(current_row) =
        COND #(
            WHEN row IS NOT INITIAL
                THEN row
            WHEN dyn_info IS BOUND
                THEN dyn_info->row ).

    DATA(current_field) =
        COND #(
            WHEN field IS NOT INITIAL
                THEN field
            WHEN dyn_info IS BOUND
                THEN dyn_info->field ).

    TRY.
        DATA(t100_exception) = CAST if_t100_message( exception ).
        add_message(
                message_id     = t100_exception->t100key-msgid
                message_number = t100_exception->t100key-msgno
                message_var_1  = get_t100_attr( exception = exception attribute = t100_exception->t100key-attr1 )
                message_var_2  = get_t100_attr( exception = exception attribute = t100_exception->t100key-attr2 )
                message_var_3  = get_t100_attr( exception = exception attribute = t100_exception->t100key-attr3 )
                message_var_4  = get_t100_attr( exception = exception attribute = t100_exception->t100key-attr4 )
                parameter      = current_parameter
                row            = current_row
                field          = current_field ).
      CATCH cx_sy_move_cast_error.
        text_message  = exception->get_text( ).
        DATA(text_message_var_1) = text_message(50).
        DATA(text_message_var_2) = text_message+50(50).

        exception->get_source_position( IMPORTING include_name = DATA(lv_include)
                                                  source_line  = DATA(lv_line) ).

        IF text_message IS NOT  INITIAL.
          add_message( message_id     = zcx_adu_messages=>text_exception_message-msgid
                       message_number = zcx_adu_messages=>text_exception_message-msgno
                       message_var_1  = text_message_var_1
                       message_var_2  = text_message_var_2
                       message_var_3  = lv_include
                       message_var_4  = lv_line
                       parameter      = current_parameter
                       row            = current_row
                       field          = current_field ).
        ENDIF.

    ENDTRY.

  ENDMETHOD.


  METHOD add_message.

    message_vars_prepare( EXPORTING message_var_1           = message_var_1
                                    message_var_2           = message_var_2
                                    message_var_3           = message_var_3
                                    message_var_4           = message_var_4
                          IMPORTING message_var_formatted_1 = DATA(lv_msgv1)
                                    message_var_formatted_2 = DATA(lv_msgv2)
                                    message_var_formatted_3 = DATA(lv_msgv3)
                                    message_var_formatted_4 = DATA(lv_msgv4) ).

    INSERT fill_return_param( message_type   = message_type
                              message_id     = message_id
                              message_number = message_number
                              message_var_1  = lv_msgv1
                              message_var_2  = lv_msgv2
                              message_var_3  = lv_msgv3
                              message_var_4  = lv_msgv4
                              parameter      = parameter
                              row            = row
                              field          = field ) INTO TABLE collected_messages.

  ENDMETHOD.


  METHOD add_messages.

    LOOP AT messages INTO DATA(ls_message).
      add_message( message_type   = ls_message-type
                   message_id     = ls_message-id
                   message_number = ls_message-number
                   message_var_1  = ls_message-message_v1
                   message_var_2  = ls_message-message_v2
                   message_var_3  = ls_message-message_v3
                   message_var_4  = ls_message-message_v4
                   parameter      = ls_message-parameter
                   row            = ls_message-row
                   field          = ls_message-field ).
    ENDLOOP.

  ENDMETHOD.


  METHOD add_t100_message.

    TRY.
        DATA(dyn_info) = CAST zif_adu_exception_dyn_info( message ).
        DATA(parameter) = dyn_info->parameter.
        DATA(row)       = dyn_info->row.
        DATA(field)     = dyn_info->field.
      CATCH cx_sy_move_cast_error.
    ENDTRY.

    add_message(
            message_id     = message->t100key-msgid
            message_number = message->t100key-msgno
            message_var_1  = get_t100_attr( exception = message attribute = message->t100key-attr1 )
            message_var_2  = get_t100_attr( exception = message attribute = message->t100key-attr2 )
            message_var_3  = get_t100_attr( exception = message attribute = message->t100key-attr3 )
            message_var_4  = get_t100_attr( exception = message attribute = message->t100key-attr4 )
            parameter      = parameter
            row            = row
            field          = field ).

  ENDMETHOD.


  METHOD add_text_message.

    DATA:
      BEGIN OF message_variables,
        var1 TYPE symsgv,
        var2 TYPE symsgv,
        var3 TYPE symsgv,
        var4 TYPE symsgv,
      END OF message_variables.

    message_variables = text.

    add_message(
        message_type   = message_type
        message_id     = message_id
        message_number = message_number
        message_var_1  = message_variables-var1
        message_var_2  = message_variables-var2
        message_var_3  = message_variables-var3
        message_var_4  = message_variables-var4
        parameter      = parameter
        row            = row
        field          = field ).

  ENDMETHOD.


  METHOD class_constructor.

    message_error_types = VALUE #( ( severity-abort )
                                   ( severity-error )
                                   ( severity-exception ) ).

  ENDMETHOD.


  METHOD create_gateway_exception.

    DATA(lt_messages) = get_messages( ).

    result = COND #( WHEN iv_tech = abap_false THEN NEW /iwbep/cx_mgw_busi_exception( )
                                                  ELSE NEW /iwbep/cx_mgw_tech_exception( ) ).

    DATA(messages_container) = CAST /iwbep/if_message_container( result->get_msg_container( ) ).

    messages_container->add_messages_from_bapi(
                        it_bapi_messages         = CORRESPONDING #( get_messages( ) )
                        iv_determine_leading_msg = /iwbep/if_message_container=>gcs_leading_msg_search_option-first ).

  ENDMETHOD.


  METHOD display_messages.

    CALL FUNCTION 'MESSAGES_INITIALIZE'
      EXCEPTIONS
        OTHERS = 0.

    DATA(messages) = get_messages( ).

    LOOP AT messages INTO DATA(message).
      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          arbgb  = message-id
          msgty  = message-type
          msgv1  = message-message_v1
          msgv2  = message-message_v2
          msgv3  = message-message_v3
          msgv4  = message-message_v4
          txtnr  = message-number
        EXCEPTIONS
          OTHERS = 0.
    ENDLOOP.

    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
        send_if_one = send_if_one
      EXCEPTIONS
        OTHERS      = 0.

    IF initialize_after_display = abap_true.
      initialize( ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_return_param.

    DATA:
      BEGIN OF ls_message_backup,
        type   LIKE message_type,
        id     LIKE message_id,
        number LIKE message_number,
        var1   LIKE message_var_1,
        var2   LIKE message_var_2,
        var3   LIKE message_var_3,
        var4   LIKE message_var_4,
      END OF ls_message_backup.

    " BALW_BAPIRETURN_GET2 will replace the system variables
    ls_message_backup-type   = sy-msgty.
    ls_message_backup-id     = sy-msgid.
    ls_message_backup-number = sy-msgno.
    ls_message_backup-var1   = sy-msgv1.
    ls_message_backup-var2   = sy-msgv2.
    ls_message_backup-var3   = sy-msgv3.
    ls_message_backup-var4   = sy-msgv4.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type      = message_type
        cl        = message_id
        number    = message_number
        par1      = message_var_1
        par2      = message_var_2
        par3      = message_var_3
        par4      = message_var_4
        parameter = parameter
        row       = row
        field     = field
      IMPORTING
        return    = result
      EXCEPTIONS
        OTHERS    = 0.

    IF ls_message_backup IS INITIAL.
      CLEAR sy-msgty.
      CLEAR sy-msgid.
      CLEAR sy-msgno.
      CLEAR sy-msgv1.
      CLEAR sy-msgv2.
      CLEAR sy-msgv3.
      CLEAR sy-msgv4.
    ELSE.
      MESSAGE ID     ls_message_backup-id
              TYPE   ls_message_backup-type
              NUMBER ls_message_backup-number
              WITH   ls_message_backup-var1
                     ls_message_backup-var2
                     ls_message_backup-var3
                     ls_message_backup-var4
              INTO   DATA(null).
    ENDIF.

  ENDMETHOD.


  METHOD get_messages.

    result = collected_messages.

  ENDMETHOD.


  METHOD get_t100_attr.

    FIELD-SYMBOLS <simple> TYPE simple.

    IF attribute IS INITIAL OR exception IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        ASSIGN exception->(attribute) TO <simple>.
        IF <simple> IS ASSIGNED.
          result = <simple>.
          RETURN.
        ENDIF.
      CATCH cx_root.
        CLEAR result.
    ENDTRY.

    result = attribute.

  ENDMETHOD.


  METHOD initialize.
    CLEAR: collected_messages.
  ENDMETHOD.


  METHOD is_error.

    DATA(messages) = get_messages( ).

    result =
        xsdbool( line_exists( messages[ KEY type type = severity-error ] ) OR
                 line_exists( messages[ KEY type type = severity-abort ] ) OR
                 line_exists( messages[ KEY type type = severity-exception ] ) ).

  ENDMETHOD.


  METHOD message_vars_prepare.

    message_var_formatted_1 = message_var_prepare( message_var_1 ).
    message_var_formatted_2 = message_var_prepare( message_var_2 ).
    message_var_formatted_3 = message_var_prepare( message_var_3 ).
    message_var_formatted_4 = message_var_prepare( message_var_4 ).

  ENDMETHOD.


  METHOD message_var_prepare.

    DESCRIBE FIELD message_var TYPE DATA(lv_field_type).

    result = SWITCH #( lv_field_type
                            WHEN 'D'
                                THEN |{ CONV d( message_var ) DATE = USER }|
                            WHEN 'T'
                                THEN |{ CONV t( message_var ) TIME = USER }|
                            WHEN 'P' OR 'I'
                                THEN |{ CONV string( message_var ) ALIGN = LEFT }|
                            WHEN 'C'
                                THEN message_var
                                ELSE |{ CONV string( message_var ) ALPHA = OUT }| ).

  ENDMETHOD.


  METHOD raise_gateway.

    DATA(lt_messages) = get_messages( ).

    DATA(exception) = NEW  /iwbep/cx_gateway( ).

    DATA(messages_container) = exception->get_message_container( ).

    LOOP AT get_messages( ) INTO DATA(message).

      messages_container->add_t100(
          iv_msg_type                 = message-type
          iv_msg_id                   = message-id
          iv_msg_number               = message-number
          iv_msg_v1                   = message-message_v1
          iv_msg_v2                   = message-message_v2
          iv_msg_v3                   = message-message_v3
          iv_msg_v4                   = message-message_v4
          iv_leading_message_for_user = xsdbool( sy-tabix = 1 ) ).

    ENDLOOP.

    RAISE EXCEPTION exception.

  ENDMETHOD.


  METHOD raise_gateway_busi_exception.

    DATA(exception) = CAST /iwbep/cx_mgw_busi_exception( create_gateway_exception( ) ).

    RAISE EXCEPTION exception.

  ENDMETHOD.


  METHOD raise_gateway_tech_exception.

    DATA(exception) = CAST /iwbep/cx_mgw_tech_exception( create_gateway_exception( ) ).

    RAISE EXCEPTION exception.

  ENDMETHOD.
ENDCLASS.
