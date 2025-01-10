CLASS zcl_adu_switch DEFINITION
  PUBLIC
  CREATE PROTECTED.

  PUBLIC SECTION.
    INTERFACES zif_adu_switch.

    CLASS-METHODS get
      IMPORTING iv_code       TYPE zadu_switch_code
      RETURNING VALUE(result) TYPE REF TO zif_adu_switch.

    METHODS constructor
      IMPORTING iv_code TYPE zadu_switch_code.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_buffer_switch,
        code     TYPE zadu_switch_code,
        instance TYPE REF TO zif_adu_switch,
      END OF ty_buffer_switch,
      ty_buffer_switchs TYPE HASHED TABLE OF ty_buffer_switch WITH UNIQUE KEY code.

    TYPES ty_users TYPE STANDARD TABLE OF uname WITH KEY table_line.

    CLASS-DATA gt_buffer TYPE ty_buffer_switchs.

    DATA ms_switch    TYPE zadu_switch.
    DATA mt_users     TYPE ty_users.
    DATA mi_condition TYPE REF TO zif_adu_switch_condition.

    CLASS-METHODS create
      IMPORTING iv_code       TYPE zadu_switch_code
      RETURNING VALUE(result) TYPE REF TO zif_adu_switch.

    METHODS load_users.

    METHODS build_custom_condition.

    METHODS check_custom_condition
      IMPORTING is_custom_data_check TYPE any
      RETURNING VALUE(result)        TYPE abap_bool.

ENDCLASS.



CLASS zcl_adu_switch IMPLEMENTATION.


  METHOD get.

    TRY.
        result = gt_buffer[ code = iv_code ]-instance.
      CATCH cx_sy_itab_line_not_found.
        result = create( iv_code ).
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    SELECT SINGLE *
      FROM zadu_switch
      WHERE code = @iv_code
      INTO @ms_switch.
    IF sy-subrc <> 0.
      ms_switch-code = iv_code.
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA(lo_instance) = NEW zcl_adu_switch( iv_code ).

    lo_instance->load_users( ).
    lo_instance->build_custom_condition( ).

    INSERT VALUE #( code     = iv_code
                    instance = lo_instance ) INTO TABLE gt_buffer.

    result = lo_instance.

  ENDMETHOD.


  METHOD zif_adu_switch~get_status.

    result = ms_switch-status.

  ENDMETHOD.


  METHOD zif_adu_switch~is_active.

    DATA(lv_status) = zif_adu_switch~get_status( ).

    result =
        xsdbool(    lv_status = zif_adu_switch=>gc_status-enabled
                 OR (     lv_status = zif_adu_switch=>gc_status-user
                      AND line_exists( mt_users[ table_line = sy-uname ] ) )
                 OR (     lv_status = zif_adu_switch=>gc_status-custom_condition
                      AND check_custom_condition( is_custom_data_check ) ) ).

  ENDMETHOD.


  METHOD load_users.

    CLEAR mt_users.

    IF zif_adu_switch~get_status( ) <> zif_adu_switch=>gc_status-user.
      RETURN.
    ENDIF.

    SELECT username
      FROM zadu_switch_user
      WHERE code = @ms_switch-code
      INTO TABLE @mt_users.
    IF sy-subrc <> 0.
      CLEAR mt_users.
    ENDIF.

  ENDMETHOD.


  METHOD build_custom_condition.

    CLEAR mi_condition.

    IF zif_adu_switch~get_status( ) <> zif_adu_switch=>gc_status-custom_condition OR ms_switch-custom_check IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT mi_condition TYPE (ms_switch-custom_check).

  ENDMETHOD.


  METHOD check_custom_condition.

    IF mi_condition IS NOT BOUND.
      RETURN.
    ENDIF.

    result = mi_condition->is_active( is_custom_data_check ).

  ENDMETHOD.


ENDCLASS.
