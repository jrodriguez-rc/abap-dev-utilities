CLASS lcl_switch_test DEFINITION DEFERRED.
CLASS zcl_adu_switch DEFINITION LOCAL FRIENDS lcl_switch_test.
CLASS lcl_switch_test DEFINITION INHERITING FROM zcl_adu_switch FOR TESTING.

  PUBLIC SECTION.
    CLASS-METHODS create_test
      RETURNING
        VALUE(result) TYPE REF TO lcl_switch_test.

    METHODS set_condition
      IMPORTING
        ii_condition TYPE REF TO zif_adu_switch_condition.

    METHODS set_status
      IMPORTING
        iv_status TYPE zadu_switch_status.

    METHODS set_users
      IMPORTING
        it_users TYPE zcl_adu_switch=>ty_users.

ENDCLASS.


CLASS lcl_switch_test IMPLEMENTATION.


  METHOD create_test.

    result = NEW #( 'UnitTest' ).

  ENDMETHOD.


  METHOD set_condition.

    CAST zcl_adu_switch( me )->mi_condition = ii_condition.

  ENDMETHOD.


  METHOD set_status.

    CAST zcl_adu_switch( me )->ms_switch-status = iv_status.

  ENDMETHOD.


  METHOD set_users.

    CAST zcl_adu_switch( me )->mt_users = it_users.

  ENDMETHOD.


ENDCLASS.


CLASS ltcl_simple DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA:
      mo_switch TYPE REF TO lcl_switch_test.

    METHODS setup.
    METHODS disabled FOR TESTING RAISING zcx_adu_switch.
    METHODS enabled FOR TESTING RAISING zcx_adu_switch.

ENDCLASS.


CLASS ltcl_simple IMPLEMENTATION.


  METHOD setup.

    mo_switch = lcl_switch_test=>create_test( ).

  ENDMETHOD.


  METHOD disabled.

    mo_switch->set_status( zif_adu_switch=>gc_status-disabled ).

    cl_abap_unit_assert=>assert_false( mo_switch->zif_adu_switch~is_active( ) ).

    cl_abap_unit_assert=>assert_equals(
        act = mo_switch->zif_adu_switch~get_status( )
        exp = zif_adu_switch=>gc_status-disabled ).

  ENDMETHOD.


  METHOD enabled.

    mo_switch->set_status( zif_adu_switch=>gc_status-enabled ).

    cl_abap_unit_assert=>assert_true( mo_switch->zif_adu_switch~is_active( ) ).

    cl_abap_unit_assert=>assert_equals(
        act = mo_switch->zif_adu_switch~get_status( )
        exp = zif_adu_switch=>gc_status-enabled ).

  ENDMETHOD.


ENDCLASS.


CLASS ltcl_user DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA:
      mo_switch TYPE REF TO lcl_switch_test.

    METHODS setup.
    METHODS disabled FOR TESTING RAISING zcx_adu_switch.
    METHODS enabled FOR TESTING RAISING zcx_adu_switch.
    METHODS change_user FOR TESTING RAISING zcx_adu_switch.

    METHODS is_disabled.
    METHODS is_enabled.

ENDCLASS.


CLASS ltcl_user IMPLEMENTATION.


  METHOD setup.

    mo_switch = lcl_switch_test=>create_test( ).

    mo_switch->set_status( zif_adu_switch=>gc_status-user ).

  ENDMETHOD.


  METHOD disabled.

    is_disabled( ).

  ENDMETHOD.


  METHOD enabled.

    mo_switch->set_users( VALUE #( ( sy-uname ) ) ).

    is_enabled( ).

  ENDMETHOD.


  METHOD change_user.

    is_disabled( ).

    mo_switch->set_users( VALUE #( ( 'TESTUSER' ) ) ).

    is_disabled( ).

    mo_switch->set_users( VALUE #( ( 'TESTUSER' ) ( sy-uname ) ) ).

    is_enabled( ).

    mo_switch->set_users( VALUE #( ) ).

    is_disabled( ).

  ENDMETHOD.


  METHOD is_disabled.

    cl_abap_unit_assert=>assert_false( mo_switch->zif_adu_switch~is_active( ) ).

    cl_abap_unit_assert=>assert_equals(
        act = mo_switch->zif_adu_switch~get_status( )
        exp = zif_adu_switch=>gc_status-user ).

  ENDMETHOD.


  METHOD is_enabled.

    cl_abap_unit_assert=>assert_true( mo_switch->zif_adu_switch~is_active( ) ).

    cl_abap_unit_assert=>assert_equals(
        act = mo_switch->zif_adu_switch~get_status( )
        exp = zif_adu_switch=>gc_status-user ).

  ENDMETHOD.


ENDCLASS.


CLASS lcl_condition_test DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES zif_adu_switch_condition.

    TYPES:
      BEGIN OF ty_data,
        enabled TYPE abap_bool,
      END OF ty_data.

ENDCLASS.


CLASS lcl_condition_test IMPLEMENTATION.


  METHOD zif_adu_switch_condition~is_active.

    DATA(ls_data) = CORRESPONDING ty_data( is_data_check ).

    result = ls_data-enabled.

  ENDMETHOD.


ENDCLASS.


CLASS ltcl_condition DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA:
      mo_switch TYPE REF TO lcl_switch_test.

    METHODS setup.
    METHODS disabled FOR TESTING RAISING zcx_adu_switch.
    METHODS enabled FOR TESTING RAISING zcx_adu_switch.

ENDCLASS.


CLASS ltcl_condition IMPLEMENTATION.


  METHOD setup.

    mo_switch = lcl_switch_test=>create_test( ).

    mo_switch->set_status( zif_adu_switch=>gc_status-custom_condition ).
    mo_switch->set_condition( NEW lcl_condition_test( ) ).

  ENDMETHOD.


  METHOD disabled.

    DATA(ls_data) = VALUE lcl_condition_test=>ty_data( enabled = abap_false ).

    cl_abap_unit_assert=>assert_false( mo_switch->zif_adu_switch~is_active( ls_data ) ).

    cl_abap_unit_assert=>assert_equals(
        act = mo_switch->zif_adu_switch~get_status( )
        exp = zif_adu_switch=>gc_status-custom_condition ).

  ENDMETHOD.


  METHOD enabled.

    DATA(ls_data) = VALUE lcl_condition_test=>ty_data( enabled = abap_true ).

    cl_abap_unit_assert=>assert_true( mo_switch->zif_adu_switch~is_active( ls_data ) ).

    cl_abap_unit_assert=>assert_equals(
        act = mo_switch->zif_adu_switch~get_status( )
        exp = zif_adu_switch=>gc_status-custom_condition ).

  ENDMETHOD.


ENDCLASS.
