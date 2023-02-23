CLASS ltcl_system_messages DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.

  PRIVATE SECTION.
    METHODS with_message FOR TESTING
      RAISING zcx_adu_messages.

    METHODS without_message FOR TESTING
      RAISING zcx_adu_messages.

ENDCLASS.


CLASS ltcl_system_messages IMPLEMENTATION.


  METHOD without_message.

    NEW zcl_adu_messages( )->add_exception( NEW zcx_adu_messages( ) ).

    cl_abap_unit_assert=>assert_initial( sy-msgty ).
    cl_abap_unit_assert=>assert_initial( sy-msgid ).
    cl_abap_unit_assert=>assert_initial( sy-msgno ).
    cl_abap_unit_assert=>assert_initial( sy-msgv1 ).
    cl_abap_unit_assert=>assert_initial( sy-msgv2 ).
    cl_abap_unit_assert=>assert_initial( sy-msgv3 ).
    cl_abap_unit_assert=>assert_initial( sy-msgv4 ).

  ENDMETHOD.


  METHOD with_message.

    MESSAGE ID zcx_adu_messages=>data_type_not_compatible-msgid
            TYPE 'E'
            NUMBER zcx_adu_messages=>data_type_not_compatible-msgno
        INTO DATA(null).

    NEW zcl_adu_messages( )->add_exception( NEW zcx_adu_messages( ) ).

    cl_abap_unit_assert=>assert_equals( act = sy-msgty
                                        exp = 'E' ).

    cl_abap_unit_assert=>assert_equals( act = sy-msgid
                                        exp = zcx_adu_messages=>data_type_not_compatible-msgid ).

    cl_abap_unit_assert=>assert_equals( act = sy-msgno
                                        exp = zcx_adu_messages=>data_type_not_compatible-msgno ).

  ENDMETHOD.


ENDCLASS.
