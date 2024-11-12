CLASS ltcl_files DEFINITION DEFERRED.
CLASS zcl_adu_utl_transport_request DEFINITION LOCAL FRIENDS ltcl_files.
CLASS ltcl_files DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA mi_utility TYPE REF TO zcl_adu_utl_transport_request.

    METHODS setup.
    METHODS build_path_cofiles            FOR TESTING.
    METHODS build_path_data               FOR TESTING.
    METHODS build_tr_from_filename_cofile FOR TESTING RAISING zcx_adu_transport_request.
    METHODS build_tr_from_filename_data   FOR TESTING RAISING zcx_adu_transport_request.
    METHODS build_tr_from_filename_error  FOR TESTING.
    METHODS filename_valid                FOR TESTING.
    METHODS filename_invalid              FOR TESTING.

ENDCLASS.



CLASS ltcl_files IMPLEMENTATION.


  METHOD setup.

    mi_utility = NEW #( ).

  ENDMETHOD.


  METHOD build_path_cofiles.

    CONSTANTS lc_transport_request TYPE trkorr VALUE 'A4HK9000000'.
    CONSTANTS lc_path_expected     TYPE string VALUE `/usr/sap/trans/cofiles/K9000000.A4H`.

    DATA(path) = mi_utility->zif_adu_utl_transport_request~build_path_cofiles( lc_transport_request ).

    cl_abap_unit_assert=>assert_equals( exp = lc_path_expected
                                        act = path ).

  ENDMETHOD.


  METHOD build_path_data.

    CONSTANTS lc_transport_request TYPE trkorr VALUE 'A4HK9000000'.
    CONSTANTS lc_path_expected     TYPE string VALUE `/usr/sap/trans/data/R9000000.A4H`.

    DATA(path) = mi_utility->zif_adu_utl_transport_request~build_path_data( lc_transport_request ).

    cl_abap_unit_assert=>assert_equals( exp = lc_path_expected
                                        act = path ).

  ENDMETHOD.


  METHOD build_tr_from_filename_cofile.

    CONSTANTS lc_transport_request TYPE trkorr VALUE 'A4HK9000000'.
    CONSTANTS lc_file_cofile       TYPE string VALUE `K9000000.A4H`.

    DATA(lv_transport_request) = mi_utility->zif_adu_utl_transport_request~convert_filename_to_tr( lc_file_cofile ).

    cl_abap_unit_assert=>assert_equals( exp = lc_transport_request
                                        act = lv_transport_request ).

  ENDMETHOD.


  METHOD build_tr_from_filename_data.

    CONSTANTS lc_transport_request TYPE trkorr VALUE 'A4HK9000000'.
    CONSTANTS lc_file_data         TYPE string VALUE `R9000000.A4H`.

    DATA(lv_transport_request) = mi_utility->zif_adu_utl_transport_request~convert_filename_to_tr( lc_file_data ).

    cl_abap_unit_assert=>assert_equals( exp = lc_transport_request
                                        act = lv_transport_request ).

  ENDMETHOD.


  METHOD build_tr_from_filename_error.

    CONSTANTS lc_file_not_supported TYPE string VALUE `SAP.EXE`.

    TRY.
        DATA(lv_transport_request) = mi_utility->zif_adu_utl_transport_request~convert_filename_to_tr( lc_file_not_supported ).
      CATCH zcx_adu_transport_request INTO DATA(lx_transport_request).
    ENDTRY.

    cl_abap_unit_assert=>assert_initial( lv_transport_request ).
    cl_abap_unit_assert=>assert_bound( lx_transport_request ).

  ENDMETHOD.


  METHOD filename_valid.

    cl_abap_unit_assert=>assert_true( mi_utility->zif_adu_utl_transport_request~is_filename_valid( `K9000000.A4H` ) ).
    cl_abap_unit_assert=>assert_true( mi_utility->zif_adu_utl_transport_request~is_filename_valid( `R9000000.A4H` ) ).

  ENDMETHOD.


  METHOD filename_invalid.

    cl_abap_unit_assert=>assert_false( mi_utility->zif_adu_utl_transport_request~is_filename_valid( `X9000000.A4H` ) ).
    cl_abap_unit_assert=>assert_false( mi_utility->zif_adu_utl_transport_request~is_filename_valid( `R90000000.A4H` ) ).
    cl_abap_unit_assert=>assert_false( mi_utility->zif_adu_utl_transport_request~is_filename_valid( `SAP.A4H` ) ).

    cl_abap_unit_assert=>assert_false( mi_utility->zif_adu_utl_transport_request~is_filename_valid( `A4HK90000000` ) ).
    cl_abap_unit_assert=>assert_false( mi_utility->zif_adu_utl_transport_request~is_filename_valid( `A4HX9000000` ) ).
    cl_abap_unit_assert=>assert_false( mi_utility->zif_adu_utl_transport_request~is_filename_valid( `SAP.EXE` ) ).

  ENDMETHOD.


ENDCLASS.
