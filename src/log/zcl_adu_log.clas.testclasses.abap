CLASS ltcl_problem_class_severity DEFINITION DEFERRED.
CLASS zcl_adu_log DEFINITION LOCAL FRIENDS ltcl_problem_class_severity.
CLASS ltcl_problem_class_severity DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    METHODS filter_0_severity_0 FOR TESTING.
    METHODS filter_0_severity_1 FOR TESTING.
    METHODS filter_0_severity_2 FOR TESTING.
    METHODS filter_0_severity_3 FOR TESTING.
    METHODS filter_0_severity_4 FOR TESTING.

    METHODS filter_1_severity_0 FOR TESTING.
    METHODS filter_1_severity_1 FOR TESTING.
    METHODS filter_1_severity_2 FOR TESTING.
    METHODS filter_1_severity_3 FOR TESTING.
    METHODS filter_1_severity_4 FOR TESTING.

    METHODS filter_2_severity_0 FOR TESTING.
    METHODS filter_2_severity_1 FOR TESTING.
    METHODS filter_2_severity_2 FOR TESTING.
    METHODS filter_2_severity_3 FOR TESTING.
    METHODS filter_2_severity_4 FOR TESTING.

    METHODS filter_3_severity_0 FOR TESTING.
    METHODS filter_3_severity_1 FOR TESTING.
    METHODS filter_3_severity_2 FOR TESTING.
    METHODS filter_3_severity_3 FOR TESTING.
    METHODS filter_3_severity_4 FOR TESTING.

    METHODS filter_4_severity_0 FOR TESTING.
    METHODS filter_4_severity_1 FOR TESTING.
    METHODS filter_4_severity_2 FOR TESTING.
    METHODS filter_4_severity_3 FOR TESTING.
    METHODS filter_4_severity_4 FOR TESTING.

ENDCLASS.



CLASS ltcl_problem_class_severity IMPLEMENTATION.


  METHOD filter_0_severity_0.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE ''.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE ''.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_true( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_0_severity_1.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE ''.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '1'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_true( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_0_severity_2.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE ''.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '2'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_true( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_0_severity_3.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE ''.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '3'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_true( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_0_severity_4.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE ''.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '4'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_true( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_1_severity_0.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '1'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE ''.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_false( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_1_severity_1.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '1'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '1'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_true( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_1_severity_2.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '1'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '2'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_true( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_1_severity_3.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '1'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '3'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_true( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_1_severity_4.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '1'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '4'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_true( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_2_severity_0.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '2'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE ''.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_false( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_2_severity_1.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '2'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '1'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_false( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_2_severity_2.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '2'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '2'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_true( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_2_severity_3.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '2'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '3'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_true( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_2_severity_4.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '2'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '4'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_true( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_3_severity_0.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '3'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE ''.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_false( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_3_severity_1.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '3'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '1'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_false( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_3_severity_2.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '3'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '2'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_false( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_3_severity_3.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '3'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '3'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_true( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_3_severity_4.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '3'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '4'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_true( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_4_severity_0.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '4'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE ''.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_false( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_4_severity_1.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '4'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '1'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_false( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_4_severity_2.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '4'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '2'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_false( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_4_severity_3.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '4'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '3'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_false( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


  METHOD filter_4_severity_4.

    CONSTANTS lc_lower_problem_class TYPE balprobcl VALUE '4'.
    CONSTANTS lc_current_problem_class TYPE balprobcl VALUE '4'.

    DATA(lo_log) = NEW zcl_adu_log( iv_object    = ''
                                    iv_subobject = ''
                                    iv_extnumber = '' ).

    lo_log->mv_lower_problem_class = lc_lower_problem_class.

    cl_abap_unit_assert=>assert_true( lo_log->apply_problem_class( lc_current_problem_class ) ).

  ENDMETHOD.


ENDCLASS.
