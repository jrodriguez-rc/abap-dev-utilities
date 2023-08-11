"! <p class="shorttext synchronized" lang="en">Run ABAP Test Cockpit checks</p>
CLASS zcl_adu_run_atc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_adu_run_atc.

    CLASS-METHODS create
      IMPORTING
        profile_name  TYPE csequence OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zif_adu_run_atc.

    METHODS constructor
      IMPORTING
        profile_name TYPE csequence OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      profile_name_of_checks TYPE satc_ci_chk_variant.

    METHODS get_default_variant
      RETURNING
        VALUE(result) TYPE satc_ci_chk_variant.

    METHODS build_project
      IMPORTING
        object_keys   TYPE satc_t_r3tr_keys
      RETURNING
        VALUE(result) TYPE REF TO if_satc_md_project.

    METHODS execute_atc_project
      IMPORTING
        atc_project   TYPE REF TO if_satc_md_project
      RETURNING
        VALUE(result) TYPE satc_d_id.

    METHODS retrieve_findings
      IMPORTING
        execution_id  TYPE satc_d_id
      RETURNING
        VALUE(result) TYPE zif_adu_run_atc=>ts_result
      RAISING
        cx_adt_rest.

    METHODS retrieve_findings_ge_754
      IMPORTING
        iv_execution_id  TYPE satc_d_id
        ii_access        TYPE REF TO if_satc_adt_result_read_access
      RETURNING
        VALUE(rs_result) TYPE zif_adu_run_atc=>ts_result
      RAISING
        cx_adt_rest.

    METHODS get_standard_check_ids
      EXPORTING
        check_profile TYPE satc_d_ac_chk_profile_name
        check_ids     TYPE satc_t_ids.

    METHODS generate_project_title
      IMPORTING
        object_keys   TYPE satc_t_r3tr_keys
      RETURNING
        VALUE(result) TYPE satc_d_description.

ENDCLASS.



CLASS zcl_adu_run_atc IMPLEMENTATION.

  METHOD create.

    result = NEW zcl_adu_run_atc( profile_name ).

  ENDMETHOD.


  METHOD constructor.

    profile_name_of_checks =
        COND #(
            WHEN profile_name IS NOT INITIAL
                THEN profile_name
                ELSE get_default_variant( ) ).

  ENDMETHOD.


  METHOD zif_adu_run_atc~check.

    DATA(atc_project) = build_project( object_keys ).

    DATA(execution_id) = execute_atc_project( atc_project ).

    result = retrieve_findings( execution_id ).

  ENDMETHOD.


  METHOD get_default_variant.

    TRY.
        DATA(atc_config) = CAST if_satc_ac_config_ci( cl_satc_ac_config_factory=>get_read_access( ) ).
        atc_config->get_ci_check_variant( IMPORTING e_name = result ).
      CATCH cx_satc_root cx_sy_move_cast_error.
        CLEAR: result.
    ENDTRY.

    IF result IS INITIAL.
      result = 'DEFAULT'.
    ENDIF.

  ENDMETHOD.


  METHOD build_project.

    DATA(key_iterator) = NEW cl_satc_ac_iterate_fixed_keys( ).
    key_iterator->set_object_keys( object_keys ).

    DATA(title) = generate_project_title( object_keys ).

    DATA(project_parameters) = NEW cl_satc_ac_project_parameters( ).

    project_parameters->set_project_title( title ).
    project_parameters->set_is_transient( abap_false ).
    project_parameters->set_check_profile_name( profile_name_of_checks ).
    project_parameters->set_evaluate_exemptions( abap_true ).
    project_parameters->set_evaluate_runtime_error( abap_true ).
    project_parameters->set_object_key_iterator( key_iterator ).

    result = cl_satc_ac_project_builder=>create_builder( )->create_project( project_parameters ).

  ENDMETHOD.


  METHOD execute_atc_project.

    DATA:
      success TYPE abap_bool.

    TRY.
        CALL FUNCTION 'SATC_EXECUTE_PROJECT'
          EXPORTING
            i_project = atc_project
          IMPORTING
            e_exec_id = result
            e_success = success.
        IF success IS INITIAL.
          CLEAR result.
        ENDIF.

      CATCH cx_satc_root.
        CLEAR result.
    ENDTRY.

  ENDMETHOD.


  METHOD retrieve_findings.

    DATA:
      lv_display_id TYPE satc_d_id.

    DATA(li_access) = cl_satc_adt_result_read_access=>create( cl_satc_adt_result_reader=>create( ) ).

    TRY.

        CALL METHOD li_access->('READ_DISPLAY_ID_4_EXECUTION_ID')
          EXPORTING
            i_execution_id = execution_id
          IMPORTING
            e_display_id   = lv_display_id.

        CALL METHOD li_access->('READ_FINDINGS')
          EXPORTING
            i_display_id = lv_display_id
          IMPORTING
            e_findings   = result-findings.

        CALL METHOD li_access->read_metrics_4_id
          EXPORTING
            i_display_id          = lv_display_id
          IMPORTING
            e_has_caused_abortion = result-has_caused_abortion.

      CATCH cx_root.
        result = retrieve_findings_ge_754( iv_execution_id = execution_id ii_access = li_access ).
    ENDTRY.

  ENDMETHOD.


  METHOD retrieve_findings_ge_754.

    DATA:
      lr_atc_result TYPE REF TO data.

    FIELD-SYMBOLS:
      <lt_veredicts>  LIKE rs_result-findings.

    CREATE DATA lr_atc_result TYPE ('CL_SATC_ADT_CH_FACTORY=>TY_ADT_ATC_RESULT').
    IF sy-subrc <> 0 OR lr_atc_result IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN lr_atc_result->* TO FIELD-SYMBOL(<ls_atc_result>).
    IF sy-subrc <> 0 OR <ls_atc_result> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    DO 3 TIMES.

      DATA(lv_uri_type) = SWITCH char1( sy-index WHEN 1 THEN cl_satc_adt_deferred_uri_mappr=>co_uri_type-adt
                                                 WHEN 2 THEN cl_satc_adt_deferred_uri_mappr=>co_uri_type-http
                                                 WHEN 3 THEN cl_satc_adt_deferred_uri_mappr=>co_uri_type-none ).

      CALL METHOD ii_access->('READ_FINDINGS')
        EXPORTING
          i_display_id = iv_execution_id
          i_uri_type   = cl_satc_adt_deferred_uri_mappr=>co_uri_type-none
        IMPORTING
          e_atc_result = <ls_atc_result>.

      ASSIGN COMPONENT 'VERDICTS' OF STRUCTURE <ls_atc_result> TO <lt_veredicts>.
      IF sy-subrc = 0 AND <lt_veredicts> IS ASSIGNED.
        INSERT LINES OF <lt_veredicts> INTO TABLE rs_result-findings.
        UNASSIGN <lt_veredicts>.
      ENDIF.

    ENDDO.

    CALL METHOD ii_access->read_metrics_4_id
      EXPORTING
        i_display_id          = iv_execution_id
      IMPORTING
        e_has_caused_abortion = rs_result-has_caused_abortion.

  ENDMETHOD.


  METHOD get_standard_check_ids.

    CLEAR: check_ids, check_profile.

    TRY.
        DATA(atc_config) = cl_satc_ac_config_cm_factory=>get_access_to_atc_config( ).
        atc_config->get_standard_profile( IMPORTING e_name = DATA(cm_profile) ).
      CATCH cx_satc_root.
        cm_profile = 'STANDARD'.
    ENDTRY.
    check_profile = cm_profile.

    DATA(check_queries) = NEW cl_satc_checkman_queries( ).
    DATA(checks) = check_queries->get_checks_of_profile( cm_profile ).

    DELETE checks WHERE dvlpr_scope = if_satc_ac_check_attributes=>scope-never. "#EC CI_SORTSEQ

    check_ids =
        VALUE #(
            FOR check IN checks
            ( check-atc_id ) ).

  ENDMETHOD.


  METHOD generate_project_title.

    IF lines( object_keys ) = 1.
      result = object_keys[ 1 ]-obj_name.
      RETURN.
    ENDIF.

    DATA(r3tr_keys) = object_keys.

    SORT r3tr_keys BY obj_name.

    DATA(title_result) =
        REDUCE #(
            INIT title TYPE string
            FOR r3tr_key IN r3tr_keys
            NEXT title =
                COND #(
                    WHEN title IS INITIAL
                        THEN r3tr_key-obj_name
                        ELSE |{ title }, { r3tr_key-obj_name }| ) ).

    result =
        COND #(
            WHEN strlen( title_result ) <= 128
                THEN title_result
                ELSE |{ title_result(125) }...| ).

  ENDMETHOD.


ENDCLASS.
