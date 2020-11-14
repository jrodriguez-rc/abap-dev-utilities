"! <p class="shorttext synchronized" lang="en">Run ABAP Test Cockpit checks</p>
CLASS zcl_adu_run_atc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_adu_run_atc.

    "! <p class="shorttext synchronized" lang="en">Generate instantiation</p>
    "!
    "! @parameter profile_name | <p class="shorttext synchronized" lang="en">SCI Profile Name</p>
    "! @parameter result | <p class="shorttext synchronized" lang="en">ABAP Test Cockpit Utility</p>
    CLASS-METHODS create
      IMPORTING
        profile_name  TYPE csequence
      RETURNING
        VALUE(result) TYPE REF TO zif_adu_run_atc.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "!
    "! @parameter profile_name | <p class="shorttext synchronized" lang="en">Variant profile name</p>
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

    DATA(access) = cl_satc_adt_result_read_access=>create( cl_satc_adt_result_reader=>create( ) ).

    access->read_display_id_4_execution_id( EXPORTING i_execution_id = execution_id
                                            IMPORTING e_display_id   = DATA(display_id) ).

    access->read_findings( EXPORTING i_display_id = display_id
                           IMPORTING e_findings   = result-findings ).

    access->read_metrics_4_id( EXPORTING i_display_id          = display_id
                               IMPORTING e_has_caused_abortion = result-has_caused_abortion ).

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
