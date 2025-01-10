FUNCTION z_adu_log_display_content.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      I_T_PARAMS STRUCTURE  SPAR
*"----------------------------------------------------------------------

  DATA(lv_content_type) = VALUE #( i_t_params[ param = zif_adu_log=>gc_parameter-content_type ]-value OPTIONAL ).

  DATA(lt_parameters) = i_t_params[].

  DELETE lt_parameters WHERE param <> zif_adu_log=>gc_parameter-content.

  IF lt_parameters IS INITIAL.
    RETURN.
  ENDIF.

  DATA(lv_base64) = REDUCE #( INIT base64 TYPE string
                              FOR <parameter> IN lt_parameters
                              NEXT base64 = |{ base64 }{ <parameter>-value }| ).

  DATA(lv_content) = cl_http_utility=>decode_base64( lv_base64 ).

  CASE lv_content_type.
    WHEN if_rest_media_type=>gc_appl_json.
      CALL TRANSFORMATION sjson2html
           SOURCE XML lv_content
           RESULT XML DATA(lv_json_html).

      cl_abap_browser=>show_html( html_string = cl_abap_codepage=>convert_from( lv_json_html ) ).

    WHEN if_rest_media_type=>gc_appl_xml.
      cl_abap_browser=>show_xml( lv_content ).

    WHEN OTHERS.
      cl_demo_output=>display( data = lv_content ).

  ENDCASE.

ENDFUNCTION.
