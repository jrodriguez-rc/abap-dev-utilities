FUNCTION zadu_transport_update_crossref.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CROSS_REFERENCE_UPDATES) TYPE  ZADU_T_CHKTR_CRREF_UPDATE
*"----------------------------------------------------------------------

  DATA: database_data TYPE zadu_chktr_crref.

  LOOP AT cross_reference_updates REFERENCE INTO DATA(cross_reference_update).

    database_data = cross_reference_update->*.

    CASE cross_reference_update->crud_ind.
      WHEN zif_adu_constants=>crud-create.
        INSERT zadu_chktr_crref FROM database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_insert_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_insert_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_CRREF'.
        ENDIF.

      WHEN zif_adu_constants=>crud-update.
        UPDATE zadu_chktr_crref FROM database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_update_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_update_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_CRREF'.
        ENDIF.

      WHEN zif_adu_constants=>crud-delete.
        DELETE zadu_chktr_crref FROM database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_delete_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_delete_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_CRREF'.
        ENDIF.

    ENDCASE.

  ENDLOOP.

ENDFUNCTION.
