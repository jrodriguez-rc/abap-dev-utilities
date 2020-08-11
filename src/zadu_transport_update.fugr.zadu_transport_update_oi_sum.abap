FUNCTION zadu_transport_update_oi_sum.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ONLINE_IMPORT_SUMMARY_UPDATES) TYPE
*"        ZADU_T_CHKTR_OISUM_UPDATE
*"----------------------------------------------------------------------

  DATA: database_data TYPE zadu_chktr_oisum.

  LOOP AT online_import_summary_updates REFERENCE INTO DATA(online_import_summary_update).

    database_data = online_import_summary_update->*.

    CASE online_import_summary_update->crud_ind.
      WHEN zif_adu_constants=>crud-create.
        INSERT zadu_chktr_oisum FROM @database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_insert_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_insert_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_OISUM'.
        ENDIF.

      WHEN zif_adu_constants=>crud-update.
        UPDATE zadu_chktr_oisum FROM @database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_update_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_update_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_OISUM'.
        ENDIF.

      WHEN zif_adu_constants=>crud-delete.
        DELETE zadu_chktr_oisum FROM @database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_delete_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_delete_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_OISUM'.
        ENDIF.

    ENDCASE.

  ENDLOOP.

ENDFUNCTION.
