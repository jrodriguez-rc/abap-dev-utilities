FUNCTION zadu_transport_update_onl_imp.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ONLINE_IMPORT_UPDATES) TYPE  ZADU_T_CHKTR_ONLIM_UPDATE
*"----------------------------------------------------------------------

  DATA: database_data TYPE zadu_chktr_onlim.

  LOOP AT online_import_updates REFERENCE INTO DATA(online_import_update).

    database_data = online_import_update->*.

    CASE online_import_update->crud_ind.
      WHEN zif_adu_constants=>crud-create.
        INSERT zadu_chktr_onlim FROM database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_insert_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_insert_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_ONLIM'.
        ENDIF.

      WHEN zif_adu_constants=>crud-update.
        UPDATE zadu_chktr_onlim FROM database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_update_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_update_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_ONLIM'.
        ENDIF.

      WHEN zif_adu_constants=>crud-delete.
        DELETE zadu_chktr_onlim FROM database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_delete_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_delete_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_ONLIM'.
        ENDIF.

    ENDCASE.

  ENDLOOP.

ENDFUNCTION.
