FUNCTION zadu_transport_update_imp_time.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IMPORT_TIME_UPDATES) TYPE  ZADU_T_CHKTR_IMTIM_UPDATE
*"----------------------------------------------------------------------

  DATA: database_data TYPE zadu_chktr_imtim.

  LOOP AT import_time_updates REFERENCE INTO DATA(import_time_update).

    database_data = import_time_update->*.

    CASE import_time_update->crud_ind.
      WHEN zif_adu_constants=>crud-create.
        INSERT zadu_chktr_imtim FROM @database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_insert_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_insert_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_IMTIM'.
        ENDIF.

      WHEN zif_adu_constants=>crud-update.
        UPDATE zadu_chktr_imtim FROM @database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_update_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_update_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_IMTIM'.
        ENDIF.

      WHEN zif_adu_constants=>crud-delete.
        DELETE zadu_chktr_imtim FROM @database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_delete_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_delete_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_IMTIM'.
        ENDIF.

    ENDCASE.

  ENDLOOP.

ENDFUNCTION.
