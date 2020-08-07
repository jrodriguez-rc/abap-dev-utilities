FUNCTION zadu_transport_update_crossrel.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CROSS_RELEASE_UPDATES) TYPE  ZADU_T_CHKTR_CRREL_UPDATE
*"----------------------------------------------------------------------

  DATA: database_data TYPE zadu_chktr_crrel.

  LOOP AT cross_release_updates REFERENCE INTO DATA(cross_release_update).

    database_data = cross_release_update->*.

    CASE cross_release_update->crud_ind.
      WHEN zif_adu_constants=>crud-create.
        INSERT zadu_chktr_crrel FROM @database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_insert_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_insert_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_CRREL'.
        ENDIF.

      WHEN zif_adu_constants=>crud-update.
        UPDATE zadu_chktr_crrel FROM @database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_update_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_update_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_CRREL'.
        ENDIF.

      WHEN zif_adu_constants=>crud-delete.
        DELETE zadu_chktr_crrel FROM @database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_delete_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_delete_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_CRREL'.
        ENDIF.

    ENDCASE.

  ENDLOOP.

ENDFUNCTION.
