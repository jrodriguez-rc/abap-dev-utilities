FUNCTION zadu_transport_update_sequence.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(SEQUENCE_UPDATES) TYPE  ZADU_T_CHKTR_SEQ_UPDATE
*"----------------------------------------------------------------------

  DATA: database_data TYPE zadu_chktr_seq.

  LOOP AT sequence_updates REFERENCE INTO DATA(sequence_update).

    database_data = sequence_update->*.

    CASE sequence_update->crud_ind.
      WHEN zif_adu_constants=>crud-create.
        INSERT zadu_chktr_seq FROM @database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_insert_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_insert_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_SEQ'.
        ENDIF.

      WHEN zif_adu_constants=>crud-update.
        UPDATE zadu_chktr_seq FROM @database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_update_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_update_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_SEQ'.
        ENDIF.

      WHEN zif_adu_constants=>crud-delete.
        DELETE zadu_chktr_seq FROM @database_data.
        IF sy-subrc <> 0.
          MESSAGE ID zcx_adu_check_transport=>error_delete_table-msgid
                  TYPE 'X'
                  NUMBER zcx_adu_check_transport=>error_delete_table-msgno
                  WITH |{ database_data-run_code }{ database_data-sequence }| 'ZADU_CHKTR_SEQ'.
        ENDIF.

    ENDCASE.

  ENDLOOP.

ENDFUNCTION.
