REPORT zca_zcx_return2_patterns MESSAGE-ID 00.

CLASS zz_zcx_return2_pattern DEFINITION.

  PUBLIC SECTION.

    METHODS z010_zexc_abap_message    RAISING zcx_return2.
    METHODS z020_zexc_cust_message    RAISING zcx_return2.
    METHODS z030_zexc_bapireturn_stru RAISING zcx_return2.
    METHODS z040_zexc_bapireturn_tab  RAISING zcx_return2.
    METHODS z050_zexc_bapiret1_struc  RAISING zcx_return2.
    METHODS z060_zexc_bapiret1_tab    RAISING zcx_return2.
    METHODS z070_zexc_bapiret2_struc  RAISING zcx_return2.
    METHODS z080_zexc_bapiret2_tab    RAISING zcx_return2.
    METHODS z085_zexc_bdc_mess_table  RAISING zcx_return2.
    METHODS z090_zexc_oo_exception    RAISING zcx_return2.
    METHODS z090_zexc_text_do_not_use RAISING zcx_return2.

    METHODS c010_zexc_catch_oo        RAISING zcx_return2.
    METHODS c020_zexc_catch_gateway   RAISING /iwbep/cx_mgw_busi_exception.
    METHODS c025_zexc_catch_abap_prox.
    METHODS c030_zexc_catch_rfc       EXCEPTIONS error.
    METHODS c040_zexc_catch_bapi
      EXPORTING et_return TYPE bapiret2_t.
    METHODS c060_zexc_catch_message.
    METHODS c999_zexc_catch_old_retur RAISING zcx_return2.

ENDCLASS.

CLASS zz_zcx_return2_pattern IMPLEMENTATION.

  METHOD z010_zexc_abap_message.

    IF sy-subrc <> 0.
      DATA(lx_return) = zcx_return2=>create_by_system_message( ).
      RAISE EXCEPTION lx_return.
    ENDIF.

  ENDMETHOD.

  METHOD z020_zexc_cust_message.

    IF sy-subrc <> 0.

      "<English message text>
      MESSAGE e001
        "WITH iv_<variable>
        INTO DATA(lv_dummy) ##NEEDED.

      DATA(lx_return) = zcx_return2=>create_by_system_message( ).
      RAISE EXCEPTION lx_return.

    ENDIF.

  ENDMETHOD.

  METHOD z030_zexc_bapireturn_stru.

    DATA ls_return TYPE bapireturn.

    "...

    DATA(lx_return) =
      zcx_return2=>create_by_bapireturn_struc(
        is_return = ls_return ).

    IF lx_return IS NOT INITIAL.
      RAISE EXCEPTION lx_return.
    ENDIF.

  ENDMETHOD.

  METHOD z040_zexc_bapireturn_tab.

    DATA lt_return TYPE zcx_return2=>gtt_bapireturn_t.

    "...

    DATA(lx_return) =
      zcx_return2=>create_by_bapireturn_table(
        it_return = lt_return ).

    IF lx_return IS NOT INITIAL.
      RAISE EXCEPTION lx_return.
    ENDIF.

  ENDMETHOD.

  METHOD z050_zexc_bapiret1_struc.

    DATA ls_return TYPE bapiret1.

    "...

    DATA(lx_return) =
      zcx_return2=>create_by_bapiret1_struc(
        is_return = ls_return ).

    IF lx_return IS NOT INITIAL.
      RAISE EXCEPTION lx_return.
    ENDIF.

  ENDMETHOD.

  METHOD z060_zexc_bapiret1_tab.

    DATA:
      lt_return           TYPE bapiret1_tab.

    "...

    DATA(lx_return) =
      zcx_return2=>create_by_bapiret1_table(
        it_return = lt_return ).

    IF lx_return IS NOT INITIAL.
      RAISE EXCEPTION lx_return.
    ENDIF.

  ENDMETHOD.

  METHOD z070_zexc_bapiret2_struc.

    DATA ls_return TYPE bapiret2.

    "...

    DATA(lx_return) =
      zcx_return2=>create_by_bapiret2_struc(
        is_return = ls_return ).

    IF lx_return IS NOT INITIAL.
      RAISE EXCEPTION lx_return.
    ENDIF.

  ENDMETHOD.

  METHOD z080_zexc_bapiret2_tab.

    DATA:
      lt_return TYPE STANDARD TABLE OF bapiret2.

    "...

    DATA(lx_return) =
      zcx_return2=>create_by_bapiret2_table(
        it_return = lt_return ).

    IF lx_return IS NOT INITIAL.
      RAISE EXCEPTION lx_return.
    ENDIF.

  ENDMETHOD.

  METHOD z085_zexc_bdc_mess_table.

    DATA lt_bdc_messages  TYPE zcx_return2=>gtt_bdc_messages.

*    CALL TRANSACTION '???'
*      USING lt_bdcdata
*      MODE '?'
*      UPDATE '?'
*      MESSAGES INTO lt_bdc_messages.

    DATA(lx_return) =
      zcx_return2=>create_by_bdc_table(
        it_bdc_messages = lt_bdc_messages ).

    IF lx_return IS NOT INITIAL.
      RAISE EXCEPTION lx_return.
    ENDIF.

  ENDMETHOD.

  METHOD z090_zexc_oo_exception.

    TRY.

        "...

      CATCH cx_root INTO DATA(lx_root). "TODO: change exception class

        "<Short error name>: &1&2&3&4
        MESSAGE e001                    "Todo: change error number
          INTO DATA(lv_dummy).

        "Method will split message text into &1 &2 &3 &4
        DATA(lr_return) =
          zcx_return2=>create_by_exception_object( lx_root ). "Change variable name

        RAISE EXCEPTION lr_return.

    ENDTRY.

  ENDMETHOD.

  METHOD z090_zexc_text_do_not_use.

    DATA(lr_return) =
      zcx_return2=>create_by_text(
        iv_type        = 'E'
        iv_message     = 'Do not used &1 &2 &3 and &4'
        iv_variable_1  = 'A'
        iv_variable_2  = 'B'
        iv_variable_3  = 'C'
        iv_variable_4  = 'D'
       ).

    RAISE EXCEPTION lr_return.

  ENDMETHOD.

  METHOD c010_zexc_catch_oo.

    TRY.

        "Execute methods
        "...

        "Commit
        "zca_db_transaction->commit( ).

      CATCH zcx_return2 INTO DATA(lx_return).

        "zca_db_transaction->roll_back( ).

        "Handle exception lx_return
        RAISE EXCEPTION lx_return.

    ENDTRY.

  ENDMETHOD.

  METHOD c020_zexc_catch_gateway.

    TRY.

        "...instiantiate object and call methods

        "Commit work (if needed)
        "zca_db_transaction->commit( ).

      CATCH zcx_return2 INTO DATA(lx_return).

        "Roll back (if needed)
        "zca_db_transaction->rollback( ).

        "Handle exception
        DATA(ls_return) = lx_return->get_bapiret2_struc( ).

        DATA(ls_message) = VALUE scx_t100key(
          msgid = ls_return-id
          msgno = ls_return-number
          attr1 = ls_return-message_v1
          attr2 = ls_return-message_v2
          attr3 = ls_return-message_v3
          attr4 = ls_return-message_v4 ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
            textid = ls_message.

    ENDTRY.

  ENDMETHOD.

  METHOD c030_zexc_catch_rfc.

    TRY.

        "...instiantiate object and call methods

        "Commit work (if needed)
        "zca_db_transaction->commit( ).

      CATCH zcx_return2 INTO DATA(lx_return).

        "Roll back (if needed)
        "zca_db_transaction->rollback( ).

        DATA(ls_return) = lx_return->get_bapiret2_struc( ).

        MESSAGE
          ID      ls_return-id
          TYPE    ls_return-type
          NUMBER  ls_return-number
          RAISING error
          WITH    ls_return-message_v1
                  ls_return-message_v2
                  ls_return-message_v3
                  ls_return-message_v4.

    ENDTRY.

  ENDMETHOD.

  METHOD c040_zexc_catch_bapi.

    TRY.

        "...

        "Commit work (if needed)
        "zca_db_transaction->commit( ).

      CATCH zcx_return2 INTO DATA(lx_return).

        "Roll back (if needed)
        "zca_db_transaction->rollback( ).

        "Fill return table
        et_return = lx_return->get_bapiret2_table( ).

    ENDTRY.

  ENDMETHOD.

  METHOD c025_zexc_catch_abap_prox.

    TRY.

        "...

        "Commit work (if needed)
        "zca_db_transaction->commit( ).

      CATCH zcx_return2 INTO DATA(lx_return).

        "Roll back (if needed)
        "zca_db_transaction->rollback( ).

        "Fill proxy exception message
        DATA(lt_bapiret2) = lx_return->get_bapiret2_table( ).

        cl_proxy_fault=>raise(
          EXPORTING
            exception_class_name = 'ZWMSCX_EXCHANGE_FAULT3'  "Change
            bapireturn_tab       = lt_bapiret2 ).

    ENDTRY.

  ENDMETHOD.

  METHOD c060_zexc_catch_message.

    TRY.

        "...

      CATCH zcx_return2 INTO DATA(lx_return).

        DATA(ls_return) = lx_return->get_bapiret2_struc( ).

        MESSAGE
          ID      ls_return-id
          TYPE    ls_return-type
          NUMBER  ls_return-number
          WITH    ls_return-message_v1
                  ls_return-message_v2
                  ls_return-message_v3
                  ls_return-message_v4.

    ENDTRY.

  ENDMETHOD.

  METHOD c999_zexc_catch_old_retur.

    TRY.

        "...

      CATCH zcx_return2 INTO DATA(lx_return). "TODO: rename zcx_return2

        DATA(lx_return2) = zcx_return2=>create_by_bapiret2_table( lx_return->get_bapiret2_table( )  ).

        RAISE EXCEPTION lx_return2.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
