REPORT ZCA_ZCX_RETURN2_PATTERNS MESSAGE-ID 00.

*    METHODS sc010_create_system_message      FOR TESTING.
*    METHODS sc020_create_system_message_2    FOR TESTING.
*    METHODS sc030_create_mess_and_text_var   FOR TESTING.
*    METHODS sc040_create_bapireturn_struc    FOR TESTING.
*    METHODS sc050_create_bapireturn_table    FOR TESTING.
*    METHODS sc060_create_by_bapiret1_struc   FOR TESTING.
*    METHODS sc070_create_by_bapiret1_table   FOR TESTING.
*    METHODS sc080_create_by_bapiret2_struc   FOR TESTING.
*    METHODS sc090_create_by_bapiret2_table   FOR TESTING.
*    METHODS sc091_create_by_bapiret2_table   FOR TESTING.

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
    METHODS z090_zexc_oo_exception    RAISING zcx_return2.

    METHODS c010_zexc_db_transaction  RAISING zcx_return2.
    METHODS c020_zexc_catch_gateway   RAISING /iwbep/cx_mgw_busi_exception.
    METHODS c030_zexc_catch_rfc       EXCEPTIONS error.
    METHODS c040_zexc_catch_bapi
      EXPORTING et_return TYPE bapiret2_t.
    METHODS c050_zexc_catch_abap_prox.
    METHODS c060_zexc_catch_message.
    METHODS c999_zexc_catch_old_retur.

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
      MESSAGE e000
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

  METHOD z090_zexc_oo_exception.

    TRY.

        "...

      CATCH cx_root INTO DATA(lx_root). "TODO: change exception class

        "<Short error name>: &1 &2 &3 &4
        MESSAGE e001                    "Todo: change error number
          INTO DATA(lv_dummy).

        "Method will split message text into &1 &2 &3 &4
        DATA(lr_return) =
          zcx_return2=>create_by_exception_object( lx_root ). "Change variable name

        RAISE EXCEPTION lr_return.

    ENDTRY.

  ENDMETHOD.

  METHOD c010_zexc_db_transaction.

    TRY.

        "Execute Business Object CRUD methods
        "...

        "Commit
        "zca_db_transaction->commit( ).

      CATCH zcx_return2 INTO DATA(lx_return).

        "zca_db_transaction->roll_back( ).

        "Handle exception lx_return
        ...

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

        DATA(ls_message) = VALUE scx_t100key(   "TODO: is this okay?
          msgid = ls_return-id
          msgno = ls_return-number
          attr1 = ls_return-message
          attr2 = ''
          attr3 = ''
          attr4 = '' ).

        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception "TODO: change
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

  METHOD c050_zexc_catch_abap_prox.

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

        "DATA(lt_bapiret2) = lx_return->get_bapiret2_t( ).

        "DATA(lt_bapiret2) = lx_return->gt_return.            "TODO: activate

        DATA(lt_bapiret2) = lx_return->get_bapiret2_table( ). "TODO: delete

        DATA(lx_return2) = zcx_return2=>create_by_bapiret2_table( lt_bapiret2  ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.