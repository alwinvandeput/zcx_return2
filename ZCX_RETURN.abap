CLASS zcx_return DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PROTECTED .

  PUBLIC SECTION.

    TYPES:
      gtt_bdc_messages  TYPE STANDARD TABLE OF bdcmsgcoll WITH DEFAULT KEY .
    TYPES:
      gtt_bapireturn TYPE STANDARD TABLE OF bapireturn WITH DEFAULT KEY .
    TYPES:
      BEGIN OF gts_position,
        program_name TYPE syrepid,
        include_name TYPE syrepid,
        source_line  TYPE i,
      END OF gts_position .

    DATA gt_return TYPE bapiret2_t READ-ONLY .
    DATA gs_return TYPE bapiret2 READ-ONLY .
    DATA gs_position TYPE gts_position .

    METHODS constructor
      IMPORTING
        !textid      LIKE textid OPTIONAL
        !previous    LIKE previous OPTIONAL
        !gt_return   TYPE bapiret2_t OPTIONAL
        !gs_return   TYPE bapiret2 OPTIONAL
        !gs_position TYPE gts_position OPTIONAL .
    CLASS-METHODS create_by_system_message
      IMPORTING
        !iv_field_name   TYPE bapi_fld OPTIONAL
      RETURNING
        VALUE(rx_return) TYPE REF TO zcx_return .
    CLASS-METHODS create_by_bapiret2_table
      IMPORTING
        !it_return          TYPE bapiret2_t
        !iv_restartable_ind TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rx_return)    TYPE REF TO zcx_return .
    CLASS-METHODS create_by_bapiret2
      IMPORTING
        !is_return       TYPE bapiret2
      RETURNING
        VALUE(rx_return) TYPE REF TO zcx_return .
    CLASS-METHODS create_by_bapireturn
      IMPORTING
        !is_return       TYPE bapireturn
      RETURNING
        VALUE(rx_return) TYPE REF TO zcx_return .
    CLASS-METHODS create_by_bapireturn_table
      IMPORTING
        !it_return          TYPE gtt_bapireturn
        !iv_restartable_ind TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rx_return)    TYPE REF TO zcx_return .
    CLASS-METHODS create_by_bdc_message_table
      IMPORTING
        !it_bdc_messages    TYPE gtt_bdc_messages
        !iv_restartable_ind TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rx_return)    TYPE REF TO zcx_return .
    CLASS-METHODS create_by_text
      IMPORTING
        !iv_type         TYPE bapi_mtype DEFAULT 'E'
        !iv_message      TYPE bapi_msg
        !iv_variable_1   TYPE symsgv OPTIONAL
        !iv_variable_2   TYPE symsgv OPTIONAL
        !iv_variable_3   TYPE symsgv OPTIONAL
        !iv_variable_4   TYPE symsgv OPTIONAL
      RETURNING
        VALUE(rx_return) TYPE REF TO zcx_return .
    CLASS-METHODS set_long_text_to_system_mess
      IMPORTING
        !iv_text TYPE char200 .
    METHODS add_bapiret2
      IMPORTING
        !is_return TYPE bapiret2 .
    METHODS add_system_message
      IMPORTING
        !iv_field_name TYPE bapi_fld OPTIONAL .
    METHODS add_bapiret2_table
      IMPORTING
        !it_return TYPE bapiret2_t .
    METHODS add_bapireturn_message
      IMPORTING
        !is_bapireturn TYPE bapireturn .
    METHODS add_bapireturn_table
      IMPORTING
        !it_return TYPE gtt_bapireturn .
    METHODS add_by_text
      IMPORTING
        !iv_type       TYPE bapi_mtype DEFAULT 'E'
        !iv_message    TYPE bapi_msg
        !iv_variable_1 TYPE symsgv
        !iv_variable_2 TYPE symsgv
        !iv_variable_3 TYPE symsgv
        !iv_variable_4 TYPE symsgv .
    CLASS-METHODS map_bapireturn_to_bapiret2
      IMPORTING
        !is_bapireturn     TYPE bapireturn
      RETURNING
        VALUE(rs_bapiret2) TYPE bapiret2 .
    CLASS-METHODS map_bdc_mess_to_bapiret2
      IMPORTING
        !is_bdc_message    TYPE bdcmsgcoll
      RETURNING
        VALUE(rs_bapiret2) TYPE bapiret2 .
    METHODS raise_exception
      RAISING
        zcx_return .

    METHODS if_message~get_text
         REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_return IMPLEMENTATION.


  METHOD add_bapiret2.

    IF gs_return IS INITIAL OR
       gs_return-type NA 'EAX'.

      gs_return = is_return.

    ENDIF.

    APPEND is_return TO gt_return.

  ENDMETHOD.


  METHOD add_bapiret2_table.

    "Fill gs_return
    IF gs_return IS INITIAL.

      LOOP AT it_return
        ASSIGNING FIELD-SYMBOL(<ls_return>).

        IF <ls_return>-type CA 'XAE'.

          gs_return = <ls_return>.

          EXIT.

        ENDIF.

      ENDLOOP.

    ENDIF.

    APPEND LINES OF it_return
      TO gt_return.

  ENDMETHOD.


  METHOD add_bapireturn_message.

    DATA(ls_bapiret2) =
      zcx_return=>map_bapireturn_to_bapiret2( is_bapireturn ).

    me->add_bapiret2( ls_bapiret2 ).

  ENDMETHOD.


  METHOD add_bapireturn_table.

    LOOP AT it_return
      ASSIGNING FIELD-SYMBOL(<ls_return>).

      add_bapireturn_message( <ls_return> ).

    ENDLOOP.

  ENDMETHOD.


  METHOD add_by_text.

    "Example:
    "IV_TYPE         = 'E'
    "IV_MESSAGE      = 'Sales order &1 not found.'
    "IV_VARIABLE_1   = '100001
    "IV_VARIABLE_2   = ''
    "IV_VARIABLE_3   = ''
    "IV_VARIABLE_4   = ''

    DATA:
      lv_message TYPE bapi_msg,
      ls_return  TYPE bapiret2.

    lv_message = iv_message.

    DO 4 TIMES.


      DATA lv_placeholder_name TYPE c LENGTH 2.
      DATA lv_variable_name TYPE c LENGTH 15.

      lv_placeholder_name  = '&' && sy-index.

      lv_variable_name  = 'iv_variable_' && sy-index.

      ASSIGN (lv_variable_name)
        TO FIELD-SYMBOL(<lv_variable>).

      REPLACE ALL OCCURRENCES OF lv_placeholder_name
        IN lv_message
        WITH <lv_variable>
        IN CHARACTER MODE.

      DATA lv_return_var_name TYPE c LENGTH 15.

      lv_return_var_name = 'MESSAGE_V' && sy-index.

      ASSIGN COMPONENT lv_return_var_name
        OF STRUCTURE ls_return
        TO FIELD-SYMBOL(<lv_return_variable>).

      <lv_return_variable> = <lv_variable>.

    ENDDO.

    ls_return-type    = iv_type.
    ls_return-message = lv_message.

    add_bapiret2( ls_return ).

  ENDMETHOD.


  METHOD add_system_message.

    DATA:
      ls_return TYPE bapiret2.

    MESSAGE ID sy-msgid
      TYPE sy-msgty
      NUMBER sy-msgno
      WITH
        sy-msgv1
        sy-msgv2
        sy-msgv3
        sy-msgv4
      INTO ls_return-message.

    ls_return-type        = sy-msgty.
    ls_return-id          = sy-msgid.
    ls_return-number      = sy-msgno.

    ls_return-message_v1  = sy-msgv1.
    ls_return-message_v2  = sy-msgv2.
    ls_return-message_v3  = sy-msgv3.
    ls_return-message_v4  = sy-msgv4.

    ls_return-field       = iv_field_name.

    add_bapiret2( ls_return ).

  ENDMETHOD.


  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    me->gt_return = gt_return .
    me->gs_return = gs_return .
    me->gs_position = gs_position .
  ENDMETHOD.


  METHOD create_by_bapiret2.

    IF is_return-type CA 'XAE'.

      rx_return = NEW #( ).

      rx_return->add_bapiret2( is_return ).

    ENDIF.

  ENDMETHOD.


  METHOD create_by_bapiret2_table.

    DATA:
      lv_error_ind  TYPE abap_bool.

    "Has return table an error?
    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<ls_return>).

      IF <ls_return>-type CA 'XAE'.

        lv_error_ind = abap_true.
        EXIT.

      ENDIF.

    ENDLOOP.

    IF lv_error_ind = abap_true.

      IF iv_restartable_ind = abap_false.

        rx_return = NEW #( ).

      ELSE.

        rx_return = NEW zcx_restartable_error( ).

      ENDIF.

      rx_return->add_bapiret2_table( it_return ).

    ENDIF.

  ENDMETHOD.


  METHOD create_by_bapireturn.

    DATA(ls_bapiret2) = map_bapireturn_to_bapiret2( is_return ).

    rx_return = create_by_bapiret2( ls_bapiret2 ).

  ENDMETHOD.


  METHOD create_by_bapireturn_table.

    DATA lt_bapiret2          TYPE bapiret2_t.

    LOOP AT it_return
      ASSIGNING FIELD-SYMBOL(<ls_return>).

      APPEND INITIAL LINE TO lt_bapiret2
        ASSIGNING FIELD-SYMBOL(<ls_bapiret2>).

      <ls_bapiret2> = map_bapireturn_to_bapiret2( <ls_return> ).

    ENDLOOP.

    rx_return =
      zcx_return=>create_by_bapiret2_table(
        it_return          = lt_bapiret2
        iv_restartable_ind = iv_restartable_ind ).

  ENDMETHOD.


  METHOD create_by_bdc_message_table.

    DATA lt_bapiret2          TYPE bapiret2_t.

    LOOP AT it_bdc_messages
      ASSIGNING FIELD-SYMBOL(<ls_bdc_message>).

      APPEND INITIAL LINE TO lt_bapiret2
        ASSIGNING FIELD-SYMBOL(<ls_bapiret2>).

      <ls_bapiret2> = map_bdc_mess_to_bapiret2( <ls_bdc_message> ).

    ENDLOOP.

    rx_return =
      zcx_return=>create_by_bapiret2_table(
        it_return          = lt_bapiret2
        iv_restartable_ind = iv_restartable_ind ).

  ENDMETHOD.


  METHOD create_by_system_message.

    rx_return = NEW #( ).

    rx_return->add_system_message(
      iv_field_name = iv_field_name ).

  ENDMETHOD.


  METHOD create_by_text.

    rx_return = NEW #( ).

    rx_return->add_by_text(
      iv_type         = iv_type
      iv_message      = iv_message
      iv_variable_1   = iv_variable_1
      iv_variable_2   = iv_variable_2
      iv_variable_3   = iv_variable_3
      iv_variable_4   = iv_variable_4 ).

  ENDMETHOD.


  METHOD if_message~get_text.

    IF gs_return-message IS INITIAL.

      MESSAGE
        ID gs_return-id TYPE gs_return-type NUMBER gs_return-number
        WITH
          gs_return-message_v1
          gs_return-message_v2
          gs_return-message_v3
          gs_return-message_v4
        INTO gs_return-message.

    ENDIF.

    result = gs_return-message.

  ENDMETHOD.


  METHOD map_bapireturn_to_bapiret2.

    rs_bapiret2-type       = is_bapireturn-type.

    "Example value field code: IS504
    rs_bapiret2-id      = is_bapireturn-code+0(2).
    rs_bapiret2-number  = is_bapireturn-code+2(3).

    rs_bapiret2-message    = is_bapireturn-message.
    rs_bapiret2-log_no     = is_bapireturn-log_no.
    rs_bapiret2-log_msg_no = is_bapireturn-log_msg_no.
    rs_bapiret2-message_v1 = is_bapireturn-message_v1.
    rs_bapiret2-message_v2 = is_bapireturn-message_v2.
    rs_bapiret2-message_v3 = is_bapireturn-message_v3.
    rs_bapiret2-message_v4 = is_bapireturn-message_v4.

  ENDMETHOD.


  METHOD map_bdc_mess_to_bapiret2.

    rs_bapiret2-type       = is_bdc_message-msgtyp.
    rs_bapiret2-id      = is_bdc_message-msgid.
    rs_bapiret2-number  = is_bdc_message-msgnr.

    rs_bapiret2-message_v1 = is_bdc_message-msgv1.
    rs_bapiret2-message_v2 = is_bdc_message-msgv2.
    rs_bapiret2-message_v3 = is_bdc_message-msgv3.
    rs_bapiret2-message_v4 = is_bdc_message-msgv4.

    MESSAGE
      ID rs_bapiret2-id
      TYPE rs_bapiret2-type
      NUMBER rs_bapiret2-number
      WITH
        rs_bapiret2-message_v1
        rs_bapiret2-message_v2
        rs_bapiret2-message_v3
        rs_bapiret2-message_v4
      INTO rs_bapiret2-message.

  ENDMETHOD.


  METHOD raise_exception.

    "This method was created for raising exceptions within function modules
    RAISE EXCEPTION me.

  ENDMETHOD.


  METHOD set_long_text_to_system_mess.

    TYPES:
      BEGIN OF ltv_variable,
        text TYPE c LENGTH 50,
      END OF ltv_variable.

    DATA:
      lv_text_string TYPE string,
      lt_text        TYPE STANDARD TABLE OF ltv_variable.

    lv_text_string = iv_text.

    CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
      EXPORTING
        i_string         = lv_text_string
        i_tabline_length = 50
      TABLES
        et_table         = lt_text.

    LOOP AT lt_text ASSIGNING FIELD-SYMBOL(<ls_variable>).

      CASE sy-tabix.
        WHEN 1.
          sy-msgv1 = <ls_variable>-text.
        WHEN 2.
          sy-msgv2 = <ls_variable>-text.
        WHEN 3.
          sy-msgv3 = <ls_variable>-text.
        WHEN 4.
          sy-msgv4 = <ls_variable>-text.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.