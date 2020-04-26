CLASS zcx_return2 DEFINITION
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

  METHODS constructor
    IMPORTING
      !textid LIKE textid OPTIONAL
      !previous LIKE previous OPTIONAL
      !gt_return TYPE bapiret2_t OPTIONAL .
  CLASS-METHODS create_by_system_message
    IMPORTING
      !iv_field_name TYPE bapi_fld OPTIONAL
    RETURNING
      VALUE(rx_return) TYPE REF TO zcx_return2 .
  CLASS-METHODS create_by_message_and_text_var
    IMPORTING
      !iv_type TYPE bapi_mtype
      !iv_id TYPE symsgid
      !iv_number TYPE symsgno
      !iv_text_variable TYPE char200
    RETURNING
      VALUE(rx_return) TYPE REF TO zcx_return2 .
  CLASS-METHODS create_by_bapireturn_struc
    IMPORTING
      !is_return TYPE bapireturn
    RETURNING
      VALUE(rx_return) TYPE REF TO zcx_return2 .
  CLASS-METHODS create_by_bapireturn_table
    IMPORTING
      !it_return TYPE gtt_bapireturn
      !iv_restartable_ind TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rx_return) TYPE REF TO zcx_return2 .
  CLASS-METHODS create_by_bapiret1_struc
    IMPORTING
      !is_return TYPE bapiret1
    RETURNING
      VALUE(rx_return) TYPE REF TO zcx_return2 .
  CLASS-METHODS create_by_bapiret1_table
    IMPORTING
      !it_return TYPE bapiret1_tab
    RETURNING
      VALUE(rx_return) TYPE REF TO zcx_return2 .
  CLASS-METHODS create_by_bapiret2_struc
    IMPORTING
      !is_return TYPE bapiret2
    RETURNING
      VALUE(rx_return) TYPE REF TO zcx_return2 .
  CLASS-METHODS create_by_bapiret2_table
    IMPORTING
      !it_return TYPE bapiret2_t
      !iv_restartable_ind TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rx_return) TYPE REF TO zcx_return2 .
  CLASS-METHODS create_by_bdc_table
    IMPORTING
      !it_bdc_messages TYPE gtt_bdc_messages
      !iv_restartable_ind TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rx_return) TYPE REF TO zcx_return2 .
  CLASS-METHODS create_by_text
    IMPORTING
      !iv_type TYPE bapi_mtype DEFAULT 'E'
      !iv_message TYPE bapi_msg
      !iv_variable_1 TYPE symsgv OPTIONAL
      !iv_variable_2 TYPE symsgv OPTIONAL
      !iv_variable_3 TYPE symsgv OPTIONAL
      !iv_variable_4 TYPE symsgv OPTIONAL
    RETURNING
      VALUE(rx_return) TYPE REF TO zcx_return2 .
  METHODS get_bapiret2_struc
    RETURNING
      VALUE(rs_bapiret2) TYPE bapiret2 .
  METHODS get_bapiret2_table
    RETURNING
      VALUE(rt_bapiret2) TYPE bapiret2_t .
  METHODS add_system_message
    IMPORTING
      !iv_field_name TYPE bapi_fld OPTIONAL .
  METHODS add_message_and_text_var
    IMPORTING
      !iv_type TYPE bapi_mtype
      !iv_id TYPE symsgid
      !iv_number TYPE symsgno
      !iv_text_variable TYPE char200 .
  METHODS add_bapireturn_struc
    IMPORTING
      !is_bapireturn TYPE bapireturn .
  METHODS add_bapireturn_table
    IMPORTING
      !it_return TYPE gtt_bapireturn .
  METHODS add_bapiret1_struc
    IMPORTING
      !is_return TYPE bapiret1 .
  METHODS add_bapiret1_table
    IMPORTING
      !it_return TYPE bapiret1_tab .
  METHODS add_bapiret2_struc
    IMPORTING
      !is_return TYPE bapiret2 .
  METHODS add_bapiret2_table
    IMPORTING
      !it_return TYPE bapiret2_t .
  METHODS add_by_text
    IMPORTING
      !iv_type TYPE bapi_mtype DEFAULT 'E'
      !iv_message TYPE bapi_msg
      !iv_variable_1 TYPE symsgv
      !iv_variable_2 TYPE symsgv
      !iv_variable_3 TYPE symsgv
      !iv_variable_4 TYPE symsgv .
  METHODS raise_exception
    RAISING
      zcx_return2 .

  METHODS if_message~get_longtext
    REDEFINITION .
  METHODS if_message~get_text
    REDEFINITION .
PROTECTED SECTION.

  DATA gt_return TYPE bapiret2_t .

  CLASS-METHODS map_text_var_to_bapiret2
    IMPORTING
      !iv_type TYPE bapi_mtype
      !iv_id TYPE symsgid
      !iv_number TYPE symsgno
      !iv_text TYPE char200
    RETURNING
      VALUE(rs_bapiret2) TYPE bapiret2 .
  CLASS-METHODS map_bapireturn_to_bapiret2
    IMPORTING
      !is_bapireturn TYPE bapireturn
    RETURNING
      VALUE(rs_bapiret2) TYPE bapiret2 .
  CLASS-METHODS map_bapiret1_to_bapiret2
    IMPORTING
      !is_bapireturn TYPE bapiret1
    RETURNING
      VALUE(rs_bapiret2) TYPE bapiret2 .
  CLASS-METHODS map_bdc_to_bapiret2
    IMPORTING
      !is_bdc_message TYPE bdcmsgcoll
    RETURNING
      VALUE(rs_bapiret2) TYPE bapiret2 .
  CLASS-METHODS check_has_messages
    IMPORTING
      !ix_return TYPE REF TO zcx_return2
    RETURNING
      VALUE(rx_return) TYPE REF TO zcx_return2 .
  PRIVATE SECTION.

ENDCLASS.



CLASS zcx_return2 IMPLEMENTATION.


  METHOD add_bapiret1_struc.

    DATA(ls_bapiret2_struc) = map_bapiret1_to_bapiret2( is_return ).

    add_bapiret2_struc( ls_bapiret2_struc ).

  ENDMETHOD.


  METHOD add_bapiret1_table.

    LOOP AT it_return
      ASSIGNING FIELD-SYMBOL(<ls_return>).

      add_bapiret1_struc( <ls_return> ).

    ENDLOOP.

  ENDMETHOD.


  METHOD add_bapiret2_struc.

    IF is_return-type CA 'EAX'.

      APPEND is_return TO gt_return.

    ENDIF.

  ENDMETHOD.


  METHOD add_bapiret2_table.

    LOOP AT it_return
      ASSIGNING FIELD-SYMBOL(<ls_return>).

      add_bapiret2_struc( <ls_return> ).

    ENDLOOP.

  ENDMETHOD.


  METHOD add_bapireturn_struc.

    DATA(ls_bapiret2) =
      zcx_return2=>map_bapireturn_to_bapiret2( is_bapireturn ).

    me->add_bapiret2_struc( ls_bapiret2 ).

  ENDMETHOD.


  METHOD add_bapireturn_table.

    LOOP AT it_return
      ASSIGNING FIELD-SYMBOL(<ls_return>).

      add_bapireturn_struc( <ls_return> ).

    ENDLOOP.

  ENDMETHOD.


  METHOD add_message_and_text_var.

    DATA(ls_bapiret2) =
      map_text_var_to_bapiret2(
        iv_type           = iv_type
        iv_id             = iv_id
        iv_number         = iv_number
        iv_text           = iv_text_variable ).

    me->add_bapiret2_struc( ls_bapiret2 ).

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

    add_bapiret2_struc( ls_return ).

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

    add_bapiret2_struc( ls_return ).

  ENDMETHOD.


  METHOD check_has_messages.

    IF ix_return->gt_return[] IS INITIAL.

      RETURN.

    ENDIF.

    rx_return = ix_return.

  ENDMETHOD.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        textid   = textid
        previous = previous.
    me->gt_return = gt_return .
  ENDMETHOD.


  METHOD create_by_bapiret1_struc.

    rx_return = NEW #( ).

    rx_return->add_bapiret1_struc( is_return ).

    rx_return = check_has_messages( rx_return ).

  ENDMETHOD.


  METHOD create_by_bapiret1_table.

    rx_return = NEW #( ).

    rx_return->add_bapiret1_table( it_return ).

    rx_return = check_has_messages( rx_return ).

  ENDMETHOD.


  METHOD create_by_bapiret2_struc.

    rx_return = NEW #( ).

    rx_return->add_bapiret2_struc( is_return ).

    rx_return = check_has_messages( rx_return ).

  ENDMETHOD.


  METHOD create_by_bapiret2_table.

    rx_return = NEW #( ).

    rx_return->add_bapiret2_table( it_return ).

    rx_return = check_has_messages( rx_return ).

  ENDMETHOD.


  METHOD create_by_bapireturn_struc.

    rx_return = NEW #( ).

    rx_return->add_bapireturn_struc( is_return ).

    rx_return = check_has_messages( rx_return ).

  ENDMETHOD.


  METHOD create_by_bapireturn_table.

    rx_return = NEW #( ).

    rx_return->add_bapireturn_table( it_return ).

    rx_return = check_has_messages( rx_return ).

  ENDMETHOD.


  METHOD create_by_bdc_table.

    DATA lt_bapiret2          TYPE bapiret2_t.

    LOOP AT it_bdc_messages
      ASSIGNING FIELD-SYMBOL(<ls_bdc_message>).

      APPEND INITIAL LINE TO lt_bapiret2
        ASSIGNING FIELD-SYMBOL(<ls_bapiret2>).

      <ls_bapiret2> = map_bdc_to_bapiret2( <ls_bdc_message> ).

    ENDLOOP.

    rx_return  = create_by_bapiret2_table( lt_bapiret2 ).

  ENDMETHOD.


  METHOD create_by_message_and_text_var.

    rx_return = NEW #( ).

    rx_return->add_message_and_text_var(
      iv_type           = iv_type
      iv_id             = iv_id
      iv_number         = iv_number
      iv_text_variable  = iv_text_variable ).

    rx_return = check_has_messages( rx_return ).

  ENDMETHOD.


  METHOD create_by_system_message.

    rx_return = NEW #( ).

    rx_return->add_system_message(
      iv_field_name = iv_field_name ).

    rx_return = check_has_messages( rx_return ).

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


  METHOD get_bapiret2_struc.

    READ TABLE gt_return
      INTO rs_bapiret2
      INDEX 1.

  ENDMETHOD.


  METHOD get_bapiret2_table.

    rt_bapiret2 = gt_return.

  ENDMETHOD.


  METHOD if_message~get_longtext.

    DATA(ls_bapiret2) = me->get_bapiret2_struc( ).

    DATA:
      ls_message TYPE bapiret2-message,
      ls_return  TYPE bapiret2,
      lt_text    TYPE STANDARD TABLE OF bapitgb WITH DEFAULT KEY.

    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = ls_bapiret2-id
        number     = ls_bapiret2-number
        language   = sy-langu
        textformat = 'ASC'
        "LINKPATTERN =
        message_v1 = ls_bapiret2-message_v1
        message_v2 = ls_bapiret2-message_v2
        message_v3 = ls_bapiret2-message_v3
        message_v4 = ls_bapiret2-message_v4
        "LINE_SIZE  =
      IMPORTING
        message    = ls_message
        return     = ls_return
      TABLES
        text       = lt_text.

    LOOP AT lt_text
      ASSIGNING FIELD-SYMBOL(<ls_text>).

      result = result && | | && <ls_text>-line.

    ENDLOOP.

  ENDMETHOD.


  METHOD if_message~get_text.

    READ TABLE gt_return
      ASSIGNING FIELD-SYMBOL(<ls_return>)
      INDEX 1.

    IF sy-subrc = 0.

      result = <ls_return>-message.

    ENDIF.

  ENDMETHOD.


  METHOD map_bapiret1_to_bapiret2.

    rs_bapiret2-type       = is_bapireturn-type.
    rs_bapiret2-id         = is_bapireturn-id.
    rs_bapiret2-number     = is_bapireturn-number.

    rs_bapiret2-message    = is_bapireturn-message.
    rs_bapiret2-log_no     = is_bapireturn-log_no.
    rs_bapiret2-log_msg_no = is_bapireturn-log_msg_no.

    rs_bapiret2-message_v1 = is_bapireturn-message_v1.
    rs_bapiret2-message_v2 = is_bapireturn-message_v2.
    rs_bapiret2-message_v3 = is_bapireturn-message_v3.
    rs_bapiret2-message_v4 = is_bapireturn-message_v4.

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


  METHOD map_bdc_to_bapiret2.

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


  METHOD map_text_var_to_bapiret2.

    "***************************************************
    " Convert text to variables
    "***************************************************

    rs_bapiret2-type   = iv_type.
    rs_bapiret2-id     = iv_id.
    rs_bapiret2-number = iv_number.

    "***************************************************
    " Convert text to variables
    "***************************************************

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
          rs_bapiret2-message_v1 = <ls_variable>-text.
        WHEN 2.
          rs_bapiret2-message_v2 = <ls_variable>-text.
        WHEN 3.
          rs_bapiret2-message_v3 = <ls_variable>-text.
        WHEN 4.
          rs_bapiret2-message_v4 = <ls_variable>-text.
      ENDCASE.

    ENDLOOP.

    "***************************************************
    "Generate message
    "***************************************************

    MESSAGE
      ID rs_bapiret2-id TYPE rs_bapiret2-type NUMBER rs_bapiret2-number
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
ENDCLASS.