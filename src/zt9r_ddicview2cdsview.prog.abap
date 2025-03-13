REPORT zt9r_ddicview2cdsview.

" Transform DDIC-View to CDS-Entity

PARAMETERS p_view TYPE dd25l-viewname DEFAULT 'H_T513_1'.

SELECTION-SCREEN COMMENT /1(70) i01.
SELECTION-SCREEN COMMENT /1(70) i02.
SELECTION-SCREEN COMMENT /1(70) i03.

CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ts_raw_data,
             line TYPE c LENGTH 100,
           END OF ts_raw_data,
           tt_raw_data TYPE STANDARD TABLE OF ts_Raw_data WITH DEFAULT KEY.
    CLASS-METHODS transform
      IMPORTING i_view_name   TYPE clike
      RETURNING VALUE(result) TYPE tt_raw_Data.
  PRIVATE SECTION.

ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD transform.
    DATA returncode TYPE i.

    DATA join_options TYPE STANDARD TABLE OF dd05m.
    DATA base_tables  TYPE STANDARD TABLE OF dd26v.
    DATA view_fields  TYPE STANDARD TABLE OF dd27v.
    DATA seloptions   TYPE STANDARD TABLE OF dd28v.

    CALL FUNCTION 'DB_GET_VIEW_DEFINITION'
      EXPORTING
        viewname        = i_view_name
      IMPORTING
        rc              = returncode
      TABLES
        db05m           = join_options                 " Tabelle der Joinbedingungen
        db26v           = base_tables                  " Tabelle der Basistabellen
        db27v           = view_fields                  " Tabelle der View-Felder
        db28v           = seloptions                   " Tabelle der Selektionsbedingungen
      EXCEPTIONS
        view_not_exists = 1                " View existiert nicht auf der Datenbank
        OTHERS          = 2.
    IF sy-subrc <> 0 OR returncode > 0.
      RETURN.
    ENDIF.

    DATA first TYPE abap_bool.
    APPEND VALUE #( line = |definition view entity { i_view_name }| ) TO result.
    APPEND VALUE #( line = |  as select from { base_tables[ 1 ]-tabname }| ) TO result.
    DATA(basetable) = base_Tables[ 1 ]-tabname.
    LOOP AT base_Tables FROM 2 INTO DATA(base_table).
      APPEND VALUE #( line = |    join { base_table-tabname } as { base_table-tabname }| ) TO result.
      first = abap_true.
      LOOP AT join_options INTO DATA(join_option)
        WHERE ( ( fortable   = base_table-tabname AND checktable = basetable )
             OR ( checktable = base_table-tabname ) )
         AND forkey <> 'MANDT'.
        DATA(option) = |{ join_option-checktable }.{ join_option-checkfield } = { join_option-fortable }.{ join_option-forkey }|.
        IF first = abap_True.
          first = abap_false.
          APPEND VALUE #( line = |on  { option }| ) TO result.
        ELSE.
          APPEND VALUE #( line = |and { option }| ) TO result.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    APPEND VALUE #( line = '{' ) TO result.

    DATA separator TYPE c LENGTH 1 VALUE ','.
    LOOP AT view_fields INTO DATA(view_field).
      IF sy-tabix = lines( view_fields ).
        CLEAR separator.
      ENDIF.
      APPEND VALUE #( line = |  { view_field-tabname }.{ view_field-fieldname } as { view_field-viewfield }{ separator }| ) TO result.

    ENDLOOP.

    APPEND VALUE #( line = '}' ) TO result.

  ENDMETHOD.
ENDCLASS.

INITIALIZATION.

  DATA(docker) = NEW cl_gui_docking_container( side = cl_gui_docking_container=>dock_at_bottom ratio = 85 ).
  DATA(splitter) = NEW cl_gui_splitter_container( parent = docker rows = 1 columns = 2 ).
  DATA(text_ddic) = NEW cl_gui_textedit( parent = splitter->get_container( row = 1 column = 1 ) ).
  text_ddic->set_readonly_mode( 1 ).
  text_ddic->set_status_text( 'Original DDIC-View' ).
  DATA(text_cds) = NEW cl_gui_textedit( parent = splitter->get_container( row = 1 column = 2 ) ).
  text_cds->set_readonly_mode( 1 ).
  text_cds->set_status_text( 'Transformation CDS-Entity' ).
  DATA view_definition TYPE lcl_app=>tt_raw_Data.

  i01 = TEXT-i01.
  i02 = TEXT-i02.
  i03 = TEXT-i03.

AT SELECTION-SCREEN.

  CALL FUNCTION 'DB_GET_VIEW_DEFINITION_RAW'
    EXPORTING
      viewname              = p_view
    TABLES
      text_tab              = view_definition
    EXCEPTIONS
      view_not_exists       = 1                " View existiert nicht auf der Datenbank
      non_supported_feature = 2                " Nicht unterstütztes DB Feature
      OTHERS                = 3.
  IF sy-subrc = 0.
    text_ddic->set_text_as_r3table( view_definition ).
  ENDIF.

  text_cds->set_text_as_r3table( lcl_app=>transform( p_View ) ).
