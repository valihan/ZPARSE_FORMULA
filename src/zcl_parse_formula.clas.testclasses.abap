CLASS lcl_consumer DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_parse_formula_consumer.
ENDCLASS.

CLASS lcl_consumer IMPLEMENTATION.
  METHOD zif_parse_formula_consumer~get_operand.
    TRY.
        rv_result = iv_formula.
      CATCH cx_sy_conversion_no_number INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_parse_formula
          EXPORTING
            previous = lx_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_consumer_tab DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_parse_formula_consumer.
    TYPES: BEGIN OF ts_data,
             num   TYPE numc3,
             wrbtr TYPE wrbtr,
           END OF ts_data.
    TYPES: tt_data TYPE STANDARD TABLE OF ts_data WITH UNIQUE HASHED KEY num COMPONENTS num.
    METHODS constructor IMPORTING it_data TYPE tt_data.
  PRIVATE SECTION.
    DATA mt_data TYPE tt_data.
ENDCLASS.

CLASS lcl_consumer_tab IMPLEMENTATION.
  METHOD constructor.
    mt_data = it_data.
  ENDMETHOD.

  METHOD zif_parse_formula_consumer~get_operand.
    ASSIGN mt_data[ KEY num num = iv_formula ] TO FIELD-SYMBOL(<ls_data>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_parse_formula MESSAGE e001(00) WITH 'Line' iv_formula 'not found'.
    ENDIF.
    rv_result = <ls_data>-wrbtr.
  ENDMETHOD.
ENDCLASS.

CLASS tcl_parse_formula DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>tcl_Parse_Formula
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_PARSE_FORMULA
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      mo_main TYPE REF TO zif_parse_formula.  "class under test
    DATA: mo_consumer TYPE REF TO zif_parse_formula_consumer.
    DATA: mo_consumer_tab TYPE REF TO zif_parse_formula_consumer.

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: p_calculate FOR TESTING.
    METHODS: n_calculate FOR TESTING.
    METHODS: p_calculate_tab FOR TESTING.
    METHODS: n_calculate_tab FOR TESTING.
ENDCLASS.       "tcl_Parse_Formula


CLASS tcl_parse_formula IMPLEMENTATION.

  METHOD class_setup.

  ENDMETHOD.


  METHOD class_teardown.



  ENDMETHOD.


  METHOD setup.
    mo_consumer = NEW lcl_consumer( ).
    mo_consumer_tab = CAST zif_parse_formula_consumer( NEW lcl_consumer_tab(
      VALUE #( ( num = '010' wrbtr = 110 )
               ( num = '011' wrbtr = 111 )
               ( num = '012' wrbtr = 112 )
               ( num = '013' wrbtr = 113 )
               ( num = '009' wrbtr = 109 )
             ) ) ).

    mo_main = zcl_parse_formula=>get_instance( ).
  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.


  METHOD p_calculate.

    TRY.
        cl_abap_unit_assert=>assert_equals(
          act   = mo_main->calculate( iv_formula  = ''
                                      io_consumer = mo_consumer )
          exp   = 0 ).

        cl_abap_unit_assert=>assert_equals(
          act   = mo_main->calculate( iv_formula  = '11'
                                      io_consumer = mo_consumer )
          exp   = 11 ).

        cl_abap_unit_assert=>assert_equals(
          act   = mo_main->calculate( iv_formula  = '-11'
                                      io_consumer = mo_consumer )
          exp   = -11 ).

        cl_abap_unit_assert=>assert_equals(
          act   = mo_main->calculate( iv_formula  = '11 + 12'
                                      io_consumer = mo_consumer )
          exp   = 23 ).
        cl_abap_unit_assert=>assert_equals(
          act   = mo_main->calculate( iv_formula  = '11+12-13'
                                      io_consumer = mo_consumer )
          exp   = 10 ).

        cl_abap_unit_assert=>assert_equals(
          act   = mo_main->calculate( iv_formula  = '11+12+13'
                                      io_consumer = mo_consumer )
          exp   = 36 ).
      CATCH zcx_parse_formula INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( lx_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.


  METHOD n_calculate.
    TRY.
        cl_abap_unit_assert=>assert_equals(
          act   = mo_main->calculate( iv_formula  = '11+12+13+a'
                                      io_consumer = mo_consumer )
          exp   = 0 ).
        cl_abap_unit_assert=>fail( 'Fail' ).
      CATCH zcx_parse_formula INTO DATA(lx_error).
    ENDTRY.
  ENDMETHOD.

  METHOD p_calculate_tab.
    TRY.
        cl_abap_unit_assert=>assert_equals(
          act   = mo_main->calculate( iv_formula  = '9'
                                      io_consumer = mo_consumer_tab )
          exp   = 109 ).
        cl_abap_unit_assert=>assert_equals(
          act   = mo_main->calculate( iv_formula  = '10+11'
                                      io_consumer = mo_consumer_tab )
          exp   = 221 ).
        cl_abap_unit_assert=>assert_equals(
          act   = mo_main->calculate( iv_formula  = '11+12-13'
                                      io_consumer = mo_consumer_tab )
          exp   = 110 ).
      CATCH zcx_parse_formula INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( lx_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD n_calculate_tab.
    TRY.
        cl_abap_unit_assert=>assert_equals(
          act   = mo_main->calculate( iv_formula  = '9+8'
                                      io_consumer = mo_consumer_tab )
          exp   = 0 ).
        cl_abap_unit_assert=>fail( 'Fail' ).
      CATCH zcx_parse_formula INTO DATA(lx_error).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
