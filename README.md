# ZPARSE_FORMULA
Простой парсинг формул

Принимает простые формулы со сложением и вычитанием.

Пример:
    
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

    DATA(lo_consumer) = NEW lcl_consumer( ).
    DATA(lv_wrbtr) = zcl_parse_formula=>get_instance( )->calucalte( iv_formula  = '11+12-13'
                                                                    io_consumer = lo_consumer )
