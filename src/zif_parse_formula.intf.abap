interface ZIF_PARSE_FORMULA
  public .


  methods CALCULATE
    importing
      !IV_FORMULA type ZPARSER_E_FORMULA
      !IO_CONSUMER type ref to ZIF_PARSE_FORMULA_CONSUMER
    returning
      value(RV_RESULT) type WRBTR
    raising
      ZCX_PARSE_FORMULA .
endinterface.
