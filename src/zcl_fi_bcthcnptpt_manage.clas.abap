CLASS zcl_fi_bcthcnptpt_manage DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA: gt_data    TYPE TABLE OF zi_fi_bcthcnptpt.
    CLASS-METHODS:
      get_instance
        RETURNING
          VALUE(ro_instance) TYPE REF TO zcl_fi_bcthcnptpt_manage,
      get_data
        IMPORTING io_request TYPE REF TO if_rap_query_request
        EXPORTING et_data    LIKE gt_data.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA: instance TYPE REF TO zcl_fi_bcthcnptpt_manage.
    CLASS-METHODS:
      get_data_db
        IMPORTING io_request TYPE REF TO if_rap_query_request
        EXPORTING et_data    LIKE gt_data.
ENDCLASS.



CLASS ZCL_FI_BCTHCNPTPT_MANAGE IMPLEMENTATION.


  METHOD get_data.
    " get list field requested ----------------------
    DATA(lt_reqs_element) = io_request->get_requested_elements( ).
    DATA(lt_aggr_element) = io_request->get_aggregation( )->get_aggregated_elements( ).
    IF lt_aggr_element IS NOT INITIAL.
      LOOP AT lt_aggr_element ASSIGNING FIELD-SYMBOL(<lfs_aggr_elements>).
        DELETE lt_reqs_element WHERE table_line = <lfs_aggr_elements>-result_element.
        DATA(lv_aggr) = |{ <lfs_aggr_elements>-aggregation_method }( { <lfs_aggr_elements>-input_element } ) as { <lfs_aggr_elements>-result_element }|.
        APPEND lv_aggr TO lt_reqs_element.
      ENDLOOP.
    ENDIF.

    DATA(lv_reqs_element) = concat_lines_of( table = lt_reqs_element sep = `, ` ).
    " get list field requested ----------------------

    " get list field ordered ------------------------
    DATA(lt_sort) = io_request->get_sort_elements( ).

    DATA(lt_sort_criteria) = VALUE string_table( FOR ls_sort IN lt_sort ( ls_sort-element_name && COND #( WHEN ls_sort-descending = abap_true THEN ` descending`
                                                                                                                                              ELSE ` ascending` ) ) ).

    DATA(lv_sort_element) = COND #( WHEN lt_sort_criteria IS INITIAL
                                    THEN `is_total ASCENDING, accountgroup ASCENDING, is_subtotal DESCENDING, cus_sup ASCENDING, glaccount ASCENDING`
                                    ELSE concat_lines_of( table = lt_sort_criteria sep = `, ` ) ).
    " get list field ordered ------------------------

    " get range of row data -------------------------
    DATA(lv_top)      = io_request->get_paging( )->get_page_size( ).
    DATA(lv_skip)     = io_request->get_paging( )->get_offset( ).
    DATA(lv_max_rows) = COND #( WHEN lv_top = if_rap_query_paging=>page_size_unlimited THEN 0
                                ELSE lv_top ).
    IF lv_max_rows = -1 .
      lv_max_rows = 1.
    ENDIF.
    " get range of row data -------------------------

    "get data --------------------------------------
    DATA: lv_fieldname   TYPE c LENGTH 30,
          lv_count       TYPE int1,
          lv_prev_serial TYPE c LENGTH 18.

    get_data_db( EXPORTING io_request = io_request IMPORTING et_data = DATA(lt_data) ).

    SELECT (lv_reqs_element)
    FROM @lt_data AS data
    ORDER BY (lv_sort_element)
    INTO CORRESPONDING FIELDS OF TABLE @et_data
    OFFSET @lv_skip UP TO @lv_max_rows ROWS.
    "get data --------------------------------------
  ENDMETHOD.


  METHOD get_data_db.
    DATA: lv_koart    TYPE koart,
          lr_koart    TYPE RANGE OF koart,
          lv_increv   TYPE zde_boolean,
          lv_notclr   TYPE zde_boolean,
          lv_fromdate TYPE dats,
          lv_todate   TYPE dats,
          lv_currtype TYPE zde_curr_type,
          lr_waers    TYPE RANGE OF waers,
          lv_bukrs    TYPE bukrs,
          lv_gjahr    TYPE n LENGTH 4,
          lr_gjahr    TYPE RANGE OF fis_gjahr_no_conv,
          lr_racct    TYPE RANGE OF zde_fi_racct,
*          lr_kunnr    TYPE RANGE OF zde_kunnr,
*          lr_lifnr    TYPE RANGE OF lifnr,
          lr_cussup   TYPE RANGE OF zde_kunnr,
          lr_ktokd    TYPE RANGE OF ktokd.

    DATA: lt_data_sum      TYPE TABLE OF zi_fi_bcthcnptpt,
          lt_data          TYPE TABLE OF zi_fi_bcthcnptpt,
          ls_data          TYPE zi_fi_bcthcnptpt,
          ls_data_total    TYPE zi_fi_bcthcnptpt,
          ls_data_subtotal TYPE zi_fi_bcthcnptpt.

    " get filter by parameter -----------------------
    DATA(lt_paramater) = io_request->get_parameters( ).
    IF lt_paramater IS NOT INITIAL.
      LOOP AT lt_paramater REFERENCE INTO DATA(ls_parameter).
        CASE ls_parameter->parameter_name.
          WHEN 'ONDISPLAY'. " D or K
            lv_koart    = ls_parameter->value.
            IF lv_koart NE 'B'.
              APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_koart ) TO lr_koart.
            ENDIF.
          WHEN 'INCLUDEREVERSEDDOCUMENTS'. " YES or NO
            lv_increv   = ls_parameter->value.
          WHEN 'DOCTYPE'. " YES or NO
            lv_notclr   = ls_parameter->value.
          WHEN 'FROMDATE'.
            lv_fromdate = ls_parameter->value.
          WHEN 'TODATE'.
            lv_todate   = ls_parameter->value.
          WHEN 'CURRENCYTYPE'. " I or T or E
            lv_currtype = ls_parameter->value.
*          WHEN 'SELECTEDSUM'. " G or P
          WHEN 'COMPANYCODE'.
            lv_bukrs    = ls_parameter->value.
          WHEN 'FISCALYEAR'.
            lv_gjahr    = ls_parameter->value.
            APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_gjahr ) TO lr_gjahr.
        ENDCASE.
      ENDLOOP.
    ENDIF.
    TRY.
        DATA(lt_filter_cond) = io_request->get_filter( )->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range INTO DATA(lx_no_sel_option).
    ENDTRY.
    IF lt_filter_cond IS NOT INITIAL.
      LOOP AT lt_filter_cond REFERENCE INTO DATA(ls_filter_cond).
        CASE ls_filter_cond->name.
          WHEN 'CURRENCY'.
            lr_waers = CORRESPONDING #( ls_filter_cond->range[] ) .
          WHEN 'GLACCOUNT'.
            lr_racct = CORRESPONDING #( ls_filter_cond->range[] ) .
*          WHEN 'CUSTOMER'.
*            lr_kunnr = CORRESPONDING #( ls_filter_cond->range ) .
*          WHEN 'SUPPLIER'.
*            lr_lifnr = CORRESPONDING #( ls_filter_cond->range ) .
          WHEN 'CUS_SUP'.
            lr_cussup = CORRESPONDING #( ls_filter_cond->range[] ) .
          WHEN 'ACCOUNTGROUP'.
            lr_ktokd  = CORRESPONDING #( ls_filter_cond->range[] ) .
          WHEN OTHERS.
        ENDCASE.
      ENDLOOP.
    ENDIF.


    IF lv_currtype = 'I' OR lv_currtype = 'T'.
      CLEAR: lr_waers.
      lr_waers = VALUE #( sign = 'I' option = 'EQ' ( low = 'VND' ) ).
    ENDIF.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_fromdate(4) - 1 ) TO lr_gjahr.
    " get filter by parameter ----------------------

    SELECT
      i_journalentryitem~accountingdocument,
      i_journalentryitem~clearingdate,
      i_journalentryitem~clearingaccountingdocument,
      i_journalentryitem~postingdate,
      i_journalentryitem~financialaccounttype,
      i_journalentryitem~customer,
      i_journalentryitem~supplier,
      i_journalentryitem~fiscalyear,
      CASE
      WHEN i_journalentryitem~financialaccounttype = 'D' THEN i_journalentryitem~customer
      WHEN i_journalentryitem~financialaccounttype = 'K' THEN i_journalentryitem~supplier
      END AS cus_sup,
      i_journalentryitem~glaccount,
      i_journalentryitem~companycodecurrency,
      i_journalentryitem~amountintransactioncurrency,
      i_journalentryitem~transactioncurrency,
      i_journalentryitem~amountincompanycodecurrency,
      i_journalentryitem~debitcreditcode
      FROM i_journalentryitem
      LEFT JOIN i_customer ON i_journalentryitem~customer = i_customer~customer
      LEFT JOIN i_supplier ON i_journalentryitem~supplier = i_supplier~supplier
      WHERE ledger = '0L'
      AND i_journalentryitem~financialaccounttype IN @lr_koart
      AND ( ( @lv_increv IS NOT INITIAL ) OR
            ( @lv_increv IS INITIAL AND
              i_journalentryitem~isreversal IS INITIAL AND
              i_journalentryitem~isreversed IS INITIAL ) )
      AND ( ( @lv_notclr IS NOT INITIAL ) OR
            ( @lv_notclr IS INITIAL AND
              i_journalentryitem~accountingdocumenttype NE 'CL' AND
              i_journalentryitem~accountingdocumenttype NE 'SU' ) )
      AND i_journalentryitem~postingdate <= @lv_todate
*      AND ( ( @lv_currtype = 'I' AND i_journalentryitem~transactioncurrency = 'VND' ) OR
*            ( @lv_currtype = 'T' ) OR
*            ( @lv_currtype = 'E' AND i_journalentryitem~companycodecurrency NE i_journalentryitem~transactioncurrency ) )
      AND ( (    i_journalentryitem~transactioncurrency IN @lr_waers AND @lv_currtype = 'E' )
            OR ( i_journalentryitem~transactioncurrency IN @lr_waers AND @lv_currtype = 'I' )
            OR ( i_journalentryitem~companycodecurrency IN @lr_waers AND @lv_currtype = 'T' )
                )
*      AND i_journalentryitem~transactioncurrency IN @lr_waers
      AND i_journalentryitem~companycode EQ @lv_bukrs
      AND i_journalentryitem~fiscalyear  IN @lr_gjahr
      AND i_journalentryitem~glaccount   IN @lr_racct
      AND ( ( i_journalentryitem~financialaccounttype = 'D' AND i_journalentryitem~customer IN @lr_cussup ) OR
            ( i_journalentryitem~financialaccounttype = 'K' AND i_journalentryitem~supplier IN @lr_cussup ) )
      AND ( ( i_journalentryitem~financialaccounttype = 'D' AND i_customer~customeraccountgroup IN @lr_ktokd ) OR
            ( i_journalentryitem~financialaccounttype = 'K' AND i_supplier~supplieraccountgroup IN @lr_ktokd ) )
      INTO TABLE @DATA(lt_data_raw).
    CHECK sy-subrc EQ 0.

    LOOP AT lt_data_raw INTO DATA(ls_data_raw).

      ls_data-cus_sup               = ls_data_raw-cus_sup.
      ls_data-customer              = ls_data_raw-customer.
      ls_data-supplier              = ls_data_raw-supplier.
      ls_data-financialaccounttype  = ls_data_raw-financialaccounttype.
      ls_data-glaccount             = ls_data_raw-glaccount.
      ls_data-currency              = ls_data_raw-transactioncurrency.
      ls_data-currency_local        = ls_data_raw-companycodecurrency.
      IF ls_data_raw-postingdate < lv_fromdate.
        " begin
        IF ls_data_raw-clearingdate IS INITIAL OR
           ls_data_raw-clearingdate >= lv_fromdate.
          IF ls_data_raw-amountintransactioncurrency >= 0.
            ls_data-amount_beg_debit          = ls_data_raw-amountintransactioncurrency.
            ls_data-amount_beg_debit_local    = ls_data_raw-amountincompanycodecurrency.
          ELSEIF ls_data_raw-amountintransactioncurrency < 0.
            ls_data-amount_beg_credit         = ls_data_raw-amountintransactioncurrency * -1.
            ls_data-amount_beg_credit_local   = ls_data_raw-amountincompanycodecurrency * -1.
          ENDIF.
        ENDIF.
      ELSE.
        IF ls_data_raw-fiscalyear = lv_gjahr.

          " in period
*        IF ls_data_raw-amountintransactioncurrency >= 0.
          IF ls_data_raw-debitcreditcode = 'S'.
            ls_data-amount_debit          = ls_data_raw-amountintransactioncurrency.
            ls_data-amount_debit_local    = ls_data_raw-amountincompanycodecurrency.
*        ELSEIF ls_data_raw-amountintransactioncurrency < 0.
          ELSEIF ls_data_raw-debitcreditcode = 'H'.
            ls_data-amount_credit         = ls_data_raw-amountintransactioncurrency * -1.
            ls_data-amount_credit_local   = ls_data_raw-amountincompanycodecurrency * -1.
          ENDIF.
        ENDIF.
      ENDIF.
      " end
      ls_data-amount_end_debit          = ls_data-amount_beg_debit + ls_data-amount_debit.
      ls_data-amount_end_debit_local    = ls_data-amount_beg_debit_local + ls_data-amount_debit_local.
      ls_data-amount_end_credit         = ls_data-amount_beg_credit + ls_data-amount_credit.
      ls_data-amount_end_credit_local   = ls_data-amount_beg_credit_local + ls_data-amount_credit_local.

      IF lv_currtype EQ 'I' OR lv_currtype EQ 'T'.
        CLEAR: ls_data-currency,
               ls_data-amount_beg_debit,
               ls_data-amount_beg_credit,
               ls_data-amount_debit,
               ls_data-amount_credit,
               ls_data-amount_end_debit,
               ls_data-amount_end_credit.
      ELSEIF lv_currtype EQ 'E'.

      ENDIF.

      APPEND ls_data TO lt_data.
      CLEAR: ls_data.
    ENDLOOP.

    SELECT
      cus_sup,
      customer,
      supplier,
      financialaccounttype,
      glaccount,
      currency,
      currency_local,
      SUM( amount_beg_debit ) AS amount_beg_debit,
      SUM( amount_beg_debit_local ) AS amount_beg_debit_local,
      SUM( amount_beg_credit ) AS amount_beg_credit     ,
      SUM( amount_beg_credit_local ) AS amount_beg_credit_local,
      SUM( amount_debit ) AS amount_debit    ,
      SUM( amount_debit_local ) AS amount_debit_local    ,
      SUM( amount_credit ) AS amount_credit   ,
      SUM( amount_credit_local ) AS amount_credit_local   ,
      SUM( amount_end_debit ) AS amount_end_debit,
      SUM( amount_end_debit_local ) AS amount_end_debit_local,
      SUM( amount_end_credit ) AS amount_end_credit     ,
      SUM( amount_end_credit_local ) AS amount_end_credit_local
      FROM @lt_data AS data
      GROUP BY
      cus_sup,
      customer,
      supplier,
      financialaccounttype,
      glaccount,
      currency,
      currency_local
      INTO CORRESPONDING FIELDS OF TABLE @lt_data_sum.

    DELETE lt_data_sum WHERE amount_beg_debit           EQ 0 AND
                             amount_beg_debit_local     EQ 0 AND
                             amount_beg_credit          EQ 0 AND
                             amount_beg_credit_local    EQ 0 AND
                             amount_debit               EQ 0 AND
                             amount_debit_local         EQ 0 AND
                             amount_credit              EQ 0 AND
                             amount_credit_local        EQ 0 AND
                             amount_end_debit           EQ 0 AND
                             amount_end_debit_local     EQ 0 AND
                             amount_end_credit          EQ 0 AND
                             amount_end_credit_local    EQ 0.
    CHECK lt_data_sum IS NOT INITIAL.

    IF lv_koart EQ 'D' OR lv_koart EQ 'B'.
      SELECT
        i_customer~customer,
        i_customer~businesspartnername1,
        i_customer~businesspartnername2,
        i_customer~businesspartnername3,
        i_customer~businesspartnername4,
        i_customer~customeraccountgroup,
        i_customeraccountgrouptext~accountgroupname
        FROM i_customer
        INNER JOIN i_customeraccountgrouptext
        ON i_customeraccountgrouptext~customeraccountgroup = i_customer~customeraccountgroup
        AND i_customeraccountgrouptext~language = @sy-langu
        FOR ALL ENTRIES IN @lt_data_sum
        WHERE i_customer~customer = @lt_data_sum-customer
        INTO TABLE @DATA(lt_data_customer).
      IF sy-subrc EQ 0.
        SORT lt_data_customer BY customer.
      ENDIF.
    ENDIF.
    IF lv_koart EQ 'K' OR lv_koart EQ 'B'.
      SELECT
        i_supplier~supplier,
        i_supplier~businesspartnername1,
        i_supplier~businesspartnername2,
        i_supplier~businesspartnername3,
        i_supplier~businesspartnername4,
        i_supplier~supplieraccountgroup,
        i_supplieraccountgrouptext~accountgroupname
        FROM i_supplier
        INNER JOIN i_supplieraccountgrouptext
        ON i_supplieraccountgrouptext~supplieraccountgroup = i_supplier~supplieraccountgroup
        AND i_supplieraccountgrouptext~language = @sy-langu
        FOR ALL ENTRIES IN @lt_data_sum
        WHERE i_supplier~supplier = @lt_data_sum-supplier
        INTO TABLE @DATA(lt_data_supplier).
      IF sy-subrc EQ 0.
        SORT lt_data_supplier BY supplier.
      ENDIF.
    ENDIF.

    LOOP AT lt_data_sum ASSIGNING FIELD-SYMBOL(<lfs_data>).
      IF <lfs_data>-financialaccounttype EQ 'D'.
        READ TABLE lt_data_customer INTO DATA(ls_data_customer)
          WITH KEY customer = <lfs_data>-customer BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF ls_data_customer-businesspartnername2 IS INITIAL AND
             ls_data_customer-businesspartnername3 IS INITIAL AND
             ls_data_customer-businesspartnername4 IS INITIAL.
            <lfs_data>-cus_sup_name = ls_data_customer-businesspartnername1.
          ELSE.
            CONCATENATE ls_data_customer-businesspartnername2
                        ls_data_customer-businesspartnername3
                        ls_data_customer-businesspartnername4
                        INTO <lfs_data>-cus_sup_name SEPARATED BY space.
          ENDIF.
          CONDENSE <lfs_data>-cus_sup_name.
          <lfs_data>-accountgroup     = ls_data_customer-customeraccountgroup.
          <lfs_data>-accountgrouptext = ls_data_customer-accountgroupname.
        ENDIF.
      ELSEIF <lfs_data>-financialaccounttype EQ 'K'.
        READ TABLE lt_data_supplier INTO DATA(ls_data_supplier)
          WITH KEY supplier = <lfs_data>-supplier BINARY SEARCH.
        IF sy-subrc EQ 0.
          IF ls_data_supplier-businesspartnername2 IS INITIAL AND
             ls_data_supplier-businesspartnername3 IS INITIAL AND
             ls_data_supplier-businesspartnername4 IS INITIAL.
            <lfs_data>-cus_sup_name = ls_data_supplier-businesspartnername1.
          ELSE.
            CONCATENATE ls_data_supplier-businesspartnername2
                        ls_data_supplier-businesspartnername3
                        ls_data_supplier-businesspartnername4
                        INTO <lfs_data>-cus_sup_name SEPARATED BY space.
          ENDIF.
          CONDENSE <lfs_data>-cus_sup_name.
          <lfs_data>-accountgroup     = ls_data_supplier-supplieraccountgroup.
          <lfs_data>-accountgrouptext = ls_data_supplier-accountgroupname.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CLEAR: lt_data.
    SORT lt_data_sum BY accountgroup cus_sup.

    DATA: lt_data_subtotal LIKE lt_data,
          lt_data_total    LIKE lt_data.

    LOOP AT lt_data_sum INTO ls_data.
      IF ls_data-amount_beg_debit_local - ls_data-amount_beg_credit_local > 0.
        ls_data-amount_beg_debit_local  -= ls_data-amount_beg_credit_local.
        ls_data-amount_beg_debit        -= ls_data-amount_beg_credit.
        ls_data-amount_beg_credit_local = 0.
        ls_data-amount_beg_credit       = 0.
      ELSE.
        ls_data-amount_beg_credit_local -= ls_data-amount_beg_debit_local.
        ls_data-amount_beg_credit       -= ls_data-amount_beg_debit.
        ls_data-amount_beg_debit_local  = 0.
        ls_data-amount_beg_debit        = 0.
      ENDIF.
      IF ls_data-amount_end_debit_local - ls_data-amount_end_credit_local > 0.
        ls_data-amount_end_debit_local  -= ls_data-amount_end_credit_local.
        ls_data-amount_end_debit        -= ls_data-amount_end_credit.
        ls_data-amount_end_credit_local = 0.
        ls_data-amount_end_credit       = 0.
      ELSE.
        ls_data-amount_end_credit_local -= ls_data-amount_end_debit_local.
        ls_data-amount_end_credit       -= ls_data-amount_end_debit.
        ls_data-amount_end_debit_local  = 0.
        ls_data-amount_end_debit        = 0.
      ENDIF.

      APPEND ls_data TO lt_data.

      APPEND ls_data TO lt_data_subtotal.

      APPEND ls_data TO lt_data_total.

      AT END OF accountgroup.

        SELECT
          accountgroup,
          accountgrouptext,
          currency,
          currency_local,
          SUM( amount_beg_debit ) AS amount_beg_debit,
          SUM( amount_beg_debit_local ) AS amount_beg_debit_local,
          SUM( amount_beg_credit ) AS amount_beg_credit     ,
          SUM( amount_beg_credit_local ) AS amount_beg_credit_local,
          SUM( amount_debit ) AS amount_debit    ,
          SUM( amount_debit_local ) AS amount_debit_local    ,
          SUM( amount_credit ) AS amount_credit   ,
          SUM( amount_credit_local ) AS amount_credit_local   ,
          SUM( amount_end_debit ) AS amount_end_debit,
          SUM( amount_end_debit_local ) AS amount_end_debit_local,
          SUM( amount_end_credit ) AS amount_end_credit     ,
          SUM( amount_end_credit_local ) AS amount_end_credit_local
          FROM @lt_data_subtotal AS data
          GROUP BY
          accountgroup,
          accountgrouptext,
          currency,
          currency_local
          INTO TABLE @DATA(lt_data_subtotal_cal).

        LOOP AT lt_data_subtotal_cal INTO DATA(ls_data_subtotal_cal).
          MOVE-CORRESPONDING ls_data_subtotal_cal TO ls_data_subtotal.

          ls_data_subtotal-is_subtotal = 'X'.

          IF ls_data_subtotal-amount_beg_credit_local > ls_data_subtotal-amount_beg_debit_local.
            ls_data_subtotal-amount_beg_credit       -= ls_data_subtotal-amount_beg_debit.
            ls_data_subtotal-amount_beg_credit_local -= ls_data_subtotal-amount_beg_debit_local.
            CLEAR: ls_data_subtotal-amount_beg_debit,
                   ls_data_subtotal-amount_beg_debit_local.
          ELSE.
            ls_data_subtotal-amount_beg_debit       -= ls_data_subtotal-amount_beg_credit.
            ls_data_subtotal-amount_beg_debit_local -= ls_data_subtotal-amount_beg_credit_local.
            CLEAR: ls_data_subtotal-amount_beg_credit,
                   ls_data_subtotal-amount_beg_credit_local.
          ENDIF.

          IF ls_data_subtotal-amount_end_credit_local > ls_data_subtotal-amount_end_debit_local.
            ls_data_subtotal-amount_end_credit       -= ls_data_subtotal-amount_end_debit.
            ls_data_subtotal-amount_end_credit_local -= ls_data_subtotal-amount_end_debit_local.
            CLEAR: ls_data_subtotal-amount_end_debit,
                   ls_data_subtotal-amount_end_debit_local.
          ELSE.
            ls_data_subtotal-amount_end_debit       -= ls_data_subtotal-amount_end_credit.
            ls_data_subtotal-amount_end_debit_local -= ls_data_subtotal-amount_end_credit_local.
            CLEAR: ls_data_subtotal-amount_end_credit,
                   ls_data_subtotal-amount_end_credit_local.
          ENDIF.

          APPEND ls_data_subtotal TO lt_data.
          CLEAR: ls_data_subtotal.
        ENDLOOP.

        CLEAR: lt_data_subtotal.
      ENDAT.
      AT LAST.
        SELECT
          currency,
          currency_local,
          SUM( amount_beg_debit ) AS amount_beg_debit,
          SUM( amount_beg_debit_local ) AS amount_beg_debit_local,
          SUM( amount_beg_credit ) AS amount_beg_credit     ,
          SUM( amount_beg_credit_local ) AS amount_beg_credit_local,
          SUM( amount_debit ) AS amount_debit    ,
          SUM( amount_debit_local ) AS amount_debit_local    ,
          SUM( amount_credit ) AS amount_credit   ,
          SUM( amount_credit_local ) AS amount_credit_local   ,
          SUM( amount_end_debit ) AS amount_end_debit,
          SUM( amount_end_debit_local ) AS amount_end_debit_local,
          SUM( amount_end_credit ) AS amount_end_credit     ,
          SUM( amount_end_credit_local ) AS amount_end_credit_local
          FROM @lt_data_total AS data
          GROUP BY
          currency,
          currency_local
          INTO TABLE @DATA(lt_data_total_cal).

        LOOP AT lt_data_total_cal INTO DATA(ls_data_total_cal).
          MOVE-CORRESPONDING ls_data_total_cal TO ls_data_total.
          ls_data_total-is_total          = 'X'.

          IF ls_data_total-amount_beg_credit_local > ls_data_total-amount_beg_debit_local.
            ls_data_total-amount_beg_credit -= ls_data_total-amount_beg_debit.
            ls_data_total-amount_beg_credit_local -= ls_data_total-amount_beg_debit_local.
            CLEAR: ls_data_total-amount_beg_debit,
                   ls_data_total-amount_beg_debit_local.
          ELSE.
            ls_data_total-amount_beg_debit -= ls_data_total-amount_beg_credit.
            ls_data_total-amount_beg_debit_local -= ls_data_total-amount_beg_credit_local.
            CLEAR: ls_data_total-amount_beg_credit,
                   ls_data_total-amount_beg_credit_local.
          ENDIF.

          IF ls_data_total-amount_end_credit_local > ls_data_total-amount_end_debit_local.
            ls_data_total-amount_end_credit -= ls_data_total-amount_end_debit.
            ls_data_total-amount_end_credit_local -= ls_data_total-amount_end_debit_local.
            CLEAR: ls_data_total-amount_end_debit,
                   ls_data_total-amount_end_debit_local.
          ELSE.
            ls_data_total-amount_end_debit -= ls_data_total-amount_end_credit.
            ls_data_total-amount_end_debit_local -= ls_data_total-amount_end_credit_local.
            CLEAR: ls_data_total-amount_end_credit,
                   ls_data_total-amount_end_credit_local.
          ENDIF.

          APPEND ls_data_total TO lt_data.
          CLEAR: ls_data_total.
        ENDLOOP.
      ENDAT.
      CLEAR: ls_data.
    ENDLOOP.

    SORT lt_data BY is_total     ASCENDING
                    accountgroup ASCENDING
                    is_subtotal  DESCENDING
                    cus_sup      ASCENDING.

    et_data = lt_data.
  ENDMETHOD.


  METHOD get_instance.
    IF instance IS INITIAL.
      CREATE OBJECT instance.
    ENDIF.
    ro_instance = instance.
  ENDMETHOD.
ENDCLASS.
