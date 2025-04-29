@EndUserText.label: 'Báo cáo tổng hợp công nợ PTPT'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_FI_BCTHCNPTPT'
@UI: {
    headerInfo: {
        typeName: 'Báo cáo tổng hợp công nợ',
        typeNamePlural: 'Báo cáo tổng hợp công nợ',
        title: {
            type: #STANDARD,
            label: 'Báo cáo tổng hợp công nợ'
        }
    }
}
define root custom entity ZI_FI_BCTHCNPTPT
  with parameters
    @Consumption.valueHelpDefinition: [{ entity: {
    name: 'ZFI_I_DISPLAY',
    element: 'value_low'
    } }]
    @EndUserText.label: 'Hiển thị theo'
    OnDisplay                : zfi_de_display,
    @Consumption.valueHelpDefinition: [{ entity: {
    name: 'ZFI_I_BOOLEAN_VH',
    element: 'value_low'
    } }]
    @EndUserText.label: 'Include Reversed Documents'
    IncludeReversedDocuments : zde_boolean,
    @Consumption.valueHelpDefinition: [{ entity: {
    name: 'ZFI_I_BOOLEAN_VH',
    element: 'value_low'
    } }]
    @EndUserText.label: 'Chứng từ cấn trừ'
    DocType                  : zde_boolean,
    @Consumption.valueHelpDefinition: [{ entity: {
    name: 'ZFI_I_YES_NO_VH',
    element: 'value_low'
    } }]
    @EndUserText.label: 'Ngày giờ in'
    PrintDate                : zfi_de_print_date,
    @Environment.systemField:#SYSTEM_DATE
    @EndUserText.label: 'Từ ngày'
    FromDate                 : vdm_v_key_date,
    @Environment.systemField:#SYSTEM_DATE
    @EndUserText.label: 'Đến ngày'
    ToDate                   : vdm_v_key_date,
    @Consumption.valueHelpDefinition: [{ entity: {
    name: 'I_CompanyCodeStdVH',
    element: 'CompanyCode'
    }}]
    @EndUserText.label: 'Company Code'
    CompanyCode              : bukrs,
    @EndUserText.label: 'Fiscal Year'
    @Semantics.calendar.year: true
    FiscalYear               : fis_gjahr_no_conv,
    @Consumption.valueHelpDefinition: [{ entity: {
    name: 'ZFI_I_CURR_TYPE',
    element: 'value_low'
    } }]
    @EndUserText.label: 'Loại mẫu báo cáo'
    CurrencyType             : zde_curr_type
{
      @Consumption.valueHelpDefinition: [ {
      entity                  :{
      name                    :'ZI_FI_ACCOUNT_GROUP_VH',
      element                 :'AccountGroup' }
      }]
      @UI                     : {
      selectionField          : [ { position: 10 } ] }
      @EndUserText.label      : 'Account Group'
  key AccountGroup            : abap.char(4);
      @Consumption.valueHelpDefinition: [ {
      entity                  :{
      name                    :'ZI_FI_CUSTOMER_SUPPLIER_VH',
      element                 :'BusinessPartner' },
      additionalBinding       : [
      {element                : 'BusinessPartnerGrouping', localElement: 'AccountGroup', usage: #FILTER_AND_RESULT}
      ]
      }]
      @UI                     : {
      selectionField          : [ { position: 11 } ] }
      @EndUserText.label      : 'Customer/Supplier'
  key Cus_Sup                 : zde_cus_sup;
      //      @Consumption.valueHelpDefinition: [ {
      //      entity                  :{
      //      name                    :'I_Customer_VH',
      //      element                 :'Customer' }
      //      }]
      //      @UI                     : {
      //      selectionField          : [ { position: 10 } ] }
      //      @EndUserText.label      : 'Customer'
      Customer                : zde_kunnr;
      //      @Consumption.valueHelpDefinition: [ {
      //      entity                  :{
      //      name                    :'I_Supplier_VH',
      //      element                 :'Supplier' }
      //      }]
      //      @UI                     : {
      //      selectionField          : [ { position: 11 } ] }
      //      @EndUserText.label      : 'Supplier'
      Supplier                : lifnr;
      Is_total                : abap.char(1);
      Is_subtotal             : abap.char(1);
      AccountGroupText        : abap.char(30);
      Cus_Sup_Name            : abap.char(255);
      @Consumption.valueHelpDefinition: [ {
      entity                  :{ name :'I_GLAccountInCompanyCodeStdVH', element :'GLAccount' },
      additionalBinding       : [ { element :'CompanyCode' , localParameter: 'CompanyCode', usage : #FILTER_AND_RESULT }]
      } ]
      @UI                     : {
      selectionField          : [ { position: 13 } ] }
      @EndUserText.label      : 'G/L Account'
      GLAccount               : zde_racct;
      @Consumption.valueHelpDefinition: [ {
      entity                  :{
      name                    :'I_CurrencyStdVH',
      element                 :'Currency' }
      }]
      @UI                     : {
      selectionField          : [ { position: 09 } ] }
      @EndUserText.label      : 'Currency'
      @Semantics.currencyCode : true
      Currency                : zde_curr;
      @Semantics.currencyCode : true
      Currency_Local          : zde_curr;
      @Semantics.amount.currencyCode: 'currency'
      Amount_Beg_Debit        : abap.curr( 23, 2 );
      @Semantics.amount.currencyCode: 'Currency_Local'
      Amount_Beg_Debit_Local  : abap.curr( 23, 2 );
      @Semantics.amount.currencyCode: 'currency'
      Amount_Beg_Credit       : abap.curr( 23, 2 );
      @Semantics.amount.currencyCode: 'Currency_Local'
      Amount_Beg_Credit_Local : abap.curr( 23, 2 );
      @Semantics.amount.currencyCode: 'currency'
      Amount_Debit            : abap.curr( 23, 2 );
      @Semantics.amount.currencyCode: 'Currency_Local'
      Amount_Debit_Local      : abap.curr( 23, 2 );
      @Semantics.amount.currencyCode: 'currency'
      Amount_Credit           : abap.curr( 23, 2 );
      @Semantics.amount.currencyCode: 'Currency_Local'
      Amount_Credit_Local     : abap.curr( 23, 2 );
      @Semantics.amount.currencyCode: 'currency'
      Amount_end_Debit        : abap.curr( 23, 2 );
      @Semantics.amount.currencyCode: 'Currency_Local'
      Amount_end_Debit_Local  : abap.curr( 23, 2 );
      @Semantics.amount.currencyCode: 'currency'
      Amount_end_Credit       : abap.curr( 23, 2 );
      @Semantics.amount.currencyCode: 'Currency_Local'
      Amount_end_Credit_Local : abap.curr( 23, 2 );
      financialaccounttype    : abap.char( 1 );
}
