@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Báo cáo tổng hợp công nợ - Header'
define view entity ZI_FI_BCTHCNPTPT_HEADER
  with parameters P_CompanyCode : bukrs
  as select from ZCORE_I_PROFILE_COMPANYCODE
{
  key CompanyCode,
      ShortName,
      LongName,
      Address,
      VATNumber
}
where CompanyCode = $parameters.P_CompanyCode
