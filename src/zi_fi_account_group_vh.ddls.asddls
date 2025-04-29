@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Account Group Value Help'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_FI_ACCOUNT_GROUP_VH
  as select distinct from I_CustomerAccountGroupText
{
  key CustomerAccountGroup as AccountGroup,
      AccountGroupName
}
where
  Language = $session.system_language
union select distinct from I_SupplierAccountGroupText
{
  key SupplierAccountGroup as AccountGroup,
      AccountGroupName
}
where
  Language = $session.system_language
