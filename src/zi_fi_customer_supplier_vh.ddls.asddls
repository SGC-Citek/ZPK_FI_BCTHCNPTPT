@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer/Suplier Value Help'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FI_CUSTOMER_SUPPLIER_VH
  as select from I_BusinessPartner as BusinessPartner
{
  key BusinessPartner.BusinessPartner,
      BusinessPartner.BusinessPartnerGrouping,
      BusinessPartner.BusinessPartnerCategory,
      BusinessPartner.BusinessPartnerIDByExtSystem,
      BusinessPartner.FirstName,
      BusinessPartner.LastName,
      BusinessPartner.OrganizationBPName1,
      BusinessPartner.GroupBusinessPartnerName1,
      BusinessPartner.BirthDate
}
