import contract from '@/api/print/contract'
import mes from '@/api/print/mes'
// import costCenter from '@/api/print/cost-center'
// import contractDetail from '@/api/print/contract-detail'
// import supplier from '@/api/print/supplier'
// import steelPlan from '@/api/print/steel/plan'
// import steelLogistics from '@/api/print/steel/logistics'
// import mesLogistics from '@/api/print/mes-logistics'
// import steelBusiness from '@/api/print/steel/business'
// import steelProduction from '@/api/print/steel/production'
// import steelReport from '@/api/print/steel/report'
// import wms from '@/api/print/wms'
// import wmsLogistics from '@/api/print/wms-logistics'
// import bridgePlan from '@/api/print/bridge/plan'
// import bridgeBusiness from '@/api/print/bridge/business'
// import bridgeLogistics from '@/api/print/bridge/logistics'
// import bridgeProduction from '@/api/print/bridge/production'

const CONTRACT_LEDGER = contract.contractLedger
// const PROJECT_COLLECTION_SUMMARY = contract.projectCollectionSummary
// const PROJECT_COLLECTION_DETAIL = contract.projectCollectionDetail
// const PROJECT_COLLECTION_TYPE_DETAIL = contract.projectCollectionTypeDetail
// const PROJECT_INVOICE_DETAIL = contract.projectInvoiceDetail
// const PROJECT_ARREARS_WARNING = contract.projectArrearsWarning
// const COMPANY_COLLECTION_PLAN = contract.companyCollectionPlan
// const COMPANY_COLLECTION_PLAN_EXECUTION = contract.companyCollectionPlanExecution
// const PROJECT_REIMBURSEMENT = contract.projectReimbursement
// const SHIP_INFO = contract.shipInfo
// const PROJECT_EXECUTION_LEDGER = contract.projectLedger

// const PROJECT_MATERIAL_COSTS_DETAIL = contractDetail.materialCost

// const PROJECT_DELIVER_AMOUNT = costCenter.projectDeliverDetail
// const PROJECT_MANAGEMENT_FEE = costCenter.projectManagementFee
// const PROJECT_MAIN_MATERIAL_CONT = costCenter.projectMainMaterial
// const PROJECT_MAIN_MATERIAL_RETURN_CONT = costCenter.projectMainMaterialReturn
// const PROJECT_AUXILIARY_MAIN_MATERIAL_CONT = costCenter.projectAuxiliaryMaterial
// const PROJECT_AUXILIARY_MAIN_MATERIAL_RETURN_CONT = costCenter.projectAuxiliaryMaterialReturn
// const PROJECT_PRODUCTION_CONT = costCenter.projectProduction

// const MATERIAL_SUPPLIER_PAYMENT_DETAIL = supplier.materialSupplierPaymentDetail
// const LOGISTICS_SUPPLIER_PAYMENT_DETAIL = supplier.logisticsSupplierPaymentDetail
// const MATERIAL_SUPPLIER_INVOICE_DETAIL = supplier.materialSupplierInvoiceDetail
// const LOGISTICS_SUPPLIER_INVOICE_DETAIL = supplier.logisticsSupplierInvoiceDetail
// const MATERIAL_SUPPLIER_STATEMENT = supplier.materialSupplierStatement
// const LOGISTICS_SUPPLIER_STATEMENT = supplier.logisticsSupplierStatement
// const MATERIAL_PAYMENT_AND_INVOICE = supplier.materialSupplierPaymentAndInvoice

// const MES_LOGISTICS_ORDER_DELIVERY_DETAIL = mesLogistics.manufacturesOrderDeliveryDetail

// const WMS_IN_WAREHOUSE_VOUCHER = wms.inWarehouseRecord
// const WMS_OUT_WAREHOUSE_RECORD_SUMMARY = wms.outWarehouseRecordSummary
// const WMS_RETURN_WAREHOUSE_VOUCHER = wms.returnWarehouseRecord
// const WMS_QUIT_WAREHOUSE_VOUCHER = wms.quitWarehouseRecord
// const WMS_MATERIAL_MOVE_HOUSE_VOUCHER = wms.moveWarehouseRecord

// const WMS_IN_RECORD_SUMMARY_DETAIL = wms.inWarehouseRecordSummaryDetail
// const WMS_OUT_RECORD_SUMMARY_DETAIL = wms.outWarehouseRecordSummaryDetail

// const WMS_PREPARES_CUSTOM_SUMMARY = wms.preparesCustomSummary
// const WMS_PREPARES_CUSTOM_STEEL_PLATE = wms.preparesCustomSteelPlate
// const WMS_PREPARES_CUSTOM_SECTION_STEEL = wms.preparesCustomSectionSteel
// const WMS_PREPARES_CUSTOM_STEEL_COIL = wms.preparesCustomSteelCoil
// const WMS_PREPARES_CUSTOM_ENCLOSURE = wms.preparesCustomEnclosure
// const WMS_PREPARES_CUSTOM_MATERIAL = wms.preparesCustomMaterial
// const WMS_PREPARES_TRACK_STEEL_PLATE = wms.preparesTrackSteelPlate
// const WMS_PREPARES_TRACK_SECTION_STEEL = wms.preparesTrackSectionSteel
// const WMS_PREPARES_TRACK_STEEL_COIL = wms.preparesTrackSteelCoil
// const WMS_PREPARES_TRACK_ENCLOSURE = wms.preparesTrackEnclosure
// const WMS_PREPARES_TRACK_MATERIAL = wms.preparesTrackMaterial
// const WMS_REPORT_IN_SUMMARY = wms.reportSummary
// const WMS_REPORT_OUT_SUMMARY = wms.reportSummary
// const WMS_REPORT_INVENTORY_SUMMARY = wms.reportSummary
// const WMS_INVENTORY_MANAGE_SUMMARY = wms.inventoryManagement
// const WMS_INVENTORY_MANAGE_END_SUMMARY_DETAIL = wms.inventoryManagementEndSummaryDetail
// const WMS_INVENTORY_MANAGE_INBOUND_SUMMARY_DETAIL = wms.inventoryManagementboundSummaryDetail
// const WMS_INVENTORY_MANAGE_OUTBOUND_SUMMARY_DETAIL = wms.inventoryManagementboundSummaryDetail
// const WMS_INVENTORY_MANAGE_BEGIN_SUMMARY = wms.inventoryManagementDetail
// const WMS_INVENTORY_MANAGE_BEGIN_GM_SUMMARY = wms.inventoryManagementDetail
// const WMS_INVENTORY_MANAGE_IN_SUMMARY = wms.inventoryManagementDetail
// const WMS_INVENTORY_MANAGE_OUT_SUMMARY = wms.inventoryManagementDetail
// const WMS_INVENTORY_MANAGE_END_SUMMARY = wms.inventoryManagementDetail

// const WMS_LOGISTICS_DELIVERY_METE = wmsLogistics.deliveryMete
// const WMS_LOGISTICS_ORDER_MATERIAL_DETAIL = wmsLogistics.orderMaterialDetail

// const BRIDGE_MES_AREA = bridgePlan.area
// const BRIDGE_MES_AREA_PLAN = bridgePlan.plan

// const BRIDGE_MES_BUSINESS_ENTRY_BOX = bridgeBusiness.boxEntry
// const BRIDGE_MES_BUSINESS_ENTRY_SINGLE_ELEMENT = bridgeBusiness.singleElementEntry
// const BRIDGE_MES_BUSINESS_ENTRY_AUXILIARY_MATERIAL = bridgeBusiness.auxiliaryMaterialEntry
// const BRIDGE_MES_BUSINESS_OUTBOUND_PROJECT = bridgeBusiness.outbound
// const BRIDGE_MES_BUSINESS_OUTBOUND_MONOMER = bridgeBusiness.outbound
// const BRIDGE_MES_BUSINESS_INSTALLATION_PROJECT_BOX = bridgeBusiness.boxInstallation
// const BRIDGE_MES_BUSINESS_INSTALLATION_PROJECT_SINGLE_ELEMENT = bridgeBusiness.singleElementInstallation
// const BRIDGE_MES_BUSINESS_INSTALLATION_MONOMER_BOX = bridgeBusiness.boxInstallation
// const BRIDGE_MES_BUSINESS_INSTALLATION_MONOMER_SINGLE_ELEMENT = bridgeBusiness.singleElementInstallation
// const BRIDGE_MES_BUSINESS_INSTALLATION_AUXILIARY_MATERIAL = bridgeBusiness.auxiliaryMaterialInstallation
// const BRIDGE_MES_BUSINESS_TRIP_TRACKING = bridgeBusiness.tripTracking

// const BRIDGE_MES_PACK = bridgeLogistics.pack
// const BRIDGE_MES_PACK_SHIP = bridgeLogistics.ship
// const BRIDGE_MES_PACK_SHIP_DETAIL = bridgeLogistics.shipDetail
// const BRIDGE_MES_PACK_RECEIPT = bridgeLogistics.receipt
// const BRIDGE_MES_PACK_SHIPMENT_DETAIL = bridgeLogistics.shipmentDetail
// const BRIDGE_MES_LOGISTICS_SUMMARY = bridgeLogistics.logistics

// const BRIDGE_MES_TASK_BOX = bridgeProduction.boxProductionLineTask
// const BRIDGE_MES_TASK_MACHINE_PART = bridgeProduction.machinePartProductionLineTask
// const BRIDGE_MES_TASK_SINGLE_ELEMENT = bridgeProduction.elementProductionLineTask
// const BRIDGE_MES_PRODUCTION_STATE_BOX = bridgeProduction.processCompleteState
// const BRIDGE_MES_PRODUCTION_STATE_SINGLE_ELEMENT = bridgeProduction.processCompleteState
// const BRIDGE_MES_PRODUCTION_STATE_MACHINE_PART = bridgeProduction.processCompleteState

// const BRIDGE_MES_WAREHOUSE_STATE_BOX = bridgeProduction.warehouseStateBox
// const BRIDGE_MES_WAREHOUSE_STATE_SINGLE_ELEMENT = bridgeProduction.warehouseStateElement
// const BRIDGE_MES_WAREHOUSE_STATE_AUXILIARY_MATERIAL = bridgeProduction.warehouseStateAuxiliaryMaterial
// const BRIDGE_MES_SITE_WAREHOUSE_STATE_BOX = bridgeProduction.siteWarehouseStateBox
// const BRIDGE_MES_SITE_WAREHOUSE_STATE_SINGLE_ELEMENT = bridgeProduction.siteWarehouseStateElement
// const BRIDGE_MES_SITE_WAREHOUSE_STATE_AUXILIARY_MATERIAL = bridgeProduction.siteWarehouseStateAuxiliaryMaterial
// const BRIDGE_MES_COMPLEX_STATE_BOX = bridgeProduction.complexStateBox
// const BRIDGE_MES_COMPLEX_STATE_SINGLE_ELEMENT = bridgeProduction.complexStateElement
// const BRIDGE_MES_COMPLEX_STATE_AUXILIARY_MATERIAL = bridgeProduction.complexStateAuxiliaryMaterial
// const BRIDGE_MES_REPORT_PROJECT_PRODUCTION = bridgeProduction.productionMeteDetail
// const BRIDGE_MES_REPORT_FACTORY_PRODUCTION = bridgeProduction.productionMeteDetail
// const BRIDGE_MES_REPORT_TEAM_OUTPUT_BOX = bridgeProduction.productionTeamDetail
// const BRIDGE_MES_REPORT_TEAM_OUTPUT_MACHINE_PART = bridgeProduction.productionTeamDetail
// const BRIDGE_MES_REPORT_TEAM_OUTPUT_SINGLE_ELEMENT = bridgeProduction.productionTeamDetail
// const BRIDGE_MES_REPORT_OUTBOUND = bridgeProduction.outboundMeteDetail
// const BRIDGE_MES_REPORT_OUTBOUND_PROJECT = bridgeProduction.outboundMeteDetail
// const BRIDGE_MES_REPORT_OUTBOUND_AUXILIARY_MATERIAL = bridgeProduction.auxiliaryMaterialOutbound
// const BRIDGE_MES_REPORT_OUTBOUND_PROJECT_AUXILIARY_MATERIAL = bridgeProduction.auxiliaryMaterialOutbound
// const BRIDGE_MES_REPORT_INSTALLATION = bridgeProduction.installationDetail
// const BRIDGE_MES_REPORT_TEAM_PIECE_WAGE = bridgeProduction.teamPieceWage
// const BRIDGE_MES_REPORT_OUTPUT_WAGE = bridgeProduction.outputWage

// const STEEL_MES_AREA = steelPlan.area
// const STEEL_MES_AREA_PLAN = steelPlan.plan

const mesPackingList = mes.packingList
const mesShipmentSummary = mes.shipmentSummary
const mesShipmentDetail = mes.shipmentDetail
const mesReceiptStatusSummary = mes.receiptStatusSummary
const mesShippingList = mes.shippingList
const mesLogisticsSummary = mes.logisticsSummary

// const STEEL_MES_BUSINESS_ENTRY_STRUCTURE = steelBusiness.structureEntry
// const STEEL_MES_BUSINESS_ENTRY_ENCLOSURE = steelBusiness.enclosureEntry
// const STEEL_MES_BUSINESS_ENTRY_AUXILIARY_MATERIAL = steelBusiness.auxiliaryMaterialEntry
// const STEEL_MES_BUSINESS_OUTBOUND_PROJECT = steelBusiness.outbound
// const STEEL_MES_BUSINESS_OUTBOUND_MONOMER = steelBusiness.outbound
// const STEEL_MES_BUSINESS_INSTALLATION_PROJECT = steelBusiness.installation
// const STEEL_MES_BUSINESS_INSTALLATION_MONOMER = steelBusiness.installation
// const STEEL_MES_BUSINESS_TRIP_TRACKING = steelBusiness.tripTracking

// const STEEL_MES_TASK_STRUCTURE = steelProduction.artifactProductionLineTask
// const STEEL_MES_TASK_MACHINE_PART = steelProduction.machinePartProductionLineTask
// const STEEL_MES_TASK_ENCLOSURE = steelProduction.enclosureProductionLineTask
// const STEEL_MES_PRODUCTION_STATE_STRUCTURE = steelProduction.processCompleteState
// const STEEL_MES_PRODUCTION_STATE_ENCLOSURE = steelProduction.processCompleteState
// const STEEL_MES_PRODUCTION_STATE_MACHINE_PART = steelProduction.processCompleteState

// const STEEL_MES_WAREHOUSE_STATE_STRUCTURE = steelProduction.warehouseStateArtifact
// const STEEL_MES_WAREHOUSE_STATE_ENCLOSURE = steelProduction.warehouseStateEnclosure
// const STEEL_MES_WAREHOUSE_STATE_AUXILIARY_MATERIAL = steelProduction.warehouseStateAuxiliaryMaterial
// const STEEL_MES_WAREHOUSE_STATE_REPORT = steelReport.warehouseStateReport
// const STEEL_MES_WAREHOUSE_STATE_REPORT_PRODUCT_ENCLOSURE = steelReport.warehouseStateReportDetail
// const STEEL_MES_WAREHOUSE_STATE_REPORT_PRODUCT_STRUCTURE = steelReport.warehouseStateReportDetail
// const STEEL_MES_WAREHOUSE_STATE_REPORT_SUMMARY_ENCLOSURE = steelReport.warehouseStateReportDetail
// const STEEL_MES_WAREHOUSE_STATE_REPORT_SUMMARY_STRUCTURE = steelReport.warehouseStateReportDetail
// const STEEL_MES_SITE_WAREHOUSE_STATE_STRUCTURE = steelProduction.siteWarehouseStateArtifact
// const STEEL_MES_SITE_WAREHOUSE_STATE_ENCLOSURE = steelProduction.siteWarehouseStateEnclosure
// const STEEL_MES_SITE_WAREHOUSE_STATE_AUXILIARY_MATERIAL = steelProduction.siteWarehouseStateAuxiliaryMaterial
// const STEEL_MES_COMPLEX_STATE_STRUCTURE = steelProduction.complexStateArtifact
// const STEEL_MES_COMPLEX_STATE_ENCLOSURE = steelProduction.complexStateEnclosure
// const STEEL_MES_COMPLEX_STATE_AUXILIARY_MATERIAL = steelProduction.complexStateAuxiliaryMaterial
// const STEEL_MES_REPORT_PROJECT_PRODUCTION_STRUCTURE = steelProduction.productionMeteDetail
// const STEEL_MES_REPORT_FACTORY_PRODUCTION_STRUCTURE = steelProduction.productionMeteDetail
// const STEEL_MES_REPORT_PROJECT_PRODUCTION_ENCLOSURE = steelProduction.productionMeteDetail
// const STEEL_MES_REPORT_FACTORY_PRODUCTION_ENCLOSURE = steelProduction.productionMeteDetail
// const STEEL_MES_REPORT_TEAM_OUTPUT_STRUCTURE = steelProduction.productionTeamDetail
// const STEEL_MES_REPORT_TEAM_OUTPUT_MACHINE_PART = steelProduction.productionTeamDetail
// const STEEL_MES_REPORT_TEAM_OUTPUT_ENCLOSURE = steelProduction.productionTeamDetail
// const STEEL_MES_REPORT_OUTBOUND_STRUCTURE = steelProduction.outboundMeteDetail
// const STEEL_MES_REPORT_OUTBOUND_PROJECT_STRUCTURE = steelProduction.outboundMeteDetail
// const STEEL_MES_REPORT_OUTBOUND_ENCLOSURE = steelProduction.outboundMeteDetail
// const STEEL_MES_REPORT_OUTBOUND_PROJECT_ENCLOSURE = steelProduction.outboundMeteDetail
// const STEEL_MES_REPORT_OUTBOUND_AUXILIARY_MATERIAL = steelProduction.auxiliaryMaterialOutbound
// const STEEL_MES_REPORT_OUTBOUND_PROJECT_AUXILIARY_MATERIAL = steelProduction.auxiliaryMaterialOutbound
// const STEEL_MES_REPORT_INSTALLATION_STRUCTURE = steelProduction.installationDetail
// const STEEL_MES_REPORT_INSTALLATION_ENCLOSURE = steelProduction.installationDetail
// const STEEL_MES_REPORT_TEAM_PIECE_WAGE_STRUCTURE = steelProduction.teamPieceWage
// const STEEL_MES_REPORT_TEAM_PIECE_WAGE_ENCLOSURE = steelProduction.teamPieceWage
// const STEEL_MES_REPORT_OUTPUT_WAGE = steelProduction.outputWage
// const STEEL_MES_REPORT_STEEL_DOSAGE = steelReport.steelDosage

export default {
  CONTRACT_LEDGER, // 合同台账

  // mes
  mesPackingList, // 打包清单
  mesShipmentSummary, // 发运汇总
  mesShipmentDetail, // 发运详情
  mesReceiptStatusSummary, // 收货状态汇总
  mesShippingList, // 发货清单
  mesLogisticsSummary // 物流汇总
}
