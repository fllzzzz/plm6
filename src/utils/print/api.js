// import contract from '@/api/print/contract'
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

// const CONTRACT_LEDGER = contract.contractLedger
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

// const STEEL_MES_PACK = steelLogistics.pack
// const STEEL_MES_PACK_SHIP = steelLogistics.ship
// const STEEL_MES_PACK_SHIP_DETAIL = steelLogistics.shipDetail
// const STEEL_MES_PACK_RECEIPT = steelLogistics.receipt
// const STEEL_MES_PACK_SHIPMENT_DETAIL = steelLogistics.shipmentDetail
// const STEEL_MES_LOGISTICS_SUMMARY = steelLogistics.logistics

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

// export default {
//   CONTRACT_LEDGER, // 合同台账
//   PROJECT_COLLECTION_SUMMARY, // 项目收款汇总
//   PROJECT_COLLECTION_DETAIL, // 项目收款明细
//   PROJECT_COLLECTION_TYPE_DETAIL, // 项目收款分类明细
//   PROJECT_INVOICE_DETAIL, // 项目开票明细
//   PROJECT_ARREARS_WARNING, // 项目欠款预警
//   COMPANY_COLLECTION_PLAN, // 项目收款计划（月）
//   COMPANY_COLLECTION_PLAN_EXECUTION, // 项目收款计划执行（月）
//   PROJECT_REIMBURSEMENT, // 项目报销记录
//   SHIP_INFO, // 发运信息
//   PROJECT_EXECUTION_LEDGER, // 项目台账

//   PROJECT_MATERIAL_COSTS_DETAIL, // 材料成本

//   PROJECT_DELIVER_AMOUNT, // 项目发货额
//   PROJECT_MANAGEMENT_FEE, // 项目管理费
//   PROJECT_MAIN_MATERIAL_CONT, // 项目主材出库成本
//   PROJECT_MAIN_MATERIAL_RETURN_CONT, // 项目主材还库成本
//   PROJECT_AUXILIARY_MAIN_MATERIAL_CONT, // 项目辅材出库成本
//   PROJECT_AUXILIARY_MAIN_MATERIAL_RETURN_CONT, // 项目辅材还库成本
//   PROJECT_PRODUCTION_CONT, // 项目人工成本

//   MATERIAL_SUPPLIER_PAYMENT_DETAIL, // 物料供应商付款记录
//   LOGISTICS_SUPPLIER_PAYMENT_DETAIL, // 物流供应商付款记录
//   MATERIAL_SUPPLIER_INVOICE_DETAIL, // 物料供应商开票记录
//   LOGISTICS_SUPPLIER_INVOICE_DETAIL, // 物流供应商开票记录
//   MATERIAL_SUPPLIER_STATEMENT, // 物料供应商对账单
//   LOGISTICS_SUPPLIER_STATEMENT, // 物流供应商对账单
//   MATERIAL_PAYMENT_AND_INVOICE, // 物料供应商付款及开票
//   WMS_IN_WAREHOUSE_VOUCHER, // wms-出入库记录-入库记录
//   WMS_OUT_WAREHOUSE_RECORD_SUMMARY, // wms-出入库记录-出库汇总单
//   WMS_RETURN_WAREHOUSE_VOUCHER, // wms-出入库记录-还库单
//   WMS_QUIT_WAREHOUSE_VOUCHER, // wms-出入库记录-退库单
//   WMS_MATERIAL_MOVE_HOUSE_VOUCHER, // wms-出入库记录-物料调拨单
//   WMS_LOGISTICS_DELIVERY_METE, // wms-物流汇总单（车牌，装载量）
//   WMS_LOGISTICS_ORDER_MATERIAL_DETAIL, // wms-订单收货详情

//   WMS_IN_RECORD_SUMMARY_DETAIL, // wms-入库记录-汇总明细
//   WMS_OUT_RECORD_SUMMARY_DETAIL, // wms-出库记录-汇总明细

//   WMS_PREPARES_CUSTOM_SUMMARY, // wms-备料定制-物料备料汇总单
//   WMS_PREPARES_CUSTOM_STEEL_PLATE, // wms-备料定制-钢板采购清单
//   WMS_PREPARES_CUSTOM_SECTION_STEEL, // wms-备料定制-型材采购清单
//   WMS_PREPARES_CUSTOM_STEEL_COIL, // wms-备料定制-钢卷采购清单
//   WMS_PREPARES_CUSTOM_ENCLOSURE, // wms-备料定制-成品围护采购清单
//   WMS_PREPARES_CUSTOM_MATERIAL, // wms-备料定制-辅材采购清单
//   WMS_PREPARES_TRACK_STEEL_PLATE, // wms-备料跟踪-钢板备料跟踪单
//   WMS_PREPARES_TRACK_SECTION_STEEL, // wms-备料跟踪-型材备料跟踪单
//   WMS_PREPARES_TRACK_STEEL_COIL, // wms-备料跟踪-钢卷备料跟踪单
//   WMS_PREPARES_TRACK_ENCLOSURE, // wms-备料跟踪-成品围护备料跟踪单
//   WMS_PREPARES_TRACK_MATERIAL, // wms-备料跟踪-辅材备料跟踪单
//   WMS_REPORT_IN_SUMMARY, // wms-报表中心-入库汇总
//   WMS_REPORT_OUT_SUMMARY, // wms-报表中心-出库汇总
//   WMS_REPORT_INVENTORY_SUMMARY, // wms-报表中心-库存汇总
//   WMS_INVENTORY_MANAGE_SUMMARY, // 物料盘存汇总表
//   WMS_INVENTORY_MANAGE_END_SUMMARY_DETAIL, // 物料盘存-当前库存汇总明细表
//   WMS_INVENTORY_MANAGE_INBOUND_SUMMARY_DETAIL, // 物料盘存-购入量汇总明细表
//   WMS_INVENTORY_MANAGE_OUTBOUND_SUMMARY_DETAIL, // 物料盘存-使用量汇总明细表
//   WMS_INVENTORY_MANAGE_BEGIN_SUMMARY, // 期初库存汇总表
//   WMS_INVENTORY_MANAGE_BEGIN_GM_SUMMARY, // 期初库存汇总表-辅材
//   WMS_INVENTORY_MANAGE_IN_SUMMARY, // 原材料入库汇总表
//   WMS_INVENTORY_MANAGE_OUT_SUMMARY, // 原材料出库汇总表
//   WMS_INVENTORY_MANAGE_END_SUMMARY, // 期末库存汇总表

//   MES_LOGISTICS_ORDER_DELIVERY_DETAIL, // 制成品物流订单发运详情

//   BRIDGE_MES_AREA, // 桥梁-区域
//   BRIDGE_MES_AREA_PLAN, // 桥梁-计划

//   BRIDGE_MES_BUSINESS_ENTRY_BOX, // 桥梁-箱体清单计价表
//   BRIDGE_MES_BUSINESS_ENTRY_SINGLE_ELEMENT, // 桥梁-单元件清单计价表
//   BRIDGE_MES_BUSINESS_ENTRY_AUXILIARY_MATERIAL, // 桥梁-辅材清单计价表
//   BRIDGE_MES_BUSINESS_OUTBOUND_PROJECT, // 桥梁-项目汇总出库表
//   BRIDGE_MES_BUSINESS_OUTBOUND_MONOMER, // 桥梁-单体汇总出库表
//   BRIDGE_MES_BUSINESS_INSTALLATION_PROJECT_BOX, // 桥梁-箱体项目安装表
//   BRIDGE_MES_BUSINESS_INSTALLATION_PROJECT_SINGLE_ELEMENT, // 桥梁-单元件项目安装表
//   BRIDGE_MES_BUSINESS_INSTALLATION_MONOMER_BOX, // 桥梁-箱体单体安装表
//   BRIDGE_MES_BUSINESS_INSTALLATION_MONOMER_SINGLE_ELEMENT, // 桥梁-单元件单体安装表
//   BRIDGE_MES_BUSINESS_INSTALLATION_AUXILIARY_MATERIAL, // 桥梁-辅材安装表
//   BRIDGE_MES_BUSINESS_TRIP_TRACKING, // 桥梁-单次发货统计表

//   BRIDGE_MES_PACK, // 桥梁-打包清单
//   BRIDGE_MES_PACK_SHIP, // 桥梁-发运汇总表
//   BRIDGE_MES_PACK_SHIP_DETAIL, // 桥梁-发运详情
//   BRIDGE_MES_PACK_RECEIPT, // 桥梁-收货状态汇总
//   BRIDGE_MES_PACK_SHIPMENT_DETAIL, // 桥梁-发货清单
//   BRIDGE_MES_LOGISTICS_SUMMARY, // 桥梁-物流汇总表

//   BRIDGE_MES_TASK_BOX, // 桥梁-箱体任务清单
//   BRIDGE_MES_TASK_MACHINE_PART, // 桥梁-零件任务清单
//   BRIDGE_MES_TASK_SINGLE_ELEMENT, // 桥梁-单元件任务清单
//   BRIDGE_MES_PRODUCTION_STATE_BOX, // 桥梁-箱体工序任务表
//   BRIDGE_MES_PRODUCTION_STATE_SINGLE_ELEMENT, // 桥梁-单元件工序任务表
//   BRIDGE_MES_PRODUCTION_STATE_MACHINE_PART, // 桥梁-零件工序任务表

//   BRIDGE_MES_WAREHOUSE_STATE_BOX, // 桥梁-箱体出入库统计表
//   BRIDGE_MES_WAREHOUSE_STATE_SINGLE_ELEMENT, // 桥梁-单元件出入库统计表
//   BRIDGE_MES_WAREHOUSE_STATE_AUXILIARY_MATERIAL, // 桥梁-辅材出入库统计表
//   BRIDGE_MES_SITE_WAREHOUSE_STATE_BOX, // 桥梁-箱体收安统计表
//   BRIDGE_MES_SITE_WAREHOUSE_STATE_SINGLE_ELEMENT, // 桥梁-单元件收安统计表
//   BRIDGE_MES_SITE_WAREHOUSE_STATE_AUXILIARY_MATERIAL, // 桥梁-辅材收安统计表
//   BRIDGE_MES_COMPLEX_STATE_BOX, // 桥梁-箱体总看板统计表
//   BRIDGE_MES_COMPLEX_STATE_SINGLE_ELEMENT, // 桥梁-单元件总看板统计表
//   BRIDGE_MES_COMPLEX_STATE_AUXILIARY_MATERIAL, // 桥梁-辅材总看板统计表

//   BRIDGE_MES_REPORT_PROJECT_PRODUCTION, // 桥梁-项目生产表
//   BRIDGE_MES_REPORT_FACTORY_PRODUCTION, // 桥梁-工厂生产表
//   BRIDGE_MES_REPORT_TEAM_OUTPUT_BOX, // 桥梁-班组箱体产量表
//   BRIDGE_MES_REPORT_TEAM_OUTPUT_MACHINE_PART, // 桥梁-班组零件产量表
//   BRIDGE_MES_REPORT_TEAM_OUTPUT_SINGLE_ELEMENT, // 桥梁-班组单元件产量表
//   BRIDGE_MES_REPORT_OUTBOUND, // 桥梁-全部项目出库表
//   BRIDGE_MES_REPORT_OUTBOUND_PROJECT, // 桥梁-项目出库表
//   BRIDGE_MES_REPORT_OUTBOUND_AUXILIARY_MATERIAL, // 桥梁-全部辅材出库表
//   BRIDGE_MES_REPORT_OUTBOUND_PROJECT_AUXILIARY_MATERIAL, // 桥梁-辅材项目出库表
//   BRIDGE_MES_REPORT_INSTALLATION, // 桥梁-箱体项目安装表
//   BRIDGE_MES_REPORT_TEAM_PIECE_WAGE, // 桥梁-班组计件工资表
//   BRIDGE_MES_REPORT_OUTPUT_WAGE, // 桥梁-产量工资表

//   STEEL_MES_AREA, // 建钢-区域
//   STEEL_MES_AREA_PLAN, // 建钢-计划

//   STEEL_MES_PACK, // 建钢-打包清单
//   STEEL_MES_PACK_SHIP, // 建钢-发运汇总表
//   STEEL_MES_PACK_SHIP_DETAIL, // 建钢-发运详情
//   STEEL_MES_PACK_RECEIPT, // 建钢-收货状态汇总
//   STEEL_MES_PACK_SHIPMENT_DETAIL, // 建钢-发货清单
//   STEEL_MES_LOGISTICS_SUMMARY, // 建钢-物流汇总表

//   STEEL_MES_BUSINESS_ENTRY_STRUCTURE, // 建钢-结构清单计价表
//   STEEL_MES_BUSINESS_ENTRY_ENCLOSURE, // 建钢-围护清单计价表
//   STEEL_MES_BUSINESS_ENTRY_AUXILIARY_MATERIAL, // 建钢-辅材清单计价表
//   STEEL_MES_BUSINESS_OUTBOUND_PROJECT, // 建钢-出库汇总表
//   STEEL_MES_BUSINESS_OUTBOUND_MONOMER, // 建钢-单体出库表
//   STEEL_MES_BUSINESS_INSTALLATION_PROJECT, // 建钢-安装汇总表
//   STEEL_MES_BUSINESS_INSTALLATION_MONOMER, // 建钢-单体安装表
//   STEEL_MES_BUSINESS_TRIP_TRACKING, // 建钢-单次发货统计表

//   STEEL_MES_TASK_STRUCTURE, // 建钢-结构任务清单
//   STEEL_MES_TASK_MACHINE_PART, // 建钢-零件任务清单
//   STEEL_MES_TASK_ENCLOSURE, // 建钢-围护任务清单
//   STEEL_MES_PRODUCTION_STATE_STRUCTURE, // 建钢-结构工序任务表
//   STEEL_MES_PRODUCTION_STATE_ENCLOSURE, // 建钢-围护工序任务表
//   STEEL_MES_PRODUCTION_STATE_MACHINE_PART, // 建钢-零件工序任务表

//   STEEL_MES_WAREHOUSE_STATE_STRUCTURE, // 建钢-结构出入库统计表
//   STEEL_MES_WAREHOUSE_STATE_ENCLOSURE, // 建钢-围护出入库统计表
//   STEEL_MES_WAREHOUSE_STATE_AUXILIARY_MATERIAL, // 建钢-辅材出入库统计表
//   STEEL_MES_WAREHOUSE_STATE_REPORT, // 建钢-入发存报表
//   STEEL_MES_WAREHOUSE_STATE_REPORT_PRODUCT_ENCLOSURE, // 建刚-入发存项目明细报表-围护
//   STEEL_MES_WAREHOUSE_STATE_REPORT_PRODUCT_STRUCTURE, // 建刚-入发存项目明细报表-结构
//   STEEL_MES_WAREHOUSE_STATE_REPORT_SUMMARY_ENCLOSURE, // 建刚-入发存汇总明细报表-围护
//   STEEL_MES_WAREHOUSE_STATE_REPORT_SUMMARY_STRUCTURE, // 建刚-入发存汇总明细报表-结构
//   STEEL_MES_SITE_WAREHOUSE_STATE_STRUCTURE, // 建钢-结构收安统计表
//   STEEL_MES_SITE_WAREHOUSE_STATE_ENCLOSURE, // 建钢-围护收安统计表
//   STEEL_MES_SITE_WAREHOUSE_STATE_AUXILIARY_MATERIAL, // 建钢-辅材收安统计表
//   STEEL_MES_COMPLEX_STATE_STRUCTURE, // 建钢-结构总看板统计表
//   STEEL_MES_COMPLEX_STATE_ENCLOSURE, // 建钢-围护总看板统计表
//   STEEL_MES_COMPLEX_STATE_AUXILIARY_MATERIAL, // 建钢-辅材总看板统计表

//   STEEL_MES_REPORT_PROJECT_PRODUCTION_STRUCTURE, // 建钢-结构项目生产表
//   STEEL_MES_REPORT_FACTORY_PRODUCTION_STRUCTURE, // 建钢-结构工厂生产表
//   STEEL_MES_REPORT_PROJECT_PRODUCTION_ENCLOSURE, // 建钢-围护项目生产表
//   STEEL_MES_REPORT_FACTORY_PRODUCTION_ENCLOSURE, // 建钢-围护工厂生产表
//   STEEL_MES_REPORT_TEAM_OUTPUT_STRUCTURE, // 建钢-班组构件产量表
//   STEEL_MES_REPORT_TEAM_OUTPUT_MACHINE_PART, // 建钢-班组零件产量表
//   STEEL_MES_REPORT_TEAM_OUTPUT_ENCLOSURE, // 建钢-班组围护产量表
//   STEEL_MES_REPORT_OUTBOUND_STRUCTURE, // 建钢-全部结构出库表
//   STEEL_MES_REPORT_OUTBOUND_PROJECT_STRUCTURE, // 建钢-结构项目出库表
//   STEEL_MES_REPORT_OUTBOUND_ENCLOSURE, // 建钢-全部围护出库表
//   STEEL_MES_REPORT_OUTBOUND_PROJECT_ENCLOSURE, // 建钢-围护工厂生产表
//   STEEL_MES_REPORT_OUTBOUND_AUXILIARY_MATERIAL, // 建钢-全部辅材出库表
//   STEEL_MES_REPORT_OUTBOUND_PROJECT_AUXILIARY_MATERIAL, // 建钢-辅材项目出库表
//   STEEL_MES_REPORT_INSTALLATION_STRUCTURE, // 建钢-结构项目安装表
//   STEEL_MES_REPORT_INSTALLATION_ENCLOSURE, // 建钢-围护项目安装表
//   STEEL_MES_REPORT_TEAM_PIECE_WAGE_STRUCTURE, // 建钢-构件班组计件工资表
//   STEEL_MES_REPORT_TEAM_PIECE_WAGE_ENCLOSURE, // 建钢-构件班组计件工资表
//   STEEL_MES_REPORT_OUTPUT_WAGE, // 建钢-产量工资表
//   STEEL_MES_REPORT_STEEL_DOSAGE // 建刚-钢材使用用量对比
// }
