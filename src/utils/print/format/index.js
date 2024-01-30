import common from './common'
import mes from './mes'
import wms from './wms'
import contract from './contract'
import scm from './supply-chain'
import enclosure from './enclosure'

const invoiceLedger = common.handleTaxRate
const invoiceRecord = common.handleTaxRate
const projectHappenedDetail = common.handleTaxRate

const myProject = contract.durationCalculation
const projectList = contract.durationCalculation
const contractLedger = contract.handleRate
const supplierPayableSummary = contract.handleSupplierPaymentRate
const logisticsPaymentLedger = contract.handleSupplierPaymentRate
const supplierPaymentLedger = contract.handleSupplierPaymentOrder
const saleOrderTracking = contract.handleRate
const collectionLedger = contract.collectionLedger

const orderPaymentLedger = contract.handleSupplierPaymentRate
const scmSupplierPaymentLedger = contract.handleSupplierPaymentRate
const purchaseLogisticsRecord = scm.handleOrderName
const scmRequisitionsDetail = scm.handleRequisitionsRecord

const mesAssembleNestingOrder = mes.assembleNestingOrder

const mesStructureProductionLine = mes.productionLineMete
const mesStructureProcess = mes.processMete

const mesStructureTeamWage = mes.meteUnit
const mesEnclosureTeamWage = mes.meteUnit
const mesStructureTeamWageDetail = mes.meteUnit
const mesEnclosureTeamWageDetail = mes.meteUnit

const mesStructureProductionStatistics = mes.unProducedMete
const mesStructureProductionStatisticsIn = mes.productionStatisticsMete
const mesEnclosureProductionStatistics = mes.unProducedMete
const mesEnclosureProductionStatisticsIn = mes.productionStatisticsMete
const mesShipmentDetail = mes.shipmentDetailWidth

const mesUnfinishedList = mes.unCompleteMete
const mesStructureProjectSummary = mes.projectSummary
const mesEnclosureProjectSummary = mes.projectSummary

const productSendReceiveStorageDetail = mes.projectConfig
const structureFinishedGoodsInventoryDetail = mes.projectConfig
const enclosureProductSendReceiveStorageDetail = mes.projectConfig

const mesWageSummary = mes.wageProducedMete
const mesWageDetail = mes.wageCompleteMete
const mesPaintingList = mes.surfaceArea

const mesEnclosureProductionLine = mes.areaConvert

const mesMainMaterialTrackUseRecord = wms.dataFormat
const mesMainMaterialTrackStock = wms.dataFormat

const wmsRmInboundReceipt = wms.dataFormat // 入库单
const wmsRmOutboundReceipt = wms.dataFormat // 出库单
const wmsRmReturnReceipt = wms.dataFormat // 退库单
const wmsRmRejectReceipt = wms.dataFormat // 退货单
const wmsRmSupplementReceipt = wms.supplementDataFormat // 调整记录
const wmsRmTransferReceipt = wms.transferDataFormat // 调拨单
const auxiliaryMaterialList = wms.dataFormat // 业财报表辅材费清单
// const gasRecord = wms.dataFormat // 合同管理/费用录入/气体统计
const conMainMaterialList = wms.dataFormat // 合同管理/业财报表主材费
const mesOutBoundStatisticsList = wms.dataFormat // 任务跟踪/在制品出库记录详情
const mesBackBoundStatisticsList = wms.dataFormat // 任务跟踪/在制品退库记录详情
const scrapPurchaser = wms.scrapPurchaser

// 围护MES
const enclosureTaskTrackingDetail = enclosure.handleProductionStatus // 围护生产跟踪详情
const enclosureTeamProduction = enclosure.handleProductionAmount // 围护班组产量
const enclosureTeamProductionDetail = enclosure.handleProductionAmount // 围护班组产量详情

export default {
  invoiceLedger,
  invoiceRecord,
  projectHappenedDetail,
  supplierPayableSummary,
  logisticsPaymentLedger,
  supplierPaymentLedger,
  orderPaymentLedger,
  scmSupplierPaymentLedger,
  myProject,
  projectList,
  contractLedger,
  saleOrderTracking,
  collectionLedger, // 收款台账收款列表

  mesAssembleNestingOrder,

  mesStructureProcess,
  mesStructureProductionLine,
  mesStructureTeamWage,
  mesEnclosureTeamWage,
  mesStructureTeamWageDetail,
  mesEnclosureTeamWageDetail,
  mesWageSummary,
  mesWageDetail,
  mesPaintingList,
  mesUnfinishedList,
  mesStructureProjectSummary,
  mesEnclosureProjectSummary,
  productSendReceiveStorageDetail,
  structureFinishedGoodsInventoryDetail,
  enclosureProductSendReceiveStorageDetail,
  mesStructureProductionStatistics,
  mesStructureProductionStatisticsIn,
  mesShipmentDetail,
  mesEnclosureProductionStatistics,
  mesEnclosureProductionStatisticsIn,
  mesEnclosureProductionLine,
  mesMainMaterialTrackUseRecord, // 主材跟踪-钢材领用记录
  mesMainMaterialTrackStock, // 主材跟踪-库存明细

  purchaseLogisticsRecord, // 供应链/物流记录
  scmRequisitionsDetail, // 供应商/申购详情

  wmsRmInboundReceipt, // 入库单
  wmsRmOutboundReceipt, // 出库单
  wmsRmReturnReceipt, // 退库单
  wmsRmRejectReceipt, // 退货单
  wmsRmSupplementReceipt, // 调整记录
  wmsRmTransferReceipt, // 调拨单
  scrapPurchaser, // 废料出售

  auxiliaryMaterialList, // 业财报表/辅材费清单
  // gasRecord, // 合同管理/费用录入/气体统计
  conMainMaterialList, // 合同管理/业财报表主材费
  mesOutBoundStatisticsList, // 任务跟踪/在制品出库记录详情
  mesBackBoundStatisticsList, // 任务跟踪/在制品退库记录详情

  // 围护MES
  enclosureTaskTrackingDetail, // 生产跟踪详情
  enclosureTeamProduction, // 围护班组产量
  enclosureTeamProductionDetail // 围护班组产量详情
}
