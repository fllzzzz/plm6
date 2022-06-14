import common from './common'
import mes from './mes'
import wms from './wms'
import contract from './contract'
import scm from './supply-chain'

const invoiceLedger = common.handleTaxRate
const invoiceRecord = common.handleTaxRate
const projectHappenedDetail = common.handleTaxRate

const myProject = contract.durationCalculation
const projectList = contract.durationCalculation
const contractLedger = contract.handleRate
const supplierPayableSummary = contract.handleSupplierPaymentRate
const logisticsPaymentLedger = contract.handleSupplierPaymentRate
const supplierPaymentLedger = contract.handleSupplierPaymentOrder
const contractEnclosurePrice = contract.handleAreaUnit

const orderPaymentLedger = contract.handleSupplierPaymentRate
const scmSupplierPaymentLedger = contract.handleSupplierPaymentRate
const purchaseLogisticsRecord = scm.handleOrderName

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

const mesUnfinishedList = mes.unCompleteMete
const mesStructureProjectSummary = mes.projectSummary
const mesEnclosureProjectSummary = mes.projectSummary

const mesWageSummary = mes.wageProducedMete
const mesWageDetail = mes.wageCompleteMete
const mesPaintingList = mes.surfaceArea

const mesEnclosureProductionLine = mes.areaConvert

const wmsRmInboundReceipt = wms.dataFormat // 入库单
const wmsRmOutboundReceipt = wms.dataFormat // 出库单
const wmsRmReturnReceipt = wms.dataFormat // 退库单
const wmsRmRejectReceipt = wms.dataFormat // 退货单
const wmsRmSupplementReceipt = wms.supplementDataFormat // 红冲记录
const wmsRmTransferReceipt = wms.transferDataFormat // 调拨单

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
  contractEnclosurePrice,
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
  mesStructureProductionStatistics,
  mesStructureProductionStatisticsIn,
  mesEnclosureProductionStatistics,
  mesEnclosureProductionStatisticsIn,
  mesEnclosureProductionLine,

  purchaseLogisticsRecord, // 供应链/物流记录

  wmsRmInboundReceipt, // 入库单
  wmsRmOutboundReceipt, // 出库单
  wmsRmReturnReceipt, // 退库单
  wmsRmRejectReceipt, // 退货单
  wmsRmSupplementReceipt, // 红冲记录
  wmsRmTransferReceipt // 调拨单
}
