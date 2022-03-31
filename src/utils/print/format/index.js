import common from './common'
import mes from './mes'
import wms from './wms'
import contract from './contract'

const invoiceLedger = common.handleTaxRate
const invoiceRecord = common.handleTaxRate
const projectHappenedDetail = common.handleTaxRate

const myProject = contract.durationCalculation
const projectList = contract.durationCalculation
const contractLedger = contract.handleRate

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

const wmsRmOutboundReceipt = wms.dataFormat

export default {
  invoiceLedger,
  invoiceRecord,
  projectHappenedDetail,
  myProject,
  projectList,
  contractLedger,
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

  wmsRmOutboundReceipt
}
