import mes from './mes'
import contract from './contract'

const myProject = contract.durationCalculation

const mesStructureProductionLine = mes.productionLineMete
const mesStructureProcess = mes.processMete

const mesPiecework = mes.meteUnit
const mesPieceworkSummary = mes.meteUnit
const mesPieceworkDetail = mes.meteUnit

const mesStructureProductionStatistics = mes.unProducedMete
const mesEnclosureProductionStatistics = mes.unProducedMete

const mesUnfinishedList = mes.unCompleteMete
const mesStructureProjectSummary = mes.projectSummary
const mesEnclosureProjectSummary = mes.projectSummary

const mesWageSummary = mes.wageProducedMete
const mesWageDetail = mes.wageCompleteMete
const mesPaintingList = mes.surfaceArea

export default {
  myProject,
  mesStructureProcess,
  mesStructureProductionLine,
  mesPiecework,
  mesWageSummary,
  mesWageDetail,
  mesPaintingList,
  mesPieceworkSummary,
  mesPieceworkDetail,
  mesUnfinishedList,
  mesStructureProjectSummary,
  mesEnclosureProjectSummary,
  mesStructureProductionStatistics,
  mesEnclosureProductionStatistics
}
