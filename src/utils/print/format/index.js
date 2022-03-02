import mes from './mes'
import contract from './contract'

const myProject = contract.durationCalculation

const mesStructureProductionLine = mes.productionLineMete
const mesStructureProcess = mes.processMete

const mesStructureTeamWage = mes.meteUnit
const mesEnclosureTeamWage = mes.meteUnit
const mesStructureTeamWageDetail = mes.meteUnit
const mesEnclosureTeamWageDetail = mes.meteUnit

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
  mesEnclosureProductionStatistics
}
