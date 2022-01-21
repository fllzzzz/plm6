import contract from '@/api/print/contract'
import mes from '@/api/print/mes'

// 合同
const contractLedger = contract.contractLedger
const myProject = contract.myProject

// mes
const mesSchedulingDetail = mes.schedulingDetail

const mesStructureProductionReport = mes.structureProductionReport
const mesEnclosureProductionReport = mes.enclosureProductionReport
const mesStructureProductionStatistics = mes.productionStatistics
const mesEnclosureProductionStatistics = mes.productionStatistics
const mesUnfinishedList = mes.unfinishedList
const mesEnclosureProductionLine = mes.enclosureProductionLine
const mesStructureProductionLine = mes.structureProductionLine
const mesStructureProcess = mes.structureProcess
const mesMachinePartDetail = mes.machinePartDetail
const mesPaintingList = mes.paintingList
const mesStructureProjectSummary = mes.structureProjectSummary
const mesEnclosureProjectSummary = mes.enclosureProjectSummary

const mesPiecework = mes.piecework
const mesPieceworkSummary = mes.pieceworkSummary
const mesPieceworkDetail = mes.pieceworkSummary
const mesWageSummary = mes.wageSummary
const mesWageDetail = mes.wageDetail

const mesPackingList = mes.packingList
const mesShipmentSummary = mes.shipmentSummary
const mesShipmentDetail = mes.shipmentDetail
const mesReceiptStatusSummary = mes.receiptStatusSummary
const mesShippingList = mes.shippingList
const mesLogisticsSummary = mes.logisticsSummary

const mesWarehouseStateStructure = mes.warehouseStateStructure
const mesWarehouseStateEnclosure = mes.warehouseStateEnclosure
const mesWarehouseStateReport = mes.warehouseStateReport

export default {
  contractLedger, // 合同台账
  myProject, // 我的项目

  // mes
  mesSchedulingDetail, // 排产详情

  mesStructureProductionReport, // 结构生产报表
  mesEnclosureProductionReport, // 围护生产报表
  mesStructureProductionStatistics, // 结构生产统计
  mesEnclosureProductionStatistics, // 围护生产统计
  mesUnfinishedList, // 未完成清单
  mesEnclosureProductionLine, // 围护生产线报表
  mesStructureProductionLine, // 结构生产线报表
  mesStructureProcess, // 结构工序报表
  mesMachinePartDetail, // 零件生产详情
  mesPaintingList, // 涂装列表
  mesStructureProjectSummary, // 结构项目汇总
  mesEnclosureProjectSummary, // 围护项目汇总

  mesPiecework, // 计件制报表报表
  mesPieceworkSummary, // 计件制汇总报表
  mesPieceworkDetail, // 计件制详情报表
  mesWageSummary, // 工资汇总
  mesWageDetail, // 工资详情

  mesPackingList, // 打包清单
  mesShipmentSummary, // 发运汇总
  mesShipmentDetail, // 发运详情
  mesReceiptStatusSummary, // 收货状态汇总
  mesShippingList, // 发货清单
  mesLogisticsSummary, // 物流汇总

  mesWarehouseStateStructure, // 结构出入库状态
  mesWarehouseStateEnclosure, // 围护出入库状态
  mesWarehouseStateReport // 入发存报表
}
