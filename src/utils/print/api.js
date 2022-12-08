import contract from '@/api/print/contract'
import mes from '@/api/print/mes'
import bridge from '@/api/print/bridge'
import wms from '@/api/print/wms'
import supply from '@/api/print/supply-chain'
import project from '@/api/print/project'
import plan from '@/api/print/plan'

// 合同
const myProject = contract.myProject
const projectList = contract.projectList
const contractLedger = contract.contractLedger
const collectionRecord = contract.collectionLedger
const collectionLedger = contract.collectionLedger
const invoiceRecord = contract.invoiceLedger
const invoiceLedger = contract.invoiceLedger
const arrearsList = contract.arrearsList
const contractStructurePrice = contract.structurePrice
const contractEnclosurePrice = contract.enclosurePrice
const contractAuxiliaryMaterialPrice = contract.auxiliaryMaterialPrice
const projectCollectionDetail = contract.collectionDetail
const projectInvoiceDetail = contract.invoiceDetail
const projectHappenedDetail = contract.happenedDetail
const transactionRecord = contract.transactionRecord

const logisticsPaymentLedger = contract.logisticsLedger
const supplierPayableSummary = contract.payableSummary
const supplierPaymentLedger = contract.supplierPayableLedger
const supplierInvoiceLedger = contract.supplierInvoiceLedger

const saleOrderTracking = contract.saleOrderTracking
const projectWarehouseRecord = contract.warehouseRecord

// 供应链
const purchaseInvoiceRecord = supply.invoiceDetail
const purchaseInboundRecord = supply.inboundRecord
const purchasePaymentRecord = supply.paymentRecord
const orderPaymentLedger = supply.orderPaymentLedger
const scmSupplierPaymentLedger = supply.supplierPaymentLedger

const purchaseLogisticsRecord = supply.logisticsRecord

const scmRequisitionsDetail = supply.requisitionsDetail // 申购详情

// mes
const mesSchedulingDetail = mes.schedulingDetail
const mesProductionTaskOrder = mes.productionTaskOrder
const mesAssembleNestingOrder = mes.assembleNestingOrder
const mesAssembleProductionTaskOrder = mes.assembleProductionTaskOrder
const mesDrillProductionTaskOrder = mes.drillProductionTaskOrder
const mesNestingProductionTaskOrder = mes.drillProductionTaskOrder

const mesStructureProductionReport = mes.productionReport
const mesAssemblePartProductionReport = mes.productionReport
const mesMachinePartProductionReport = mes.productionReport
const mesEnclosureProductionReport = mes.productionReport
const mesStructureProductionStatistics = mes.productionStatistics
const mesStructureProductionStatisticsIn = mes.productionStatisticsIn
const mesStructureProductionStatisticsUn = mes.productionStatisticsUn
const mesStructureProductionStatisticsComplete = mes.productionStatisticsComplete
const mesEnclosureProductionStatistics = mes.productionStatistics
const mesEnclosureProductionStatisticsIn = mes.productionStatisticsIn
const mesEnclosureProductionStatisticsUn = mes.productionStatisticsUn
const mesEnclosureProductionStatisticsComplete = mes.productionStatisticsComplete
const mesUnfinishedList = mes.unfinishedList
const mesEnclosureProductionLine = mes.enclosureProductionLine
const mesStructureProductionLine = mes.structureProductionLine
const mesStructureProcess = mes.structureProcess
const mesMachinePartDetail = mes.machinePartDetail
const mesMachinePartList = mes.machinePartList
const mesPaintingList = mes.paintingList
const mesStructureProjectSummary = mes.structureProjectSummary
const mesEnclosureProjectSummary = mes.enclosureProjectSummary
const mesQHSEProductionLineReport = mes.qhseProductionLineReport
const mesProjectOverviewList = mes.projectOverviewList

const mesStructureTeamWage = mes.teamWage
const mesEnclosureTeamWage = mes.teamWage
const mesStructureTeamWageDetail = mes.teamWageDetail
const mesEnclosureTeamWageDetail = mes.teamWageDetail

const mesPackingList = mes.packingList
const mesShipmentSummary = mes.shipmentSummary
const mesShipmentDetail = mes.shipmentDetail
const mesReceiptStatusSummary = mes.receiptStatusSummary
const mesShippingList = mes.shippingList
const mesShipmentAudit = mes.shipmentAudit
const mesLogisticsSummary = mes.logisticsSummary

const mesWarehouseStateStructure = mes.warehouseStateStructure
const mesWarehouseStateEnclosure = mes.warehouseStateEnclosure
const mesWarehouseStateReport = mes.warehouseStateReport

const mesArtifactClassList = mes.artifactClassList
const mesAssembleClassList = mes.assembleClassList
const mesMachinePartClassList = mes.machinePartClassList

const mesMonthlyTaskList = mes.monthlyTaskList
const mesProcessList = mes.processList
const mesProductionLineList = mes.productionLineList
const mesWorkOrderTrackingList = mes.workOrderTrackingList

const mesFactoryWorkshopReport = mes.factoryWorkshopReport

const mesStudSleeveStatisticsList = mes.studSleeveStatisticsList
const mesProjectShipDetail = mes.mesProjectShipDetail
const mesProductSendReceiveStorage = mes.productSendReceiveStorage
const productSendReceiveStorageDetail = mes.productSendReceiveStorageDetail

// bridge
const bridgeSchedulingDetail = bridge.schedulingDetail
const bridgeProductionTaskOrder = bridge.productionTaskOrder
const bridgeAssembleNestingOrder = bridge.assembleNestingOrder
const bridgeAssembleProductionTaskOrder = bridge.assembleProductionTaskOrder
const bridgeDrillProductionTaskOrder = bridge.drillProductionTaskOrder
const bridgeNestingProductionTaskOrder = bridge.drillProductionTaskOrder

const bridgeStructureProductionReport = bridge.productionReport
const bridgeAssemblePartProductionReport = bridge.productionReport
const bridgeMachinePartProductionReport = bridge.productionReport
const bridgeEnclosureProductionReport = bridge.productionReport
const bridgeStructureProductionStatistics = bridge.productionStatistics
const bridgeStructureProductionStatisticsIn = bridge.productionStatisticsIn
const bridgeStructureProductionStatisticsUn = bridge.productionStatisticsUn
const bridgeStructureProductionStatisticsComplete = bridge.productionStatisticsComplete
const bridgeEnclosureProductionStatistics = bridge.productionStatistics
const bridgeEnclosureProductionStatisticsIn = bridge.productionStatisticsIn
const bridgeEnclosureProductionStatisticsUn = bridge.productionStatisticsUn
const bridgeEnclosureProductionStatisticsComplete = bridge.productionStatisticsComplete
const bridgeUnfinishedList = bridge.unfinishedList
const bridgeStructureProductionLine = bridge.structureProductionLine
const bridgeStructureProcess = bridge.structureProcess
const bridgeMachinePartDetail = bridge.machinePartDetail
const bridgeMachinePartList = bridge.machinePartList
const bridgePaintingList = bridge.paintingList
const bridgeStructureProjectSummary = bridge.structureProjectSummary
const bridgeEnclosureProjectSummary = bridge.enclosureProjectSummary
const bridgeQHSEProductionLineReport = bridge.qhseProductionLineReport
const bridgeProjectOverviewList = bridge.projectOverviewList

const bridgeStructureTeamWage = bridge.teamWage
const bridgeEnclosureTeamWage = bridge.teamWage
const bridgeStructureTeamWageDetail = bridge.teamWageDetail
const bridgeEnclosureTeamWageDetail = bridge.teamWageDetail

const bridgePackingList = bridge.packingList
const bridgeShipmentSummary = bridge.shipmentSummary
const bridgeShipmentDetail = bridge.shipmentDetail
const bridgeReceiptStatusSummary = bridge.receiptStatusSummary
const bridgeShippingList = bridge.shippingList
const bridgeShipmentAudit = bridge.shipmentAudit
const bridgeLogisticsSummary = bridge.logisticsSummary

const bridgeWarehouseStateStructure = bridge.warehouseStateStructure
const bridgeWarehouseStateEnclosure = bridge.warehouseStateEnclosure
const bridgeWarehouseStateReport = bridge.warehouseStateReport

const bridgeBoxClassList = bridge.boxClassList
const bridgeElementClassList = bridge.elementClassList
const bridgeMachinePartClassList = bridge.machinePartClassList

const bridgeMonthlyTaskList = bridge.monthlyTaskList
const bridgeProcessList = bridge.processList
const bridgeProductionLineList = bridge.productionLineList
const bridgeWorkOrderTrackingList = bridge.workOrderTrackingList

const bridgeFactoryWorkshopReport = bridge.factoryWorkshopReport

const bridgeStudSleeveStatisticsList = bridge.studSleeveStatisticsList
const bridgeProjectShipDetail = bridge.bridgeProjectShipDetail
const bridgeProductSendReceiveStorage = bridge.productSendReceiveStorage
const bridgeProductSendReceiveStorageDetail = bridge.productSendReceiveStorageDetail

// wms
const wmsRmOutboundReceipt = wms.wmsRmOutboundReceipt // 出库
const wmsRmInboundReceipt = wms.wmsRmInboundReceipt // 入库
const wmsRmReturnReceipt = wms.wmsRmReturnReceipt // 退库
const wmsRmRejectReceipt = wms.wmsRmRejectReceipt // 退货
const wmsRmTransferReceipt = wms.wmsRmTransferReceipt // 调拨
const wmsRmSupplementReceipt = wms.wmsRmSupplementReceipt // 红冲

// project
const deliveryCargoList = project.deliveryCargoList // 自制收货记录
const deliveryReportList = project.deliveryReportList // 收货报表
const deliveryInstallList = project.deliveryInstallList // 收安报表
const installReportList = project.installReportList // 安装报表

// plan
const auxiliaryMaterialSummary = plan.auxiliaryMaterialSummary // 配套件汇总

export default {
  myProject, // 我的项目
  projectList, // 项目列表
  contractLedger, // 合同台账
  collectionLedger, // 收款台账
  collectionRecord, // 项目收款记录
  invoiceLedger, // 开票台账
  invoiceRecord, // 项目开票记录
  arrearsList, // 欠款清单

  logisticsPaymentLedger, // 物流台账
  supplierPayableSummary, // 供应商应付汇总
  supplierPaymentLedger, // 供应商付款台账
  supplierInvoiceLedger, // 供应商收票台账

  contractStructurePrice, // 结构计价表
  contractEnclosurePrice, // 围护计价表
  contractAuxiliaryMaterialPrice, // 配套件计价表
  projectCollectionDetail, // 项目收款详情
  projectInvoiceDetail, // 项目开票详情
  projectHappenedDetail, // 项目发运详情
  transactionRecord, // 客户交易记录
  saleOrderTracking, // 订单跟踪
  projectWarehouseRecord, // 销售管理入库记录

  // 供应链
  purchaseInvoiceRecord, // 收票记录
  purchaseInboundRecord, // 入库记录
  purchasePaymentRecord, // 付款记录
  orderPaymentLedger, // 采购合同付款台账
  scmSupplierPaymentLedger, // 供应商付款台账

  purchaseLogisticsRecord, // 物流记录

  scmRequisitionsDetail, // 申购详情

  // mes
  mesSchedulingDetail, // 工单详情
  mesProductionTaskOrder, // 工单管理：构件生产任务单
  mesAssembleNestingOrder, // 工单管理：部件套料清单
  mesAssembleProductionTaskOrder, // 工单管理：部件生产任务单
  mesNestingProductionTaskOrder, // 工单管理：切割生产任务单
  mesDrillProductionTaskOrder, // 工单管理：钻孔生产任务单

  mesStructureProductionReport, // 结构生产报表
  mesAssemblePartProductionReport, // 部件生产报表
  mesMachinePartProductionReport, // 零件生产报表
  mesEnclosureProductionReport, // 围护生产报表
  mesStructureProductionStatistics, // 结构在制品统计
  mesStructureProductionStatisticsIn, // 结构在制品统计明细
  mesStructureProductionStatisticsUn, // 结构未生产统计明细
  mesStructureProductionStatisticsComplete, // 结构完成品统计明细
  mesEnclosureProductionStatistics, // 围护在制品统计
  mesEnclosureProductionStatisticsIn, // 围护在制品统计明细
  mesEnclosureProductionStatisticsUn, // 围护未生产统计明细
  mesEnclosureProductionStatisticsComplete, // 围护完成品统计明细
  mesUnfinishedList, // 未完成清单
  mesEnclosureProductionLine, // 围护生产线报表
  mesStructureProductionLine, // 结构生产线报表
  mesStructureProcess, // 结构工序报表
  mesMachinePartDetail, // 零件生产详情
  mesMachinePartList, // 零部件生产清单详情
  mesPaintingList, // 油漆用量明细
  mesStructureProjectSummary, // 结构项目汇总
  mesEnclosureProjectSummary, // 围护项目汇总
  mesQHSEProductionLineReport, // 生产线质检报表
  mesProjectOverviewList, // 工序生产明细清单

  mesStructureTeamWage, // 结构班组工资
  mesEnclosureTeamWage, // 围护班组工资
  mesStructureTeamWageDetail, // 结构班组工资详情
  mesEnclosureTeamWageDetail, // 围护班组工资详情

  mesPackingList, // 打包清单
  mesShipmentSummary, // 发运汇总
  mesShipmentDetail, // 发运详情
  mesReceiptStatusSummary, // 收货状态汇总
  mesShippingList, // 发货清单
  mesShipmentAudit, // 发运审核
  mesLogisticsSummary, // 物流汇总

  mesWarehouseStateStructure, // 结构出入库状态
  mesWarehouseStateEnclosure, // 围护出入库状态
  mesWarehouseStateReport, // 入发存报表

  mesArtifactClassList, // 构件分类清单明细
  mesAssembleClassList, // 部件分类清单明细
  mesMachinePartClassList, // 零件分类清单明细

  mesMonthlyTaskList, // 月度任务跟踪清单详情
  mesProductionLineList, //  产线跟踪清单详情
  mesWorkOrderTrackingList, // 工单跟踪清单详情
  mesProcessList, // 工序呆滞清单详情

  mesFactoryWorkshopReport, // 车间报表清单详情

  mesStudSleeveStatisticsList, // 栓钉套筒统计清单详情

  mesProjectShipDetail, // 发运管理项目发运详情
  mesProductSendReceiveStorage, // 发运管理制成品入发存
  productSendReceiveStorageDetail, // 发运管理制成品入发存详情

  // bridge
  bridgeSchedulingDetail, // 工单详情
  bridgeProductionTaskOrder, // 工单管理：构件生产任务单
  bridgeAssembleNestingOrder, // 工单管理：部件套料清单
  bridgeAssembleProductionTaskOrder, // 工单管理：部件生产任务单
  bridgeNestingProductionTaskOrder, // 工单管理：切割生产任务单
  bridgeDrillProductionTaskOrder, // 工单管理：钻孔生产任务单

  bridgeStructureProductionReport, // 结构生产报表
  bridgeAssemblePartProductionReport, // 部件生产报表
  bridgeMachinePartProductionReport, // 零件生产报表
  bridgeEnclosureProductionReport, // 围护生产报表
  bridgeStructureProductionStatistics, // 结构在制品统计
  bridgeStructureProductionStatisticsIn, // 结构在制品统计明细
  bridgeStructureProductionStatisticsUn, // 结构未生产统计明细
  bridgeStructureProductionStatisticsComplete, // 结构完成品统计明细
  bridgeEnclosureProductionStatistics, // 围护在制品统计
  bridgeEnclosureProductionStatisticsIn, // 围护在制品统计明细
  bridgeEnclosureProductionStatisticsUn, // 围护未生产统计明细
  bridgeEnclosureProductionStatisticsComplete, // 围护完成品统计明细
  bridgeUnfinishedList, // 未完成清单
  bridgeStructureProductionLine, // 结构生产线报表
  bridgeStructureProcess, // 结构工序报表
  bridgeMachinePartDetail, // 零件生产详情
  bridgeMachinePartList, // 零部件生产清单详情
  bridgePaintingList, // 油漆用量明细
  bridgeStructureProjectSummary, // 结构项目汇总
  bridgeEnclosureProjectSummary, // 围护项目汇总
  bridgeQHSEProductionLineReport, // 生产线质检报表
  bridgeProjectOverviewList, // 工序生产明细清单

  bridgeStructureTeamWage, // 结构班组工资
  bridgeEnclosureTeamWage, // 围护班组工资
  bridgeStructureTeamWageDetail, // 结构班组工资详情
  bridgeEnclosureTeamWageDetail, // 围护班组工资详情

  bridgePackingList, // 打包清单
  bridgeShipmentSummary, // 发运汇总
  bridgeShipmentDetail, // 发运详情
  bridgeReceiptStatusSummary, // 收货状态汇总
  bridgeShippingList, // 发货清单
  bridgeShipmentAudit, // 发运审核
  bridgeLogisticsSummary, // 物流汇总

  bridgeWarehouseStateStructure, // 结构出入库状态
  bridgeWarehouseStateEnclosure, // 围护出入库状态
  bridgeWarehouseStateReport, // 入发存报表

  bridgeBoxClassList, // 分段分类清单明细
  bridgeElementClassList, // 单元件分类清单明细
  bridgeMachinePartClassList, // 零件分类清单明细

  bridgeMonthlyTaskList, // 月度任务跟踪清单详情
  bridgeProductionLineList, //  产线跟踪清单详情
  bridgeWorkOrderTrackingList, // 工单跟踪清单详情
  bridgeProcessList, // 工序呆滞清单详情

  bridgeFactoryWorkshopReport, // 车间报表清单详情

  bridgeStudSleeveStatisticsList, // 栓钉套筒统计清单详情

  bridgeProjectShipDetail, // 发运管理项目发运详情
  bridgeProductSendReceiveStorage, // 发运管理制成品入发存
  bridgeProductSendReceiveStorageDetail, // 发运管理制成品入发存详情

  // wms
  wmsRmOutboundReceipt, // 出库（领料单）单
  wmsRmInboundReceipt, // 入库单
  wmsRmReturnReceipt, // 退库单
  wmsRmRejectReceipt, // 退货单
  wmsRmTransferReceipt, // 调拨单
  wmsRmSupplementReceipt, // 红冲记录

  // 项目管理
  deliveryCargoList, // 自制收货记录
  deliveryReportList, // 收货报表
  deliveryInstallList, // 收安报表
  installReportList, // 安装报表

  // 计划管理
  auxiliaryMaterialSummary // 配套件汇总
}
