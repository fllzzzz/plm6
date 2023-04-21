import { constantize } from '../../enum/base'
// PS: 模块类型和表格类型的key请不要设置成相同的

// 模块类型
const contract = {
  contract_report: '合同报表',
  supplier_payment: '供应商付款',
  sales_manage: '销售管理',
  expense_entry: '费用录入',
  property_report: '业财报表'
}

const wms = {
  wms_warehouse: '出入库报表',
  wms_rm_receipt: '原材料单据',
  wms_purchaseApply: '申购报表'
}

const mes = {
  mes_task: '任务报表',
  mes_production: '生产报表',
  mes_warehouse: '出入库报表',
  mes_logistics: '物流报表',
  mes_project_report: '项目报表',
  mes_wage: '工资报表',
  mes_production_order: '生产订单',
  mes_task_tracking: '任务跟踪报表',
  mes_factory_report: '工厂报表',
  mes_production_line_wage_statistics: '产线工资统计',
  mes_pack_and_ship_manage: '发运管理'
}

const supply = {
  purchase_reconciliation: '采购对账报表',
  logistics_reconciliation: '物流对账报表'
}

const project = {
  delivery_manage: '收货管理',
  install_manage: '安装管理'
}

const plan = {
  technical_manage: '技术管理'
}

const enclosure = {
  production_manage: '生产管理',
  production_report: '生产报表'
}

const moduleType = {
  contract: { L: '合同管理', V: contract },
  supply: { L: '供应链', V: supply },
  wms: { L: 'WMS', V: wms },
  mes: { L: 'MES', V: mes },
  project: { L: '项目管理', V: project },
  plan: { L: '计划管理', V: plan },
  enclosure: { L: '围护MES', V: enclosure }
}

const mt = moduleType

// 表格类型
const tableType = {
  // 如果一个表格属于两个模块，T: []
  // 合同
  myProject: { L: '我的项目', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },
  projectList: { L: '项目列表', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },
  contractLedger: { L: '合同台账', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },
  collectionLedger: { L: '收款台账', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },
  collectionRecord: { L: '收款记录', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },
  invoiceLedger: { L: '开票台账', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },
  invoiceRecord: { L: '开票记录', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },
  arrearsList: { L: '欠款清单', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },

  logisticsPaymentLedger: { L: '物流台账', M: 'supplier_payment', T: mt.contract.L + ' / ' + contract.supplier_payment },
  supplierPayableSummary: { L: '应付汇总', M: 'supplier_payment', T: mt.contract.L + ' / ' + contract.supplier_payment },

  contractStructurePrice: { L: '结构计价表', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  contractMachinePartPrice: { L: '散发制品计价表', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  contractEnclosurePrice: { L: '围护计价表', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  contractAuxiliaryMaterialPrice: { L: '配套件计价表', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  projectCollectionDetail: { L: '项目收款明细表', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  projectInvoiceDetail: { L: '项目开票明细表', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  projectHappenedDetail: { L: '项目出库明细表', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  transactionRecord: { L: '客户交易记录', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  saleOrderTracking: { L: '销售台账', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  projectWarehouseRecord: { L: '入库记录', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  contractStructureProduct: { L: '结构制品', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  contractEnclosureProduct: { L: '围护制品', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  contractAuxiliaryMaterialProduct: { L: '配套制品', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },

  industryElectricRecord: { L: '工业用电电费清单', M: 'expense_entry', T: mt.contract.L + ' / ' + contract.expense_entry },
  civilElectricRecord: { L: '民用用电电费清单', M: 'expense_entry', T: mt.contract.L + ' / ' + contract.expense_entry },
  waterRecord: { L: '水费清单', M: 'expense_entry', T: mt.contract.L + ' / ' + contract.expense_entry },
  gasRecord: { L: '气体费用清单', M: 'expense_entry', T: mt.contract.L + ' / ' + contract.expense_entry },
  expenseReimburseList: { L: '费用报销清单', M: 'expense_entry', T: mt.contract.L + ' / ' + contract.expense_entry },
  managementSalaryList: { L: '人员工资清单', M: 'expense_entry', T: mt.contract.L + ' / ' + contract.expense_entry },
  propertyFeeList: { L: '物业费清单', M: 'expense_entry', T: mt.contract.L + ' / ' + contract.expense_entry },

  conMainMaterialList: { L: '主材费清单', M: 'property_report', T: mt.contract.L + ' / ' + contract.property_report },
  auxiliaryMaterialList: { L: '辅材费清单', M: 'property_report', T: mt.contract.L + ' / ' + contract.property_report },
  manualList: { L: '人工费清单', M: 'property_report', T: mt.contract.L + ' / ' + contract.property_report },
  shippingFeeList: { L: '运输费清单', M: 'property_report', T: mt.contract.L + ' / ' + contract.property_report },
  testingFee: { L: '检测费清单', M: 'property_report', T: mt.contract.L + ' / ' + contract.property_report },
  subcontractFee: { L: '分包费清单', M: 'property_report', T: mt.contract.L + ' / ' + contract.property_report },
  projectManagementFee: { L: '项目管理费清单', M: 'property_report', T: mt.contract.L + ' / ' + contract.property_report },
  managementFee: { L: '管理费清单', M: 'property_report', T: mt.contract.L + ' / ' + contract.property_report },
  waterElectricFee: { L: '水电费清单', M: 'property_report', T: mt.contract.L + ' / ' + contract.property_report },
  depreciationFee: { L: '折旧费清单', M: 'property_report', T: mt.contract.L + ' / ' + contract.property_report },
  fortuneReportList: { L: '业财报表', M: 'property_report', T: mt.contract.L + ' / ' + contract.property_report },

  // 供应链
  scmRequisitionsDetail: { L: '申购详情', M: 'purchase_reconciliation', T: mt.supply.L + ' / ' + supply.purchase_reconciliation },
  purchaseInvoiceRecord: { L: '收票记录', M: 'purchase_reconciliation', T: mt.supply.L + ' / ' + supply.purchase_reconciliation },
  purchaseInboundRecord: { L: '入库记录', M: 'purchase_reconciliation', T: mt.supply.L + ' / ' + supply.purchase_reconciliation },
  purchasePaymentRecord: { L: '付款记录', M: 'purchase_reconciliation', T: mt.supply.L + ' / ' + supply.purchase_reconciliation },
  orderPaymentLedger: { L: '采购付款台账', M: 'purchase_reconciliation', T: mt.supply.L + ' / ' + supply.purchase_reconciliation },
  scmSupplierPaymentLedger: { L: '供应商付款台账', M: 'purchase_reconciliation', T: mt.supply.L + ' / ' + supply.purchase_reconciliation },

  purchaseLogisticsRecord: { L: '物流记录', M: 'logistics_reconciliation', T: mt.supply.L + ' / ' + supply.logistics_reconciliation },

  // wms
  wmsRmInboundReceipt: { L: '入库单', M: 'wms_rm_receipt', T: mt.wms.L + ' / ' + wms.wms_rm_receipt },
  wmsRmOutboundReceipt: { L: '出库（领料）单', M: 'wms_rm_receipt', T: mt.wms.L + ' / ' + wms.wms_rm_receipt },
  wmsRmReturnReceipt: { L: '退库单', M: 'wms_rm_receipt', T: mt.wms.L + ' / ' + wms.wms_rm_receipt },
  wmsRmRejectReceipt: { L: '退货单', M: 'wms_rm_receipt', T: mt.wms.L + ' / ' + wms.wms_rm_receipt },
  wmsRmTransferReceipt: { L: '调拨单', M: 'wms_rm_receipt', T: mt.wms.L + ' / ' + wms.wms_rm_receipt },

  // mes
  mesSchedulingDetail: { L: '工单详情', M: 'mes_task', T: mt.mes.L + ' / ' + mes.mes_task },
  mesProductionTaskOrder: { L: '构件生产任务单', M: 'mes_task', T: mt.mes.L + ' / ' + mes.mes_task },
  mesAssembleProductionTaskOrder: { L: '部件生产任务单', M: 'mes_task', T: mt.mes.L + ' / ' + mes.mes_task },
  mesAssembleNestingOrder: { L: '部件套料清单', M: 'mes_task', T: mt.mes.L + ' / ' + mes.mes_task },
  mesDrillProductionTaskOrder: { L: '钻孔生产任务单', M: 'mes_task', T: mt.mes.L + ' / ' + mes.mes_task },
  mesNestingProductionTaskOrder: { L: '零件生产任务单', M: 'mes_task', T: mt.mes.L + ' / ' + mes.mes_task },

  mesAssemblePartProductionReport: { L: '部件生产报表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesMachinePartProductionReport: { L: '零件生产报表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesStructureProductionReport: { L: '结构生产报表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesEnclosureProductionReport: { L: '围护生产报表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesStructureProductionStatistics: { L: '结构在制品统计', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesStructureProductionStatisticsIn: { L: '结构在制品统计明细', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesStructureProductionStatisticsUn: { L: '结构未生产统计明细', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesStructureProductionStatisticsComplete: { L: '结构完成品统计明细', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesEnclosureProductionStatistics: { L: '围护在制品统计', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesEnclosureProductionStatisticsIn: { L: '围护在制品统计明细', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesEnclosureProductionStatisticsUn: { L: '围护未生产统计明细', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesEnclosureProductionStatisticsComplete: { L: '围护完成品统计明细', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesUnfinishedList: { L: '未完成清单', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesStructureProductionLine: { L: '结构生产线报表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesStructureProcess: { L: '结构工序报表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesEnclosureProductionLine: { L: '围护生产线报表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesStructureProjectSummary: { L: '结构项目汇总', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesEnclosureProjectSummary: { L: '围护项目汇总', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesMachinePartDetail: { L: '零件生产详情', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesMachinePartList: { L: '零部件生产清单详情', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesPaintingList: { L: '油漆用量明细', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesQHSEProductionLineReport: { L: '生产线质检不合格报表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesProjectOverviewList: { L: '工序生产明细清单', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesSchedulingDataList: { L: '排产数据明细清单', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },

  mesStructureTeamWage: { L: '结构班组工资', M: 'mes_wage', T: mt.mes.L + ' / ' + mes.mes_wage },
  mesEnclosureTeamWage: { L: '围护班组工资', M: 'mes_wage', T: mt.mes.L + ' / ' + mes.mes_wage },
  mesStructureTeamWageDetail: { L: '结构班组工资详情报表', M: 'mes_wage', T: mt.mes.L + ' / ' + mes.mes_wage },
  mesEnclosureTeamWageDetail: { L: '围护班组工资详情报表', M: 'mes_wage', T: mt.mes.L + ' / ' + mes.mes_wage },

  mesWarehouseStateStructure: { L: '结构出入库状态', M: 'mes_warehouse', T: mt.mes.L + ' / ' + mes.mes_warehouse },
  mesWarehouseStateEnclosure: { L: '围护出入库状态', M: 'mes_warehouse', T: mt.mes.L + ' / ' + mes.mes_warehouse },
  mesWarehouseStateReport: { L: '入发存报表', M: 'mes_warehouse', T: mt.mes.L + ' / ' + mes.mes_warehouse },
  mesBeginningInventoryDetail: { L: '期初库存', M: 'mes_warehouse', T: mt.mes.L + ' / ' + mes.mes_warehouse },
  mesEndInventoryDetail: { L: '期末库存', M: 'mes_warehouse', T: mt.mes.L + ' / ' + mes.mes_warehouse },
  mesInboundInventoryDetail: { L: '入库量', M: 'mes_warehouse', T: mt.mes.L + ' / ' + mes.mes_warehouse },
  mesOutboundInventoryDetail: { L: '出库量', M: 'mes_warehouse', T: mt.mes.L + ' / ' + mes.mes_warehouse },

  mesPackingList: { L: '打包清单', M: 'mes_logistics', T: mt.mes.L + ' / ' + mes.mes_logistics },
  mesShipmentSummary: { L: '发运汇总', M: 'mes_logistics', T: mt.mes.L + ' / ' + mes.mes_logistics },
  mesShipmentDetail: { L: '发运详情', M: 'mes_logistics', T: mt.mes.L + ' / ' + mes.mes_logistics },
  mesShipmentAudit: { L: '发运审核', M: 'mes_logistics', T: mt.mes.L + ' / ' + mes.mes_logistics },
  mesReceiptStatusSummary: { L: '收货状态汇总', M: 'mes_logistics', T: mt.mes.L + ' / ' + mes.mes_logistics },
  mesShippingList: { L: '发货清单', M: 'mes_logistics', T: mt.mes.L + ' / ' + mes.mes_logistics },
  mesLogisticsSummary: { L: '物流汇总', M: 'mes_logistics', T: mt.mes.L + ' / ' + mes.mes_logistics },

  mesArtifactClassList: { L: '构件分类清单明细', M: 'mes_production_order', T: mt.mes.L + ' / ' + mes.mes_production_order },
  mesAssembleClassList: { L: '部件分类清单明细', M: 'mes_production_order', T: mt.mes.L + ' / ' + mes.mes_production_order },
  mesMachinePartClassList: { L: '零件分类清单明细', M: 'mes_production_order', T: mt.mes.L + ' / ' + mes.mes_production_order },

  mesProductionKanbanList: { L: '生产监控看板明细', M: 'mes_production_order', T: mt.mes.L + ' / ' + mes.mes_production_order },
  mesProductionKanbanGroupList: { L: '生产监控看板班组明细', M: 'mes_production_order', T: mt.mes.L + ' / ' + mes.mes_production_order },

  mesWorkOrderTrackingList: { L: '工单跟踪清单详情', M: 'mes_task_tracking', T: mt.mes.L + ' / ' + mes.mes_task_tracking },
  mesMonthlyTaskList: { L: '月度任务清单详情', M: 'mes_task_tracking', T: mt.mes.L + ' / ' + mes.mes_task_tracking },
  mesProductionLineList: { L: '产线跟踪清单详情', M: 'mes_task_tracking', T: mt.mes.L + ' / ' + mes.mes_task_tracking },
  mesProcessList: { L: '工序呆滞清单详情', M: 'mes_task_tracking', T: mt.mes.L + ' / ' + mes.mes_task_tracking },
  mesWipStatisticsList: { L: '在制品统计清单详情', M: 'mes_task_tracking', T: mt.mes.L + ' / ' + mes.mes_task_tracking },
  mesProcessStatisticsList: { L: '在制品工序清单详情', M: 'mes_task_tracking', T: mt.mes.L + ' / ' + mes.mes_task_tracking },
  mesTaskStatisticsList: { L: '在制品排产记录清单详情', M: 'mes_task_tracking', T: mt.mes.L + ' / ' + mes.mes_task_tracking },
  mesOutBoundStatisticsList: { L: '在制品出库记录清单详情', M: 'mes_task_tracking', T: mt.mes.L + ' / ' + mes.mes_task_tracking },

  mesFactoryWorkshopReport: { L: '车间报表清单（平台）', M: 'mes_factory_report', T: mt.mes.L + ' / ' + mes.mes_factory_report },
  mesGroupsReport: { L: '班组报表清单（平台）', M: 'mes_factory_report', T: mt.mes.L + ' / ' + mes.mes_factory_report },

  mesStudSleeveStatisticsList: { L: '栓钉套筒统计清单详情', M: 'mes_production_line_wage_statistics', T: mt.mes.L + ' / ' + mes.mes_production_line_wage_statistics },
  mesProjectShipDetail: { L: '项目发运详情报表', M: 'mes_pack_and_ship_manage', T: mt.mes.L + ' / ' + mes.mes_pack_and_ship_manage },
  mesShipMeteDetail: { L: '清单总量详情报表', M: 'mes_pack_and_ship_manage', T: mt.mes.L + ' / ' + mes.mes_pack_and_ship_manage },
  mesShipTaskMeteDetail: { L: '任务总量详情报表', M: 'mes_pack_and_ship_manage', T: mt.mes.L + ' / ' + mes.mes_pack_and_ship_manage },
  mesShipInboundMeteDetail: { L: '入库量详情报表', M: 'mes_pack_and_ship_manage', T: mt.mes.L + ' / ' + mes.mes_pack_and_ship_manage },
  mesShipTotalMeteDetail: { L: '累计发运详情报表', M: 'mes_pack_and_ship_manage', T: mt.mes.L + ' / ' + mes.mes_pack_and_ship_manage },
  mesShipMonthMeteDetail: { L: '本月发运详情报表', M: 'mes_pack_and_ship_manage', T: mt.mes.L + ' / ' + mes.mes_pack_and_ship_manage },
  mesShipStockMeteDetail: { L: '库存详情报表', M: 'mes_pack_and_ship_manage', T: mt.mes.L + ' / ' + mes.mes_pack_and_ship_manage },
  mesShipTrainMeteDetail: { L: '累计车次详情报表', M: 'mes_pack_and_ship_manage', T: mt.mes.L + ' / ' + mes.mes_pack_and_ship_manage },
  mesProductSendReceiveStorage: { L: '制成品入发存', M: 'mes_pack_and_ship_manage', T: mt.mes.L + ' / ' + mes.mes_pack_and_ship_manage },
  productSendReceiveStorageDetail: { L: '制成品入发存详情', M: 'mes_pack_and_ship_manage', T: mt.mes.L + ' / ' + mes.mes_pack_and_ship_manage },

  // 项目报表
  mesMainMaterialTrack: { L: '主材跟踪汇总', M: 'mes_project_report', T: mt.mes.L + ' / ' + mes.mes_project_report },
  mesMainMaterialTrackUseRecord: { L: '主材跟踪-钢材领用记录', M: 'mes_project_report', T: mt.mes.L + ' / ' + mes.mes_project_report },
  mesMainMaterialTrackStock: { L: '主材跟踪-库存明细', M: 'mes_project_report', T: mt.mes.L + ' / ' + mes.mes_project_report },

  // 项目管理
  deliveryCargoList: { L: '自制收货记录', M: 'delivery_manage', T: mt.project.L + ' / ' + project.delivery_manage },
  deliveryReportList: { L: '收货报表', M: 'delivery_manage', T: mt.project.L + ' / ' + project.delivery_manage },
  deliveryInstallList: { L: '收安报表', M: 'delivery_manage', T: mt.project.L + ' / ' + project.delivery_manage },
  installReportList: { L: '安装报表', M: 'install_manage', T: mt.project.L + ' / ' + project.install_manage },

  // 计划管理
  auxiliaryMaterialSummary: { L: '配套件汇总', M: 'technical_manage', T: mt.plan.L + ' / ' + plan.technical_manage },

  // 围护MES
  enclosureSchedulingWorkOrderDetail: { L: '排产工单详情', M: 'production_manage', T: mt.plan.L + ' / ' + enclosure.production_manage },
  enclosureTaskTrackingDetail: { L: '生产跟踪详情', M: 'production_manage', T: mt.plan.L + ' / ' + enclosure.production_manage },
  enclosureProductionStatistics: { L: '围护生产统计', M: 'production_report', T: mt.plan.L + ' / ' + enclosure.production_report },
  enclosureTeamProduction: { L: '围护班组产量', M: 'production_report', T: mt.plan.L + ' / ' + enclosure.production_report },
  enclosureTeamProductionDetail: { L: '围护班组产量详情', M: 'production_report', T: mt.plan.L + ' / ' + enclosure.production_report }

}

// 一个模板对应多个接口，尽量一一对应，在特殊情况下需要做特殊处理
// 接口：tableType-key
const apikey = {}
constantize(apikey)

export {
  moduleType, // 模块类型
  tableType, // 表格类型
  apikey
}
