import { constantize } from '../../enum/base'
// PS: 模块类型和表格类型的key请不要设置成相同的

// 模块类型
const contract = {
  contract_report: '合同报表',
  sales_manage: '销售管理'
}

const wms = {
  wms_warehouse: '出入库报表',
  wms_purchaseApply: '申购报表'
}
const mes = {
  mes_task: '任务报表',
  mes_production: '生产报表',
  mes_warehouse: '出入库报表',
  mes_logistics: '物流报表',
  mes_wage: '工资报表'
}
const moduleType = {
  contract: { L: '合同管理', V: contract },
  wms: { L: 'WMS', V: wms },
  mes: { L: 'MES', V: mes }
}

const mt = moduleType

// 表格类型
const tableType = { // 如果一个表格属于两个模块，T: []
  // 合同
  myProject: { L: '我的项目', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },
  projectList: { L: '项目列表', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },
  contractLedger: { L: '合同台账', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },
  collectionLedger: { L: '收款台账', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },
  collectionRecord: { L: '收款记录', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },
  invoiceLedger: { L: '开票台账', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },
  invoiceRecord: { L: '开票记录', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },
  arrearsList: { L: '欠款清单', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },

  contractStructurePrice: { L: '结构计价表', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  contractEnclosurePrice: { L: '围护计价表', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  contractAuxiliaryMaterialPrice: { L: '配套件计价表', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  projectCollectionDetail: { L: '项目收款明细表', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  projectInvoiceDetail: { L: '项目开票明细表', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  projectHappenedDetail: { L: '项目发运明细表', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },
  transactionRecord: { L: '客户交易记录', M: 'sales_manage', T: mt.contract.L + ' / ' + contract.sales_manage },

  // wms
  wmsInventorySummary: { L: '物料库存汇总表', M: 'wms_warehouse', T: mt.wms.L + ' / ' + wms.wms_warehouse },

  // mes
  mesSchedulingDetail: { L: '工单详情', M: 'mes_task', T: mt.mes.L + ' / ' + mes.mes_task },

  mesAssemblePartProductionReport: { L: '组立生产报表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
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
  mesPaintingList: { L: '涂装列表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },

  mesStructureTeamWage: { L: '结构班组工资', M: 'mes_wage', T: mt.mes.L + ' / ' + mes.mes_wage },
  mesEnclosureTeamWage: { L: '围护班组工资', M: 'mes_wage', T: mt.mes.L + ' / ' + mes.mes_wage },
  mesStructureTeamWageDetail: { L: '结构班组工资详情报表', M: 'mes_wage', T: mt.mes.L + ' / ' + mes.mes_wage },
  mesEnclosureTeamWageDetail: { L: '围护班组工资详情报表', M: 'mes_wage', T: mt.mes.L + ' / ' + mes.mes_wage },

  mesWarehouseStateStructure: { L: '结构出入库状态', M: 'mes_warehouse', T: mt.mes.L + ' / ' + mes.mes_warehouse },
  mesWarehouseStateEnclosure: { L: '围护出入库状态', M: 'mes_warehouse', T: mt.mes.L + ' / ' + mes.mes_warehouse },
  mesWarehouseStateReport: { L: '入发存报表', M: 'mes_warehouse', T: mt.mes.L + ' / ' + mes.mes_warehouse },

  mesPackingList: { L: '打包清单', M: 'mes_logistics', T: mt.mes.L + ' / ' + mes.mes_logistics },
  mesShipmentSummary: { L: '发运汇总', M: 'mes_logistics', T: mt.mes.L + ' / ' + mes.mes_logistics },
  mesShipmentDetail: { L: '发运详情', M: 'mes_logistics', T: mt.mes.L + ' / ' + mes.mes_logistics },
  mesReceiptStatusSummary: { L: '收货状态汇总', M: 'mes_logistics', T: mt.mes.L + ' / ' + mes.mes_logistics },
  mesShippingList: { L: '发货清单', M: 'mes_logistics', T: mt.mes.L + ' / ' + mes.mes_logistics },
  mesLogisticsSummary: { L: '物流汇总', M: 'mes_logistics', T: mt.mes.L + ' / ' + mes.mes_logistics }
}

// 一个模板对应多个接口，尽量一一对应，在特殊情况下需要做特殊处理
// 接口：tableType-key
const apikey = {
}
constantize(apikey)

export {
  moduleType, // 模块类型
  tableType, // 表格类型
  apikey
}
