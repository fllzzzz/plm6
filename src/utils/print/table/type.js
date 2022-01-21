import { constantize } from '../../enum/base'
// PS: 模块类型和表格类型的key请不要设置成相同的

// 模块类型
const contract = {
  contract_report: '合同报表'
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
  contractLedger: { L: '合同台账', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },
  myProject: { L: '我的项目', M: 'contract_report', T: mt.contract.L + ' / ' + contract.contract_report },

  // wms
  wmsInventorySummary: { L: '物料库存汇总表', M: 'wms_warehouse', T: mt.wms.L + ' / ' + wms.wms_warehouse },

  // mes
  mesSchedulingDetail: { L: '排产详情', M: 'mes_task', T: mt.mes.L + ' / ' + mes.mes_task },

  mesStructureProductionReport: { L: '结构生产报表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesEnclosureProductionReport: { L: '围护生产报表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesStructureProductionStatistics: { L: '结构生产统计', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesEnclosureProductionStatistics: { L: '围护生产统计', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesUnfinishedList: { L: '未完成清单', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesStructureProductionLine: { L: '结构生产线报表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesStructureProcess: { L: '结构工序报表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesEnclosureProductionLine: { L: '围护生产线报表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesStructureProjectSummary: { L: '结构项目汇总', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesEnclosureProjectSummary: { L: '围护项目汇总', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesMachinePartDetail: { L: '零件生产详情', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },
  mesPaintingList: { L: '涂装列表', M: 'mes_production', T: mt.mes.L + ' / ' + mes.mes_production },

  mesPiecework: { L: '计件制报表报表', M: 'mes_wage', T: mt.mes.L + ' / ' + mes.mes_wage },
  mesPieceworkSummary: { L: '计件制汇总报表', M: 'mes_wage', T: mt.mes.L + ' / ' + mes.mes_wage },
  mesPieceworkDetail: { L: '计件制详情报表', M: 'mes_wage', T: mt.mes.L + ' / ' + mes.mes_wage },
  mesWageSummary: { L: '编外工资汇总', M: 'mes_wage', T: mt.mes.L + ' / ' + mes.mes_wage },
  mesWageDetail: { L: '编外工资详情', M: 'mes_wage', T: mt.mes.L + ' / ' + mes.mes_wage },

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
