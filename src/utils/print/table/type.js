import { constantize } from '../../enum/base'
// PS: 模块类型和表格类型的key请不要设置成相同的

// 模块类型
const wms = {
  wms_warehouse: '出入库表单',
  wms_purchaseApply: '申购表单'
}
const mes = {
  mes_plan: '计划表单',
  mes_business: '商务表单',
  mes_task: '任务表单',
  mes_production: '生产表单',
  mes_warehouse: '出入库表单',
  mes_summary: '汇总表单',
  mes_logistics: '物流表单',
  mes_wage: '工资表单',
  mes_report: '报表表单'
}
const moduleType = {
  wms: { L: 'WMS', V: wms },
  mes: { L: 'MES', V: mes }
}

const mt = moduleType

// 表格类型
const tableType = { // 如果一个表格属于两个模块，T: []
  // WMS_INVENTORY_MANAGE_SUMMARY
  wmsInventorySummary: { L: '物料库存汇总表', M: 'wms_warehouse', T: mt.wms.L + ' / ' + wms.wms_warehouse },

  // mes
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
  USER_CONTRACT_ACCOUNT: 'CONTRACT_ACCOUNT' // 我的项目：合同台账
}
constantize(apikey)

export {
  moduleType, // 模块类型
  tableType, // 表格类型
  apikey
}
