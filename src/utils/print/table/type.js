import { constantize, key2val, toArr, getBits, setValue } from '../enum/base'
// PS: 模块类型和表格类型的key请不要设置成相同的

// 模块类型
const moduleType = {
  wms: {
    warehouse: 'WMS > 出入库表单',
    purchaseApply: 'WMS > 申购表单'
  }
}

const mt = moduleType
// 表格类型
const tableType = { // 如果一个表格属于两个模块，T: []
  // WMS_INVENTORY_MANAGE_SUMMARY
  wmsInventorySummary: { L: '物料库存汇总表', T: mt.wms.warehouse }
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

export default {
  key2val,
  toArr,
  getBits,
  setValue
}
