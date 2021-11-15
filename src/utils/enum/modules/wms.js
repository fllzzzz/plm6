import { constantize } from '../base'

// 入库填写方式（金额及工厂在什么阶段填写）
const inboundFillWayEnum = {
  AUDITING: { L: '入库审核时填写', K: 'AUDITING', V: 1 << 0 },
  APPLICATION: { L: '入库提交时填写', K: 'APPLICATION', V: 1 << 1 }
}
constantize(inboundFillWayEnum)

// 计量配置
const measureTypeEnum = {
  MEASURE: { L: '计量', K: 'MEASURE', V: 1 },
  ACCOUNTING: { L: '核算', K: 'ACCOUNTING', V: 2 }
}
constantize(measureTypeEnum)

// 仓库类型
const warehouseTypeEnum = {
  NORMAL: { L: '普通', K: 'NORMAL', V: 1 << 0 },
  WORKSHOP: { L: '车间', K: 'WORKSHOP', V: 1 << 1 }
}
constantize(warehouseTypeEnum)

// 订单供货类型
const orderSupplyTypeEnum = {
  SELF: { L: '自采物料', K: 'SELF', V: 1 << 0 },
  PARTY_A: { L: '甲供物料', K: 'PARTY_A', V: 1 << 1 }
}
constantize(orderSupplyTypeEnum)

// 采购状态(订单采购状态/申购单采购状态)
const purchaseStatusEnum = {
  UNFINISHED: { L: '采购中', K: 'UNFINISHED', V: 1 },
  FINISHED: { L: '已完成', K: 'FINISHED', V: 2 }
}
constantize(purchaseStatusEnum)

export {
  inboundFillWayEnum,
  measureTypeEnum,
  warehouseTypeEnum,
  orderSupplyTypeEnum,
  purchaseStatusEnum
}

export default {
  inboundFillWayEnum,
  measureTypeEnum,
  warehouseTypeEnum,
  orderSupplyTypeEnum,
  purchaseStatusEnum
}
