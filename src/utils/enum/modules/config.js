
import { constantize } from '../base'

// 文件编号
const numberTypeEnum = {
  MASTER_CONTRACT: { L: '主合同', K: 'MASTER_CONTRACT', V: 1 },
  CONTRACT: { L: '承包合同', K: 'CONTRACT', V: 2 },
  PURCHASE_CONTRACT: { L: '采购合同', K: 'PURCHASE_CONTRACT', V: 3 },
  SUB_CONTRACT: { L: '分包合同', K: 'SUB_CONTRACT', V: 4 },
  FOREIGN_VISA: { L: '对外签证', K: 'FOREIGN_VISA', V: 5 },
  DOMESTIC_VISA: { L: '对内签证', K: 'DOMESTIC_VISA', V: 6 },
  BREACH_RECORD: { L: '违约记录', K: 'BREACH_RECORD', V: 7 },
  REQUISITIONS: { L: '申购合同', K: 'REQUISITIONS', V: 8 },
  INBOUND: { L: '入库单', K: 'INBOUND', V: 9 },
  OUTBOUND_APPLY: { L: '出库申请单', K: 'OUTBOUND_APPLY', V: 10 },
  SALES_RETURN: { L: '退货单', K: 'SALES_RETURN', V: 11 },
  MATERIAL_RETURN: { L: '退库单', K: 'MATERIAL_RETURN', V: 12 },
  WAREHOUSE_VERIFICATION: { L: '入库核销单', K: 'WAREHOUSE_VERIFICATION', V: 13 },
  WITHDRAWAL_VERIFICATION: { L: '退货核销单', K: 'WITHDRAWAL_VERIFICATION', V: 14 },
  LOGISTICS_ORDER: { L: '供应商', K: 'LOGISTICS_ORDER', V: 16 },
  SUPPLIER: { L: '物流订单', K: 'SUPPLIER', V: 15 },
  OUTBOUND_REVIEW: { L: '出库审核单', K: 'OUTBOUND_REVIEW', V: 17 },
  TRANSFER: { L: '调拨单', K: 'TRANSFER', V: 18 }
}
constantize(numberTypeEnum)

export {
  numberTypeEnum // 文件编号
}

export default {
  numberTypeEnum // 文件编号
}
