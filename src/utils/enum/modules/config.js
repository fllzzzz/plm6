
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
  TRANSFER: { L: '调拨单', K: 'TRANSFER', V: 18 },
  SUPPLEMENT: { L: '调整单', K: 'SUPPLEMENT', V: 19 }
}
constantize(numberTypeEnum)

const taxRateEnum = {
  STEEL_PLATE: { L: '钢板', K: 'STEEL_PLATE', V: 1 << 0 },
  SECTION_STEEL: { L: '型材', K: 'SECTION_STEEL', V: 1 << 1 },
  STEEL_COIL: { L: '钢卷', K: 'STEEL_COIL', V: 1 << 2 },
  MATERIAL: { L: '辅材', K: 'MATERIAL', V: 1 << 3 },
  GAS: { L: '气体', K: 'GAS', V: 1 << 4 },
  STRUC_MANUFACTURED: { L: '成品构件', K: 'STRUC_MANUFACTURED', V: 1 << 5 },
  ENCL_MANUFACTURED: { L: '成品围护', K: 'ENCL_MANUFACTURED', V: 1 << 6 },
  OTHER: { L: '其它', K: 'OTHER', V: 1 << 7 },
  LOGISTICS: { L: '物流', K: 'LOGISTICS', V: 1 << 10 },
  CONTRACT: { L: '合同', K: 'CONTRACT', V: 1 << 11 }
  // PROFESSIONAL_SUBCONTRACTING: { L: '专业分包', K: 'PROFESSIONAL_SUBCONTRACTING', V: 1 << 15 },
  // LABOR_SUBCONTRACTING: { L: '劳务分包', K: 'LABOR_SUBCONTRACTING', V: 1 << 16 }
}
constantize(taxRateEnum)

// 费用归属
const costAscriptionEnum = {
  DIRECT_COSTS: { L: '直接费用', K: 'DIRECT_COSTS', V: 1 << 0 },
  INDIRECT_COSTS: { L: '间接费用', K: 'INDIRECT_COSTS', V: 1 << 1 },
  PERIOD_COSTS: { L: '期间费用', K: 'PERIOD_COSTS', V: 1 << 2 },
  PROJECT_RETENTION: { L: '项目留存', K: 'PROJECT_RETENTION', V: 1 << 3 }
}
constantize(costAscriptionEnum)

export {
  numberTypeEnum, // 文件编号
  taxRateEnum,
  costAscriptionEnum // 费用归属
}

export default {
  numberTypeEnum, // 文件编号
  taxRateEnum,
  costAscriptionEnum // 费用归属
}
