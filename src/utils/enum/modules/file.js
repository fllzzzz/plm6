import { constantize } from '../base'

// 文件分类
const fileClassifyEnum = {
  NORMAL: { L: '公共文件', K: 'NORMAL', V: 1 },
  SECTION_ATT: { L: '型材导入附件', K: 'SECTION_ATT', V: 10 },
  SUPPLIER_ATT: { L: '供应商附件', K: 'SUPPLIER_ATT', V: 50 },
  PURCHASE_ORDER_ATT: { L: '采购订单附件', K: 'PURCHASE_ORDER_ATT', V: 100 },
  CONTRACT_ATT: { L: '合同附件', K: 'CONTRACT_ATT', V: 200 },
}

constantize(fileClassifyEnum)

export {
  fileClassifyEnum
}

export default {
  fileClassifyEnum
}
