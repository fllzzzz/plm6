import { constantize } from '../base'

// 文件分类
const fileClassifyEnum = {
  NORMAL: { L: '公共文件', K: 'NORMAL', V: 1 },
  SECTION_ATT: { L: '型材导入附件', K: 'SECTION_ATT', V: 10 },
  SUPPLIER_ATT: { L: '供应商附件', K: 'SUPPLIER_ATT', V: 50 },
  PURCHASE_ORDER_ATT: { L: '采购合同附件', K: 'PURCHASE_ORDER_ATT', V: 100 },
  CONTRACT_ATT: { L: '合同附件', K: 'CONTRACT_ATT', V: 200 },
  CONTRACT_VISA: { L: '合同签证附件', K: 'CONTRACT_VISA', V: 201 },
  CHANGE_LIST_ATT: { L: '变更清单附件', K: 'CHANGE_LIST_ATT', V: 300 }
}

constantize(fileClassifyEnum)

export {
  fileClassifyEnum
}

export default {
  fileClassifyEnum
}
