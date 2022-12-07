import { constantize } from '../base'

// 桥梁含有工序的材料类型
const bridgeProcessTypeEnum = {
  BOX: { L: '分段', K: 'BOX', V: 1, T: '' },
  CELL: { L: '单元', K: 'CELL', V: 2, T: 'info' },
  MACHINE_PART: { L: '零件', K: 'MACHINE_PART', V: 3, T: 'success' }
}
constantize(bridgeProcessTypeEnum)

export {
  bridgeProcessTypeEnum
}

export default {
  bridgeProcessTypeEnum
}
