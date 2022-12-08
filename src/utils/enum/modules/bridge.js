import { constantize } from '../base'

const componentTypeEnum = {
  BOX: { L: '分段', K: 'BOX', V: 1 << 0, T: '' },
  CELL: { L: '单元件', K: 'CELL', V: 1 << 1, T: 'info' },
  MACHINE_PART: { L: '零件', K: 'MACHINE_PART', V: 1 << 2, T: 'success' }
}
constantize(componentTypeEnum)

// 桥梁含有工序的材料类型
const bridgeProcessTypeEnum = {
  BOX: { L: '分段', K: 'BOX', V: componentTypeEnum.BOX.V, T: '' },
  CELL: { L: '单元', K: 'CELL', V: componentTypeEnum.CELL.V, T: 'info' },
  MACHINE_PART: { L: '零件', K: 'MACHINE_PART', V: componentTypeEnum.MACHINE_PART.V, T: 'success' }
}
constantize(bridgeProcessTypeEnum)

// 桥梁工序类
const bridgeProcessCategoryEnum = {
  ASSEMBLY_RIVETING_WELDING: { L: '组铆焊类', K: 'ASSEMBLY_RIVETING_WELDING', V: 1 << 0 },
  PAINT: { L: '油漆类', K: 'PAINT', V: 1 << 1 },
  MAKINGS: { L: '下料类', K: 'MAKINGS', V: 1 << 2 },
  DRILL_HOLE: { L: '钻孔类', K: 'DRILL_HOLE', V: 1 << 3 },
  PRESSING: { L: '压板类', K: 'PRESSING', V: 1 << 4 }
}
constantize(bridgeProcessCategoryEnum)

// 生产订单 零件是否有孔
const hasHoleEnum = {
  TRUE: { L: '有孔', K: 'TRUE', V: true },
  FALSE: { L: '无孔', K: 'FALSE', V: false }
}
constantize(hasHoleEnum)

export {
  componentTypeEnum,
  bridgeProcessTypeEnum,
  hasHoleEnum,
  bridgeProcessCategoryEnum
}

export default {
  componentTypeEnum,
  bridgeProcessTypeEnum,
  hasHoleEnum,
  bridgeProcessCategoryEnum
}
