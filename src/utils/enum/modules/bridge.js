import { constantize } from '../base'

const bridgeComponentTypeEnum = {
  BOX: { L: '分段', K: 'BOX', SL: '分段', V: 1 << 0, T: 'success', COLOR: '#00babd' },
  CELL: { L: '单元件', K: 'CELL', SL: '单元件', V: 1 << 1, T: 'success', COLOR: '#40ed8d' },
  MACHINE_PART: { L: '零件', K: 'MACHINE_PART', SL: '零件', V: 1 << 2, T: '', COLOR: '#fad400' },
  AUXILIARY_MATERIAL: { L: '配套件', K: 'AUXILIARY_MATERIAL', V: 1 << 3, T: 'warning' }
}
constantize(bridgeComponentTypeEnum)

// 桥梁含有工序的材料类型
const bridgeProcessTypeEnum = {
  BOX: { L: '分段', K: 'BOX', SL: '分段', V: bridgeComponentTypeEnum.BOX.V, T: 'success', COLOR: '#00babd' },
  CELL: { L: '单元件', K: 'CELL', SL: '单元件', V: bridgeComponentTypeEnum.CELL.V, T: 'success', COLOR: '#40ed8d' },
  MACHINE_PART: { L: '零件', K: 'MACHINE_PART', SL: '零件', V: bridgeComponentTypeEnum.MACHINE_PART.V, T: '', COLOR: '#fad400' }
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

// 可打包类型
// const bridgePackTypeEnum = {
//   STRUCTURE: { L: '结构', SL: '结构', K: 'STRUCTURE', V: bridgeComponentTypeEnum.ARTIFACT.V, T: '' },
//   ENCLOSURE: { L: '围护', SL: '围护', K: 'ENCLOSURE', V: bridgeComponentTypeEnum.ENCLOSURE.V, T: 'warning' },
//   AUXILIARY_MATERIAL: { L: '辅材', SL: '配套件', K: 'AUXILIARY_MATERIAL', V: bridgeComponentTypeEnum.AUXILIARY_MATERIAL.V, T: 'success' }
// }
// constantize(bridgePackTypeEnum)

// 生产订单 零件是否有孔
const hasHoleEnum = {
  TRUE: { L: '有孔', K: 'TRUE', V: true },
  FALSE: { L: '无孔', K: 'FALSE', V: false }
}
constantize(hasHoleEnum)

export {
  bridgeComponentTypeEnum,
  bridgeProcessTypeEnum,
  hasHoleEnum,
  bridgeProcessCategoryEnum
}

export default {
  bridgeComponentTypeEnum,
  bridgeProcessTypeEnum,
  hasHoleEnum,
  bridgeProcessCategoryEnum
}
