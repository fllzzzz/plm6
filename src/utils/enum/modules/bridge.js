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
const bridgePackTypeEnum = {
  BOX: { L: '分段', SL: '分段', K: 'BOX', V: bridgeComponentTypeEnum.BOX.V, T: '' },
  CELL: { L: '单元', SL: '单元', K: 'ENCLOSURE', V: bridgeComponentTypeEnum.CELL.V, T: 'warning' },
  AUXILIARY_MATERIAL: { L: '辅材', SL: '配套件', K: 'AUXILIARY_MATERIAL', V: bridgeComponentTypeEnum.AUXILIARY_MATERIAL.V, T: 'success' }
}
constantize(bridgePackTypeEnum)

// 标签类型
const bridgeLabelTypeEnum = {
  COMMON: { L: '常规型', K: 'COMMON', V: 1 << 0, size: {
    [bridgeComponentTypeEnum.BOX.V]: ' 100 * 75 ',
    [bridgeComponentTypeEnum.CELL.V]: ' 100 * 75 '
    // [bridgeComponentTypeEnum.ENCLOSURE.V]: ' 100 * 30 '
  }},
  SIMPLE: { L: '简约型', K: 'SIMPLE', V: 1 << 1, size: {
    [bridgeComponentTypeEnum.BOX.V]: ' 100 * 75 ',
    [bridgeComponentTypeEnum.CELL.V]: ' 100 * 75 '
    // [bridgeComponentTypeEnum.ENCLOSURE.V]: ' 无 '
  }},
  CUSTOM: { L: '定制型', K: 'CUSTOM', V: 1 << 2, size: {
    [bridgeComponentTypeEnum.BOX.V]: ' 100 * 75 ',
    [bridgeComponentTypeEnum.CELL.V]: ' 100 * 75 '
    // [bridgeComponentTypeEnum.ENCLOSURE.V]: ' 100 * 50 '
  }}
}
constantize(bridgeLabelTypeEnum)

// 打印的产品类型
const bridgePrintProductTypeEnum = {
  BOX: { L: '分段', K: 'BOX', V: 1 << 0 },
  // ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: 1 << 1 },
  CELL: { L: '单元件', K: 'CELL', V: 1 << 1 },
  PACKAGE: { L: '包', K: 'PACKAGE', V: 1 << 2 }
}
constantize(bridgePrintProductTypeEnum)

const bridgeTaskTypeEnum = {
  MACHINE_PART: bridgeComponentTypeEnum.MACHINE_PART,
  CELL: bridgeComponentTypeEnum.CELL,
  BOX: bridgeComponentTypeEnum.BOX,
  AUXILIARY_MATERIAL: bridgeComponentTypeEnum.AUXILIARY_MATERIAL
}
constantize(bridgeTaskTypeEnum)

export {
  bridgeComponentTypeEnum,
  bridgeProcessTypeEnum,
  bridgeProcessCategoryEnum,
  bridgePackTypeEnum,
  bridgeLabelTypeEnum,
  bridgePrintProductTypeEnum,
  bridgeTaskTypeEnum
}

export default {
  bridgeComponentTypeEnum,
  bridgeProcessTypeEnum,
  bridgeProcessCategoryEnum,
  bridgePackTypeEnum,
  bridgeLabelTypeEnum,
  bridgePrintProductTypeEnum,
  bridgeTaskTypeEnum
}
