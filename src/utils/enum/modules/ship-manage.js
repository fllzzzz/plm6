import { constantize } from '../base'

// 车间
const packWorkshopTypeEnum = {
  MES_WORKSHOP: { L: '建钢车间', K: 'MES_WORKSHOP', V: 1 << 0 },
  ENCLOSURE_WORKSHOP: { L: '围护车间', K: 'ENCLOSURE_WORKSHOP', V: 1 << 2 },
  BRIDGE_WORKSHOP: { L: '桥梁车间', K: 'BRIDGE_WORKSHOP', V: 1 << 1 }
}
constantize(packWorkshopTypeEnum)

// 产品类型
const componentTypeEnum = {
  MACHINE_PART: { L: '零件', SL: '零件', K: 'MACHINE_PART', V: 1 << 0, T: '', COLOR: '#fad400' },
  ASSEMBLE: { L: '部件', SL: '部件', K: 'ASSEMBLE', V: 1 << 4, T: 'success', COLOR: '#40ed8d' },
  ARTIFACT: { L: '构件', SL: '构件', K: 'ARTIFACT', V: 1 << 1, T: 'success', COLOR: '#00babd' },
  ENCLOSURE: { L: '围护', SL: '围护', K: 'ENCLOSURE', V: 1 << 2, T: 'warning', COLOR: '#ff7800' },
  AUXILIARY_MATERIAL: { L: '辅材', SL: '辅材', K: 'AUXILIARY_MATERIAL', T: 'info', V: 1 << 3, COLOR: '#f5f7fa' }
}
constantize(componentTypeEnum)
// 围护产品类型
const enclosureTypeEnum = {
  PRESSED_PLATE: { L: '压型彩板', K: 'PRESSED_PLATE', V: 1 << 1 },
  SANDWICH_BOARD: { L: '夹芯板', K: 'SANDWICH_BOARD', V: 1 << 2 },
  TRUSS_FLOOR_PLATE: { L: '桁架楼承板', K: 'TRUSS_FLOOR_PLATE', V: 1 << 3 },
  PRESSED_FLOOR_PLATE: { L: '压型楼承板', K: 'PRESSED_FLOOR_PLATE', V: 1 << 4 },
  FOLDING_PIECE: { L: '折边件', K: 'FOLDING_PIECE', V: 1 << 5 }
  // FLOOR_PLATE: { L: '楼承板', K: 'FLOOR_PLATE', V: floorPlateTypeEnum.TRUSS_FLOOR_PLATE.V | floorPlateTypeEnum.PRESSED_FLOOR_PLATE.V }
}
constantize(enclosureTypeEnum)

// 发运管理/建钢-发运统计
const mesShipStatisticsTypeEnum = {
  STRUCTURE: { L: '结构制品', K: 'STRUCTURE', V: 1 << 0 },
  AUXILIARY_MATERIAL: { L: '配套制品', K: 'AUXILIARY_MATERIAL', V: 1 << 1 }
}
constantize(mesShipStatisticsTypeEnum)

// 发运管理/围护-发运统计
const enclosureShipStatisticsTypeEnum = {
  PRESSED_PLATE: { L: '压型彩板', K: 'PRESSED_PLATE', V: 1 << 1 },
  SANDWICH_BOARD: { L: '夹芯板', K: 'SANDWICH_BOARD', V: 1 << 2 },
  TRUSS_FLOOR_PLATE: { L: '桁架楼承板', K: 'TRUSS_FLOOR_PLATE', V: 1 << 3 },
  PRESSED_FLOOR_PLATE: { L: '压型楼承板', K: 'PRESSED_FLOOR_PLATE', V: 1 << 4 },
  FOLDING_PIECE: { L: '折边件', K: 'FOLDING_PIECE', V: 1 << 5 },
  AUXILIARY_MATERIAL: { L: '配套制品', K: 'AUXILIARY_MATERIAL', V: 1 << 0 }
}
constantize(enclosureShipStatisticsTypeEnum)

// 打包操作
const packTypeEnum = {
  STRUCTURE: { L: '结构', SL: '结构', K: 'STRUCTURE', V: componentTypeEnum.ARTIFACT.V, T: '' },
  ENCLOSURE: { L: '围护', SL: '围护', K: 'ENCLOSURE', V: componentTypeEnum.ENCLOSURE.V, T: 'warning' },
  MACHINE_PART: { L: '直发件', SL: '直发件', K: 'MACHINE_PART', V: componentTypeEnum.MACHINE_PART.V | componentTypeEnum.ASSEMBLE.V, T: 'danger' },
  AUXILIARY_MATERIAL: { L: '辅材', SL: '配套件', K: 'AUXILIARY_MATERIAL', V: componentTypeEnum.AUXILIARY_MATERIAL.V, T: 'success' }
}
constantize(packTypeEnum)

export {
  packWorkshopTypeEnum,
  componentTypeEnum,
  enclosureTypeEnum,
  mesShipStatisticsTypeEnum,
  enclosureShipStatisticsTypeEnum,
  packTypeEnum
}

export default {
  packWorkshopTypeEnum,
  componentTypeEnum,
  enclosureTypeEnum,
  mesShipStatisticsTypeEnum,
  enclosureShipStatisticsTypeEnum,
  packTypeEnum
}
