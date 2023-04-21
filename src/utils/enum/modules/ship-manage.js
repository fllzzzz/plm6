import { constantize } from '../base'

// 发运管理/建钢-发运统计
const mesShipStatisticsTypeEnum = {
  STRUCTURE: { L: '结构制品', K: 'STRUCTURE', V: 1 << 0 },
  KIT: { L: '配套制品', K: 'KIT', V: 1 << 1 }
}
constantize(mesShipStatisticsTypeEnum)

// 发运管理/围护-发运统计
const enclosureShipStatisticsTypeEnum = {
  TRUSS_FLOOR_PLATE: { L: '桁架楼承板', K: 'TRUSS_FLOOR_PLATE', V: 1 << 3 },
  PRESSED_FLOOR_PLATE: { L: '压型楼承板', K: 'PRESSED_FLOOR_PLATE', V: 1 << 4 },
  KIT: { L: '配套制品', K: 'KIT', V: 1 << 0 }
}
constantize(enclosureShipStatisticsTypeEnum)

export {
  mesShipStatisticsTypeEnum,
  enclosureShipStatisticsTypeEnum
}

export default {
  mesShipStatisticsTypeEnum,
  enclosureShipStatisticsTypeEnum
}
