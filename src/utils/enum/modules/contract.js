import { constantize } from '../base'

/**
 * @author dhh
 * L:label 名称,
 * SL: short label 简称
 * K:key 键,
 * V:value 值
 * T: type 类型，往往对应element中的type
 */

// 项目状态
const projectStatusEnum = {
  PROCESS: { L: '进行中', K: 'PROCESS', V: 1 },
  SUSPEND: { L: '已暂停', K: 'SUSPEND', V: 2 },
  COMPLETE: { L: '已完工', K: 'COMPLETE', V: 3 }
}
constantize(projectStatusEnum)

// 项目展示状态
const projectNameArrangementModeEnum = {
  SERIAL_NUMBER_START: { L: '合同编号 项目名称', K: 'SERIAL_NUMBER_START', V: 1 },
  SERIAL_NUMBER_END: { L: '项目名称 合同编号', K: 'SERIAL_NUMBER_END', V: 2 }
}
constantize(projectNameArrangementModeEnum)

// 项目类型
const projectTypeEnum = {
  STEEL: { L: '建钢', K: 'STEEL', V: 1 << 0 },
  BRIDGE: { L: '桥梁', K: 'BRIDGE', V: 1 << 1 },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: 1 << 2 }
}
constantize(projectTypeEnum)

// 技术交底类型
const TechnologyTypeEnum = {
  STRUCTURE: { L: '结构', K: 'STRUCTURE', V: 5 },
  PROFILEDPLATE: { L: '压型彩板', K: 'PROFILEDPLATE', V: 2 },
  TRUSSFLOORPLATE: { L: '桁架楼承板', K: 'TRUSSFLOORPLATE', V: 3 },
  PRESSUREBEARINGPLATE: { L: '压型楼承板', K: 'PRESSUREBEARINGPLATE', V: 4 },
  SANDWICH_BOARD: { L: '夹芯板', K: 'SANDWICH_BOARD', V: 1 }
}
constantize(TechnologyTypeEnum)

// // 合同订单类型
// const orderTypeEnum = {
//   MACHINING: { L: '加工订单', K: 'MACHINING', V: 0 },
//   INSTALLATION: { L: '工程项目', K: 'INSTALLATION', V: 1 }
// }
// constantize(orderTypeEnum)

export {
  projectStatusEnum, // 项目状态
  projectTypeEnum, // 项目类型
  projectNameArrangementModeEnum, // 项目名称显示
  TechnologyTypeEnum // 技术交底
}

export default {
  projectStatusEnum, // 项目状态
  projectTypeEnum, // 项目类型
  projectNameArrangementModeEnum, // 项目名称显示
  TechnologyTypeEnum // 技术交底
}
