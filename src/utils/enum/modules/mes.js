import { constantize } from '../base'

// 班组类型
const teamTypeEnum = {
  TEAM: { L: '班组', V: 0 },
  INSPECTION: { L: '质检', V: 1 }
}
constantize(teamTypeEnum)

// 班组属性
const teamAttributeEnum = {
  IN_STAFF: { L: '编内', K: 'IN_STAFF', V: true },
  OFF_STAFF: { L: '编外', K: 'OFF_STAFF', V: false }
}
constantize(teamAttributeEnum)

// 工序类型
const processTypeEnum = {
  ONCE: { L: '一次工序', K: 'ONCE', V: false },
  TWICE: { L: '二次工序', K: 'TWICE', V: true }
}
constantize(processTypeEnum)

// 工序生产检验方式
const processInspectTypeEnum = {
  SINGLE_UNSCAN: { L: '单件(不扫码)', K: 'SINGLE_UNSCAN', V: 0, T: 'info' },
  SINGLE_SCAN: { L: '单件(需扫码)', K: 'SINGLE_SCAN', V: 1, T: 'warning' },
  BATCH_UNSCAN: { L: '批量(不扫码)', K: 'BATCH_UNSCAN', V: 2, T: 'danger' },
  BATCH_SCAN: { L: '批量(需扫码)', K: 'BATCH_SCAN', V: 3, T: 'success' }
}
constantize(processInspectTypeEnum)

// 工序生产上报方式
const processReportTypeEnum = {
  SINGLE_UNSCAN: { L: '单件(不扫码)', K: 'SINGLE_UNSCAN', V: 0, T: 'info' },
  SINGLE_SCAN: { L: '单件(需扫码)', K: 'SINGLE_SCAN', V: 1, T: 'warning' },
  BATCH_UNSCAN: { L: '批量(不扫码)', K: 'BATCH_UNSCAN', V: 2, T: 'danger' },
  BATCH_SCAN: { L: '批量(需扫码)', K: 'BATCH_SCAN', V: 3, T: 'success' }
}
constantize(processReportTypeEnum)

// mes 产品类型
const productTypeEnum = {
  ARTIFACT: { L: '构件', K: 'ARTIFACT', V: 1 << 1 },
  MACHINE_PART: { L: '零件', K: 'MACHINE_PART', V: 1 << 0 },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: 1 << 2 },
  AUXILIARY_MATERIAL: { L: '辅材', K: 'AUXILIARY_MATERIAL', V: 1 << 3 },
  ASSEMBLE: { L: '组立', K: 'ASSEMBLE', V: 1 << 4 }
}
constantize(productTypeEnum)

// 含有工序的材料类型
const processMaterialListTypeEnum = {
  ARTIFACT: { L: '构件', K: 'ARTIFACT', V: productTypeEnum.ARTIFACT.V, T: '' },
  MACHINE_PART: { L: '零件', K: 'MACHINE_PART', V: productTypeEnum.MACHINE_PART.V, T: 'success' },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: productTypeEnum.ENCLOSURE.V, T: 'warning' }
}
constantize(processMaterialListTypeEnum)

// 工价定额单价类型
const wageQuotaTypeEnum = {
  WEIGHT: { L: '重量', K: 'WEIGHT', V: 1 << 0, F: 'weightPrice', unit: '元/吨' },
  LENGTH: { L: '长度', K: 'LENGTH', V: 1 << 1, F: 'lengthPrice', unit: '元/米' },
  AREA: { L: '面积', K: 'AREA', V: 1 << 2, F: 'areaPice', unit: '元/平方米' }
}
constantize(wageQuotaTypeEnum)

// 围护产品类型
const mesEnclosureTypeEnum = {
  SANDWICH_BOARD: { L: '夹芯板', K: 'SANDWICH_BOARD', V: 1 },
  PRESSED_PLATE: { L: '压型板', K: 'PRESSED_PLATE', V: 2 },
  FLOOR_PLATE: { L: '开闭口楼承板', K: 'FLOOR_PLATE', V: 4 },
  TRUSS_FLOOR_PLATE: { L: '桁架式楼承板', K: 'TRUSS_FLOOR_PLATE', V: 3 },
  FOLDING_PIECE: { L: '折边件', K: 'FOLDING_PIECE', V: 6 }
}
constantize(mesEnclosureTypeEnum)

// 任务下发状态类型
const taskIssueTypeEnum = {
  NOT_ISSUED: { L: '未下发', K: 'NOT_ISSUED', V: false, T: 'danger' },
  HAS_ISSUED: { L: '已下发', K: 'HAS_ISSUED', V: true, T: 'success' }
}
constantize(taskIssueTypeEnum)

// 可打包类型
const packTypeEnum = {
  STRUCTURE: { L: '结构', K: 'STRUCTURE', V: productTypeEnum.ARTIFACT.V, T: '' },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: productTypeEnum.ENCLOSURE.V, T: 'warning' },
  AUXILIARY_MATERIAL: { L: '辅材', K: 'AUXILIARY_MATERIAL', V: productTypeEnum.AUXILIARY_MATERIAL.V, T: 'success' }
}
constantize(packTypeEnum)

const qrCodeTypeEnum = {
  PACKAGE: { L: '包', K: 'PACKAGE', V: 11 }
}
constantize(qrCodeTypeEnum)

// 打包清单状态
const packStatusTypeEnum = {
  // UNENTRUCK: { L: '未装车', K: 'UNENTRUCK', V: 1 },
  // ENTRUCK: { L: '已装车', K: 'ENTRUCK', V: 2 },
  // CHECKED: { L: '已出库', K: 'CHECKED', V: 3 }
  UNENTRUCK: { L: '未装车', K: 'UNENTRUCK', V: false, T: '' },
  ENTRUCK: { L: '已装车', K: 'ENTRUCK', V: true, T: 'warning' }
}
constantize(packStatusTypeEnum)

export {
  teamTypeEnum,
  teamAttributeEnum,
  productTypeEnum,
  processMaterialListTypeEnum,
  processTypeEnum,
  processInspectTypeEnum,
  processReportTypeEnum,
  wageQuotaTypeEnum,
  mesEnclosureTypeEnum,
  taskIssueTypeEnum,
  packTypeEnum,
  qrCodeTypeEnum,
  packStatusTypeEnum
}

export default {
  teamTypeEnum,
  teamAttributeEnum,
  productTypeEnum,
  processMaterialListTypeEnum,
  processTypeEnum,
  processInspectTypeEnum,
  processReportTypeEnum,
  wageQuotaTypeEnum,
  mesEnclosureTypeEnum,
  taskIssueTypeEnum,
  packTypeEnum,
  qrCodeTypeEnum,
  packStatusTypeEnum
}
