import { constantize } from '../base'

// 班组类型
const teamTypeEnum = {
  TEAM: { L: '班组', V: 1 << 0 },
  INSPECTION: { L: '质检', V: 1 << 1 }
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
const componentTypeEnum = {
  ASSEMBLE: { L: '组立', SL: '一次工序', K: 'ASSEMBLE', V: 1 << 4, COLOR: '#40ed8d' },
  ARTIFACT: { L: '构件', SL: '二次工序', K: 'ARTIFACT', V: 1 << 1, COLOR: '#00babd' },
  MACHINE_PART: { L: '零件', SL: '零件', K: 'MACHINE_PART', V: 1 << 0, COLOR: '#fad400' },
  ENCLOSURE: { L: '围护', SL: '围护', K: 'ENCLOSURE', V: 1 << 2, COLOR: '#ff7800' },
  AUXILIARY_MATERIAL: { L: '辅材', SL: '辅材', K: 'AUXILIARY_MATERIAL', V: 1 << 3, COLOR: '#f5f7fa' }
}
constantize(componentTypeEnum)

// 构件工序
const artifactProcessEnum = {
  ONCE: { L: '一次工序', K: 'ONCE', V: componentTypeEnum.ASSEMBLE.V },
  TWICE: { L: '二次工序', K: 'TWICE', V: componentTypeEnum.ARTIFACT.V }
}
constantize(artifactProcessEnum)

// 含有工序的材料类型
const processMaterialListTypeEnum = {
  ARTIFACT: { L: '构件', K: 'ARTIFACT', V: componentTypeEnum.ARTIFACT.V, T: '' },
  MACHINE_PART: { L: '零件', K: 'MACHINE_PART', V: componentTypeEnum.MACHINE_PART.V, T: 'success' },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: componentTypeEnum.ENCLOSURE.V, T: 'warning' }
}
constantize(processMaterialListTypeEnum)

// 工价定额单价类型
const wageQuotaTypeEnum = {
  WEIGHT: { L: '按重量计价', K: 'WEIGHT', V: 1 << 0, F: 'weightPrice', unit: '元/吨', meteUnit: '吨', C_UNIT: 't', DP: 'COM_WT__T' },
  LENGTH: { L: '按长度计价', K: 'LENGTH', V: 1 << 1, F: 'lengthPrice', unit: '元/米', meteUnit: '米', C_UNIT: 'm', DP: 'COM_L__M' },
  AREA: { L: '按面积计价', K: 'AREA', V: 1 << 2, F: 'areaPice', unit: '元/平方米', meteUnit: '平方米', C_UNIT: '㎡', DP: 'COM_AREA__M2' }
}
constantize(wageQuotaTypeEnum)

// 楼承板子类型
const floorPlateTypeEnum = {
  TRUSS_FLOOR_PLATE: { L: '桁架式楼承板', K: 'TRUSS_FLOOR_PLATE', V: 1 << 3 },
  PRESSED_FLOOR_PLATE: { L: '压型楼承板', K: 'PRESSED_FLOOR_PLATE', V: 1 << 4 }
  // OPEN_CLOSED_FLOOR_PLATE: { L: '开闭口楼承板', K: 'OPEN_CLOSED_FLOOR_PLATE', V: 1 << 4 }
}
constantize(floorPlateTypeEnum)

// 围护产品类型
const mesEnclosureTypeEnum = {
  PRESSED_PLATE: { L: '压型板', K: 'PRESSED_PLATE', V: 1 << 1 },
  SANDWICH_BOARD: { L: '夹芯板', K: 'SANDWICH_BOARD', V: 1 << 2 },
  TRUSS_FLOOR_PLATE: floorPlateTypeEnum.TRUSS_FLOOR_PLATE,
  PRESSED_FLOOR_PLATE: floorPlateTypeEnum.PRESSED_FLOOR_PLATE,
  FOLDING_PIECE: { L: '折边件', K: 'FOLDING_PIECE', V: 1 << 5 }
  // FLOOR_PLATE: { L: '楼承板', K: 'FLOOR_PLATE', V: floorPlateTypeEnum.TRUSS_FLOOR_PLATE.V | floorPlateTypeEnum.PRESSED_FLOOR_PLATE.V }
}
constantize(mesEnclosureTypeEnum)

const projectComponentTypeEnum = {
  ARTIFACT: { L: '结构', K: 'ARTIFACT', V: 1 << 0 },
  ...mesEnclosureTypeEnum.ENUM
}
constantize(projectComponentTypeEnum)

// 报表展示 结构、围护
const reportComponentTypeEnum = {
  ARTIFACT: { L: '结构', K: 'ARTIFACT', V: componentTypeEnum.ARTIFACT.V },
  ENCLOSURE: componentTypeEnum.ENCLOSURE
}
constantize(reportComponentTypeEnum)

// 任务下发状态类型
const taskIssueTypeEnum = {
  NOT_ISSUED: { L: '未下发', K: 'NOT_ISSUED', V: false, T: 'danger' },
  HAS_ISSUED: { L: '已下发', K: 'HAS_ISSUED', V: true, T: 'success' }
}
constantize(taskIssueTypeEnum)

// 可打包类型
const packTypeEnum = {
  STRUCTURE: { L: '结构', K: 'STRUCTURE', V: componentTypeEnum.ARTIFACT.V, T: '' },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: componentTypeEnum.ENCLOSURE.V, T: 'warning' },
  AUXILIARY_MATERIAL: { L: '辅材', K: 'AUXILIARY_MATERIAL', V: componentTypeEnum.AUXILIARY_MATERIAL.V, T: 'success' }
}
constantize(packTypeEnum)

// 打包清单状态
const packStatusTypeEnum = {
  // UNENTRUCK: { L: '未装车', K: 'UNENTRUCK', V: 1 },
  // ENTRUCK: { L: '已装车', K: 'ENTRUCK', V: 2 },
  // CHECKED: { L: '已出库', K: 'CHECKED', V: 3 }
  UNENTRUCK: { L: '未装车', K: 'UNENTRUCK', V: false, T: '' },
  ENTRUCK: { L: '已装车', K: 'ENTRUCK', V: true, T: 'warning' }
}
constantize(packStatusTypeEnum)

// 收货状态
const receiptStatusEnum = {
  RECEIVED: { L: '已收货', K: 'RECEIVED', V: 1, COLOR: '#40ed8d', T: 'success' },
  UNRECEIVED: { L: '未收货', K: 'UNRECEIVED', V: 2, COLOR: '#f5f7fa', T: 'warning' }
}
constantize(receiptStatusEnum)

// 物流计价方式
const logisticsPriceTypeEnum = {
  WEIGHT: { L: '重量', K: 'WEIGHT', V: 1, T: '', unit: '元/吨' },
  TRAINS: { L: '车次', K: 'TRAINS', V: 2, T: 'warning', unit: '元/车' }
}
constantize(logisticsPriceTypeEnum)

// 发运审核状态
const shipAuditStatusEnum = {
  UNCHECKED: { L: '未审核', K: 'UNCHECKED', V: 1, T: 'warning' },
  CHECKED: { L: '已审核', K: 'CHECKED', V: 2, T: 'success' }
}
constantize(shipAuditStatusEnum)

// 变更异常处理状态
const abnormalHandleStatusEnum = {
  PENDING: { L: '待处理', K: 'PENDING', V: 1 << 0 },
  PROCESSING: { L: '处理中', K: 'PROCESSING', V: 1 << 1 },
  PROCESSING_COMPLETE: { L: '处理完成', K: 'PROCESSING_COMPLETE', V: 1 << 2 }
}
constantize(abnormalHandleStatusEnum)

// 变更上报类型状态
const abnormalReportTypeEnum = {
  NORMAL: { L: '正常上报', K: 'NORMAL', V: 0 },
  ABNORMAL: { L: '异常上报', K: 'ABNORMAL', V: 1 }
}
constantize(abnormalReportTypeEnum)

// 变更变更类型状态
const abnormalChangeTypeEnum = {
  ARTIFACT_REDUCE: { L: '构件减少', K: 'ARTIFACT_REDUCE', V: 1 << 0 },
  ARTIFACT_DELETE: { L: '构件删除', K: 'ARTIFACT_DELETE', V: 1 << 1 },
  ASSEMBLE_REDUCE: { L: '组立减少', K: 'ASSEMBLE_REDUCE', V: 1 << 2 },
  ASSEMBLE_DELETE: { L: '组立删除', K: 'ASSEMBLE_DELETE', V: 1 << 3 },
  MACHINE_PART_REDUCE: { L: '零件减少', K: 'MACHINE_PART_REDUCE', V: 1 << 4 },
  MACHINE_PART_DELETE: { L: '零件删除', K: 'MACHINE_PART_DELETE', V: 1 << 5 }
}
constantize(abnormalChangeTypeEnum)

// 问题整改状态
const improveStatusEnum = {
  WAIT_RECTIFIED: { L: '未整改', K: 'WAIT_RECTIFIED', V: 1 << 0, T: 'danger' },
  RECTIFIED: { L: '已整改', K: 'RECTIFIED', V: 1 << 1, T: 'success' },
  ADOPT: { L: '整改通过', K: 'ADOPT', V: 1 << 2, T: 'warning' },
  UN_ADOPT: { L: '整改未通过', K: 'UN_ADOPT', V: 1 << 3, T: 'info' }
}
constantize(improveStatusEnum)

// 油漆类型
const paintingTypeEnum = {
  PRIMER: { L: '底漆', K: 'PRIMER', V: 1 << 0 },
  INTERMEDIATE_PAINT: { L: '中间漆', K: 'INTERMEDIATE_PAINT', V: 1 << 1 },
  TOPCOAT: { L: '面漆', K: 'TOPCOAT', V: 1 << 2 }
}
constantize(paintingTypeEnum)

// 标签类型
const labelTypeEnum = {
  COMMON: { L: '常规型', K: 'COMMON', V: 1 << 0, size: {
    [componentTypeEnum.ARTIFACT.V]: ' 100 * 75 ',
    [componentTypeEnum.ENCLOSURE.V]: ' 100 * 30 '
  }},
  SIMPLE: { L: '简约型', K: 'SIMPLE', V: 1 << 1, size: {
    [componentTypeEnum.ARTIFACT.V]: ' 100 * 75 ',
    [componentTypeEnum.ENCLOSURE.V]: ' 无 '
  }},
  CUSTOM: { L: '定制型', K: 'CUSTOM', V: 1 << 2, size: {
    [componentTypeEnum.ARTIFACT.V]: ' 100 * 75 ',
    [componentTypeEnum.ENCLOSURE.V]: ' 100 * 50 '
  }}
}
constantize(labelTypeEnum)

// 打印的产品类型
const printProductTypeEnum = {
  ARTIFACT: { L: '构件', K: 'ARTIFACT', V: 1 << 0 },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: 1 << 1 },
  PACKAGE: { L: '包', K: 'PACKAGE', V: 1 << 2 }
}
constantize(printProductTypeEnum)

export {
  teamTypeEnum,
  teamAttributeEnum,
  componentTypeEnum,
  processMaterialListTypeEnum,
  processTypeEnum,
  processInspectTypeEnum,
  processReportTypeEnum,
  wageQuotaTypeEnum,
  mesEnclosureTypeEnum,
  floorPlateTypeEnum,
  taskIssueTypeEnum,
  packTypeEnum,
  packStatusTypeEnum,
  receiptStatusEnum,
  logisticsPriceTypeEnum,
  shipAuditStatusEnum,
  abnormalHandleStatusEnum,
  abnormalReportTypeEnum,
  abnormalChangeTypeEnum,
  improveStatusEnum,
  projectComponentTypeEnum,
  artifactProcessEnum,
  paintingTypeEnum,
  reportComponentTypeEnum,
  labelTypeEnum,
  printProductTypeEnum
}

export default {
  teamTypeEnum,
  teamAttributeEnum,
  componentTypeEnum,
  processMaterialListTypeEnum,
  processTypeEnum,
  processInspectTypeEnum,
  processReportTypeEnum,
  wageQuotaTypeEnum,
  mesEnclosureTypeEnum,
  floorPlateTypeEnum,
  taskIssueTypeEnum,
  packTypeEnum,
  packStatusTypeEnum,
  receiptStatusEnum,
  logisticsPriceTypeEnum,
  shipAuditStatusEnum,
  abnormalHandleStatusEnum,
  abnormalReportTypeEnum,
  abnormalChangeTypeEnum,
  improveStatusEnum,
  projectComponentTypeEnum,
  artifactProcessEnum,
  paintingTypeEnum,
  reportComponentTypeEnum,
  labelTypeEnum,
  printProductTypeEnum
}
