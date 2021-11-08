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

export {
  teamTypeEnum,
  teamAttributeEnum,
  productTypeEnum,
  processMaterialListTypeEnum,
  processTypeEnum,
  processInspectTypeEnum,
  processReportTypeEnum,
  wageQuotaTypeEnum
}

export default {
  teamTypeEnum,
  teamAttributeEnum,
  productTypeEnum,
  processMaterialListTypeEnum,
  processTypeEnum,
  processInspectTypeEnum,
  processReportTypeEnum,
  wageQuotaTypeEnum
}
