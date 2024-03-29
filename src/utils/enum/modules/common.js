import { constantize } from '../base'

// 使用状态
const enabledEnum = {
  TRUE: { L: '启用', K: 'TRUE', V: true },
  FALSE: { L: '禁用', K: 'FALSE', V: false }
}
constantize(enabledEnum)

// 是否
const whetherEnum = {
  TRUE: { L: '是', K: 'TRUE', V: true, COLOR: '' },
  FALSE: { L: '否', K: 'FALSE', V: false, COLOR: '' }
}
constantize(whetherEnum)

// 是否打印
const boolPrintedEnum = {
  FALSE: { L: '未打印', K: 'TRUE', V: false, COLOR: '' },
  TRUE: { L: '已打印', K: 'FALSE', V: true, COLOR: '' }
}
constantize(boolPrintedEnum)

// 配置状态
const configureStatusEnum = {
  UNFINISHED: { L: '未配置', K: 'UNFINISHED', V: 0 },
  FINISHED: { L: '已配置', K: 'FINISHED', V: 1 }
}
constantize(configureStatusEnum)

// 重量类型
const weightTypeEnum = {
  NET: { L: '净重', K: 'NET', V: 0 },
  GROSS: { L: '毛重', K: 'GROSS', V: 1 },
  NONE: { L: '不显示', K: 'NONE', V: -1 }
}
constantize(weightTypeEnum)

// 进行中/暂停  流程状态
const processingEnum = {
  PROCESS: { L: '进行', K: 'TRUE', V: 1 },
  PAUSE: { L: '暂停', K: 'FALSE', V: 0 }
}
constantize(processingEnum)

// TODO: 废弃（请勿使用）商务审核状态
const businessAuditStatusEnum = {
  AUDIT_NOT_PASS: { L: '未审核', K: 'AUDIT_NOT_PASS', V: 0 },
  AUDIT_PASS: { L: '已通过', K: 'AUDIT_PASS', V: 1 },
  AUDIT_REFUSE: { L: '已拒绝', K: 'AUDIT_REFUSE', V: 2 }
}
constantize(businessAuditStatusEnum)

// 审核状态
const reviewStatusEnum = {
  UNREVIEWED: { L: '待审核', SL: '待确签', K: 'UNREVIEWED', V: 1 << 0, TAG: '' },
  PASS: { L: '已通过', SL: '已确签', K: 'PASS', V: 1 << 1, TAG: 'success' },
  REFUSE: { L: '已拒绝', SL: '已拒绝', K: 'REFUSE', V: 1 << 2, TAG: 'danger' }
}
constantize(reviewStatusEnum)

// 安装审核状态
const installationAuditStatusEnum = {
  UNCHECKED: { L: '未审核', K: 'UNCHECKED', V: 0 },
  CHECKED: { L: '已审核', K: 'CHECKED', V: 1 }
}
constantize(installationAuditStatusEnum)

// 签证类型
const visaTypeEnum = {
  VISA: { L: '签证单', K: 'VISA', V: 1 },
  SETTLEMENT: { L: '结算单', K: 'SETTLEMENT', V: 2 }
}
constantize(visaTypeEnum)

// 单位类型
const unitTypeEnum = {
  DIGIT: { L: '计数单位', K: 'DIGIT', V: 1 },
  WEIGHT: { L: '质量单位', K: 'WEIGHT', V: 2 },
  LENGTH: { L: '长度单位', K: 'LENGTH', V: 3 },
  AREA: { L: '面积单位', K: 'AREA', V: 4 },
  VOLUME: { L: '体积单位', K: 'VOLUME', V: 5 }
}
constantize(unitTypeEnum)

// 数值或百分比
const numOrPctEnum = {
  NUMBER: { L: '数值', K: 'NUMBER', V: 1 },
  PERCENTAGE: { L: '百分比', K: 'PERCENTAGE', V: 2 }
}
constantize(numOrPctEnum)

// 数据来源
const dataSourceSysEnum = {
  CURRENT: { L: '当前系统', K: 'CURRENT', V: 1 },
  OA: { L: 'OA', K: 'OA', V: 2 },
  DD: { L: '钉钉', K: 'DD', V: 3 }
}
constantize(dataSourceSysEnum)

// 操作类型
const operationTypeEnum = {
  ADD: { L: '新增', K: 'ADD', V: 1, T: 'primary' },
  EDIT: { L: '修改', K: 'EDIT', V: 2, T: 'info' },
  DELETE: { L: '删除', K: 'DELETE', V: 3, T: 'danger' }
}
constantize(operationTypeEnum)

// 车间类型
const workshopTypeEnum = {
  BUILDING: { L: '建钢', K: 'BUILDING', V: 1 << 0 },
  BRIDGE: { L: '桥梁', K: 'BRIDGE', V: 1 << 1 },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: 1 << 2 }
}
constantize(workshopTypeEnum)

// 文件分类
// const fileClassifyEnum = {
//   MODEL: { L: '3D模型', K: 'MODEL', V: -2 },
//   OTHER: { L: '其他', K: 'OTHER', V: -1 },
//   CONTRACT_ATT: { L: '合同附件', K: 'CONTRACT_ATT', V: 1 },
//   CHANGE_LIST_ATT: { L: '变更清单附件', K: 'CHANGE_LIST_ATT', V: 4 },
//   PROBLEM_REPORT_ATT: { L: '问题报告附件', K: 'PROBLEM_REPORT_ATT', V: 5 },
//   SUPPLIER_ATT: { L: '供应商附件', K: 'SUPPLIER_ATT', V: 7 },
//   SECTION_ATT: { L: '型材导入附件', K: 'SECTION_ATT', V: 8 },
//   CONTRACT_BUSINESS_SETTLEMENT_ATT: { L: '合同商务结算附件', K: 'CONTRACT_BUSINESS_SETTLEMENT_ATT', V: 9 },
//   PURCHASE_ORDER_AIT: { L: '采购合同附件', K: 'PURCHASE_ORDER_AIT', V: 12 },
//   CONSTRUCTION_AIT: { L: '施工资料附件', K: 'CONSTRUCTION_AIT', V: 801 }
// }
// constantize(fileClassifyEnum)

export {
  // fileClassifyEnum, // 文件分类
  dataSourceSysEnum, // 数据来源
  processingEnum, // 进行中/暂停  流程状态
  enabledEnum, // 使用状态
  whetherEnum, // 是否
  boolPrintedEnum, // 是否打印
  configureStatusEnum, // 配置状态
  numOrPctEnum, // 数字或者百分比类型
  weightTypeEnum, // 重量类型
  unitTypeEnum, // 单位类型
  reviewStatusEnum, // 审核状态
  visaTypeEnum, // 签证类型
  businessAuditStatusEnum, // 商务审核状态
  installationAuditStatusEnum, // 安装审核状态
  workshopTypeEnum, // 车间类型
  operationTypeEnum // 操作类型
}

export default {
  // fileClassifyEnum, // 文件分类
  dataSourceSysEnum, // 数据来源
  processingEnum, // 进行中/暂停  流程状态
  enabledEnum, // 使用状态
  whetherEnum, // 是否
  boolPrintedEnum, // 是否打印
  configureStatusEnum, // 配置状态
  numOrPctEnum, // 数字或者百分比类型
  weightTypeEnum, // 重量类型
  unitTypeEnum, // 单位类型
  reviewStatusEnum, // 审核状态
  visaTypeEnum, // 签证类型
  businessAuditStatusEnum, // 商务审核状态
  installationAuditStatusEnum, // 安装审核状态
  workshopTypeEnum, // 车间类型
  operationTypeEnum // 操作类型
}

