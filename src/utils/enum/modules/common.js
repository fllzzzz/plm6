import { constantize } from '../base'

// 使用状态
const enabledEnum = {
  TRUE: { L: '启用', K: 'TRUE', V: true },
  FALSE: { L: '禁用', K: 'FALSE', V: false }
}
constantize(enabledEnum)

// 是否
const whetherEnum = {
  TRUE: { L: '是', K: 'TRUE', V: true },
  FALSE: { L: '否', K: 'FALSE', V: false }
}
constantize(whetherEnum)

// 重量类型
const weightTypeEnum = {
  NET: { L: '净重', K: 'NET', V: 0 },
  GROSS: { L: '毛重', K: 'GROSS', V: 1 }
}
constantize(weightTypeEnum)

// 进行中/暂停  流程状态
const processingEnum = {
  PROCESS: { L: '进行', K: 'TRUE', V: 1 },
  PAUSE: { L: '暂停', K: 'FALSE', V: 0 }
}
constantize(processingEnum)

// 商务审核状态
const businessAuditStatusEnum = {
  AUDIT_NOT_PASS: { L: '未审核', K: 'AUDIT_NOT_PASS', V: 0 },
  AUDIT_PASS: { L: '已通过', K: 'AUDIT_PASS', V: 1 },
  AUDIT_REFUSE: { L: '已拒绝', K: 'AUDIT_REFUSE', V: 2 }
}
constantize(businessAuditStatusEnum)

// 安装审核状态
const installationAuditStatusEnum = {
  UNCHECKED: { L: '未审核', K: 'UNCHECKED', V: 0 },
  CHECKED: { L: '已审核', K: 'CHECKED', V: 1 }
}
constantize(installationAuditStatusEnum)

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
  DELETE: { L: '删除', K: 'DELETE', V: 2, T: 'danger' },
  EDIT: { L: '修改', K: 'EDIT', V: 3, T: 'info' }
}
constantize(operationTypeEnum)

export {
  dataSourceSysEnum, // 数据来源
  processingEnum, // 进行中/暂停  流程状态
  enabledEnum, // 使用状态
  whetherEnum, // 是否
  numOrPctEnum, // 数字或者百分比类型
  weightTypeEnum, // 重量类型
  unitTypeEnum, // 单位类型
  businessAuditStatusEnum, // 商务审核状态
  installationAuditStatusEnum, // 安装审核状态
  operationTypeEnum // 操作类型
}

export default {
  dataSourceSysEnum, // 数据来源
  processingEnum, // 进行中/暂停  流程状态
  enabledEnum, // 使用状态
  whetherEnum, // 是否
  numOrPctEnum, // 数字或者百分比类型
  weightTypeEnum, // 重量类型
  unitTypeEnum, // 单位类型
  businessAuditStatusEnum, // 商务审核状态
  installationAuditStatusEnum, // 安装审核状态
  operationTypeEnum // 操作类型
}

