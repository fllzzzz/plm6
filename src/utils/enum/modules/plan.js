import { constantize } from '../base'

/**
 * L:label 名称,
 * SL: short label 简称
 * K:key 键,
 * V:value 值
 * T: type 类型，往往对应element中的type
 */

// 制造类型
const manufactureTypeEnum = {
  HOMEMADE: { L: '自制', K: 'HOMEMADE', V: 0 },
  OUTSOURCE: { L: '外包', K: 'OUTSOURCE', V: 1 }
}
constantize(manufactureTypeEnum)

// 部门计划类型
const overallPlanTypeEnum = {
  DEEPEN: { L: '深化计划', K: 'DEEPEN', V: 0 },
  PROCESS: { L: '加工计划', K: 'PROCESS', V: 1 },
  INSTALL: { L: '安装计划', K: 'INSTALL', V: 2 },
  PURCHASE: { L: '采购计划', K: 'PURCHASE', V: 3 }
}
constantize(overallPlanTypeEnum)

// 部门计划类型
const areaPlanTypeEnum = {
  DEEPEN: { L: '深化计划', K: 'DEEPEN', V: 1 << 0 },
  PROCESS: { L: '加工计划', K: 'PROCESS', V: 1 << 1 },
  INSTALL: { L: '安装计划', K: 'INSTALL', V: 1 << 2 }
}
constantize(areaPlanTypeEnum)

// 计划状态
const overallPlanStatusEnum = {
  PROCESS: { L: '进行中', K: 'PROCESS', V: 0 },
  COMPLETE: { L: '已完成', K: 'COMPLETE', V: 1 },
  PARTIALLY_COMPLETED: { L: '部分完成', K: 'COMPLETE', V: 2 }
}
constantize(overallPlanStatusEnum)

// 零件剪板类型
const shearTypeEnum = {
  SHEARING: { L: '剪板', K: 'SHEARING', V: 1 },
  CNC: { L: '数控', K: 'CNC', V: 2 }
}
constantize(shearTypeEnum)

// 进行中/暂停
const processingEnum = {
  PROCESS: { L: '进行', K: 'TRUE', V: 1 },
  PAUSE: { L: '暂停', K: 'FALSE', V: 0 }
}
constantize(processingEnum)

// 图纸类型
const planTypeEnum = {
  ARTIFACT: { L: '构件', K: 'ARTIFACT', V: 0 },
  MACHINE_PART: { L: '零件', K: 'MACHINE_PART', V: 1 },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: 2 }
}
constantize(planTypeEnum)

// 技术资料类型
const technicalDataTypeEnum = {
  BLUEPRINT: { L: '蓝图', K: 'BLUEPRINT', V: 1 },
  CHANGE_FILE: { L: '变更文件', K: 'CHANGE_FILE', V: 2 },
  MODEL: { L: '模型', K: 'MODEL', V: 3 },
  OTHER_FILE: { L: '其他文件', K: 'OTHER_FILE', V: 4 }
}
constantize(technicalDataTypeEnum)

// 围护创建类型
const enclosureCreateTypeEnum = {
  UPLOAD: { L: '导入', K: 'UPLOAD', V: 1 },
  TYPE_IN: { L: '输入', K: 'TYPE_IN', V: 2 }
}
constantize(enclosureCreateTypeEnum)

export {
  manufactureTypeEnum, // 制造类型
  overallPlanTypeEnum, // 部门计划类型
  overallPlanStatusEnum, // 计划状态
  shearTypeEnum, // 零件剪板类型
  processingEnum,
  planTypeEnum,
  technicalDataTypeEnum,
  areaPlanTypeEnum,
  enclosureCreateTypeEnum
}

export default {
  manufactureTypeEnum, // 制造类型
  overallPlanTypeEnum, // 部门计划类型
  overallPlanStatusEnum, // 计划状态
  shearTypeEnum, // 零件剪板类型
  processingEnum,
  planTypeEnum,
  technicalDataTypeEnum,
  areaPlanTypeEnum,
  enclosureCreateTypeEnum
}
