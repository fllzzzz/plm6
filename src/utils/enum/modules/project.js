import { constantize } from '../base'

/**
 * L:label 名称,
 * SL: short label 简称
 * K:key 键,
 * V:value 值
 * T: type 类型，往往对应element中的type
 */

// 收货状态
const deliveryStatusEnum = {
  ENTRUCK: { L: '装车', K: 'ENTRUCK', V: 1 << 0 },
  WEIGH: { L: '过磅', K: 'WEIGH', V: 1 << 1 },
  SHIPMENT: { L: '发运', K: 'SHIPMENT', V: 1 << 2 },
  DELIVERY: { L: '收货', K: 'DELIVERY', V: 1 << 3 }
}
constantize(deliveryStatusEnum)

// 施工阶段
const constructionEnum = {
  BEFORE: { L: '开工前', K: 'BEFORE', V: 1 << 0 },
  IN: { L: '施工中', K: 'IN', V: 1 << 1 },
  AFTER: { L: '竣工后', K: 'AFTER', V: 1 << 2 }
}
constantize(constructionEnum)

// 安装设置
const installSetEnum = {
  APP: { L: 'APP填报', K: 'APP', V: 1 },
  PC: { L: 'PC填报', K: 'PC', V: 0 }
}
constantize(installSetEnum)

// 产品类型
const installProjectTypeEnum = {
  ARTIFACT: { L: '结构制品', SL: '结构', K: 'ARTIFACT', V: 1 << 1, unit: '件', accountUnit: 'kg' },
  ENCLOSURE: { L: '围护制品', SL: '围护', K: 'ENCLOSURE', V: 1 << 2, unit: '张', accountUnit: 'm' },
  AUXILIARY: { L: '配套件', SL: '配套件', K: 'AUXILIARY', V: 1 << 3, unit: '', accountUnit: '' }
}
constantize(installProjectTypeEnum)

// 收安报表
const deliveryInstallTypeEnum = {
  ARTIFACT: { L: '结构制品', K: 'ARTIFACT', V: 1 << 1 },
  ENCLOSURE: { L: '围护制品', K: 'ENCLOSURE', V: 1 << 2 }
}
constantize(deliveryInstallTypeEnum)

// 质安整改
const qualityProblemChangeType = {
  FALSE: { L: '未整改', K: 'FALSE', V: false },
  TRUE: { L: '已整改', K: 'TRUE', V: true }
}
constantize(qualityProblemChangeType)

// 安装审核状态
const installAuditTypeEnum = {
  FALSE: { L: '未审核', K: 'FALSE', V: false },
  TRUE: { L: '已审核', K: 'TRUE', V: true }
}
constantize(installAuditTypeEnum)

// 分包账号状态
const relationStatusEnum = {
  FALSE: { L: '删除', K: 'FALSE', V: false },
  TRUE: { L: '正常', K: 'TRUE', V: true }
}
constantize(relationStatusEnum)

export {
  deliveryStatusEnum,
  constructionEnum,
  installSetEnum,
  installProjectTypeEnum,
  deliveryInstallTypeEnum,
  qualityProblemChangeType,
  installAuditTypeEnum,
  relationStatusEnum
}

export default {
  deliveryStatusEnum,
  constructionEnum,
  installSetEnum,
  installProjectTypeEnum,
  deliveryInstallTypeEnum,
  qualityProblemChangeType,
  installAuditTypeEnum,
  relationStatusEnum
}
