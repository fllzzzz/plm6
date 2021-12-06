import { constantize } from '../base'

/**
 * @author dhh
 * L:label 名称,
 * SL: short label 简称
 * K:key 键,
 * V:value 值
 * T: type 类型，往往对应element中的type
 */

// TODO:项目状态
const projectStatusEnum = {
  PROCESS: { L: '进行中', K: 'PROCESS', V: 0 },
  SUSPEND: { L: '已暂停', K: 'SUSPEND', V: 1 },
  COMPLETE: { L: '已完工', K: 'COMPLETE', V: 2 }
}
constantize(projectStatusEnum)

// 项目状态
// const projectStatusEnum = {
//   PROCESS: { L: '进行中', K: 'PROCESS', V: 1 },
//   SUSPEND: { L: '已暂停', K: 'SUSPEND', V: 2 },
//   COMPLETE: { L: '已完工', K: 'COMPLETE', V: 3 }
// }
// constantize(projectStatusEnum)

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
  STRUCTURE: { L: '结构', K: 'STRUCTURE', V: 1<<0 },
  PROFILEDPLATE: { L: '压型彩板', K: 'PROFILEDPLATE', V: 1<<1 },
  TRUSSFLOORPLATE: { L: '桁架楼承板', K: 'TRUSSFLOORPLATE', V: 1<<3 },
  PRESSUREBEARINGPLATE: { L: '压型楼承板', K: 'PRESSUREBEARINGPLATE', V: 1<<4 },
  SANDWICH_BOARD: { L: '夹芯板', K: 'SANDWICH_BOARD', V: 1<<2 }
}
constantize(TechnologyTypeEnum)

// all技术交底类型
const TechnologyTypeAllEnum = {
  STRUCTURE: { L: '结构', K: 'STRUCTURE', V: 1<<0 },
  PROFILEDPLATE: { L: '压型彩板', K: 'PROFILEDPLATE', V: 1<<1 },
  TRUSSFLOORPLATE: { L: '桁架楼承板', K: 'TRUSSFLOORPLATE', V: 1<<3 },
  PRESSUREBEARINGPLATE: { L: '压型楼承板', K: 'PRESSUREBEARINGPLATE', V: 1<<4 },
  SANDWICH_BOARD: { L: '夹芯板', K: 'SANDWICH_BOARD', V: 1<<2 },
  BENDING: { L: '折边件', K: 'BENDING', V: 6 }
}
constantize(TechnologyTypeAllEnum)

// 项目类型xin
const projectTypeEnumN = {
  STEEL: { L: '建筑钢结构', K: 'STEEL', V: 1 },
  BRIDGE: { L: '桥梁钢结构', K: 'BRIDGE', V: 2 },
  CARBARN: { L: '立体停车库', K: 'CARBARN', V: 3 }
  // ENCLOSURE: { L: '围护材料', K: 'ENCLOSURE', V: 4 }
}
constantize(projectTypeEnumN)


// 业务类型
const businessTypeEnum = {
  MACHINING: { L: '加工承揽', K: 'MACHINING', V: 1 },
  INSTALLATION: { L: '项目承包', K: 'INSTALLATION', V: 2 }
}
constantize(businessTypeEnum)

// // 合同订单类型
// const orderTypeEnum = {
//   MACHINING: { L: '加工订单', K: 'MACHINING', V: 0 },
//   INSTALLATION: { L: '工程项目', K: 'INSTALLATION', V: 1 }
// }
// constantize(orderTypeEnum)

// 结算状态
const settlementStatusEnum = {
  UNSETTLEMENT: { L: '未结算', K: 'THEORY', V: 0, COLOR: '#e64242' },
  SETTLED: { L: '已结算', K: 'OVERWEIGHT', V: 1, COLOR: '#13ce66' }
}
constantize(settlementStatusEnum)

// 付款方式
const paymentModeEnum = {
  PUBLIC_TRANSFER: { L: '对公转账', K: 'PUBLIC_TRANSFER', V: 1 << 0 },
  PRIVATE_TRANSFER: { L: '对私转账', K: 'PRIVATE_TRANSFER', V: 1 << 1 },
  OTHER_TRANSFER: { L: '其他转账', K: 'OTHER_TRANSFER', V: 1 << 2 }
}
constantize(paymentModeEnum)

// 票据类型
const invoiceTypeEnum = {
  SPECIAL: { L: '增值税专用发票', SL: '专票', K: 'SPECIAL', V: 1 << 0 },
  ORDINARY: { L: '增值税普通发票', SL: '普票', K: 'ORDINARY', V: 1 << 1 },
  RECEIPT: { L: '收据', SL: '收据', K: 'RECEIPT', V: 1 << 2 }
}
constantize(invoiceTypeEnum)

// 是否含税
const isTaxEnum = {
  YES: { L: '是',  K: 'YES', V: true },
  NO: { L: '否',  K: 'NO', V: false }
}
constantize(isTaxEnum)

// 工程结算类型新
const engineerSettlementTypeEnumN = {
  THEORY: { L: '理计结算', SL: '理计', K: 'THEORY', V: 1 << 0, COLOR: '#1682e6' },
  OVERWEIGHT: { L: '磅计结算', SL: '磅计', K: 'OVERWEIGHT', V: 1 << 1, COLOR: '#e64242' }
}
constantize(engineerSettlementTypeEnumN)

// 审核类型
const auditTypeEnum = {
  AUDITING: { L: '审核中', K: 'AUDITING', V: 1<<0 },
  PASS: { L: '通过', K: 'PASS', V: 1 << 1 },
  REJECT: { L: '驳回', K: 'REJECT', V: 1 << 2 }
}
constantize(auditTypeEnum)

// 围护结算类型
const enclosureSettlementTypeEnum = {
  LENGTH: { L: '按长度计价', SL: '长度', K: 'LENGTH', V: 1 },
  AREA: { L: '按面积计价', SL: '面积', K: 'AREA', V: 2 }
}
constantize(enclosureSettlementTypeEnum)

// TODO: 运输方式 与提货方式重复
const transportModeEnum = {
  HOME_DELIVERY: { L: '运送到场', SL: '到场', K: 'HOME_DELIVERY', V: 1 << 0 },
  SELF_DELIVERY: { L: '客户自提', SL: '自提', K: 'SELF_DELIVERY', V: 1 << 1 }
}
constantize(transportModeEnum)

export {
  projectStatusEnum, // 项目状态
  projectTypeEnum, // 项目类型
  projectNameArrangementModeEnum, // 项目名称显示
  TechnologyTypeEnum, // 技术交底
  businessTypeEnum, //业务类型
  settlementStatusEnum, // 结算状态
  projectTypeEnumN, // 项目类型xin
  paymentModeEnum, // 付款方式
  invoiceTypeEnum,
  isTaxEnum,
  engineerSettlementTypeEnumN, // 工程结算类型
  enclosureSettlementTypeEnum, // 围护结算类型
  transportModeEnum,
  auditTypeEnum, //审核状态
  TechnologyTypeAllEnum
}

export default {
  projectStatusEnum, // 项目状态
  projectTypeEnum, // 项目类型
  projectNameArrangementModeEnum, // 项目名称显示
  TechnologyTypeEnum, // 技术交底
  settlementStatusEnum, // 结算状态
  projectTypeEnumN, // 项目类型xin
  paymentModeEnum, // 付款方式
  invoiceTypeEnum,
  isTaxEnum,
  engineerSettlementTypeEnumN, // 工程结算类型
  enclosureSettlementTypeEnum, // 围护结算类型
  transportModeEnum,
  auditTypeEnum, //审核状态
  TechnologyTypeAllEnum
}
