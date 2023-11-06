
import { constantize } from '../base'

const boolWeightedAverageEnum = {
  TRUE: { L: '加权平均', K: 'TRUE', V: true },
  FALSE: { L: '不加权平均', K: 'FALSE', V: false }
}
constantize(boolWeightedAverageEnum)

// 重量计量方式
const weightMeasurementModeEnum = {
  THEORY: { L: '理计', K: 'THEORY', V: 1 << 0, COLOR: '#1682e6' },
  OVERWEIGHT: { L: '磅计', K: 'OVERWEIGHT', V: 1 << 1, COLOR: '#e64242' },
  MIXTURE: { L: '混合计', K: 'MIXTURE', V: 1 << 2 }
}
constantize(weightMeasurementModeEnum)

// 结算状态
const settlementStatusEnum = {
  UNSETTLEMENT: { L: '未结算', K: 'UNSETTLEMENT', V: 0, COLOR: '#e64242' },
  SETTLED: { L: '已结算', K: 'SETTLED', V: 1, COLOR: '#13ce66' }
}
constantize(settlementStatusEnum)

// 围护结算类型
const enclosureSettlementTypeEnum = {
  LENGTH: { L: '按长度计价', SL: '长度', K: 'LENGTH', V: 1 },
  AREA: { L: '按面积计价', SL: '面积', K: 'AREA', V: 2 }
}
constantize(enclosureSettlementTypeEnum)

// 欠款状态
const arrearsStatusEnum = {
  ARREARS: { L: '有欠款', K: 'ARREARS', V: 0 },
  NO_ARREARS: { L: '未欠款', K: 'NO_ARREARS', V: 1 }
}
constantize(arrearsStatusEnum)

// 新欠款状态
const newArrearsStatusEnum = {
  ARREARS: { L: '有欠款', K: 'ARREARS', V: 1 },
  NO_ARREARS: { L: '未欠款', K: 'NO_ARREARS', V: 0 }
}
constantize(newArrearsStatusEnum)

// 票据类型
const invoiceTypeEnum = {
  SPECIAL: { L: '增值税专用发票', SL: '专票', K: 'SPECIAL', V: 1 << 0 },
  ORDINARY: { L: '增值税普通发票', SL: '普票', K: 'ORDINARY', V: 1 << 1 },
  RECEIPT: { L: '收据', SL: '收据', K: 'RECEIPT', V: 1 << 2 }
}
constantize(invoiceTypeEnum)

// 付款方式
const paymentModeEnum = {
  PUBLIC_TRANSFER: { L: '对公转账', K: 'PUBLIC_TRANSFER', V: 1 << 0 },
  PRIVATE_TRANSFER: { L: '对私转账', K: 'PRIVATE_TRANSFER', V: 1 << 1 },
  OTHER_TRANSFER: { L: '其他转账', K: 'OTHER_TRANSFER', V: 1 << 2 }
}
constantize(paymentModeEnum)

// 付款(精细)方式
const paymentFineModeEnum = {
  PUBLIC_TRANSFER: { L: '对公转账', K: 'PUBLIC_TRANSFER', V: paymentModeEnum.PUBLIC_TRANSFER.V },
  ACCEPTANCE_DRAFT: { L: '承兑汇票', K: 'ACCEPTANCE_DRAFT', V: 1 << 3 },
  TRANSFER_CHECK: { L: '转账支票', K: 'TRANSFER_CHECK', V: 1 << 4 },
  PRIVATE_TRANSFER: { L: '对私转账', K: 'PRIVATE_TRANSFER', V: paymentModeEnum.PRIVATE_TRANSFER.V },
  CASH: { L: '现金', K: 'CASH', V: 1 << 5 },
  ALLOT_ACCOUNT: { L: '调账', K: 'ALLOT_ACCOUNT', V: 1 << 6 },
  GIVE_UP_PROFIT: { L: '让利', K: 'GIVE_UP_PROFIT', V: 1 << 7 },
  EXCHANGE_ACCOUNT: { L: '兑账', K: 'EXCHANGE_ACCOUNT', V: 1 << 8 },
  OTHER: { L: '其他', K: 'OTHER', V: 1 << 2 }
}
constantize(paymentFineModeEnum)

// 付款方式（增加现金）
const paymentOtherModeEnum = {
  PUBLIC_TRANSFER: { L: '对公转账', K: 'PUBLIC_TRANSFER', V: paymentModeEnum.PUBLIC_TRANSFER.V },
  ACCEPTANCE_DRAFT: { L: '承兑汇票', K: 'ACCEPTANCE_DRAFT', V: 1 << 3 },
  PRIVATE_TRANSFER: { L: '对私转账', K: 'PRIVATE_TRANSFER', V: paymentModeEnum.PRIVATE_TRANSFER.V },
  CASH: { L: '现金', K: 'CASH', V: 1 << 5 }
}
constantize(paymentOtherModeEnum)

// 付款方式（无现金）
const paymentNoCashModeEnum = {
  PUBLIC_TRANSFER: { L: '对公转账', K: 'PUBLIC_TRANSFER', V: paymentModeEnum.PUBLIC_TRANSFER.V },
  ACCEPTANCE_DRAFT: { L: '承兑汇票', K: 'ACCEPTANCE_DRAFT', V: 1 << 3 },
  PRIVATE_TRANSFER: { L: '对私转账', K: 'PRIVATE_TRANSFER', V: paymentModeEnum.PRIVATE_TRANSFER.V },
  ALLOT_ACCOUNT: { L: '调账', K: 'ALLOT_ACCOUNT', V: 1 << 6 },
  GIVE_UP_PROFIT: { L: '让利', K: 'GIVE_UP_PROFIT', V: 1 << 7 },
  EXCHANGE_ACCOUNT: { L: '兑账', K: 'EXCHANGE_ACCOUNT', V: 1 << 8 },
  OTHER: { L: '其他', K: 'OTHER', V: 1 << 2 }
}
constantize(paymentNoCashModeEnum)

const paymentReasonEnum = {
  ADVANCE_PAYMENT: { L: '预付款', K: 'ADVANCE_PAYMENT', V: '1' },
  SETTLED_PAYMENT: { L: '结算款', K: 'SETTLED_PAYMENT', V: '2' },
  PROGRESS_PAYMENT: { L: '进度款', K: 'PROGRESS_PAYMENT', V: '3' }
}
constantize(paymentReasonEnum)

export default {
  boolWeightedAverageEnum, // 是否加权平均
  invoiceTypeEnum, // 票据类型
  paymentModeEnum, // 付款方式
  paymentFineModeEnum, // 付款（精细）方式
  paymentOtherModeEnum,
  paymentNoCashModeEnum,
  arrearsStatusEnum, // 欠款状态
  newArrearsStatusEnum,
  weightMeasurementModeEnum, // 工程结算方式
  settlementStatusEnum, // 结算状态
  paymentReasonEnum, // 结算事由
  enclosureSettlementTypeEnum //  围护结算类型
}

export {
  boolWeightedAverageEnum, // 是否加权平均
  invoiceTypeEnum, // 票据类型
  paymentModeEnum, // 付款方式
  paymentFineModeEnum, // 付款（精细）方式
  paymentOtherModeEnum,
  paymentNoCashModeEnum,
  arrearsStatusEnum, // 欠款状态
  newArrearsStatusEnum,
  weightMeasurementModeEnum, // 工程结算方式
  settlementStatusEnum, // 结算状态
  paymentReasonEnum, // 结算事由
  enclosureSettlementTypeEnum //  围护结算类型
}
