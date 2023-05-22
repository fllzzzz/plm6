import { constantize } from '../base'

// 变更记录状态
const changeRecordStatusEnum = {
  UNCONFIRMED: { L: '未确认', K: 'UNCONFIRMED', V: 1 << 0, T: 'danger' },
  CONFIRMING: { L: '处理中', K: 'CONFIRMING', V: 1 << 1, T: '' },
  CONFIRMED: { L: '处理完成', K: 'CONFIRMED', V: 1 << 2, T: 'success' }
}
constantize(changeRecordStatusEnum)

// 制造类型
const manufactureTypeEnum = {
  HOMEMADE: { L: '自制', K: 'HOMEMADE', V: 1, T: '' },
  OUTSOURCE: { L: '外包', K: 'OUTSOURCE', V: 2, T: 'warning' }
}
constantize(manufactureTypeEnum)

const processingColorsEnum = {
  UNSTART: { L: '未开始', K: 'NORMAL', V: 1 << 0, T: 'info', COLOR: '#f5f7fa' },
  PROCESS: { L: '进行中', K: 'PROCESS', V: 1 << 1, T: 'warning', COLOR: '#ffba00' },
  COMPLETE: { L: '已完成', K: 'NORMAL', V: 1 << 2, T: 'success', COLOR: '#40ed8d' },
  ABNORMAL: { L: '异常', K: 'ABNORMAL', V: 1 << 3, T: 'danger', COLOR: '#ff6d6d' }
}
constantize(processingColorsEnum)

const problemTypeEnum = {
  QUALITY: { L: '质量问题', K: 'QUALITY', V: 1 << 0, T: 'info' },
  SECURITY: { L: '安全问题', K: 'SECURITY', V: 1 << 1, T: 'warning' },
  ENVIRONMENT: { L: '环境问题', K: 'ENVIRONMENT', V: 1 << 2, T: 'success' },
  SPECIAL_QUALITY: { L: '特殊质量问题', K: 'SPECIAL_QUALITY', V: 1 << 3, T: 'danger' }
}
constantize(problemTypeEnum)

// 生产订单排期状态
const scheduleStatusEnum = {
  NOT: { L: '未排期', K: 'NOT', V: 1 << 0 },
  PART: { L: '部分排期', K: 'PART', V: 1 << 1 },
  COMPLETED: { L: '排期完毕', K: 'COMPLETED', V: 1 << 2 }
}
constantize(scheduleStatusEnum)

// 生产订单 月份
const monthNumEnum = {
  ONE: { L: '最近一个月交货', K: 'ONE', V: 1 },
  TWO: { L: '最近二个月交货', K: 'TWO', V: 2 },
  THREE: { L: '最近三个月交货', K: 'THREE', V: 3 },
  SIX: { L: '最近半年交货', K: 'SIX', V: 6 },
  TWELVE: { L: '最近一年交货', K: 'TWELVE', V: 12 }
}
constantize(monthNumEnum)

export {
  changeRecordStatusEnum,
  manufactureTypeEnum,
  processingColorsEnum,
  scheduleStatusEnum,
  monthNumEnum,
  problemTypeEnum
}

export default {
  changeRecordStatusEnum,
  manufactureTypeEnum,
  processingColorsEnum,
  scheduleStatusEnum,
  monthNumEnum,
  problemTypeEnum
}
