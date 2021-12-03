import { constantize } from '../base'

// 制造类型
const manufactureTypeEnum = {
  HOMEMADE: { L: '自制', K: 'HOMEMADE', V: 1, T: '' },
  OUTSOURCE: { L: '外包', K: 'OUTSOURCE', V: 2, T: 'warning' }
}
constantize(manufactureTypeEnum)

const processingColorsEnum = {
  UNSTART: { L: '未开始', K: 'NORMAL', V: 1 << 0, COLOR: '#f5f7fa' },
  PROCESS: { L: '进行中', K: 'PROCESS', V: 1 << 1, COLOR: '#ffba00' },
  COMPLETE: { L: '已完成', K: 'NORMAL', V: 1 << 2, COLOR: '#40ed8d' },
  ABNORMAL: { L: '异常', K: 'ABNORMAL', V: 1 << 3, COLOR: '#e64242' }
}
constantize(processingColorsEnum)

export {
  manufactureTypeEnum,
  processingColorsEnum
}

export default {
  manufactureTypeEnum,
  processingColorsEnum
}
