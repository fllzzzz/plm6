import { constantize } from '../base'

// 排产状态
const schedulingEnum = {
  NOT_SCHEDULING: { L: '未排产', K: 'NOT_SCHEDULING', V: 1 << 0, TAG: 'danger' },
  PARTIAL_SCHEDULING: { L: '部分排产', K: 'PARTIAL_SCHEDULING', V: 1 << 1, TAG: 'warning' },
  SCHEDULING_END: { L: '排产结束', K: 'SCHEDULING_END', V: 1 << 2, TAG: 'success' }
}
constantize(schedulingEnum)

export {
  schedulingEnum
}

export default {
  schedulingEnum
}
