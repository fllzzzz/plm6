import { constantize } from '../base'

// 围护创建计划状态
const enclosurePlanTypeEnum = {
  NOT: { L: '计划未创建', K: 'NOT', V: 0, TAG: 'danger' },
  DOING: { L: '部分计划已创建', K: 'DOING', V: 2, TAG: 'warning' },
  ALREADY: { L: '计划已创建', K: 'ALREADY', V: 1, TAG: 'success' }
}
constantize(enclosurePlanTypeEnum)

// 排产状态
const schedulingEnum = {
  NOT_SCHEDULING: { L: '未排产', K: 'NOT_SCHEDULING', V: 1 << 0, TAG: 'danger' },
  PARTIAL_SCHEDULING: { L: '部分排产', K: 'PARTIAL_SCHEDULING', V: 1 << 1, TAG: 'warning' },
  SCHEDULING_END: { L: '排产结束', K: 'SCHEDULING_END', V: 1 << 2, TAG: 'success' }
}
constantize(schedulingEnum)

export {
  schedulingEnum,
  enclosurePlanTypeEnum
}

export default {
  schedulingEnum,
  enclosurePlanTypeEnum
}
