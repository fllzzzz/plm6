import { constantize } from '../base'

// 入库填写方式（金额及工厂在什么阶段填写）
const inboundFillWayEnum = {
  AUDITING: { L: '入库审核时填写', K: 'AUDITING', V: 1 << 0 },
  APPLICATION: { L: '入库提交时填写', K: 'APPLICATION', V: 1 << 1 }
}
constantize(inboundFillWayEnum)

export {
  inboundFillWayEnum
}

export default {
  inboundFillWayEnum
}
