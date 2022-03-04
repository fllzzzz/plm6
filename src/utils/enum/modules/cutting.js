import { constantize } from '../base'

// 钢板状态

// 0未分配 1 已分配 2已下发 3切割中 4已切割 5暂停 6终止
const steelPlateEnum = {
  ALL: { L: '全部', K: 'ALL', V: undefined },
  UNASSIGNED: { L: '未分配', K: 'UNASSIGNED', V: 0 },
  ASSIGNED: { L: '已分配', K: 'ASSIGNED', V: 1 },
  ISSUED: { L: '已下发', K: 'ISSUED', V: 2 },
  CUTTING: { L: '切割中', K: 'CUTTING', V: 3 },
  CUT: { L: '已切割', K: 'CUT', V: 4 },
  SUSPEND: { L: '暂停', K: 'SUSPEND', V: 5 },
  TERMINATION: { L: '终止', K: 'TERMINATION', V: 6 }
}
constantize(steelPlateEnum)

// 代理连接状态
const MessageTypeEnum = {
  SMS_NOTIFICATION: { L: '离线', K: 'SMS_NOTIFICATION', V: 0 },
  VERIFICATION_CODE: { L: '在线', K: 'VERIFICATION_CODE', V: 1 },
  PROMOTION_SMS: { L: '连线中', K: 'PROMOTION_SMS', V: 2 }
}
constantize(MessageTypeEnum)

export {
  steelPlateEnum,
  MessageTypeEnum
}

export default {
  steelPlateEnum,
  MessageTypeEnum
}
