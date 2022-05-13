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

const PlateTypeEnum = {
  WING_PLATE: { L: '翼腹板', K: 'VERIFICATION_CODE', V: 16},
  PART_BOARD: { L: '零件板', K: 'VERIFICATION_CODE', V: 2}
}
constantize(PlateTypeEnum)

const TypeEnum = {
  WING_PLATE: { L: '按设备查看', K: 'VERIFICATION_CODE', V: 0 },
  PART_BOARD: { L: '按项目查看', K: 'VERIFICATION_CODE', V: 1 }
}
constantize(TypeEnum)

// 排套状态
const NestingStateEnum = {
  NOT_LINED_UP:{ L: '未排套',  K:'VERIFICATION_CODE', V: 0},
  PARTIAL_ROW_SETS:{ L: '部分排套', K:'VERIFICATION_CODE', V: 1},
  LINED_UP:{ L: '排套结束', K:'VERIFICATION_CODE', V: 2},
}
constantize(NestingStateEnum)

// 套料状态
const NestingEnum = {
  NOT_LINED_UP:{ L: '未套料', K:'VERIFICATION_CODE', V: 2},
  PARTIAL_ROW_SETS:{ L: '部分套料', K:'VERIFICATION_CODE', V: 1 },
  LINED_UP:{ L: '套料结束', K:'VERIFICATION_CODE', V: 0},
}
constantize(NestingEnum)

// 设备类型
const MachineTypeEnum = {
  FLAME_CUTTING:{L:'火焰切割设备',K:'VERIFICATION_CODE', V: '0'},
  PLASMA_CUTTING:{L:'等离子切割设备',K:'VERIFICATION_CODE', V: '1'},
  LASER_CUTTING:{L:'激光切割设备',K:'VERIFICATION_CODE', V: '2'}
}
constantize(MachineTypeEnum)

export {
  TypeEnum,
  steelPlateEnum,
  MessageTypeEnum,
  PlateTypeEnum,
  NestingStateEnum,
  NestingEnum,
  MachineTypeEnum
}

export default {
  TypeEnum,
  steelPlateEnum,
  MessageTypeEnum,
  PlateTypeEnum,
  NestingStateEnum,
  NestingEnum,
  MachineTypeEnum
}

