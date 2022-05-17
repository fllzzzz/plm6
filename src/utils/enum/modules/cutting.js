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
const messageTypeEnum = {
  SMS_NOTIFICATION: { L: '离线', K: 'SMS_NOTIFICATION', V: 0 },
  VERIFICATION_CODE: { L: '在线', K: 'VERIFICATION_CODE', V: 1 },
  PROMOTION_SMS: { L: '连线中', K: 'PROMOTION_SMS', V: 2 }
}
constantize(messageTypeEnum)

// 任务包零件上传状态
const uploadEnum = {
  NOT_UPLOAD: { L: '未上传', K: 'VERIFICATION_CODE', V: '0' },
  UPLOADING_UP: { L: '已上传', K: 'VERIFICATION_CODE', V: '1' },
  UPLOADING_UP_ENDING: { L: '套料结束', K: 'VERIFICATION_CODE', V: '2' }
}
constantize(uploadEnum)

const plateTypeEnum = {
  WING_PLATE: { L: '翼腹板', K: 'VERIFICATION_CODE', V: 16 },
  PART_BOARD: { L: '零件板', K: 'VERIFICATION_CODE', V: 2 }
}
constantize(plateTypeEnum)
const typeEnum = {
  WING_PLATE: { L: '按设备查看', K: 'VERIFICATION_CODE', V: 0 },
  PART_BOARD: { L: '按项目查看', K: 'VERIFICATION_CODE', V: 1 }
}
constantize(typeEnum)

// 排套状态
const nestingStateEnum = {
  NOT_LINED_UP: { L: '未排套', K: 'VERIFICATION_CODE', V: 0 },
  PARTIAL_ROW_SETS: { L: '部分排套', K: 'VERIFICATION_CODE', V: 1 },
  LINED_UP: { L: '排套结束', K: 'VERIFICATION_CODE', V: 2 }
}
constantize(nestingStateEnum)

// 套料状态
const nestingEnum = {
  NOT_LINED_UP: { L: '未套料', K: 'VERIFICATION_CODE', V: 0 },
  PARTIAL_ROW_SETS: { L: '部分套料', K: 'VERIFICATION_CODE', V: 1 },
  LINED_UP: { L: '套料结束', K: 'VERIFICATION_CODE', V: 2 }
}
constantize(nestingEnum)

// 排产状态
const schedulingEnum = {
  NOT_Scheduling_up: { L: '未排产', K: 'VERIFICATION_CODE', V: 0 },
  PARTIAL_Scheduling_up: { L: '部分排产', K: 'VERIFICATION_CODE', V: 1 },
  Scheduling_up: { L: '排产结束', K: 'VERIFICATION_CODE', V: 2 }
}
constantize(schedulingEnum)

// 切割状态
const cuttingEnum = {
  NOT_Cutting_up: { L: '未切割', K: 'VERIFICATION_CODE', V: 0 },
  PARTIAL_Cutting_up: { L: '部分切割', K: 'VERIFICATION_CODE', V: 1 },
  Cutting_up: { L: '切割完成', K: 'VERIFICATION_CODE', V: 2 }
}
constantize(cuttingEnum)

// 设备类型
const machineTypeEnum = {
  FLAME_CUTTING: { L: '火焰切割设备', K: 'VERIFICATION_CODE', V: '0' },
  PLASMA_CUTTING: { L: '等离子切割设备', K: 'VERIFICATION_CODE', V: '1' },
  LASER_CUTTING: { L: '激光切割设备', K: 'VERIFICATION_CODE', V: '2' }
}
constantize(machineTypeEnum)

export {
  typeEnum,
  steelPlateEnum,
  messageTypeEnum,
  plateTypeEnum,
  nestingStateEnum,
  nestingEnum,
  machineTypeEnum,
  schedulingEnum,
  cuttingEnum,
  uploadEnum
}

export default {
  typeEnum,
  steelPlateEnum,
  messageTypeEnum,
  plateTypeEnum,
  nestingStateEnum,
  nestingEnum,
  machineTypeEnum,
  schedulingEnum,
  cuttingEnum,
  uploadEnum
}

