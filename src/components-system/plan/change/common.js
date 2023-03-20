import { constantize } from '@/utils/enum/base'

// 处理构件变更状态
const artifactHandleStatusEnum = {
  UN_HANDLE: {
    L: '未处理',
    K: 'UN_HANDLE',
    V: 1 << 0,
    style: `border:1px solid rgb(0, 120, 252); background: #fff; color: rgb(0, 120, 252);`
  },
  HANDLED: {
    L: '已处理',
    K: 'HANDLED',
    V: 1 << 1,
    style: `background: #0078fc; color: #fff;`
  },
  TAG: {
    L: '标记',
    K: 'TAG',
    V: 1 << 2,
    style: `border:1px dashed rgb(0, 120, 252); background: #fff; color: rgb(0, 120, 252);`
  },
  NOT_HANDLE: {
    L: '无需处理',
    K: 'NOT_HANDLE',
    V: 1 << 3,
    style: `background: #66aefd; color: #fff;`
  },
  CANCEL_HANDLE: {
    L: '取消变更',
    K: 'CANCEL_HANDLE',
    V: 1 << 4,
    style: `background: #da0000; color: #fff;`
  }
}
constantize(artifactHandleStatusEnum)

// 变更类型枚举
const changeTypeEnum = {
  NEW: { L: '新', K: 'NEW', V: 1 << 0, C: '#0063d0' },
  ADD: { L: '加', K: 'ADD', V: 1 << 1, C: '#00ae11' },
  EDIT: { L: '改', K: 'EDIT', V: 1 << 2, C: '#ae4e00' },
  REDUCE: { L: '减', K: 'REDUCE', V: 1 << 3, C: '#e28a18' },
  DEL: { L: '删', K: 'DEL', V: 1 << 4, C: '#ff0000' }
}
constantize(changeTypeEnum)

// 部件变更操作类型
const assembleOperateTypeEnum = {
  EDIT: { L: '变更', K: 'EDIT', V: 1, T: 'warning' },
  NEW: { L: '新增', K: 'NEW', V: 2, T: 'primary' }
}
constantize(assembleOperateTypeEnum)

// 部件变更处理方式
const assembleHandleMethodEnum = {
  ADD_LENGTH: { L: '加长', K: 'ADD_LENGTH', V: 1 << 0 },
  TRUNCATE: { L: '截短', K: 'TRUNCATE', V: 1 << 1 },
  PUNCH_HOLE: { L: '打孔', K: 'PUNCH_HOLE', V: 1 << 2 },
  OTHER: { L: '其他', K: 'OTHER', V: 1 << 3 }
}
constantize(assembleHandleMethodEnum)

export { changeTypeEnum, assembleHandleMethodEnum, artifactHandleStatusEnum, assembleOperateTypeEnum }
