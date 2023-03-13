import { constantize } from '@/utils/enum/base'

// 处理构件变更状态
export const artifactHandleStatus = {
  UN_HANDLE: {
    L: '未处理',
    K: 'UN_HANDLE',
    style: `border:1px solid rgb(0, 120, 252); background: #fff; color: rgb(0, 120, 252);`
  },
  HANDLED: {
    L: '已处理',
    K: 'HANDLED',
    style: `background: #0078fc; color: #fff;`
  },
  TAG: {
    L: '标记',
    K: 'TAG',
    style: `border:1px dashed rgb(0, 120, 252); background: #fff; color: rgb(0, 120, 252);`
  },
  NOT_HANDLE: {
    L: '无需处理',
    K: 'NOT_HANDLE',
    style: `background: #66aefd; color: #fff;`
  },
  CANCEL_HANDLE: {
    L: '取消变更',
    K: 'CANCEL_HANDLE',
    style: `background: #da0000; color: #fff;`
  }
}

// 变更类型枚举
const changeTypeEnum = {
  NEW: { L: '新', K: 'NEW', V: 1, C: '#0063d0' },
  ADD: { L: '加', K: 'ADD', V: 2, C: '#00ae11' },
  MINUS: { L: '减', K: 'MINUS', V: 3, C: '#e28a18' },
  EDIT: { L: '改', K: 'EDIT', V: 4, C: '#ae4e00' },
  DEL: { L: '删', K: 'DEL', V: 5, C: '#ff0000' }
}
constantize(changeTypeEnum)

// 部件变更操作类型
const assembleOperateTypeEnum = {
  EDIT: { L: '变更', K: 'EDIT', V: 1 },
  NEW: { L: '新增', K: 'NEW', V: 2 }
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

export { changeTypeEnum, assembleHandleMethodEnum, assembleOperateTypeEnum }
