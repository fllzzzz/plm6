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
