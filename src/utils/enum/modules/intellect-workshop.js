import { constantize } from '../base'

/**
 * @author dhh
 * L:label 名称,
 * SL: short label 简称
 * K:key 键,
 * V:value 值
 * T: type 类型，往往对应element中的type
 */

// TODO:入发存状态
const intellectWorkshopStatusEnum = {
  PROCESS: { L: '进行中', K: 'PROCESS', V: 1 << 0, TAG: '' },
  COMPLETE: { L: '已完工', K: 'COMPLETE', V: 1 << 2, TAG: 'warning' },
  SUSPEND: { L: '暂停中', K: 'SUSPEND', V: 1 << 1, TAG: 'danger' }
}
constantize(intellectWorkshopStatusEnum)

// TODO:部件类型
const assembleTypeEnum = {
  WELD: { L: '焊接', K: 'WELD', V: 1 << 0 },
  STEEL: { L: '型材', K: 'STEEL', V: 1 << 1 }
}
constantize(assembleTypeEnum)

// TODO:查询类型
const sendSearchTypeEnum = {
  RECEIVE: { L: '入库', K: 'RECEIVE', V: 1 },
  SEND: { L: '出库', K: 'SEND', V: 2 },
  STOCK: { L: '库存', K: 'STOCK', V: 3 }
}
constantize(sendSearchTypeEnum)

export {
  intellectWorkshopStatusEnum, // 入发存状态
  assembleTypeEnum, // 部件类型
  sendSearchTypeEnum // 查询类型
}

export default {
  intellectWorkshopStatusEnum, // 入发存状态
  assembleTypeEnum, // 部件类型
  sendSearchTypeEnum // 查询类型
}
