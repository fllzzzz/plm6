import { constantize } from '../base'

// 下料类别
const materialTypeEnum = {
  MANMADE_BLANKING: { L: '人工下料', SL: '人工下料', V: 1, COLOR: '#40ed8d' },
  NC_BLANKING: { L: '数控下料', SL: '数控下料', V: 2, COLOR: '#00babd' }
}
constantize(materialTypeEnum)

// 下料方式
const layOffWayTypeEnum = {
  NESTING: { L: '套料', SL: '套料', V: 1, COLOR: '#67C23A' },
  UN_NESTING: { L: '无需套料', SL: '无需套料', V: 2, COLOR: '#F56C6C' }
}
constantize(layOffWayTypeEnum)

export {
  materialTypeEnum,
  layOffWayTypeEnum
}
export default {
  materialTypeEnum,
  layOffWayTypeEnum
}
