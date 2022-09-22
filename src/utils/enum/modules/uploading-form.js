import { constantize } from '../base'

// 班组类型
const materialTypeEnum = {
  MANMADE_BLANKING: { L: '人工下料', SL: '人工下料', V: 1, COLOR: '#40ed8d' },
  NC_BLANKING: { L: '数控下料', SL: '数控下料', V: 2, COLOR: '#00babd' }
}
constantize(materialTypeEnum)

export {
  materialTypeEnum
}
export default {
  materialTypeEnum
}
