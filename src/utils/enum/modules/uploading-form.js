import { constantize } from '../base'

// 班组类型
const materialTypeEnum = {
  MANMADE_BLANKING: { L: '人工下料', V: 1 },
  NC_BLANKING: { L: '数控下料', V: 2 }
}
constantize(materialTypeEnum)

export {
  materialTypeEnum
}
export default {
  materialTypeEnum
}
