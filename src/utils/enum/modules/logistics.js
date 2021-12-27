import { constantize } from '../base'

// 物流运输方式
const logisticsTransportTypeEnum = {
  FREIGHT: { L: '货运', K: 'FREIGHT', V: 1 << 0 },
  POST: { L: '邮递', K: 'POST', V: 1 << 1 }
}
constantize(logisticsTransportTypeEnum)

// 付款方
const logisticsPayerEnum = {
  DEMAND: { L: '需方承担', K: 'DEMAND', V: 1 << 0 },
  SUPPLIER: { L: '供方承担', K: 'FREIGHT', V: 1 << 1 }
}
constantize(logisticsPayerEnum)

export {
  logisticsTransportTypeEnum,
  logisticsPayerEnum
}

export default {
  logisticsTransportTypeEnum,
  logisticsPayerEnum
}
