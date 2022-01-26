// 表单类型
const formTypeEnum = {
  BEGIN_PERIOD: { L: '期初数据', K: 'BEGIN_PERIOD', V: 1 },
  END_PERIOD: { L: '入库数据', K: 'END_PERIOD', V: 2 },
  INBOUND: { L: '出库数据', K: 'INBOUND', V: 3 },
  OUTBOUND: { L: '期末数据', K: 'OUTBOUND', V: 4 }
}

export {
  formTypeEnum
}
