const getProductionLine = {
  url: '/api/mes/building/wageQuota/machine_part/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 2,
        'content': [{
          'id': 6,
          'name': '零件1',
          'wageQuotaType': 1,
          'weightPrice': 2000.00000000,
          'lengthPrice': 0E-8,
          'areaPice': 0E-8
        }, {
          'id': 7,
          'name': '零件2',
          'wageQuotaType': 1,
          'weightPrice': 633.00000000,
          'lengthPrice': 0E-8,
          'areaPice': 0E-8
        }]
      }
    }
  }
}

export default [
  getProductionLine
]
