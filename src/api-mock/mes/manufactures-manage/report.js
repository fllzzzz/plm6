const getList = {
  url: '/api/mes/building/warehouse/report',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 1,
        'content|1-100': [{
          'id': 1,
          'name': '@cword',
          'shortName': '@cword(2,3)',
          'serialNumber': '@word',
          'productType|1': [2, 4],
          'unit|1': ['kg', 'm'],
          'beginMete|1-1000.1-8': 0,
          'inboundMete|1-1000.1-8': 300.00000000,
          'outboundMete|1-1000.1-8': 1,
          'endMete|1-1000.1-8': 300.00000000
        }]
      }
    }
  }
}

const getSummary = {
  url: '/api/mes/building/warehouse/report/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'beginMete|1-1000.1-8': 0,
        'inboundMete|1-1000.1-8': 300.00000000,
        'outboundMete|1-1000.1-8': 1,
        'endMete|1-1000.1-8': 300.00000000
      }
    }
  }
}

export default [
  getList,
  getSummary
]
