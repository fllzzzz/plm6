const productDashboard = {
  url: '/api/mes/building/kanban/product',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 2,
        'content|1-100': [{
          'completeQuantity|1-100': 1,
          'id': 1,
          'name': '@cword(2,6)',
          'serialNumber': '@word(2,6)',
          'taskQuantity|1-100': 1
        }]
      }
    }
  }
}

export default [
  productDashboard
]
