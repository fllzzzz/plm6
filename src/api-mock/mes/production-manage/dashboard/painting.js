const get = {
  url: '/api/mes/building/kanban/painting/list',
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
          'changeArea|1-1000.1-8': 1,
          'loss|1-99.1-2': 1,
          'material': '@word',
          'measure|1-1000.1-8': 1,
          'monomerId|1-100': 1,
          'name': '@cword(2,5)',
          'paintCategory': '@cword(2,5)',
          'paintingType|1': [1, 2, 4],
          'projectId|1-100': 1,
          'surfaceArea|1-1000.1-8': 1,
          'thickness|1-1000.1-8': 1,
          'volumeSolids|1-99.1-2': 1
        }]
      }
    }
  }
}

const change = {
  url: '/api/mes/building/kanban/painting/change',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

export default [
  get,
  change
]
