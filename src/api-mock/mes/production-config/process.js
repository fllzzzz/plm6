const getProcess = {
  url: '/api/mes/building/process/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 5,
        'content|1-100': [{
          'createTime': '@datetime',
          'id|+1': 1,
          'name': '@cword(2,5)',
          'reportType|0-3': 2,
          'inspectType|0-3': 2,
          'type|1-2': false,
          'productType': 16,
          'userId': 1,
          'sort': 1
        }]
      }
    }
  }
}

const addProcess = {
  url: '/api/mes/building/process',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editProcess = {
  url: '/api/mes/building/process',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const delProcess = {
  url: '/api/mes/building/process',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

export default [
  getProcess,
  addProcess,
  editProcess,
  delProcess
]
