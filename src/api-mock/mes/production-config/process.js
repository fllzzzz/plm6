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
        'content': [{
          'createTime': 1635733195000,
          'id': 1,
          'name': '组立1',
          'reportType': 2,
          'inspectType': 2,
          'type': false,
          'sequenceType': 2,
          'productType': 16,
          'userId': 1,
          'sort': 1
        }, {
          'createTime': 1635733204000,
          'id': 2,
          'name': '组立2',
          'reportType': 2,
          'inspectType': 2,
          'type': false,
          'sequenceType': 2,
          'productType': 16,
          'userId': 1,
          'sort': 1
        }, {
          'createTime': 1635733212000,
          'id': 3,
          'name': '组立3',
          'reportType': 2,
          'inspectType': 2,
          'type': false,
          'sequenceType': 2,
          'productType': 16,
          'userId': 1,
          'sort': 1
        }, {
          'createTime': 1635733228000,
          'id': 4,
          'name': '组立4',
          'reportType': 2,
          'inspectType': 2,
          'type': false,
          'sequenceType': 2,
          'productType': 16,
          'userId': 1,
          'sort': 1
        }, {
          'createTime': 1635733237000,
          'id': 5,
          'name': '组立5',
          'reportType': 2,
          'inspectType': 2,
          'type': false,
          'sequenceType': 2,
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
