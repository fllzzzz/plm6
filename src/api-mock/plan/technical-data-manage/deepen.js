const getDeepen = {
  url: '/api/plan/drawing/struct',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 1,
        'content': [{
          'id': 204,
          'serialNumber': 'GZ1-4',
          'fileName': 'GZ1-4.pdf',
          'type': 0,
          'enclosureCategory': null,
          'createTime': '2021-11-08T07:55:08.000+00:00',
          'createUserName': '超级管理员'
        }]
      }
    }
  }
}

const deepenUpload = {
  url: '/api/plan/drawing/struct/upload',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

export default [
  getDeepen,
  deepenUpload
]