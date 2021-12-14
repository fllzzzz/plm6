const getOther = {
  url: '/api/plan/otherData',
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
        'content': [{
            'id': 7,
            'name': '单体%231_20211104144959.zip',
            'type': 1,
            'remark': null,
            'createTime': '2021-11-08T03:41:09.000+00:00',
            'createUserName': '超级管理员'
          },
          {
            'id': 8,
            'name': '单体%231_20211104144959.zip',
            'type': 1,
            'remark': null,
            'createTime': '2021-11-08T03:41:19.000+00:00',
            'createUserName': '超级管理员'
          }
        ]
      }
    }
  }
}

const otherUpload = {
  url: '/api/plan/otherData/upload',
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
  getOther,
  otherUpload
]