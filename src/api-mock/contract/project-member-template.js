const getMemberTemplate ={
  url: '/api/deploy/project/templates',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 1,
        'content': [{
          'createTime': 1632465782089,
          'id': 3,
          'isDefault': true,
          'remark': '测试模板1',
          'status': 1,
          'templateName': '测试模板1'
        }]
      }
    }
  }
}

const getUserByTemplate ={
  url: '/api/deploy/project/template/details',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 2,
        'content': [{
          'id': 1,
          'name': '超级管理员',
          'deptId': 1,
          'deptName': '公司'
        }, {
          'id': 2,
          'name': '张磊',
          'deptId': 1,
          'deptName': '公司'
        }]
      }
    }
  }
}
export default [
  getMemberTemplate,
  getUserByTemplate
]