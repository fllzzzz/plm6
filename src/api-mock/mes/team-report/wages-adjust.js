const getList = {
  url: '/api/mes/building/wages',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 1,
        'content': [{
          'productProcessId': 1,
          'productProcessName': 'L-115',
          'processList': [{
            'processId': 1,
            'processName': '部件1',
            'price': 1.00000000
          }, {
            'processId': 2,
            'processName': '部件2',
            'price': 0
          }, {
            'processId': 3,
            'processName': '部件3',
            'price': 0
          }]
        }]
      }
    }
  }
}

const edit = {
  url: '/api/mes/building/wages',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const checkList = {
  url: '/api/mes/building/wages/audit/list',
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
          'project': {
            'id': 38,
            'name': 'hgg',
            'contractNo': 'hggggggg',
            'shortName': 'hghghgh',
            'type': null
          },
          'monomer': {
            'id': 3,
            'name': '单体#1'
          },
          'productProcessId': 1,
          'productProcessName': 'L-115',
          'processId': 1,
          'processName': '部件1',
          'oldPrice': 1.00,
          'newPrice': 4.00,
          'auditTime': null,
          'userId': 1,
          'userName': '超级管理员',
          'auditUserId': null,
          'auditUserName': null,
          'auditStatus': 1
        }, {
          'id': 2,
          'project': {
            'id': 38,
            'name': 'hgg',
            'contractNo': 'hggggggg',
            'shortName': 'hghghgh',
            'type': null
          },
          'monomer': {
            'id': 3,
            'name': '单体#1'
          },
          'productProcessId': 1,
          'productProcessName': 'L-115',
          'processId': 1,
          'processName': '部件1',
          'oldPrice': 1.00,
          'newPrice': 5.00,
          'auditTime': null,
          'userId': 1,
          'userName': '超级管理员',
          'auditUserId': null,
          'auditUserName': null,
          'auditStatus': 1
        }]
      }
    }
  }
}

const check = {
  url: '/api/mes/building/wages/status',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const checkNumber = {
  url: '/api/mes/building/wages/audit/number',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data|1-10': 1
    }
  }
}

export default [
  getList,
  edit,
  checkList,
  check,
  checkNumber
]
