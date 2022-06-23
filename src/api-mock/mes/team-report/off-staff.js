const getList = {
  url: '/api/mes/building/wages/out_staff',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'artifact': null,
        'enclosure': null,
        'assemble': [{
          'monomer': {
            'id': 1,
            'name': '测试单体1'
          },
          'taskQuantity': 4,
          'areaDetail': {
            'id': 1,
            'name': '测试区域1'
          },
          'leaderName': '超级管理员',
          'name': null,
          'serialNumber': 'H1',
          'specification': null,
          'material': null,
          'completeQuantity': 2,
          'completeNetWeight': null,
          'mate': 3500.00,
          'wage': 0,
          'wageQuotaType': 2,
          'productType': 16,
          'productId': 14,
          'teamId': 16,
          'userId': null,
          'auditUserId': null,
          'auditTime': null,
          'createTime': null,
          'auditStatus': null,
          'id': null,
          'taskNetWeight': null,
          'taskLength': 7000.00
        }],
        'machinePart': null
      }
    }
  }
}

const edit = {
  url: '/api/mes/building/wages/out_staff',
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
  url: '/api/mes/building/wages/out_staff/auditList',
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
  url: '/api/mes/building/wages/out_staff/status',
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
  url: '/api/mes/building/wages/out_staff/auditList/count',
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
