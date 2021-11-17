const getProductProcess = {
  url: '/api/mes/building/productProcess/page',
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
        'content|1-100': [{
          'createTime': '@datetime',
          'id|+1': 1,
          'name': '@cword(2,5)',
          'alias': '@cword(2,5)',
          'processType|1-2': false,
          'sequenceType': 2,
          'productType': 16,
          'boolEnabledEnum|1-2': true,
          'userId': 1,
          'medBuildingProductProcessLinkList|1-10': [{
            'createTime': '@datetime',
            'id|+1': 1,
            'productProcessId': 1,
            'processId|+1': 1,
            'sequence': 0,
            'processName': '@cword(2,5)',
            'wageQuota': {
              'createTime': '@datetime',
              'id|+1': 1,
              'productProcessId': null,
              'processId': null,
              'wageQuotaType': 2,
              'weightPrice|1-100': 1,
              'lengthPrice|1-100': 1,
              'areaPice|1-100': 1,
              'userId': null
            }
          }]
        }]
      }
    }
  }
}

const addProductProcess = {
  url: '/api/mes/building/productProcess',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editProductProcess = {
  url: '/api/mes/building/productProcess',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editProductProcessStatus = {
  url: '/api/mes/building/productProcess/changeState',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const delProductProcess = {
  url: '/api/mes/building/productProcess',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editWageQuota = {
  url: '/api/mes/building/wageQuota',
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
  getProductProcess,
  addProductProcess,
  editProductProcess,
  delProductProcess,
  editProductProcessStatus,
  editWageQuota
]
