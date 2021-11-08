const getProductProcess = {
  url: '/api/mes/building/productProcess/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return { 'code': 20000, 'message': '成功', 'data': { 'hasPreviousPage': false, 'hasNextPage': false, 'totalElements': 1, 'content': [{ 'createTime': 1635733418000, 'id': 1, 'name': '1-1', 'alias': null, 'processType': false, 'sequenceType': 2, 'productType': 16, 'boolEnabledEnum': true, 'sort': null, 'userId': 1, 'medBuildingProductProcessLinkList': [{ 'createTime': 1635734121000, 'id': 11, 'productProcessId': 1, 'processId': 1, 'sequence': 0, 'processName': '组立1', 'wageQuota': { 'createTime': null, 'id': null, 'productProcessId': null, 'processId': null, 'wageQuotaType': null, 'weightPrice': null, 'lengthPrice': null, 'areaPice': null, 'userId': null }}, { 'createTime': 1635734121000, 'id': 12, 'productProcessId': 1, 'processId': 2, 'sequence': 1, 'processName': '组立2', 'wageQuota': { 'createTime': null, 'id': null, 'productProcessId': null, 'processId': null, 'wageQuotaType': null, 'weightPrice': null, 'lengthPrice': null, 'areaPice': null, 'userId': null }}, { 'createTime': 1635734121000, 'id': 13, 'productProcessId': 1, 'processId': 3, 'sequence': 2, 'processName': '组立3', 'wageQuota': { 'createTime': null, 'id': null, 'productProcessId': null, 'processId': null, 'wageQuotaType': null, 'weightPrice': null, 'lengthPrice': null, 'areaPice': null, 'userId': null }}, { 'createTime': 1635734121000, 'id': 14, 'productProcessId': 1, 'processId': 4, 'sequence': 3, 'processName': '组立4', 'wageQuota': { 'createTime': null, 'id': null, 'productProcessId': null, 'processId': null, 'wageQuotaType': null, 'weightPrice': null, 'lengthPrice': null, 'areaPice': null, 'userId': null }}, { 'createTime': 1635734121000, 'id': 15, 'productProcessId': 1, 'processId': 5, 'sequence': 4, 'processName': '组立5', 'wageQuota': { 'createTime': null, 'id': null, 'productProcessId': null, 'processId': null, 'wageQuotaType': null, 'weightPrice': null, 'lengthPrice': null, 'areaPice': null, 'userId': null }}] }] }}
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
