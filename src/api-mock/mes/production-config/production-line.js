const getProductionLine = {
  url: '/api/mes/building/productionLine/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content|1-100': [
          {
            'boolDeleteEnum|1-2': false,
            'boolEnabledEnum|1-2': true,
            'createTime': '@datetime',
            'factoryId|1-10': 1,
            'id|+1': 1,
            'name': '@cword(2,5)',
            'remark': '@cword(4,60)',
            'shortName': '@cword(2,5)',
            'sort|+1': 1,
            'updateTime': '@datetime',
            'userId': 1,
            'version': 1,
            'workshopId|1-10': 1
          }
        ],
        'hasNextPage': false,
        'hasPreviousPage': false,
        'totalElements': 1
      },
      'message': '成功'
    }
  }
}

const addProductionLine = {
  url: '/api/mes/building/productionLine',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editProductionLine = {
  url: '/api/mes/building/productionLine',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const delProductionLine = {
  url: '/api/mes/building/productionLine',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editProductionLineStatus = {
  url: '/api/mes/building/productionLine/changeState',
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
  getProductionLine,
  addProductionLine,
  editProductionLine,
  delProductionLine,
  editProductionLineStatus
]
