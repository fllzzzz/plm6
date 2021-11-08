const getProductionLine = {
  url: '/api/mes/building/productionLine/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content': [
          {
            'boolDeleteEnum': false,
            'boolEnabledEnum': true,
            'createTime': '2021-10-08T10:16:20',
            'factoryId': 3,
            'id': 4,
            'name': '一号生产线',
            'remark': '11',
            'shortName': '一线',
            'sort': 1,
            'updateTime': '2021-10-08T10:48:24',
            'userId': 1,
            'version': 1,
            'workshopId': 4
          },
          {
            'boolDeleteEnum': false,
            'boolEnabledEnum': true,
            'createTime': '2021-10-08T10:16:20',
            'factoryId': 3,
            'id': 5,
            'name': '二号生产线',
            'remark': '22',
            'shortName': '二线',
            'sort': 1,
            'updateTime': '2021-10-08T10:48:24',
            'userId': 1,
            'version': 1,
            'workshopId': 4
          },
          {
            'boolDeleteEnum': false,
            'boolEnabledEnum': true,
            'createTime': '2021-10-08T10:16:20',
            'factoryId': 3,
            'id': 6,
            'name': '三号生产线',
            'remark': '33',
            'shortName': '三线',
            'sort': 1,
            'updateTime': '2021-10-08T10:48:24',
            'userId': 1,
            'version': 1,
            'workshopId': 4
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
