const getWorkshop = {
  url: '/api/mes/building/workshop/page',
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
            'createTime': '2021-10-08T10:13:55',
            'factoryId': 3,
            'id': 4,
            'name': '一号车间',
            'remark': '',
            'shortName': '一车',
            'sort': 1,
            'updateTime': '2021-10-08T10:13:55',
            'userId': 1,
            'version': 2
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

const addWorkshop = {
  url: '/api/mes/building/workshop',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editWorkshop = {
  url: '/api/mes/building/workshop',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const delWorkshop = {
  url: '/api/mes/building/workshop',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editWorkshopStatus = {
  url: '/api/mes/building/workshop/changeState',
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
  getWorkshop,
  addWorkshop,
  editWorkshop,
  delWorkshop,
  editWorkshopStatus
]
