const getWorkshop = {
  url: '/api/mes/building/workshop/page',
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
            'id+1': 1,
            'name': '@cword(2,5)',
            'remark': '@cword(4,60)',
            'shortName': '@cword(2)',
            'sort|+1': 1,
            'updateTime': '@datetime',
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
