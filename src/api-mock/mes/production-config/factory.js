const getFactory = {
  url: '/api/mes/building/factory/page',
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
            'id|+1': 1,
            'name': '@cword(2,5)',
            'remark': '@cword(4,60)',
            'shortName': '@cword(2)',
            'sort|+1': 1,
            'tagColor': '@color',
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

const addFactory = {
  url: '/api/mes/building/factory',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editFactory = {
  url: '/api/mes/building/factory',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const delFactory = {
  url: '/api/mes/building/factory',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const editFactoryStatus = {
  url: '/api/mes/building/factory/changeState',
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
  getFactory,
  addFactory,
  editFactory,
  delFactory,
  editFactoryStatus
]
