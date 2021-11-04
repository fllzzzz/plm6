const getFactory = {
  url: '/api/mes/building/factory/page',
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
            'createTime': '2021-10-08T10:07:22',
            'id': 3,
            'name': '一号工厂',
            'remark': 'remark',
            'shortName': '一工',
            'sort': 1,
            'tagColor': 'rgba(250, 212, 0, 1)',
            'updateTime': '2021-10-08T10:10:36',
            'userId': 1,
            'version': 2
          },
          {
            'boolDeleteEnum': false,
            'boolEnabledEnum': true,
            'createTime': '2021-10-08T10:07:22',
            'id': 4,
            'name': '二号工厂',
            'remark': 'remark',
            'shortName': '二工',
            'sort': 1,
            'tagColor': 'rgba(250, 150, 0, 1)',
            'updateTime': '2021-10-08T10:10:36',
            'userId': 1,
            'version': 2
          },
          {
            'boolDeleteEnum': false,
            'boolEnabledEnum': true,
            'createTime': '2021-10-08T10:07:22',
            'id': 5,
            'name': '三号工厂',
            'remark': 'remark',
            'shortName': '三工',
            'sort': 1,
            'tagColor': 'rgba(150, 212, 0, 1)',
            'updateTime': '2021-10-08T10:10:36',
            'userId': 1,
            'version': 2
          },
          {
            'boolDeleteEnum': false,
            'boolEnabledEnum': true,
            'createTime': '2021-10-08T10:07:22',
            'id': 5,
            'name': '四号工厂',
            'remark': 'remark',
            'shortName': '四工',
            'sort': 1,
            'tagColor': 'rgba(250, 212, 150, 1)',
            'updateTime': '2021-10-08T10:10:36',
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
