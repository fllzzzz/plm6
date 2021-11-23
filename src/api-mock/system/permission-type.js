
const getPermissionType = {
  url: '/api/permission/type',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'totalElements': 4,
        'content': [{
          'createTime': 1631956539811,
          'id': 4,
          'icon': 'complex',
          'name': '复合权',
          'sort': 4
        }, {
          'createTime': 1631956539804,
          'id': 3,
          'icon': 'download',
          'name': '下载权',
          'sort': 3
        }, {
          'createTime': 1631956539795,
          'id': 2,
          'icon': 'write',
          'name': '编辑权',
          'sort': 2
        }, {
          'createTime': 1631956539787,
          'id': 1,
          'icon': 'eye-open',
          'name': '查看权',
          'sort': 1
        }]
      }
    }
  }
}

const addPermissionType = {
  url: '/api/permission/type',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': 10
    }
  }
}

const editPermissionType = {
  url: '/api/permission/type',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': 10
    }
  }
}

const delPermissionType = {
  url: '/api/permission/type',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const getPermissionTypeAll = {
  url: '/api/permission/type',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'totalElements': 4,
        'content': [{
          'createTime': 1631956539811,
          'id': 4,
          'icon': 'complex',
          'name': '复合权',
          'sort': 4
        }, {
          'createTime': 1631956539804,
          'id': 3,
          'icon': 'download',
          'name': '下载权',
          'sort': 3
        }, {
          'createTime': 1631956539795,
          'id': 2,
          'icon': 'write',
          'name': '编辑权',
          'sort': 2
        }, {
          'createTime': 1631956539787,
          'id': 1,
          'icon': 'eye-open',
          'name': '查看权',
          'sort': 1
        }]
      }
    }
  }
}

export default [
  getPermissionType,
  addPermissionType,
  editPermissionType,
  delPermissionType,
  getPermissionTypeAll
]
