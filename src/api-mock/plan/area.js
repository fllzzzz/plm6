
const getArea = {
  url: '/api/plan/area',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 1,
        'content': [{
          'createTime': 1635406956000,
          'id': 5,
          'name': '放松放松',
          'date': 1635350400000,
          'projectId': 42,
          'monomerId': 4,
          'axis': '热望热望',
          'type': 0,
          'supplier': null,
          'productType': 1,
          'sort': 1,
          'remark': '我仍然'
        }]
      }
    }
  }
}

const addArea = {
  url: '/api/plan/area',
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

const editArea = {
  url: '/api/plan/area',
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

const delArea = {
  url: '/api/plan/area',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

export default [
  getArea,
  addArea,
  editArea,
  delArea
]
