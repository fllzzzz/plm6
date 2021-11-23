const getDictDetail = {
  url: '/api/dictDetail',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 3,
        'content': [{
          'createTime': 1632476140017,
          'id': 3,
          'label': '保函',
          'value': '1',
          'name': 'margin_type',
          'remark': '合同-保证金类型',
          'sort': 1
        }, {
          'createTime': 1636963732274,
          'id': 9,
          'label': '哈哈',
          'value': '5',
          'name': 'margin_type',
          'remark': '合同-保证金类型',
          'sort': 1
        }, {
          'createTime': 1632478158633,
          'id': 4,
          'label': '现金',
          'value': '2',
          'name': 'margin_type',
          'remark': '合同-保证金类型',
          'sort': 2
        }]
      }
    }
  }
}

const addDictDetail = {
  url: '/api/dictDetail',
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

const delDictDetail = {
  url: RegExp('/api/dictDetail/' + '.*'),
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const ediDictDetail = {
  url: '/api/dictDetail',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': true
    }
  }
}

export default [
  getDictDetail,
  addDictDetail,
  delDictDetail,
  ediDictDetail
]
