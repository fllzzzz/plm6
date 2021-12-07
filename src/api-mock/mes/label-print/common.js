const getPrintConfig = {
  url: '/api/mes/building/print/config',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'copiesQuantity': 1,
        'createTime': '@datetime',
        'id': 3,
        'manufacturerName': '@cword(2,10)',
        'projectId': 12,
        'showArea|1-2': true,
        'showMonomer|1-2': true,
        'showProductionLine|1-2': true,
        'userId': null,
        'weight|1': [0, 1]
      },
      'message': '操作成功'
    }
  }
}

const setPrintConfig = {
  url: '/api/mes/building/print/config',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功'
    }
  }
}

export default [
  getPrintConfig,
  setPrintConfig
]
