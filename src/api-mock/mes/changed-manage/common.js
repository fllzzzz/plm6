const getArtifactChangeList = {
  url: '/api/mes/building/abnormal',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': [{
        'id|+1': 1,
        'project': {
          'id': 1,
          'name': '@cword(2,15)',
          'shortName': '@cword(2,9)',
          'contractNo': '@guid'
        },
        changeTime: '@datetime',
        changePersonName: '@cname',
        serialNumber: '@word(2,10)',
        'productId|1-100': 1,
        'productType|1': [1, 2, 16],
        // 'oldQuantity|1-1000': 1,
        // 'totalInProductionQuantity|1-20': 1,
        // 'changeType|0-5': 0,
        // 'newQuantity|1-20': 1
        // 'taskQuantity|1-1000': 60,
        'status': 0, // 0 待处理 ；1 处理中 ；2 处理完成
        'oldQuantity': 100,
        'totalInProductionQuantity': 20,
        'changeType|0-5': 0,
        'newQuantity': 30,
        'taskQuantity': 60
      },
      {
        'id|+1': 1,
        'project': {
          'id': 1,
          'name': '@cword(2,15)',
          'shortName': '@cword(2,9)',
          'contractNo': '@guid'
        },
        changeTime: '@datetime',
        changePersonName: '@cname',
        serialNumber: '@word(2,10)',
        'productId|1-100': 1,
        'productType|1': [1, 2, 16],
        'oldQuantity': 100,
        'totalInProductionQuantity': 20,
        'status': 0, // 0 待处理 ；1 处理中 ；2 处理完成
        'changeType|0-5': 0,
        'newQuantity': 10,
        'taskQuantity': 60
      },
      {
        'id|+1': 1,
        'project': {
          'id': 1,
          'name': '@cword(2,15)',
          'shortName': '@cword(2,9)',
          'contractNo': '@guid'
        },
        changeTime: '@datetime',
        changePersonName: '@cname',
        serialNumber: '@word(2,10)',
        'productId|1-100': 1,
        'productType|1': [1, 2, 16],
        'oldQuantity': 100,
        'totalInProductionQuantity': 100,
        'status': 0, // 0 待处理 ；1 处理中 ；2 处理完成
        'changeType|0-5': 0,
        'newQuantity': 50,
        'taskQuantity': 100
      }],
      'message': '成功'
    }
  }
}

export default [
  getArtifactChangeList
]
