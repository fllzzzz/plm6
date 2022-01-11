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
        createTime: '@datetime',
        userName: '@cname',
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
        createTime: '@datetime',
        userName: '@cname',
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
        createTime: '@datetime',
        userName: '@cname',
        serialNumber: '@word(2,10)',
        'productId|1-100': 1,
        'productType|1': [1, 2, 16],
        'oldQuantity': 100,
        'totalInProductionQuantity': 100,
        'status': 0, // 0 待处理 ；1 处理中 ；2 处理完成
        'changeType|0-5': 0,
        'newQuantity': 50,
        'taskQuantity': 100
      },
      {
        'id': 1,
        'project': {
          'id': 29,
          'name': '零件数',
          'contractNo': '零件数',
          'shortName': '零件数',
          'type': null
        },
        'monomer': {
          'id': 25,
          'name': '零件数'
        },
        'areaDetail': {
          'id': 33,
          'name': '零件数'
        },
        'productId': 895,
        'productType': 2,
        'oldQuantity': 100,
        'taskQuantity': 10,
        'newQuantity': 5,
        'changeType': 1,
        'userId': 1,
        'userName': '超级管理员',
        'createTime': 1641807655000,
        'status': 0,
        'serialNumber': 'GL-103',
        'totalTaskQuantity': 10,
        'totalInProductionQuantity': 0,
        'usedQuantity': null,
        'producedQuantity': null
      }
      ],
      'message': '成功'
    }
  }
}

export default [
  getArtifactChangeList
]
