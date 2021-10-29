// 获取科目规格
const getSpecification = {
  url: RegExp('/api/config/classification/specification/' + '.*'),
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      'data|0-3': [{
        'id|+1': 1,
        'name|+1': ['直径', '长度', '材质'],
        'isWeightMean|0-1': 0,
        'isCustomizeable|0-1': 0,
        'details|11-14': [{
          'id': 1,
          'code': /^([0-9]{2})$/,
          'value': /^([A-Z0-9]{3})$/
        }
        //  {
        //   'id': 2,
        //   'code': '002',
        //   'value': 'Q255B'
        // }
        ]
      }]
    }
  }
}

export default [
  getSpecification
]
