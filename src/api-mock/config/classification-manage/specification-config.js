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
        'list|11-14': [{
          'id': 1,
          'code': /^([0-9]{2})$/,
          'value': /^([A-Z0-9]{3})$/,
          'sort|1-999': 1
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

// 添加科目新规格
const addSpecification = {
  url: '/api/config/classification/specification',
  method: 'post',
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 更新科目新规格
const editSpecification = {
  url: '/api/config/classification/specification',
  method: 'put',
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 删除科目新规格
const delSpecification = {
  url: '/api/config/classification/specification',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [
  getSpecification,
  addSpecification,
  editSpecification,
  delSpecification
]
