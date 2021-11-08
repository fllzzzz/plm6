// 获取科目规格
const getSpecification = {
  url: RegExp('/api/config/classification/specification/' + '.*'),
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'content|0-3': [{
          'id|+1': 1,
          'name|+1': ['直径', '长度', '材质'],
          'boolWeightMean|1-2': false,
          'boolCustomizeable|1-2': false,
          'list|11-14': [{
            'id': 1,
            'code': /^([0-9]{2})$/,
            'value': /^([A-Z0-9]{3})$/,
            'boolUsed|1-2': false,
            'sort|1-999': 1
          }
          ]
        }]
      }
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
