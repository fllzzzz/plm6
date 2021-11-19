const getDictAll = {
  url: '/api/dictDetail/all',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '操作成功',
      data: {
        totalElements: 3,
        'content': [
          {
            'createTime': 1596170867108,
            'id': 49,
            'label': '有限责任公司',
            'name': 'enterprise_type',
            'remark': '企业类型',
            'sort': 1,
            'updateTime': 1596170867108,
            'value': '1'
          },
          {
            'createTime': 1596170884162,
            'id': 50,
            'label': '有限责任公司(自然人投资或控股)',
            'name': 'enterprise_type',
            'remark': '企业类型',
            'sort': 2,
            'updateTime': 1596170884162,
            'value': '2'
          }
        ]
      }
    }
  }
}

export default [
  getDictAll
]
