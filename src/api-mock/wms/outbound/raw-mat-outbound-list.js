
// 原材料出库清单列表
const get = {
  url: '/api/wms/outbound/application/review/raw-materials',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'content|20': [
          {
            'id|+1': 1,
            'basicClass|1-31': 1,
            'recipientName': '@cname',
            'userUpdateTime': '@datetime(T)',
            'createTime': '@datetime(T)'
          }
        ],
        totalElements: 20
      }
    }
  }
}

// 获取当前用户的出库单的详情数量
const getDetailNumberByCurrentUser = {
  url: '/api/wms/outbound/application/review/raw-materials/current-user/detail-number',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: 2
    }
  }
}

export default [get, getDetailNumberByCurrentUser]

