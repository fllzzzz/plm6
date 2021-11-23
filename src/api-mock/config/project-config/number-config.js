// 获取所有单位
const getNumber = {
  url: '/api/config/project-config/number',
  timeout: 500,
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'totalElements': 15,
        'content|15': [
          {
            'type|+1': 1,
            'code': /[A-Z0-9]{1,10}/,
            'number|1-10': 1
          }
        ]
      }
    }
  }
}

const saveNumber = {
  url: '/api/config/project-config/number',
  method: 'post',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [
  getNumber,
  saveNumber
]
