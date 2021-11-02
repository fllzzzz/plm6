// 获取施工日志列表
const getConstructionLog = {
  url: '/api/project/data/construction/log',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'totalElements': 1,
        'content': [
          {
            'createTime': 1635149130000,
            'id': 7,
            'projectId': 1,
            'createUserId': 1,
            'subTime': 1633622400000,
            'morningWeather': '晴天',
            'afternoonWeather': '阴天',
            'maxTemperature': 20,
            'minTemperature': 10,
            'productionRecord': '正常',
            'safetyRecord': ''
          }
        ]
      }
    }
  }
}

// 保存施工日志列表
const setConstructionLog = {
  url: '/api/project/data/construction/log',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      data: true,
      message: '成功'
    }
  }
}

export default [
  getConstructionLog,
  setConstructionLog
]
