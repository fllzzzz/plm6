const changeStatus = {
  url: RegExp('/api/mes/building/abnormal/' + '[1-9][0-9]*'),
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

const exceptionList = {
  url: '/api/mes/building/abnormal/report',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'reportList': [{
          'id': 1,
          'quantity': 100,
          'reportType|0-1': 0,
          'serialNumber': 'GZ1-3',
          'factoryName': 'dzx一号工厂',
          'workshopName': 'dzx一号车间',
          'productionLineName': 'dzx一号生产线',
          'userName': '超级管理员',
          'processName': '构件1',
          'processId': '4'
        }],
        'limitList': [{
          'quantity': 50,
          'processId': 4
        }]
      }
    }
  }
}

const extraTaskList = {
  url: '/api/mes/building/changed_list/extra_task',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'content': [{
          serialNumber: '@word(2,10)',
          teamName: '@cword(2,5)',
          lineName: '@cword(2,5)',
          type: '多余任务',
          extraQuantity: 40
        },
        {
          serialNumber: '@word(2,10)',
          teamName: '@cword(2,5)',
          lineName: '@cword(2,5)',
          type: '多余任务',
          extraQuantity: 40
        },
        {
          serialNumber: '@word(2,10)',
          teamName: '@cword(2,5)',
          lineName: '@cword(2,5)',
          type: '多余任务',
          extraQuantity: 40
        }
        ]
      }
    }
  }
}

const artifactChange = {
  url: '/api/mes/building/abnormal/artifact',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [
  changeStatus,
  exceptionList,
  extraTaskList,
  artifactChange
]
