const getForTask = {
  url: RegExp('/api/mes/building/print/record/task' + '[1][0][0-9]'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content|1-50': [{
          'endTime': '@datetime',
          'userName': '@cname',
          'quantity|1-100': 1,
          'startTime': '@datetime'
        }],
        'totalElements': 6
      },
      'message': '操作成功'
    }
  }
}

const taskAdd = {
  url: '/api/mes/building/print/record/task',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功'
    }
  }
}

const getForPackage = {
  url: '/api/mes/building/print/record/package',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content|1-50': [{
          'endTime': '@datetime',
          'userName': '@cname',
          'quantity|1-100': 1,
          'startTime': '@datetime'
        }],
        'totalElements': 6
      },
      'message': '操作成功'
    }
  }
}

const packageRecordAdd = {
  url: '/api/mes/building/print/record/package',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功'
    }
  }
}

export default [
  getForTask,
  taskAdd,
  getForPackage,
  packageRecordAdd
]
