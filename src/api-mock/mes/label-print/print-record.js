const getForTask = {
  url: '/api/mes/building/print/task/record',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content|1-50': [{
          'endTime': '@datetime',
          'operator': '@cname',
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
  url: '/api/mes/building/print/task/record',
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
  url: '/api/mes/building/print/package/record',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content|1-50': [{
          'endTime': '@datetime',
          'operator': '@cname',
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
  url: '/api/mes/building/print/package/record',
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
