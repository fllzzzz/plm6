const taskList = {
  url: '/api/mes/building/abnormal/assemble/task',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'content|1-10': [{
          'id|+1': 1,
          serialNumber: '@word(2,10)',
          lineName: '@cword(2,5)',
          'taskQuantity|1-50': 1,
          'producedQuantity|1-50': 1
        }]
      }
    }
  }
}

const assembleChange = {
  url: '/api/mes/building/abnormal/assemble/task_change',
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
  taskList,
  assembleChange
]
