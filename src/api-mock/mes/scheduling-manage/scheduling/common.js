const saveScheduling = {
  url: '/api/mes/building/scheduling',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const clearScheduling = {
  url: '/api/mes/building/scheduling/clear',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

export default [
  saveScheduling,
  clearScheduling
]
