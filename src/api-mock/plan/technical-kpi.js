const getUserYearMete = {
  url: '/api/plan/deepenRecord/listUserMete',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 1,
        'content|1-30': [{
          'userId|+1': 1,
          'userName': '@cname(2,5)',
          'mete|1-10000.1-5': 12143.83369000
        }]
      }
    }
  }
}

const getUserMonthMete = {
  url: '/api/plan/deepenRecord/listUserMonthMete',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 1,
        'content|1-30': [{
          'userId|+1': 1,
          'userName': '@cname(2,5)',
          'mete|1-10000.1-5': 12143.83369000,
          'lastMonthMete|1-10000.1-5': 10641.10613000
        }]
      }
    }
  }
}

export default [
  getUserYearMete,
  getUserMonthMete
]
