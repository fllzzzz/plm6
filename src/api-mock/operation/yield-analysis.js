const yieldList = {
  url: '/api/operational/analysis/production',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'data': {
        'content': [{
          'productionDetailsList': [],
          'year': '2020'
        }, {
          'productionDetailsList': [],
          'year': '2021'
        }, {
          'productionDetailsList': [{
            'month': '2022-03',
            'production': 568964.0600
          }, {
            'month': '2022-04',
            'production': 1204844.8100
          }],
          'year': '2022'
        }],
        'totalElements': 3
      },
      'message': '成功'
    }
  }
}

export default [
  yieldList
]
