
const getPlan = {
  url: RegExp('/api/plan/planDetail/list/' + '.*'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 3,
        'content': [{
          'id': 14,
          'type': 0,
          'startDate': 1635004800000,
          'endDate': 1635350400000,
          'remark': null,
          'areaId': 6,
          'name': '烦烦烦',
          'date': 1635350400000,
          'axis': '热情我认为',
          'areaType': 1,
          'supplier': null
        }, {
          'id': 16,
          'type': 0,
          'startDate': 1635004800000,
          'endDate': 1635264000000,
          'remark': 'ill',
          'areaId': 7,
          'name': '也一样',
          'date': 1635264000000,
          'axis': '也有人要',
          'areaType': 1,
          'supplier': null
        }, {
          'id': 18,
          'type': 0,
          'startDate': null,
          'endDate': null,
          'remark': null,
          'areaId': 8,
          'name': '1',
          'date': 1635264000000,
          'axis': '1',
          'areaType': 0,
          'supplier': null
        }]
      }
    }
  }
}


const editPlan = {
  url: '/api/plan/planDetail/update',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': 10
    }
  }
}


export default [
  getPlan,
  editPlan
]
