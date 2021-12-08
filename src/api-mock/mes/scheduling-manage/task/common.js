const getScheduling = {
  url: '/api/mes/building/scheduling',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 2,
        'content': [{
          'createTime': null,
          'id': 1,
          'factoryId': 1,
          'factoryName': 'dzx一号工厂',
          'workshopId': 1,
          'workshopName': 'dzx一号车间',
          'productionLineId': 3,
          'productionLineName': 'test 66',
          'projectId': 38,
          'projectName': 'hgg',
          'monomerId': 3,
          'monomerName': '单体#1',
          'areaId': 4,
          'districtName': '结构区域1',
          'productId': 1,
          'productName': null,
          'serialNumber': 'L-115',
          'specification': null,
          'productType': null,
          'processType': false,
          'schedulingQuantity': 10,
          'taskQuantity': 10,
          'completeQuantity': 10,
          'askCompleteTime': null,
          'completeTime': 1636613990000,
          'status': true,
          'issueStatus': true,
          'userId': 1,
          'productProcessId': 1
        }, {
          'createTime': null,
          'id': 7,
          'factoryId': 1,
          'factoryName': 'dzx一号工厂',
          'workshopId': 1,
          'workshopName': 'dzx一号车间',
          'productionLineId': 4,
          'productionLineName': 't111',
          'projectId': 39,
          'projectName': 'content测试',
          'monomerId': 5,
          'monomerName': '单体1',
          'areaId': 8,
          'districtName': '区域1',
          'productId': 6,
          'productName': null,
          'serialNumber': 'L-115',
          'specification': null,
          'productType': null,
          'processType': false,
          'schedulingQuantity': 5,
          'taskQuantity': 0,
          'completeQuantity': 0,
          'askCompleteTime': null,
          'completeTime': null,
          'status': true,
          'issueStatus': false,
          'userId': 1,
          'productProcessId': 1
        }]
      }
    }
  }
}

const modifySchedulingQuantity = {
  url: '/api/mes/building/scheduling/schedulingQuantity',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const taskIssue = {
  url: '/api/mes/building/task',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

export default [
  getScheduling,
  modifySchedulingQuantity,
  taskIssue
]
