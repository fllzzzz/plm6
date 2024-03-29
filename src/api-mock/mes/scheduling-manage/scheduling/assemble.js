const getAssemble = {
  url: '/api/mes/building/scheduling/assemble/page',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 4,
        'content': [{
          'createTime': null,
          'id': 3,
          'projectId': 1,
          'monomerId': 1,
          'areaId': 1,
          'serialNumber': 'L-115',
          'quantity': 3,
          'producedQuantity': 0,
          'usedQuantity': 0,
          'remark': null,
          'totalSchedulingQuantity': 2,
          'totalTaskQuantity': 0,
          'schedulingProductionLineDTOS': [
            {
              'producedQuantity': 0,
              'productionLineId': 1,
              'totalSchedulingQuantity': 2,
              'totalTaskQuantity': 0,
              'usedQuantity': null
            }
          ],
          'projectName': 'fsfsfsfs',
          'monomerName': '单体一',
          'areaName': '区域一'
        }, {
          'createTime': null,
          'id': 4,
          'projectId': 1,
          'monomerId': 1,
          'areaId': 1,
          'serialNumber': 'L-116',
          'quantity': 2,
          'producedQuantity': 0,
          'usedQuantity': 0,
          'remark': null,
          'totalSchedulingQuantity': 0,
          'totalTaskQuantity': 0,
          'schedulingProductionLineDTOS': [],
          'projectName': 'fsfsfsfs',
          'monomerName': '单体一',
          'areaName': '区域一'
        }, {
          'createTime': null,
          'id': 11,
          'projectId': 38,
          'monomerId': 3,
          'areaId': 4,
          'serialNumber': 'L-115',
          'quantity': 3,
          'producedQuantity': 0,
          'usedQuantity': 0,
          'remark': null,
          'totalSchedulingQuantity': 0,
          'totalTaskQuantity': 0,
          'schedulingProductionLineDTOS': [],
          'projectName': 'hgg',
          'monomerName': '单体#1',
          'areaName': '结构区域1'
        }, {
          'createTime': null,
          'id': 12,
          'projectId': 38,
          'monomerId': 3,
          'areaId': 4,
          'serialNumber': 'L-116',
          'quantity': 5,
          'producedQuantity': 0,
          'usedQuantity': 0,
          'remark': null,
          'totalSchedulingQuantity': 0,
          'totalTaskQuantity': 0,
          'schedulingProductionLineDTOS': [],
          'projectName': 'hgg',
          'monomerName': '单体#1',
          'areaName': '结构区域1'
        }]
      }
    }
  }
}

export default [
  getAssemble
]
