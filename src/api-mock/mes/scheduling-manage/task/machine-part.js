const getMachinePartSummary = {
  url: '/api/mes/building/scheduling/machine_part/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'totalElements': 1,
        'content': [{
          'schedulingQuantity': 20,
          'totalSchedulingNetWeight': null,
          'totalSchedulingGrossWeight': null,
          'taskQuantity': 20,
          'totalTaskNetWeight': null,
          'totalTaskGrossWeight': null,
          'completeQuantity': 0,
          'totalCompleteNetWeight': null,
          'totalCompleteGrossWeight': null,
          'date': '2021-11-11'
        }]
      }
    }
  }
}

export default [
  getMachinePartSummary
]
