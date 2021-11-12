const getEnclosureSummary = {
  url: '/api/mes/building/scheduling/enclosure/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return { 'code': 20000, 'message': '成功', 'data': { 'totalElements': 1, 'content': [{ 'schedulingQuantity': 5, 'totalSchedulingLength': 61350.00000000, 'totalSchedulingArea': 50307000.0000000000000000, 'taskQuantity': 0, 'totalTaskLength': 0E-8, 'totalTaskArea': 0E-16, 'completeQuantity': 0, 'totalCompleteLength': 0E-8, 'totalCompleteArea': 0E-16, 'date': '2021-11-11' }] }}
  }
}

export default [
  getEnclosureSummary
]
