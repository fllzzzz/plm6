// 未关闭的申购单
const getUnclosedRequisitionsBrief = {
  url: '/api/wms/requisitions/unclosed/brief',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '操作成功',
      data: {
        'content|10': [
          {
            'basicClass|1-16': 1,
            'serialNumber|+1': [
              'SG-AFTER-123456',
              'SG-AFTER-133456',
              'SG-AFTER-123457',
              'SG-AFTER-133458',
              'SG-AFTER-123459',
              'SG-AFTER-133451',
              'SG-AFTER-123452',
              'SG-AFTER-133453',
              'SG-AFTER-123454',
              'SG-AFTER-133455'
            ],
            'projectId|+1': [undefined, 1, 2, 3, 4]
          }
        ]
      }
    }
  }
}

export default [getUnclosedRequisitionsBrief]
