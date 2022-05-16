// 客户交易列表
const list = {
  url: '/api/business/check/summary',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'totalElements': 4,
        'content': [
          {
            'name': '宝钢建设',
            'businessType': 1,
            'quantity': 1,
            'totalContractAmount': 100000,
            'totalSettlementAmount': 0
          },
          {
            'name': '杭州羽毛球协会',
            'businessType': 1,
            'quantity': 2,
            'totalContractAmount': 700000,
            'totalSettlementAmount': 0
          },
          {
            'name': '浙江建工集团有限公司',
            'businessType': 1,
            'quantity': 1,
            'totalContractAmount': 3000000,
            'totalSettlementAmount': 0
          },
          {
            'name': '深圳华云声信息技术',
            'businessType': 1,
            'quantity': 1,
            'totalContractAmount': 5000,
            'totalSettlementAmount': 20000
          }
        ]
      }
    }
  }
}

// 订单跟踪详情
const detail = {
  url: '/api/business/check/detail',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'totalElements': 1,
        'content': [
          {
            'project': {
              'id': 5,
              'name': '深圳华云声信息技术',
              'shortName': '华云声',
              'serialNumber': 'AAA-'
            },
            'contractAmount': 5000,
            'settlementAmount': 20000,
            'signingDate': 1652112000000,
            'signerName': '超级管理员'
          }
        ]
      }
    }
  }
}

export default [
  list,
  detail
]

