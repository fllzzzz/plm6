const getContractLedger = {
  url: '/api/project/listAllProject/print',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'footer': null,
        'header': {
          'printDate': 1641369072747,
          'printer': '超级管理员',
          'statisticsDate': 1641369072747
        },
        'qrCode': null,
        'table': [
          {
            'orderType': '工程项目',
            'deptName': '工程部',
            'contractNo': 'CMQL001',
            'signingDate': 1604160000000,
            'projectType': 2,
            'project': {
              'contractNo': 'CMQL001',
              'id': 1,
              'name': '洛阳市王城大道快速路与火车站北广场连接线工程施工第二标段钢箱梁',
              'shortName': '北广场制作安装站'
            },
            'businessLeaderId': '36',
            'invoiceAmount': 1027800,
            'businessLeaderName': '刘文成',
            'signedMete': 800000,
            'invoiceType': 1,
            'collectionRate': 16.67,
            'actualSettlementAmount': 1000,
            'id': 1,
            'settlementType': 2,
            'collectionAmount': 1000000,
            'leaderPhone': '13909876578',
            'customerName': '河南六建建筑集团有限公司',
            'invoiceRate': 17.13,
            'leaderName': '刘志强',
            'listMete': null,
            'structureType': '9',
            'contractAmount': 6000000,
            'isTaxInclusive': true,
            'transportMode': 1,
            'projectName': '洛阳市王城大道快速路与火车站北广场连接线工程施工第二标段钢箱梁',
            'status': 0
          },
          {
            'orderType': '加工订单',
            'deptName': null,
            'contractNo': 'CM-0001',
            'signingDate': 1606320000000,
            'projectType': 1,
            'project': {
              'contractNo': 'CM-0001',
              'id': 2,
              'name': '东部战区空军五大队驻杭州江干区空中餐厅',
              'shortName': '战区一食堂'
            },
            'businessLeaderId': null,
            'invoiceAmount': 1001000,
            'businessLeaderName': null,
            'signedMete': 1000000,
            'invoiceType': 1,
            'collectionRate': 20.02,
            'actualSettlementAmount': 1005000,
            'id': 2,
            'settlementType': 2,
            'collectionAmount': 1001000,
            'leaderPhone': '',
            'customerName': '司令部',
            'invoiceRate': 20.02,
            'leaderName': '',
            'listMete': 941230.1099999968,
            'structureType': '2',
            'contractAmount': 5000000,
            'isTaxInclusive': true,
            'transportMode': 1,
            'projectName': '东部战区空军五大队驻杭州江干区空中餐厅',
            'status': 0
          }
        ]
      }
    }
  }
}

export default [
  getContractLedger
]
