// 订单跟踪列表
const list = {
  url: '/api/project/tracking',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 2,
        'content': [
          {
            'project': {
              'id': 1,
              'name': '杭州亚运会射击场馆改造工程',
              'shortName': '射击场馆',
              'serialNumber': 'CMJK220502'
            },
            'status': 1,
            'settlementStatus': 0,
            'projectContentName': '主结构加工',
            'contractAmount': 3000000,
            'settlementAmount': null,
            'collectionAmount': 500000,
            'invoiceAmount': 0,
            'happenedAmount': 29950.74,
            'transportQuantity': 3
          },
          {
            'project': {
              'id': 5,
              'name': '深圳华云声信息技术',
              'shortName': '华云声',
              'serialNumber': 'AAA-'
            },
            'status': 4,
            'settlementStatus': 1,
            'projectContentName': '主结构加工',
            'contractAmount': 5000,
            'settlementAmount': 20000,
            'collectionAmount': 11000,
            'invoiceAmount': 7000,
            'happenedAmount': 0,
            'transportQuantity': 0
          }
        ],
        'totalAmount': null
      }
    }
  }
}

// 收款记录
const collectionRecord = {
  url: '/api/contract/collection/listPage',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 1,
        'content': [
          {
            'id': 1,
            'projectId': 1,
            'project': {
              'id': 1,
              'name': '杭州亚运会射击场馆改造工程',
              'shortName': '射击场馆',
              'serialNumber': 'CMJK220502'
            },
            'businessType': 1,
            'contractAmount': 3000000,
            'haveCollectionAmount': 500000,
            'collectionUnit': '杭州初鸣重工科技有限公司',
            'collectionUnitId': 1,
            'collectionDepositBank': '中国银行',
            'collectionBankAccountId': 49,
            'collectionBankAccount': '123456',
            'collectionDate': 1651420800000,
            'collectionMode': 1,
            'acceptanceDraftJson': null,
            'acceptanceDraftDTOList': null,
            'collectionAmount': 500000,
            'collectionReason': '1',
            'paymentUnit': '浙江建工集团有限公司',
            'paymentDepositBank': null,
            'paymentBankAccount': null,
            'writtenById': 1,
            'writtenByName': '超级管理员',
            'auditorId': 1,
            'auditorName': '超级管理员',
            'auditTime': 1651455325000,
            'auditStatus': 2,
            'lastEditorUserId': null,
            'lastEditorUserName': null,
            'remark': null,
            'isPrepayment': 0,
            'createTime': 1651455322831
          }
        ],
        'totalAmount': null
      }
    }
  }
}

// 开票记录
const invoiceRecord = {
  url: '/api/contract/invoice/listPage',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 1,
        'content': [
          {
            'id': 2,
            'projectId': 5,
            'project': {
              'id': 5,
              'name': '深圳华云声信息技术',
              'shortName': '华云声',
              'serialNumber': 'AAA-'
            },
            'businessType': 1,
            'contractAmount': 5000,
            'invoiceAmount': 7000,
            'invoiceUnitId': '1',
            'invoiceUnit': '杭州初鸣重工科技有限公司',
            'collectionUnit': '深圳华云声信息技术',
            'invoiceDate': 1652112000000,
            'invoiceNo': '121212',
            'invoiceType': 1,
            'tax': 700,
            'taxRate': 10,
            'writtenById': 1,
            'writtenByName': '超级管理员',
            'createTime': 1652061770000,
            'auditStatus': 2,
            'auditorId': 1,
            'auditorName': '超级管理员',
            'auditTime': 1652061784000,
            'lastEditorUserId': 1,
            'lastEditorUserName': null,
            'remark': null,
            'attachmentList': null
          }
        ],
        'totalAmount': null
      }
    }
  }
}

// 发运记录
const shipRecord = {
  url: '/api/project/tracking/record',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 3,
        'content': [
          {
            'project': {
              'id': 1,
              'name': '杭州亚运会射击场馆改造工程',
              'shortName': '射击场馆',
              'serialNumber': 'CMJK220502'
            },
            'monomer': {
              'id': 1,
              'name': '射击馆'
            },
            'area': {
              'id': 1,
              'name': '第一批次'
            },
            'name': '钢梁',
            'serialNumber': '7-2GL-2',
            'specification': 'BH600*250*8*12',
            'material': 'Q355B',
            'measure': '件',
            'nuclear': 't',
            'quantity': 2,
            'totalMete': 1.37,
            'unitPrice': 0,
            'totalPrice': 0,
            'licensePlate': '浙A12345',
            'auditTime': 1651484858000
          },
          {
            'project': {
              'id': 1,
              'name': '杭州亚运会射击场馆改造工程',
              'shortName': '射击场馆',
              'serialNumber': 'CMJK220502'
            },
            'monomer': {
              'id': 1,
              'name': '射击馆'
            },
            'area': {
              'id': 2,
              'name': '第二批次'
            },
            'name': '钢梁',
            'serialNumber': '7-2GL-2',
            'specification': 'BH600*250*8*12',
            'material': 'Q355B',
            'measure': '件',
            'nuclear': 't',
            'quantity': 3,
            'totalMete': 2.06,
            'unitPrice': 8500,
            'totalPrice': 17510,
            'licensePlate': '浙A12345',
            'auditTime': 1651548121000
          },
          {
            'project': {
              'id': 1,
              'name': '杭州亚运会射击场馆改造工程',
              'shortName': '射击场馆',
              'serialNumber': 'CMJK220502'
            },
            'monomer': {
              'id': 1,
              'name': '射击馆'
            },
            'area': {
              'id': 2,
              'name': '第二批次'
            },
            'name': '钢梁',
            'serialNumber': '7-2GL-1',
            'specification': 'BH600*250*8*12',
            'material': 'Q355B',
            'measure': '件',
            'nuclear': 't',
            'quantity': 2,
            'totalMete': 1.38,
            'unitPrice': 9000,
            'totalPrice': 12420,
            'licensePlate': '浙A12345',
            'auditTime': 1651548121000
          }
        ],
        'totalAmount': null
      }
    }
  }
}

export default [
  shipRecord,
  collectionRecord,
  invoiceRecord,
  list
]

