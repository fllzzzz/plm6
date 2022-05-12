// 获取签证列表
const getVisaList = {
  url: '/api/visa/foreign',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 3,
        'content': [
          {
            'id': 3,
            'project': {
              'id': 5,
              'name': '深圳华云声信息技术',
              'shortName': '华云声',
              'serialNumber': 'AAA-'
            },
            'type': 1,
            'serialNumber': 'AAA--DW2',
            'reasonName': '设计院变更',
            'amount': 10000,
            'createUserName': '超级管理员',
            'checkUserName': '超级管理员',
            'status': 2,
            'isSubmitSettle': 1,
            'settlementRecord': true,
            'remark': '0000',
            'createTime': 1652062113000,
            'attachments': null,
            'attachmentRemark': '0000',
            'contractAmount': 5000
          },
          {
            'id': 2,
            'project': {
              'id': 5,
              'name': '深圳华云声信息技术',
              'shortName': '华云声',
              'serialNumber': 'AAA-'
            },
            'type': 1,
            'serialNumber': 'AAA--DW1',
            'reasonName': '自身设计缺陷',
            'amount': 10000,
            'createUserName': '超级管理员',
            'checkUserName': '超级管理员',
            'status': 2,
            'isSubmitSettle': 1,
            'settlementRecord': true,
            'remark': '零零零零',
            'createTime': 1652062024000,
            'attachments': null,
            'attachmentRemark': '六六六',
            'contractAmount': 5000
          },
          {
            'id': 1,
            'project': {
              'id': 1,
              'name': '杭州亚运会射击场馆改造工程',
              'shortName': '射击场馆',
              'serialNumber': 'CMJK220502'
            },
            'type': 1,
            'serialNumber': 'CMJK220502-DW1',
            'reasonName': '甲方变更',
            'amount': 100,
            'createUserName': '超级管理员',
            'checkUserName': '超级管理员',
            'status': 1,
            'isSubmitSettle': 0,
            'settlementRecord': false,
            'remark': '',
            'createTime': 1651544157000,
            'attachments': null,
            'attachmentRemark': null,
            'contractAmount': 3000000
          }
        ],
        'totalAmount': null
      }
    }
  }
}

// 添加签证单
const addVisa = {
  url: '/api/visa/foreign',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 添加结算单
const addSettlement = {
  url: '/api/visa/project/handle',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 编辑签证单
const editVisa = {
  url: '/api/visa/foreign',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 获取项目详情
const getProjectInfo = {
  url: RegExp('/api/visa/project/' + '\\d'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'id': 4,
        'name': '杭州体育馆',
        'shortName': '杭州体育馆',
        'serialNumber': 'HZTY220506-1',
        'contractAmount': 200000,
        'contractSignBodyName': '安徽初鸣',
        'customerUnit': '杭州羽毛球协会',
        'signingDate': null,
        'projectManagerName': '超级管理员',
        'marginAmount': 0,
        'marginType': null,
        'visaAmount': 0,
        'collectionAmount': 0,
        'invoiceAmount': 0,
        'happenedAmount': 0,
        'projectContentName': '主结构加工',
        'signerName': null,
        'invoiceType': 1,
        'taxRate': 0,
        'isTax': 1
      }
    }
  }
}

// 确签（审核）
const check = {
  url: '/api/visa',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 签证详情
const visaDetail = {
  url: RegExp('/api/visa/' + '\\d'),
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'id': 1,
        'type': 1,
        'serialNumber': 'CMJK220502-DW1',
        'visaDate': 1651507200000,
        'userId': 1,
        'userName': '超级管理员',
        'reasonId': '2',
        'reasonName': '甲方变更',
        'visaAmount': 100,
        'settlementDate': null,
        'processingSettlementAmount': null,
        'settlementAmount': null,
        'supervisionUnit': '',
        'designUnit': '',
        'governmentRegulation': '',
        'remark': '',
        'project': {
          'id': 1,
          'name': '杭州亚运会射击场馆改造工程',
          'shortName': '射击场馆',
          'serialNumber': 'CMJK220502',
          'contractAmount': 3000000,
          'contractSignBodyName': '杭州初鸣重工科技有限公司',
          'customerUnit': '浙江建工集团有限公司',
          'signingDate': 1651420800000,
          'projectManagerName': null,
          'marginAmount': 0,
          'marginType': null,
          'collectionAmount': 500000,
          'invoiceAmount': 0,
          'happenedAmount': 29950.74,
          'projectContentName': '主结构加工',
          'signerName': '周庄民',
          'invoiceType': 1,
          'taxRate': 0,
          'isTax': 1
        },
        'breachAmount': null,
        'attachments': [
          {
            'id': 339,
            'name': 'a.png',
            'imageUrl': 'https://plm-dev.hzchum.com/files/contract_visa/1651544169844_u=1916874321,3263909857&fm=253&fmt=auto&app=138&f=PNG.png',
            'tinyImageUrl': 'https://plm-dev.hzchum.com/files/null',
            'createUserId': 1,
            'createUserName': '超级管理员',
            'type': 201,
            'createTime': 1651544170000
          }
        ],
        'attachmentRemark': null
      }
    }
  }
}

export default [
  getVisaList,
  addVisa,
  editVisa,
  addSettlement,
  check,
  visaDetail,
  getProjectInfo
]
