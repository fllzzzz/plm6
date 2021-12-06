import { businessTypeEnum } from '@enum-ms/contract'

const getContractRecord ={
  url: '/api/project/listContracts',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功',
      'data': {
        'hasPreviousPage': false,
        'hasNextPage': false,
        'totalElements': 8,
        'content': [{
          'id': 44,
          'serialNumber': 'tech2',
          'name': 'tech2',
          'shortName': 'tech2',
          'businessType': 2,
          'projectType': 1,
          'projectContent': '结构工程,围护工程',
          'attachmentCount': 0
        }, {
          'id': 43,
          'serialNumber': 'dsas541565',
          'name': '高科技的干活呢',
          'shortName': '郭德纲的',
          'businessType': 1,
          'projectType': 1,
          'projectContent': '主结构加工,檩条加工,天沟加工,夹芯板加工',
          'attachmentCount': 0
        }, {
          'id': 42,
          'serialNumber': 'tech测试',
          'name': 'tech测试',
          'shortName': 'tech测试',
          'businessType': 1,
          'projectType': 1,
          'projectContent': '主结构加工,檩条加工,天沟加工,夹芯板加工,桁架楼承板加工,压型楼承板加工,压型彩板加工',
          'attachmentCount': 0
        }, {
          'id': 41,
          'serialNumber': 'aaaatest',
          'name': 'aaaatest',
          'shortName': 'aaaa',
          'businessType': 1,
          'projectType': 1,
          'projectContent': '主结构加工,檩条加工,天沟加工,夹芯板加工,桁架楼承板加工,压型彩板加工,压型楼承板加工',
          'attachmentCount': 0
        }, {
          'id': 40,
          'serialNumber': 'ssstest',
          'name': 'ssstest',
          'shortName': 'sss',
          'businessType': 2,
          'projectType': 1,
          'projectContent': '围护工程,结构工程',
          'attachmentCount': 0
        }, {
          'id': 39,
          'serialNumber': 'content测试',
          'name': 'content测试',
          'shortName': 'content测试',
          'businessType': 1,
          'projectType': 1,
          'projectContent': '主结构加工,天沟加工,桁架楼承板加工,夹芯板加工,压型彩板加工,压型楼承板加工',
          'attachmentCount': 0
        }, {
          'id': 38,
          'serialNumber': 'hggggggg',
          'name': 'hgg',
          'shortName': 'hghghgh',
          'businessType': 1,
          'projectType': 1,
          'projectContent': '主结构加工,桁架楼承板加工,压型楼承板加工,压型彩板加工',
          'attachmentCount': 0
        }, {
          'id': 37,
          'serialNumber': 'fsdfsfsfsfsf',
          'name': 'fsfsfsfs',
          'shortName': 'fssfsfsfsf',
          'businessType': 1,
          'projectType': 1,
          'projectContent': '主结构加工,檩条加工,桁架楼承板加工,压型楼承板加工',
          'attachmentCount': 0
        }]
      }
    }
  }
}

export default [
  getContractRecord
]
