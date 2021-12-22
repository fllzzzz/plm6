import { TechnologyTypeEnum } from '@enum-ms/contract'
const getTrussDetail = {
  url: '/api/enclosure/dictionaries/dictDetail/all',
  method: 'get',
  timeout: 1000,
  response: (res) => {
    switch (Number(res.query.type)) {
      case TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V:
        return {
          'code': 20000,
          'message': '操作成功',
          'data': {
            'totalElements': 7,
            'content': [{
              'createTime': 1632714653795,
              'id': 230,
              'label': '150',
              'name': 'high',
              'remark': '高度',
              'sort': 1,
              'type': 3,
              'code': '编号1',
              'status': 1
            }, {
              'createTime': 1632893259912,
              'id': 258,
              'label': '150',
              'name': 'firstQuarter',
              'remark': '上弦',
              'sort': 2,
              'type': 3,
              'code': '编号1',
              'status': 1
            }, {
              'createTime': 1632893259918,
              'id': 259,
              'label': '1',
              'name': 'lastQuarter',
              'remark': '下弦',
              'sort': 3,
              'type': 3,
              'code': '编号1',
              'status': 1
            }, {
              'createTime': 1632893259922,
              'id': 260,
              'label': '1',
              'name': 'webMember',
              'remark': '腹杆',
              'sort': 4,
              'type': 3,
              'code': '编号1',
              'status': 1
            }, {
              'createTime': 1632893259926,
              'id': 261,
              'label': '1',
              'name': 'vertical',
              'remark': '竖向',
              'sort': 5,
              'type': 3,
              'code': '编号1',
              'status': 1
            }, {
              'createTime': 1632893259931,
              'id': 262,
              'label': '1',
              'name': 'level',
              'remark': '水平',
              'sort': 6,
              'type': 3,
              'code': '编号1',
              'status': 1
            }, {
              'createTime': 1632893259934,
              'id': 263,
              'label': '1',
              'name': 'basementMembrane',
              'remark': '底膜',
              'sort': 7,
              'type': 3,
              'code': '编号1',
              'status': 1
            }]
          }
        }
      default:
        return {
          'code': 20000,
          'message': '操作成功',
          'data': {
            'totalElements': 2,
            'content': [{
              'createTime': 1636694521650,
              'id': 284,
              'label': '1',
              'name': 'brand',
              'remark': '品牌',
              'sort': 2,
              'type': 1,
              'code': null,
              'status': 1
            }, {
              'createTime': 1632478805118,
              'id': 226,
              'label': '名牌',
              'name': 'brand',
              'remark': '品牌',
              'sort': 6,
              'type': 1,
              'code': null,
              'status': 1
            }]
          }
        }
    }
  }
}

const editTrussDetail = {
  url: `/api/enclosure/dictionaries/dictDetailSave/type/${TechnologyTypeEnum.TRUSS_FLOOR_PLATE.V}`,
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': true
    }
  }
}

export default [
  getTrussDetail,
  editTrussDetail
]
