const getEnclosure = {
  url: RegExp('/api/plan/enclosure/list/' + '[1-9][0-9]*' + '/' + '[1-9][0-9]*'),
  method: 'get',
  timeout: 1000,
  response: (res) => {
    switch (Number(res.query.category)) {
      case 1:
        return{
          'code': 20000,
          'message': '成功',
          'data': {
            'hasPreviousPage': false,
            'hasNextPage': false,
            'totalElements': 1,
            'content': [{
              'id': 17,
              'name': '韩德板',
              'serialNumber': null,
              'plate': 'HV-1000',
              'color': null,
              'material': null,
              'weight': null,
              'thickness': 100.00000000,
              'width': 850.00000000,
              'length': 1000.00000000,
              'quantity': 10,
              'totalLength': 10.00000000,
              'totalArea': 8.50000000,
              'remark': null,
              'boolStatusEnum': 1,
              'brand': '中华',
              'type': '玻璃丝棉',
              'capacity': 120.00000000,
              'sandwichColorBoardList': [{
                'id': 1,
                'type': 1,
                'boardShape': '加肋',
                'brand': '宝钢',
                'thickness': 0.36000000,
                'width': 1200.00000000,
                'claddingMaterial': 'AZ85',
                'coating': 'PE',
                'color': '海蓝'
              }, {
                'id': 2,
                'type': 2,
                'boardShape': '三棱',
                'brand': '烨辉',
                'thickness': 0.55000000,
                'width': 1000.00000000,
                'claddingMaterial': 'A100',
                'coating': 'PVDF',
                'color': '白灰'
              }],
              'attachmentId': null
            }]
          }
        }
      default:
        return {
          'code': 20000,
          'message': '成功',
          'data': {
            'hasPreviousPage': false,
            'hasNextPage': false,
            'totalElements': 2,
            'content': [{
              'id': 14,
              'name': '闭口式楼承板',
              'serialNumber': 'QB1',
              'plate': 'HV-900',
              'color': null,
              'material': 'PE+AZ150',
              'weight': 1000.00000000,
              'thickness': 0.47600000,
              'width': 900.00000000,
              'length': 10000.00000000,
              'quantity': 100,
              'totalLength': 1000.00000000,
              'totalArea': 900.00000000,
              'remark': null,
              'boolStatusEnum': 1,
              'brand': null,
              'type': null,
              'capacity': null,
              'sandwichColorBoardList': null,
              'attachmentId': null
            }, {
              'id': 13,
              'name': '开口式楼层板',
              'serialNumber': 'WB1',
              'plate': 'HV-820',
              'color': null,
              'material': 'PE+AZ150',
              'weight': 1000.00000000,
              'thickness': 0.53000000,
              'width': 820.00000000,
              'length': 12270.00000000,
              'quantity': 147,
              'totalLength': 1803.69000000,
              'totalArea': 1479.02580000,
              'remark': null,
              'boolStatusEnum': 1,
              'brand': null,
              'type': null,
              'capacity': null,
              'sandwichColorBoardList': null,
              'attachmentId': null
            }]
          }
        }
      
    }
  }
}

const editEnclosure = {
  url: '/api/plan/enclosure/update',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': 10
    }
  }
}

const editEnclosureStatus = {
  url: RegExp('api/plan/enclosure/updateStatus/' + '.*'),
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': 10
    }
  }
}

const delEnclosure = {
  url: RegExp('api/plan/enclosure/deleteEnclosure/' + '.*'),
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '成功'
    }
  }
}

const listUploadEnclosure = {
  url: '/api/plan/enclosure/import',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      'code': 20000,
      'message': '操作成功',
      'data': 10
    }
  }
}

export default [
  getEnclosure,
  editEnclosure,
  editEnclosureStatus,
  delEnclosure,
  listUploadEnclosure
]
