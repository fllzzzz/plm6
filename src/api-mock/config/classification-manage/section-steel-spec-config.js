// 获取型材列表
const getSectionSteelList = {
  url: '/api/config/classification/material/section-steel',
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        content: [{
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 71,
          'id': 28,
          'name': '扁钢',
          'standardId': 1,
          children: [
            {
              'serialNumber': '@natural(1000,9999)',
              'attachmentId': 71,
              'id': 1000,
              'name': '扁钢-1',
              'standardId': 1
            }
          ]
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 70,
          'id': 27,
          'name': '扁铁',
          'standardId': 2
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 65,
          'id': 26,
          'name': '镀锌焊管',
          'standardId': 2
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 58,
          'id': 25,
          'name': '圆钢',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 39,
          'id': 24,
          'name': '镀锌角钢',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 40,
          'id': 23,
          'name': '镀锌管',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 41,
          'id': 22,
          'name': '镀锌扁铁',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 42,
          'id': 21,
          'name': '不等边角钢',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 44,
          'id': 19,
          'name': '高频焊H型钢',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 45,
          'id': 18,
          'name': '圆管',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 76,
          'id': 17,
          'name': '无缝管',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 47,
          'id': 16,
          'name': '热轧H型钢',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 48,
          'id': 15,
          'name': '螺纹钢',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 38,
          'id': 14,
          'name': '角钢',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 75,
          'id': 13,
          'name': '焊管',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 67,
          'id': 12,
          'name': '方管',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 51,
          'id': 11,
          'name': '镀锌C型钢',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 52,
          'id': 10,
          'name': '槽钢',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 53,
          'id': 9,
          'name': 'U肋',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 55,
          'id': 7,
          'name': '矩形管',
          'standardId': 1
        }, {
          'serialNumber': '@natural(1000,9999)',
          'attachmentId': 69,
          'id': 2,
          'name': '工字钢',
          'standardId': 1
        }]
      }
    }
  }
}

// 获取国标
const getStandard = {
  url: '/api/config/classification/material/section-steel/standard',
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: [
        {
          id: 1,
          name: 'GB-08',
          deletable: false
        },
        {
          id: 2,
          name: 'GB-97',
          deletable: true
        }
      ]
    }
  }
}

// 添加国标
const addStandard = {
  url: '/api/config/classification/material/section-steel/standard',
  method: 'post',
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 删除国标
const delStandard = {
  url: '/api/config/classification/material/section-steel/standard',
  method: 'delete',
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 设置国标
const setStandard = {
  url: '/api/config/classification/material/section-steel/standard/settings',
  method: 'put',
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 批量设置国标
const batchSetStandard = {
  url: '/api/config/classification/material/section-steel/standard/batch-settings',
  method: 'put',
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [
  getStandard, // 获取国标
  addStandard, // 添加国标
  delStandard, // 删除国标
  setStandard, // 设置国标
  batchSetStandard, // 设置批量国标
  getSectionSteelList // 获取型材列表
]
