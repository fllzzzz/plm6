import { purchaseStatusEnum } from '@enum-ms/wms'

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

// 获取申购单详情
const getRequisitionsDetailBySN = {
  url: RegExp('/api/wms/requisitions/serial-number/' + '.*'),
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '操作成功',
      data: {
        'basicClass|1-16': 1,
        'serialNumber|+1': ['SG-AFTER-123456', 'SG-AFTER-133456'],
        'projectId|+1': 1
      }
    }
  }
}

// 获取申购单列表
const getRequisitionsList = {
  url: '/api/wms/requisitions',
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '操作成功',
      data: {
        'content|10': [
          {
            'id|+1': 1,
            'basicClass|1-16': 1, // 物流种类
            'purchaseNo': 'XXX工程', // 申购单号
            'purchaseOrderNoList': [
              '浙江宝板2021'
            ], // 关联采购单号
            status: purchaseStatusEnum.UNFINISHED.V, // 采购状态
            writtenByName: '@cname', // 填写人
            applicantName: '@cname', // 创建人
            lastOperatorName: '@cname', // 最后编辑人
            'project': {
              'contractNo': '11001',
              'id': 12,
              'name': '安徽及兔物流仓库 ',
              'shortName': '及兔物流'
            }, // 项目
            'projectId': 12
          }
        ],
        totalElements: 10
      }
    }
  }
}

// 添加采购订单
const add = {
  url: '/api/wms/requisitions',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 修改采购订单
const edit = {
  url: '/api/wms/requisitions',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 修改采购单状态状态
const editStatus = {
  url: '/api/wms/requisitions/status',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 删除采购订单
const del = {
  url: '/api/wms/requisitions',
  method: 'delete',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 下载excel表格
const download = {
  url: '/api/wms/requisitions/export',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '操作成功'
    }
  }
}

export default [
  getUnclosedRequisitionsBrief,
  getRequisitionsDetailBySN,
  getRequisitionsList,
  add,
  edit,
  editStatus,
  del,
  download
]
