import { enabledEnum } from '@enum-ms/common'
import { warehouseTypeEnum } from '@enum-ms/wms'

// 获取仓库位置
const getWarehouse = {
  url: '/api/wms/config/material/warehouse',
  method: 'get',
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'content|10': [{
          'createTime': 1621315424103,
          'factoryId': 1,
          'id|+1': 1,
          'name': '@increment()' + '号仓库',
          'type|1-2': warehouseTypeEnum.NORMAL.V,
          'materialType|1-64': 1,
          'sort|+1': 1,
          'enabled|1-2': enabledEnum.TRUE.V,
          'updateTime': 1621315424103
        }
        ],
        'totalElements': 10
      }
    }
  }
}

// 批量添加单位
const addWarehouse = {
  url: '/api/wms/config/material/warehouse',
  method: 'post',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 批量添加单位
const batchAddWarehouse = {
  url: '/api/wms/config/material/warehouse/batch',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 修改启用状态
const editWarehouse = {
  url: '/api/wms/config/material/warehouse',
  method: 'put',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 修改启用状态
const editEnabled = {
  url: RegExp('/api/wms/config/material/warehouse/enabled'),
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 删除单位
const delWarehouse = {
  url: '/api/wms/config/material/warehouse',
  method: 'delete',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [
  getWarehouse,
  addWarehouse,
  batchAddWarehouse,
  editWarehouse,
  editEnabled,
  delWarehouse
]
