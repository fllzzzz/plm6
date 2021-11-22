import { supplierClassEnum } from '@enum-ms/supplier'

// 获取常用税率列表（简要的）
const getTaxRateBrief = {
  url: '/api/wms/config/material/tax-rate/all/brief',
  method: 'get',
  response: () => {
    return {
      message: '操作成功',
      code: 20000,
      data: {
        content: [
          {
            classification: supplierClassEnum.STEEL_PLATE.V,
            id: 1,
            taxRateList: [13.0]
          },
          {
            classification: supplierClassEnum.SECTION_STEEL.V,
            id: 2,
            taxRateList: [13.0]
          },
          {
            classification: supplierClassEnum.STEEL_COIL.V,
            id: 3,
            taxRateList: [13.0]
          },
          {
            classification: supplierClassEnum.MATERIAL.V,
            id: 4,
            taxRateList: [13.0, 3.0]
          },
          {
            classification: supplierClassEnum.GAS.V,
            id: 5,
            taxRateList: [13.0]
          },
          {
            classification: supplierClassEnum.STRUC_MANUFACTURED.V,
            id: 6,
            taxRateList: [13.0]
          },
          {
            classification: supplierClassEnum.ENCL_MANUFACTURED.V,
            id: 7,
            taxRateList: [13.0]
          },
          {
            classification: supplierClassEnum.LOGISTICS.V,
            id: 8,
            taxRateList: [9.0, 3.0]
          }
        ]
      }
    }
  }
}

// 获取常用税率
const get = {
  url: '/api/wms/config/material/tax-rate',
  method: 'get',
  response: () => {
    return {
      message: '操作成功',
      code: 20000,
      data: {
        content: [
          {
            classification: supplierClassEnum.STEEL_PLATE.V,
            id: 1,
            taxRateList: [13.0],
            createTime: '@datetime(T)',
            updateTime: '@datetime(T)'
          },
          {
            classification: supplierClassEnum.SECTION_STEEL.V,
            id: 2,
            taxRateList: [13.0],
            createTime: '@datetime(T)',
            updateTime: '@datetime(T)'
          },
          {
            classification: supplierClassEnum.STEEL_COIL.V,
            id: 3,
            taxRateList: [13.0],
            createTime: '@datetime(T)',
            updateTime: '@datetime(T)'
          },
          {
            classification: supplierClassEnum.MATERIAL.V,
            id: 4,
            taxRateList: [13.0, 3.0],
            createTime: '@datetime(T)',
            updateTime: '@datetime(T)'
          },
          {
            classification: supplierClassEnum.GAS.V,
            id: 5,
            taxRateList: [13.0],
            createTime: '@datetime(T)',
            updateTime: '@datetime(T)'
          },
          {
            classification: supplierClassEnum.STRUC_MANUFACTURED.V,
            id: 6,
            taxRateList: [13.0],
            createTime: '@datetime(T)',
            updateTime: '@datetime(T)'
          },
          {
            classification: supplierClassEnum.ENCL_MANUFACTURED.V,
            id: 7,
            taxRateList: [13.0],
            createTime: '@datetime(T)',
            updateTime: '@datetime(T)'
          },
          {
            classification: supplierClassEnum.LOGISTICS.V,
            id: 8,
            taxRateList: [9.0, 3.0],
            createTime: '@datetime(T)',
            updateTime: '@datetime(T)'
          }
        ],
        totalElements: 8
      }
    }
  }
}

// 修改常用税率
const edit = {
  url: '/api/wms/config/material/tax-rate',
  method: 'put',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

export default [getTaxRateBrief, get, edit]
