import { validatorPhone } from '@/utils/validate/pattern'
import { measureTypeEnum } from '@enum-ms/wms'
import { materialClassificationEnum } from '@enum-ms/classification'
// 根据id, 获取末级物料分类
const getFinalMatClsById = {
  url: RegExp('/api/config/classification/final-material/' + '[1-9][0-9]*'),
  method: 'get',
  timeout: 500,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data:
        // {
        //   'id|1-100': 1,
        //   name: '角铁',
        //   fullName: '角铁',
        //   measureUnit: '根', // 计量单位
        //   accountingUnit: '千克', // 核算单位
        //   accountingPrecision: 2, // 核算单位小数精度
        //   measurePrecision: 0, // 计量单位小数精度
        //   outboundUnit: measureTypeEnum.MEASURE.V, // 出库方式
        //   basicClass: materialClassificationEnum.SECTION_STEEL.V,
        //   specConfig: [
        //     {
        //       id: 1,
        //       name: 'GB-06',
        //       'list|20': [
        //         { code: '01', name: /(\d{1,3}\*){3}\d{1,3}/ }
        //       ]
        //     }
        //   ]
        // }
        {
          'id|1-100': 1,
          name: '大六角',
          fullName: '紧固件>高强螺栓>大六角',
          measureUnit: '套', // 计量单位
          accountingUnit: '套', // 核算单位
          accountingPrecision: 0, // 核算单位小数精度
          measurePrecision: 0, // 计量单位小数精度
          outboundUnit: measureTypeEnum.MEASURE.V, // 出库方式
          basicClass: materialClassificationEnum.MATERIAL.V,
          specConfig: [
            {
              id: 1,
              name: '直径',
              list: [
                { code: '01', name: 'M24' },
                { code: '02', name: 'M26' },
                { code: '03', name: 'M27' },
                { code: '04', name: 'M28' },
                { code: '05', name: 'M29' },
                { code: '06', name: 'M30' },
                { code: '07', name: 'M31' },
                { code: '08', name: 'M32' },
                { code: '09', name: 'M33' },
                { code: '10', name: 'M34' }
              ]
            },
            {
              id: 2,
              name: '长度',
              list: [
                { code: '01', name: '60' },
                { code: '02', name: '65' },
                { code: '03', name: '66' },
                { code: '04', name: '67' },
                { code: '05', name: '68' },
                { code: '06', name: '69' },
                { code: '07', name: '70' },
                { code: '08', name: '71' },
                { code: '09', name: '72' },
                { code: '10', name: '73' },
                { code: '11', name: '74' }
              ]
            }
          ]
        }
    }
  }
}

// 获取所有用户
const getUserAllSimple = {
  url: '/api/user/all/simple',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '操作成功',
      data: {
        totalElements: 100,
        'content|100': [
          {
            'id|1-100': 1,
            name: '@cname',
            deptName: null,
            phone: validatorPhone,
            email: '@email'
          }
        ]
      }
    }
  }
}

const getDeptAllSimple = {
  url: '/api/dept/all/simple',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      data: {
        content: [
          {
            children: [
              { children: null, id: 2, name: '合同管理部' },
              { children: null, id: 3, name: '技术部' },
              { children: null, id: 4, name: '工程部' },
              { children: null, id: 5, name: '商务管理部' },
              { children: null, id: 11, name: '计划管理部' },
              { children: null, id: 12, name: '采购部' },
              {
                children: [
                  { children: null, id: 22, name: '萧山仓库' },
                  { children: null, id: 23, name: '江干仓库' }
                ],
                id: 13,
                name: '仓库管理部'
              },
              { children: null, id: 14, name: '财务部' },
              {
                children: [
                  { children: null, id: 20, name: '萧山质检' },
                  { children: null, id: 21, name: '江干质检' }
                ],
                id: 15,
                name: '质量管理部'
              },
              { children: null, id: 16, name: '行政部' },
              {
                children: [
                  { children: null, id: 18, name: '萧山工厂' },
                  { children: null, id: 19, name: '江干工厂' },
                  { children: null, id: 31, name: '吕冰工厂' }
                ],
                id: 17,
                name: '生产部'
              },
              {
                children: [
                  { children: null, id: 25, name: '桥梁生产车间' },
                  { children: null, id: 26, name: '桥梁质检' },
                  { children: null, id: 27, name: '桥梁发运' },
                  { children: null, id: 29, name: '桥梁管理部' },
                  { children: null, id: 30, name: '桥梁项目部' }
                ],
                id: 28,
                name: '桥梁事业部'
              },
              { children: null, id: 32, name: '仓管' }
            ],
            id: 1,
            name: '初鸣建科'
          }
        ],
        totalElements: 1
      },
      message: '操作成功'
    }
  }
}

export default [
  getUserAllSimple,
  getFinalMatClsById,
  getDeptAllSimple]
