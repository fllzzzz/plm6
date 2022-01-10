// 获取打印模板列表
const getPrintTemplateList = {
  url: '/api/tablePrintTemplate',
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'content': [
          {
            'config': '{"footer":{"verticleAlign":3,"emptyVal":"","size":10,"show":false,"width":190,"tip":{"size":9,"show":false,"above":true,"text":"","bold":"unset","align":1},"bold":"bold","align":1,"fields":[],"allPage":false,"height":15},"paddingLR":10,"unitPrecision":0,"type":"STEEL_MES_BUSINESS_OUTBOUND_PROJECT","title":{"verticleAlign":3,"size":17,"show":true,"bold":"bold","title":"出库汇总单","align":2,"allPage":false,"height":10},"paddingTB":10,"unit":"mm","fontUnit":"pt","name":"出库表(汇总)（平台）","width":210,"logo":{"top":10,"left":10,"show":false,"width":20,"url":"","allPage":false,"height":20},"header":{"verticleAlign":3,"emptyVal":"","size":10,"show":true,"width":190,"bold":"bold","align":1,"fields":[{"show":false,"width":70,"style":"width:70mm;","source":1,"title":"合同编号：","type":"CONTRACT_NO","key":"project.contractNo"},{"show":true,"width":190,"format":{"showProjectFullName":false,"projectNameShowMode":0,"lineBreak":false,"showContractNo":true},"style":"width:190mm;","source":1,"title":"项目：","type":"PROJECT","key":"project"},{"show":true,"width":70,"format":"YY/MM/DD","style":"width:70mm;","source":1,"title":"统计日期：","type":"DATES","key":"statisticsDate"},{"show":false,"width":40,"format":{"enum":"engineerSettlementTypeEnum","key":"SL"},"style":"width:40mm;","source":1,"title":"结构结算方式：","type":"ENUM","key":"settlementType"},{"show":false,"width":40,"format":{"enum":"enclosureSettlementTypeEnum","key":"SL"},"style":"width:40mm;","source":1,"title":"围护结算方式：","type":"ENUM","key":"enclosurePriceType"},{"show":false,"width":40,"format":{"enum":"weightTypeEnum","key":"L"},"style":"width:40mm;","source":1,"title":"重量类型：","type":"ENUM","key":"weightType"},{"show":false,"width":150,"format":"YY/MM/DD","style":"width:150mm;","source":1,"title":"打印日期：","type":"DATE","key":"printDate"},{"show":false,"width":40,"style":"width:40mm;","source":1,"title":"打印人：","type":"USER_NAME","key":"printer"}],"allPage":false,"height":11},"page":{"size":11,"bottom":2,"show":false,"format":1,"bold":"unset","align":2},"table":{"td":{"size":9,"lineHeight":5,"bold":"unset","paddingTB":2},"summary":{"show":true,"title":"合计"},"emptyVal":"/","th":{"size":10,"lineHeight":12,"bold":"bold","paddingTB":1},"index":{"show":true,"width":10,"style":"text-align:center;width:10mm;","title":"序号","align":2},"style":"<style>\\n      .preview-table th { font-size: 10pt; font-weight: bold; line-height: 12pt!important;}\\n      .preview-table th >div{ padding-top: 1mm!important; padding-bottom: 1mm!important;}\\n      .preview-table td { font-size: 9pt; font-weight: unset; line-height: 5pt!important;}\\n      .preview-table td >div{ padding-top: 2mm!important; padding-bottom: 2mm!important;}</style>","fields":[{"show":true,"style":"text-align:left;","source":1,"title":"名称","align":1,"type":"STRUCTURE_NAME","key":"name"},{"show":true,"style":"text-align:center;","source":1,"title":"材质/规格","align":2,"type":"MATERIAL","key":"projectMaterial"},{"show":true,"style":"text-align:center;","source":1,"title":"单位","align":2,"type":"ACCOUNTING_UNIT","key":"unit"},{"show":true,"format":{"toThousandFilter":false,"precision":3},"style":"text-align:right;","source":1,"title":"总量","align":3,"type":"METE","key":"totalMete"},{"show":true,"format":{"unit":"y","toThousandFilter":true,"precision":2},"style":"text-align:right;","source":1,"title":"单价（元）","align":3,"type":"AMOUNT","key":"price"},{"show":true,"format":{"unit":"y","toThousandFilter":true,"precision":2},"sum":true,"style":"text-align:right;","source":1,"title":"总计（元）","align":3,"type":"AMOUNT","key":"totalPrice"}],"extraFields":[{"format":{"enum":"materialTypeEnum"},"title":"类型","type":"ENUM","key":"materialType"}]},"height":297}',
            'createTime': 1607131644143,
            'enabled': 1,
            'id': 8,
            'isDefault': false,
            'moduleType': 'STEEL_MES_BUSINESS_STATISTICS',
            'name': '出库表(汇总)',
            'remark': '',
            'type': 'STEEL_MES_BUSINESS_OUTBOUND_PROJECT',
            'updateTime': 1623223064776
          },
          {
            'config': '{"footer":{"verticleAlign":3,"emptyVal":"","size":10,"show":false,"width":190,"tip":{"size":9,"show":false,"above":true,"text":"","bold":"unset","align":1},"bold":"bold","align":1,"fields":[],"allPage":false,"height":15},"paddingLR":10,"unitPrecision":0,"type":"STEEL_MES_BUSINESS_OUTBOUND_MONOMER","title":{"verticleAlign":3,"size":17,"show":true,"bold":"bold","title":"出库汇总单（单体）","align":2,"allPage":false,"height":10},"paddingTB":10,"unit":"mm","fontUnit":"pt","name":"出库表(单体)（平台）","width":210,"logo":{"top":10,"left":10,"show":false,"width":20,"url":"","allPage":false,"height":20},"header":{"verticleAlign":3,"emptyVal":"","size":10,"show":true,"width":190,"bold":"bold","align":1,"fields":[{"show":false,"width":70,"style":"width:70mm;","source":1,"title":"合同编号：","type":"CONTRACT_NO","key":"project.contractNo"},{"show":true,"width":190,"format":{"showProjectFullName":false,"projectNameShowMode":0,"lineBreak":false,"showContractNo":true},"style":"width:190mm;","source":1,"title":"项目：","type":"PROJECT","key":"project"},{"show":true,"width":150,"style":"width:150mm;","source":1,"title":"单体：","type":"MONOMER_NAME","key":"monomer"},{"show":false,"width":40,"format":{"enum":"weightTypeEnum","key":"L"},"style":"width:40mm;","source":1,"title":"重量类型：","type":"ENUM","key":"weightType"},{"show":true,"width":110,"format":"YY/MM/DD","style":"width:110mm;","source":1,"title":"统计日期：","type":"DATE","key":"statisticsDate"},{"show":true,"width":40,"format":{"enum":"engineerSettlementTypeEnum","key":"SL"},"style":"width:40mm;","source":1,"title":"结构结算方式：","type":"ENUM","key":"settlementType"},{"show":false,"width":40,"format":{"enum":"enclosureSettlementTypeEnum","key":"SL"},"style":"width:40mm;","source":1,"title":"围护结算方式：","type":"ENUM","key":"enclosurePriceType"},{"show":false,"width":40,"format":"YY/MM/DD","style":"width:40mm;","source":1,"title":"打印日期：","type":"DATE","key":"printDate"},{"show":false,"width":30,"style":"width:30mm;","source":1,"title":"打印人：","type":"USER_NAME","key":"printer"}],"allPage":false,"height":18},"page":{"size":11,"bottom":2,"show":false,"format":1,"bold":"unset","align":2},"table":{"td":{"size":9,"lineHeight":13,"bold":"unset","paddingTB":2},"summary":{"show":true,"title":"合计"},"emptyVal":"/","th":{"size":10,"lineHeight":15,"bold":"bold","paddingTB":1},"index":{"show":true,"width":10,"style":"text-align:center;width:10mm;","title":"序号","align":2},"style":"<style>\\n      .preview-table th { font-size: 10pt; font-weight: bold; line-height: 15pt!important;}\\n      .preview-table th >div{ padding-top: 1mm!important; padding-bottom: 1mm!important;}\\n      .preview-table td { font-size: 9pt; font-weight: unset; line-height: 13pt!important;}\\n      .preview-table td >div{ padding-top: 2mm!important; padding-bottom: 2mm!important;}</style>","fields":[{"show":true,"minWidth":18,"style":"text-align:left;min-width:18mm;","source":1,"title":"名称","align":1,"type":"STRUCTURE_NAME","key":"name"},{"show":true,"minWidth":18,"style":"text-align:left;min-width:18mm;","source":1,"title":"编号","align":1,"type":"SERIAL_NUMBER","key":"serialNumber"},{"show":true,"minWidth":18,"style":"text-align:left;min-width:18mm;","source":1,"title":"规格","align":1,"type":"SPECIFICATION","key":"specification"},{"show":true,"minWidth":18,"style":"text-align:center;min-width:18mm;","source":1,"title":"材质","align":2,"type":"MATERIAL","key":"material"},{"show":true,"width":12,"style":"text-align:center;width:12mm;","source":1,"title":"计量单位","align":2,"type":"MEASUREMENT_UNIT","key":"measurementUnit"},{"show":true,"format":{"toThousandFilter":false,"precision":0},"minWidth":14,"sum":true,"style":"text-align:center;min-width:14mm;","source":1,"title":"数量","align":2,"type":"QUANTITY","key":"quantity"},{"show":true,"width":12,"style":"text-align:center;width:12mm;","source":1,"title":"核算单位","align":2,"type":"ACCOUNTING_UNIT","key":"checkUnit"},{"show":true,"format":{"toThousandFilter":false,"precision":3},"minWidth":18,"style":"text-align:right;min-width:18mm;","source":1,"title":"总量","align":3,"type":"METE","key":"totalMete"},{"show":true,"format":{"unit":"y","toThousandFilter":true,"precision":2},"minWidth":18,"style":"text-align:right;min-width:18mm;","source":1,"title":"单价","align":3,"type":"AMOUNT","key":"price"},{"show":true,"format":{"unit":"y","toThousandFilter":true,"precision":2},"minWidth":18,"sum":true,"style":"text-align:right;min-width:18mm;","source":1,"title":"总计","align":3,"type":"AMOUNT","key":"totalPrice"}],"extraFields":[{"format":{"enum":"materialTypeEnum"},"title":"类型","type":"ENUM","key":"materialType"}]},"height":297}',
            'createTime': 1607132041461,
            'enabled': 1,
            'id': 9,
            'isDefault': true,
            'moduleType': 'STEEL_MES_BUSINESS_STATISTICS',
            'name': '出库表(单体)',
            'remark': '',
            'type': 'STEEL_MES_BUSINESS_OUTBOUND_MONOMER',
            'updateTime': 1623222996726
          }
        ],
        'totalElements': 2
      }
    }
  }
}

// 获取表格模板
const getByType = {
  url: RegExp('/api/tablePrintTemplate/type/'),
  method: 'get',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功',
      data: {
        'content': [],
        'totalElements': 0
      }
    }
  }
}

// 新增打印模板
const addPrintTemplate = {
  url: '/api/tablePrintTemplate',
  method: 'post',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 编辑打印模板
const editPrintTemplate = {
  url: '/api/tablePrintTemplate',
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 修改隐藏状态
const editStatus = {
  url: RegExp('/api/wms/supplier/enabled'),
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 删除打印模板
const delPrintTemplate = {
  url: '/api/tablePrintTemplate',
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
  getByType,
  getPrintTemplateList,
  editPrintTemplate,
  addPrintTemplate,
  editStatus,
  delPrintTemplate
]
