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
            'config': '{"footer":{"verticleAlign":3,"emptyVal":"","size":10,"show":false,"width":190,"tip":{"size":9,"show":false,"above":true,"text":"","bold":"unset","align":1},"bold":"bold","align":1,"fields":[],"allPage":false,"height":15},"paddingLR":10,"unitPrecision":0,"type":"BRIDGE_MES_PACK","title":{"verticleAlign":3,"size":17,"show":true,"bold":"bold","title":"打包清单","align":2,"allPage":false,"height":10},"paddingTB":10,"unit":"mm","qrCode":{"top":10,"left":180,"show":true,"width":20.5,"allPage":false,"height":20.5},"fontUnit":"pt","name":"打包清单（平台）","width":210,"logo":{"top":10,"left":10,"show":false,"width":20,"url":"","allPage":false,"height":20},"header":{"verticleAlign":3,"emptyVal":"","size":10,"show":true,"width":190,"bold":"bold","align":1,"fields":[{"show":false,"width":70,"style":"width:70mm;","source":1,"title":"合同编号：","type":"CONTRACT_NO","key":"project.contractNo"},{"show":true,"width":190,"format":{"showProjectFullName":false,"projectNameShowMode":0,"lineBreak":false,"showContractNo":true},"style":"width:190mm;","source":1,"title":"项目：","type":"PROJECT","key":"project"},{"show":true,"width":190,"style":"width:190mm;","source":1,"title":"包单号：","type":"GUID","key":"bagSerial"},{"show":true,"width":135,"style":"width:135mm;","source":1,"title":"打包人：","type":"USER_NAME","key":"packUserName"},{"show":true,"width":55,"format":"YY/MM/DD kk:mm:ss","style":"width:55mm;","source":1,"title":"打包时间：","type":"DATE","key":"packDate"},{"show":false,"width":55,"format":"YY/MM/DD kk:mm:ss","style":"width:55mm;","source":1,"title":"打印日期：","type":"DATE","key":"printDate"},{"show":false,"width":35,"style":"width:35mm;","source":1,"title":"打印人：","type":"USER_NAME","key":"printer"}],"allPage":false,"height":17},"page":{"size":11,"bottom":2,"show":false,"format":1,"bold":"unset","align":2},"table":{"td":{"size":9,"lineHeight":13,"bold":"unset","paddingTB":2},"summary":{"show":true,"title":"合计"},"emptyVal":"/","th":{"size":10,"lineHeight":15,"bold":"bold","paddingTB":1},"index":{"show":true,"width":10,"style":"text-align:center;width:10mm;","title":"序号","align":2},"style":"<style>\\n      .preview-table th { font-size: 10pt; font-weight: bold; line-height: 15pt!important;}\\n      .preview-table th >div{ padding-top: 1mm!important; padding-bottom: 1mm!important;}\\n      .preview-table td { font-size: 9pt; font-weight: unset; line-height: 13pt!important;}\\n      .preview-table td >div{ padding-top: 2mm!important; padding-bottom: 2mm!important;}</style>","fields":[{"show":true,"minWidth":18,"style":"text-align:left;min-width:18mm;","source":1,"title":"名称","align":1,"type":"STRUCTURE_NAME","key":"name"},{"show":true,"minWidth":18,"style":"text-align:left;min-width:18mm;","source":1,"title":"编号","align":1,"type":"SERIAL_NUMBER","key":"serialNumber"},{"show":true,"minWidth":18,"style":"text-align:center;min-width:18mm;","source":1,"title":"单位","align":2,"type":"ACCOUNTING_UNIT","key":"unit"},{"show":true,"minWidth":18,"style":"text-align:left;min-width:18mm;","source":1,"title":"规格","align":1,"type":"SPECIFICATION","key":"specification"},{"show":true,"format":{"toThousandFilter":false,"precision":3},"minWidth":18,"sum":true,"style":"text-align:right;min-width:18mm;","source":1,"title":"总量","align":3,"type":"METE","key":"totalMete"}],"extraFields":[{"format":{"enum":"materialTypeEnum"},"title":"类型","type":"ENUM","key":"materialType"}]},"height":297}',
            'createTime': 1606632822266,
            'enabled': true,
            'id': 7,
            'isDefault': false,
            'name': '打包清单',
            'remark': '',
            'type': 'mesPackingList',
            'updateTime': 1635731698859
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
const editEnabled = {
  url: RegExp('/api/tablePrintTemplate/\\d/enabled'),
  method: 'put',
  timeout: 1000,
  response: () => {
    return {
      code: 20000,
      message: '成功'
    }
  }
}

// 修改默认模板
const editDefault = {
  url: RegExp('/api/tablePrintTemplate/\\d/default'),
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
  editEnabled,
  editDefault,
  delPrintTemplate
]
