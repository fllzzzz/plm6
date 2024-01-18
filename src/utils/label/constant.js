import { labelTypeEnum, componentTypeEnum } from '@enum-ms/mes'
import { weightTypeEnum as printWeightTypeEnum } from '@enum-ms/common'
import labelLogo from '@/assets/logo/label-logo.png'
import { parseTime } from '@/utils/date'
import { emptyTextFormatter } from '@/utils/data-type'

const defComponent = {
  projectName: '项目名称',
  printTime: parseTime((new Date()).getTime(), '{y}/{m}/{d}'),
  monomerName: '单体名称',
  serialNumber: '编号',
  oneCode: 'X/X',
  name: 'XX',
  quantity: 'XX',
  weight: 'XX',
  length: 'XX',
  thickness: 'XX',
  plate: 'XX',
  color: 'XX',
  specification: 'XX',
  areaName: 'XX'
}

// 构件-常规标签
const ARTIFACT_COMMON_L_HTML = function ({ component = defComponent, printConfig, manufacturerName = '制造商名称' }) {
  return `
  <div class="artifact-label">
    <table class="artifact-table">
      <tr>
        <td style="width:50%;font-size:10pt;">${emptyTextFormatter(component.projectName)}</td>
        <td style="width:50%;font-size:10pt;${printConfig?.showMonomer ? '' : 'display:none;'}">${emptyTextFormatter(component.monomerName)}</td>
      </tr>
    </table>
    <table class="artifact-table">
      <tr class="row-2">
        <td colspan="3" class="amplify-content">
          <span class="amplify-no">NO:</span>
          <span class="amplify-text">${emptyTextFormatter(component.serialNumber)}</span>
          <span class="amplify-date" style="${printConfig?.dateInProduced ? '' : 'display:none;'}">生产日期：${emptyTextFormatter(component.printTime)}</span>
        </td>
      </tr>
    </table>
    <table class="artifact-table">
      <tr>
        <td style="${printConfig?.weight !== printWeightTypeEnum.NONE.V ? '' : 'width:50%;'}">名称：${emptyTextFormatter(component.name)}</td>
        <td style="${printConfig?.weight !== printWeightTypeEnum.NONE.V ? '' : 'width:50%;'}">任务数(件)：${emptyTextFormatter(component.quantity)}</td>
        <td style="${printConfig?.weight !== printWeightTypeEnum.NONE.V ? '' : 'display:none;'}">单重(kg)：${emptyTextFormatter(component.weight)}</td>
      </tr>
    </table>
    <table class="artifact-table">
      <tr>
        <td>长度(mm)：${emptyTextFormatter(component.length)}</td>
        <td>规格：${emptyTextFormatter(component.specification)}</td>
        <td rowspan="3" class="qr-content"></td>
      </tr>
      <tr>
        <td colspan="${printConfig?.showArea && component?.oneCode ? 1 : 2}" style="${printConfig?.showArea ? '' : 'display:none;'}">区域：${emptyTextFormatter(component.areaName)}</td>
        <td colspan="${printConfig?.showArea && component?.oneCode ? 1 : 2}" style="${printConfig?.showArea ? 'display:none;' : ''}"></td>
        <td colspan="${printConfig?.showArea && component?.oneCode ? 1 : 2}" style="${component?.oneCode ? '' : 'display:none;'}">编码：${component.oneCode}</td>
      </tr>
      <tr>
        <td colspan="2">${emptyTextFormatter(manufacturerName)}</td>
      </tr>
    </table>
  </div>
`
}
// style="${!printConfig?.showArea && !component?.oneCode ? 'border-bottom:1px solid #000;' : ''}" xp系统样式，因此样式产品标签打印会有一条横线贯穿

// 构件-简约标签
const ARTIFACT_SIMPLE_L_HTML = function ({ component = defComponent, printConfig }) {
  return `
  <div class="artifact-label">
  <table class="artifact-table">
    <tr class="row-2">
      <td>
        <span style="font-size: 60px">${emptyTextFormatter(component.serialNumber)}</span>
      </td>
    </tr>
  </table>
  <table class="artifact-table">
      <tr>
        <td style="width:66.66%">
          <span style="${printConfig?.showSpecification ? '' : 'display:none;'}">
            规格：${emptyTextFormatter(component.specification)}
          </span>
        </td>
        <td rowspan="2" class="qr-content"></td>
      </tr>
      <tr>
        <td style="overflow: hidden">
          <div style="${printConfig?.dateInProduced ? '' : 'display:none;'}">
            生产日期：${emptyTextFormatter(component.printTime)}
          </div>
          <span style="${component?.oneCode ? '' : 'display:none;'}">
            编码：${component.oneCode}
          </span>
        </td>
      </tr>
    </table>
  </div>
  `
}

// 构件-定制标签
const ARTIFACT_CUSTOM_L_HTML = function ({ component = defComponent, printConfig, manufacturerName = '制造商名称', logo = labelLogo }) {
  return `
  <div class="artifact-label">
    <table class="artifact-table">
      <tr>
        <td style="width:50%;"><img src="${logo}" alt="logo" style="max-width: 96%;vertical-align: middle;display: block;margin: auto;" /></td>
        <td style="width:50%;font-size:10pt;">${emptyTextFormatter(manufacturerName)}</td>
      </tr>
    </table>
    <table class="artifact-table">
      <tr class="row-2">
        <td colspan="3" class="amplify-content">
          <span class="amplify-no">NO:</span>
          <span class="amplify-text">${emptyTextFormatter(component.serialNumber)}</span>
          <span class="amplify-date" style="${printConfig?.dateInProduced ? '' : 'display:none;'}">生产日期：${emptyTextFormatter(component.printTime)}</span>
        </td>
      </tr>
    </table>
    <table class="artifact-table">
      <tr>
        <td style="width:50%;font-size:10pt;">${emptyTextFormatter(component.projectName)}</td>
        <td style="width:50%;font-size:10pt;${printConfig?.showMonomer ? '' : 'display:none;'}">${emptyTextFormatter(component.monomerName)}</td>
      </tr>
    </table>
    <table class="artifact-table">
      <tr>
        <td colspan="${printConfig?.showArea && component?.oneCode ? 1 : 2}" style="${printConfig?.showArea ? '' : 'display:none;'}">区域：${emptyTextFormatter(component.areaName)}</td>
        <td colspan="${printConfig?.showArea && component?.oneCode ? 1 : 2}" style="${component?.oneCode ? '' : 'display:none;'}">编码：${component.oneCode}</td>
        <td rowspan="3" class="qr-content"></td>
      </tr>
      <tr>
        <td colspan="2" style="font-size:0;padding:0px;border-bottom:none;${!printConfig?.showArea && !component?.oneCode ? 'border-top:1px solid #000;' : ''}">
          <table class="artifact-table">
            <tr>
              <td style="${printConfig?.weight !== printWeightTypeEnum.NONE.V ? '' : 'width:50%;'}">任务数(件)：${emptyTextFormatter(component.quantity)}</td>
              <td style="${printConfig?.weight !== printWeightTypeEnum.NONE.V ? '' : 'width:50%;border-right:none;'}">长度(mm)：${emptyTextFormatter(component.length)}</td>
              <td style="${printConfig?.weight !== printWeightTypeEnum.NONE.V ? 'border-right:none;' : 'display:none;'}">单重(kg)：${emptyTextFormatter(component.weight)}</td>
            </tr>
          </table>
        </td>
      </tr>
      <tr>
        <td>${emptyTextFormatter(component.name)}</td>
        <td>${emptyTextFormatter(component.specification)}</td>
      </tr>
    </table>
  </div>
  `
}

// 构件标签样式
const ARTIFACT_STYLE = function ({
  fClass = '',
  colContent = 'left',
  rowHeight = 60,
  colPadding = 10,
  unit = 'px',
  qrPosition = {}
}) {
  return {
    fClass,
    qrPosition,
    style: `
    <style>
    .${fClass} .artifact-label {
      width: 100%;
      font-size: 0;
      border-top: 1px solid #000;
      border-left: 1px solid #000;
    }

    .${fClass} .artifact-table {
      font-family: "微软雅黑";
      font-size: 9pt;
      color: black;
      width: 100%;
      border-collapse:collapse;
      text-align: ${colContent};
      vertical-align:middle;
    }
    
    .${fClass} .artifact-label tr {
      width: 100%;
      height: ${rowHeight}${unit};
    }

    .${fClass} .artifact-label img{
      height: ${rowHeight * 0.7}${unit};
    }
    
    .${fClass} .artifact-label .row-2 {
      height: ${rowHeight * 2}${unit};
    }
    
    .${fClass} .artifact-label .row-3 {
      height: ${rowHeight * 3}${unit};
    }
  
    .${fClass} .artifact-label tr td{
      width: 33.333%;
      padding: 0 ${colPadding}${unit};
      word-break: break-all;
      border-right: 1px solid #000;
      border-bottom: 1px solid #000;
    }
  
    .${fClass} .artifact-label .amplify-content{
      position:relative;
    }
  
    .${fClass} .artifact-label .amplify-content .amplify-no{
      position: absolute; 
      top: 5px;
      font-size:7pt;
    }
  
    .${fClass} .artifact-label .amplify-content .amplify-text{
      font-size: 44px; 
      line-height: 36px;
      font-weight: 600; 
    }
  
    .${fClass} .artifact-label .amplify-content .amplify-date{
      position: absolute; 
      bottom: 5px; 
      right: 10px;
      font-size:7pt;
    }
    </style>
    `
  }
}

// 围护-常规标签
const ENCLOSURE_COMMON_L_HTML = function ({ component = defComponent, printConfig, manufacturerName = '制造商名称' }) {
  return `
  <div>
    <table class="enclosure-label">
      <tr>
        <td rowspan="6" class="qr-content">
        </td>
      </tr>
      <tr>
        <td class="row">品名：${emptyTextFormatter(component.name)}</td>
        <td class="row">编号：${emptyTextFormatter(component.serialNumber)}</td>
      </tr>
      <tr>
        <td class="row">厚度(mm)：${emptyTextFormatter(component.thickness)}</td>
        <td class="row">总张数：${emptyTextFormatter(component.quantity)}</td>
      </tr>
      <tr>
      <tr>
        <td class="row">单长(mm)：${emptyTextFormatter(component.length)}</td>
        <td class="row">板型：${emptyTextFormatter(component.plate)}</td>
      </tr>
      <tr>
        <td class="row">颜色：${emptyTextFormatter(component.color)}</td>
        <td class="row" style="${printConfig?.dateInProduced ? '' : 'display:none;'}">生产日期：${emptyTextFormatter(component.printTime)}</td>
      </tr>
    </table>
  </div>
`
}

// 围护-定制标签
const ENCLOSURE_CUSTOM_L_HTML = function ({ component = defComponent, printConfig, manufacturerName = '制造商名称', logo = labelLogo }) {
  return `
  <div class="enclosure-label">
    <table>
      <tr class="company">
        <td style="width:35%;text-align: center;">
         <img src="${logo}" alt="logo" style="height:70%;width:100%;vertical-align: middle;" />
        </td>
      <td style="width:63%; text-align:center;">${emptyTextFormatter(manufacturerName)}</td>
    </tr>
    </table>
    <table>
    <tr>
      <td class="row">品名：${emptyTextFormatter(component.name)}</td>
      <td class="row">编号：${emptyTextFormatter(component.serialNumber)}</td>
    </tr>
    <tr>
      <td class="row">厚度(mm)：${emptyTextFormatter(component.thickness)}</td>
      <td class="row">总张数：${emptyTextFormatter(component.quantity)}</td>
    </tr>
    <tr>
    <tr>
      <td class="row">单长(mm)：${emptyTextFormatter(component.length)}</td>
      <td class="row">板型：${emptyTextFormatter(component.plate)}</td>
    </tr>
    <tr>
      <td class="row">颜色：${emptyTextFormatter(component.color)}</td>
      <td class="row" style="${printConfig?.dateInProduced ? '' : 'display:none;'}">生产日期：${emptyTextFormatter(component.printTime)}</td>
    </tr>
    <tr>
      <td rowspan="8" class="qr-content">
        </td>
    </tr>
  </table>
  </div>
`
}

// 围护标签样式
const ENCLOSURE_STYLE = function ({
  fClass = '',
  qrPosition = {},
  rowHeight = 40,
  headerHeight = 180,
  padding = 15,
  border = 1,
  unit = 'px'
}) {
  return {
    fClass,
    qrPosition,
    style: `
    <style>
    .${fClass} .enclosure-label {
      font-family: "微软雅黑";
      font-size: 9pt;
      color: black;
      width: 100%;
      height: 100%;
      box-sizing: border-box;
      padding: 0 ${padding}${unit};
      border:${border}px solid #000;
      border-radius: 15px;
    }

    .${fClass} .company{
      width: 100%;
      font-size: 25pt;
    }

    .${fClass} .enclosure-label tr {
      width: 33.333%;
    }

    .${fClass} .enclosure-label tr td {
      width: 33.333%;
      height: 25%
    }

    .${fClass} .enclosure-label .row {
      height: ${rowHeight}${unit};
    } 
    </style>
    `
  }
}

// 配套件-常规标签
const AUX_MAT_COMMON_L_HTML = function ({ component = defComponent, printConfig }) {
  return `
  <div class="aux-mat-label">
    <div class="row">
      <div class="col col-center" style="font-size:10pt;">项目：${emptyTextFormatter(component.projectName)}</div>
    </div>
    <div class="row">
      <div class="col" style="font-size:10pt;">单体：${emptyTextFormatter(component.monomerName)}</div>
      <div class="col" style="font-size:10pt;">区域：${emptyTextFormatter(component.areaName)}</div>
    </div>
    <div class="contains-rows">
      <div class="col" style="flex: 2">
        <div class="row row-3">
          <div class="col" style="font-size:20pt;text-align:center;font-weight: 600;">${emptyTextFormatter(component.name)}</div>
        </div>
        <div class="row">
          <div class="col">规格：${emptyTextFormatter(component.specification)}</div>
          <div class="col">总数：${emptyTextFormatter(component.quantity)}</div>
        </div>
        <div class="row">
          <div class="col">单位：${emptyTextFormatter(component.unit)}</div>
          <div class="col">使用范围：${emptyTextFormatter(component.useProperty)}</div>
        </div>
      </div>
      <div class="col" style="flex: 1">
        <div class="row row-5">
          <div class="col qr-content">
          </div>
        </div>
      </div>
    </div>
  </div>
  `
}

// 配套件标签样式
const AUX_MAT_STYLE = function ({
  fClass = '',
  colContent = 'start',
  rowHeight = 60,
  colPadding = 10,
  unit = 'px',
  qrPosition = {}
}) {
  return {
    fClass,
    qrPosition,
    style: `
    <style>
    .${fClass} .aux-mat-label {
      font-family: "微软雅黑";
      font-size: 9pt;
      color: black;
      box-sizing: border-box;
      display: flex;
      flex-direction: column;
    }
    
    .${fClass} .aux-mat-label .row {
      display: flex;
      border: 1px solid #000;
      box-sizing: border-box;
      height: ${rowHeight}${unit};
    }
    
    .${fClass} .aux-mat-label .row-2 {
      height: ${rowHeight * 2}${unit};
    }
    
    .${fClass} .aux-mat-label .row-3 {
      height: ${rowHeight * 3}${unit};
    }
    
    .${fClass} .aux-mat-label .row-5 {
      height: ${rowHeight * 5}${unit};
    }
    
    .${fClass} .aux-mat-label .row:not(:last-child) {
      border-bottom: none;
    }
  
    .${fClass} .aux-mat-label .row > .col {
      height: 100%;
      padding: 0 ${colPadding}${unit};
      box-sizing: border-box;
      word-break: break-all;
      flex: 1;
      display: flex;
      align-items: center;
      justify-content: ${colContent};
    }
  
    .${fClass} .aux-mat-label .row > .col-center {
      justify-content: center;
    }
      
    .${fClass} .aux-mat-label .row > .col:not(:last-child) {
      border-right: 1px solid #000;
    }
    
    .${fClass} .aux-mat-label .contains-rows {
      display: flex;
      box-sizing: border-box;
      border-left: 1px solid #000;
    }
    
    .${fClass} .aux-mat-label .contains-rows .row {
      border-left: none;
    }
    .${fClass} .aux-mat-label .contains-rows .col {
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: stretch;
    }
    </style>
    `
  }
}

export const LABEL_HTML = {
  [componentTypeEnum.ARTIFACT.V]: {
    [labelTypeEnum.COMMON.V]: ARTIFACT_COMMON_L_HTML,
    [labelTypeEnum.SIMPLE.V]: ARTIFACT_SIMPLE_L_HTML,
    [labelTypeEnum.CUSTOM.V]: ARTIFACT_CUSTOM_L_HTML
  },
  [(componentTypeEnum.MACHINE_PART.V | componentTypeEnum.ASSEMBLE.V)]: {
    [labelTypeEnum.COMMON.V]: ARTIFACT_COMMON_L_HTML,
    [labelTypeEnum.SIMPLE.V]: ARTIFACT_SIMPLE_L_HTML,
    [labelTypeEnum.CUSTOM.V]: ARTIFACT_CUSTOM_L_HTML
  },
  [componentTypeEnum.ENCLOSURE.V]: {
    [labelTypeEnum.COMMON.V]: ENCLOSURE_COMMON_L_HTML,
    [labelTypeEnum.SIMPLE.V]: '',
    [labelTypeEnum.CUSTOM.V]: ENCLOSURE_CUSTOM_L_HTML
  },
  [componentTypeEnum.AUXILIARY_MATERIAL.V]: {
    [labelTypeEnum.COMMON.V]: AUX_MAT_COMMON_L_HTML,
    [labelTypeEnum.SIMPLE.V]: '',
    [labelTypeEnum.CUSTOM.V]: ''
  }
}

// 产品标签预览样式
export const PRE_LABEL_STYLE = {
  [componentTypeEnum.ARTIFACT.V]: {
    [labelTypeEnum.COMMON.V]: ARTIFACT_STYLE({
      fClass: 'pre-com-al',
      qrPosition: { right: '18px', bottom: '8px', size: 160 },
      rowHeight: 60
    }),
    [labelTypeEnum.SIMPLE.V]: ARTIFACT_STYLE({
      fClass: 'pre-sim-al',
      qrPosition: { right: '18px', bottom: '25px', size: 160 },
      // rowHeight: 210,
      rowHeight: 100,
      colContent: 'center'
    }),
    [labelTypeEnum.CUSTOM.V]: ARTIFACT_STYLE({
      fClass: 'pre-cus-al',
      qrPosition: { right: '18px', bottom: '8px', size: 160 },
      rowHeight: 60
    })
  },
  [(componentTypeEnum.MACHINE_PART.V | componentTypeEnum.ASSEMBLE.V)]: {
    [labelTypeEnum.COMMON.V]: ARTIFACT_STYLE({
      fClass: 'pre-com-al',
      qrPosition: { right: '18px', bottom: '8px', size: 160 },
      rowHeight: 60
    }),
    [labelTypeEnum.SIMPLE.V]: ARTIFACT_STYLE({
      fClass: 'pre-sim-al',
      qrPosition: { right: '18px', bottom: '25px', size: 160 },
      // rowHeight: 210,
      rowHeight: 100,
      colContent: 'center'
    }),
    [labelTypeEnum.CUSTOM.V]: ARTIFACT_STYLE({
      fClass: 'pre-cus-al',
      qrPosition: { right: '18px', bottom: '8px', size: 160 },
      rowHeight: 60
    })
  },
  [componentTypeEnum.ENCLOSURE.V]: {
    [labelTypeEnum.COMMON.V]: ENCLOSURE_STYLE({
      fClass: 'pre-com-al',
      qrPosition: { top: '17px', left: '15px', size: 130 },
      rowHeight: 40
    }),
    [labelTypeEnum.SIMPLE.V]: '',
    [labelTypeEnum.CUSTOM.V]: ENCLOSURE_STYLE({
      fClass: 'pre-cus-al',
      qrPosition: { top: '75px', left: '', right: '15px', size: 130 },
      rowHeight: 40
    })
  },
  [componentTypeEnum.AUXILIARY_MATERIAL.V]: {
    [labelTypeEnum.COMMON.V]: AUX_MAT_STYLE({
      fClass: 'pre-com-al',
      qrPosition: { right: '20px', bottom: '65px', size: 160 },
      rowHeight: 60
    })
  }
}

// 产品标签打印配置预览样式
export const MINI_LABEL_STYLE = {
  [componentTypeEnum.ARTIFACT.V]: {
    [labelTypeEnum.COMMON.V]: ARTIFACT_STYLE({
      fClass: 'mini-com-al',
      rowHeight: 40
    }),
    [labelTypeEnum.SIMPLE.V]: ARTIFACT_STYLE({
      fClass: 'mini-sim-al',
      // rowHeight: 140,
      rowHeight: 50,
      colContent: 'center'
    }),
    [labelTypeEnum.CUSTOM.V]: ARTIFACT_STYLE({
      fClass: 'mini-cus-al',
      rowHeight: 40
    })
  },
  [(componentTypeEnum.MACHINE_PART.V | componentTypeEnum.ASSEMBLE.V)]: {
    [labelTypeEnum.COMMON.V]: ARTIFACT_STYLE({
      fClass: 'mini-com-al',
      rowHeight: 40
    }),
    [labelTypeEnum.SIMPLE.V]: ARTIFACT_STYLE({
      fClass: 'mini-sim-al',
      // rowHeight: 140,
      rowHeight: 50,
      colContent: 'center'
    }),
    [labelTypeEnum.CUSTOM.V]: ARTIFACT_STYLE({
      fClass: 'mini-cus-al',
      rowHeight: 40
    })
  },
  [componentTypeEnum.ENCLOSURE.V]: {
    [labelTypeEnum.COMMON.V]: ENCLOSURE_STYLE({
      fClass: 'mini-com-al',
      rowHeight: 25
    }),
    [labelTypeEnum.SIMPLE.V]: '',
    [labelTypeEnum.CUSTOM.V]: ENCLOSURE_STYLE({
      fClass: 'mini-cus-al',
      headerHeight: 25,
      rowHeight: 25
    })
  }
}

// 产品标签打印样式
export const PRINT_LABEL_STYLE = {
  [componentTypeEnum.ARTIFACT.V]: {
    [labelTypeEnum.COMMON.V]: ARTIFACT_STYLE({
      fClass: 'print-com-al',
      rowHeight: 9,
      colPadding: 1,
      unit: 'mm'
    }),
    [labelTypeEnum.SIMPLE.V]: ARTIFACT_STYLE({
      fClass: 'print-sim-al',
      // rowHeight: 32,
      rowHeight: 17,
      colPadding: 1,
      colContent: 'center',
      unit: 'mm'
    }),
    [labelTypeEnum.CUSTOM.V]: ARTIFACT_STYLE({
      fClass: 'print-cus-al',
      rowHeight: 9,
      colPadding: 1,
      unit: 'mm'
    })
  },
  [(componentTypeEnum.MACHINE_PART.V | componentTypeEnum.ASSEMBLE.V)]: {
    [labelTypeEnum.COMMON.V]: ARTIFACT_STYLE({
      fClass: 'print-com-al',
      rowHeight: 9,
      colPadding: 1,
      unit: 'mm'
    }),
    [labelTypeEnum.SIMPLE.V]: ARTIFACT_STYLE({
      fClass: 'print-sim-al',
      // rowHeight: 32,
      rowHeight: 17,
      colPadding: 1,
      colContent: 'center',
      unit: 'mm'
    }),
    [labelTypeEnum.CUSTOM.V]: ARTIFACT_STYLE({
      fClass: 'print-cus-al',
      rowHeight: 9,
      colPadding: 1,
      unit: 'mm'
    })
  },
  [componentTypeEnum.ENCLOSURE.V]: {
    [labelTypeEnum.COMMON.V]: ENCLOSURE_STYLE({
      fClass: 'print-com-al',
      rowHeight: 7,
      padding: 1,
      border: 0,
      unit: 'mm'
    }),
    [labelTypeEnum.SIMPLE.V]: '',
    [labelTypeEnum.CUSTOM.V]: ENCLOSURE_STYLE({
      fClass: 'print-cus-al',
      headerHeight: 14,
      padding: 1,
      rowHeight: 8,
      border: 0,
      unit: 'mm'
    })
  },
  [componentTypeEnum.AUXILIARY_MATERIAL.V]: {
    [labelTypeEnum.COMMON.V]: AUX_MAT_STYLE({
      fClass: 'print-com-al',
      rowHeight: 9,
      colPadding: 1,
      unit: 'mm'
    }),
    [labelTypeEnum.SIMPLE.V]: '',
    [labelTypeEnum.CUSTOM.V]: ''
  }
}
