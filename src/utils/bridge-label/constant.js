import { bridgeLabelTypeEnum, bridgeComponentTypeEnum } from '@enum-ms/bridge'
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

// 分段-常规标签
const BOX_COMMON_L_HTML = function ({ component = defComponent, productionLineName, logo, manufacturerPhone, manufacturerURL, printConfig, manufacturerName = '制造商名称' }) {
  return `
<div class="box-label">
<div class="row row-3">
  <div class="col" style="flex: 2;border:none;">
    <img src="${logo}" style="height:70%;max-width: 96%;vertical-align: middle;">
  </div>
  <div class="col qr-content" style="flex: 1">
  </div>
</div>
<div class="row">
  <div class="col" style="font-size:10pt;">项目：${emptyTextFormatter(component.projectName)}</div>
</div>
<div class="row">
  <div class="col" style="font-size:10pt;${printConfig?.showMonomer ? '' : 'display:none;'}">区域：${emptyTextFormatter(component.monomerName)}-${emptyTextFormatter(component.areaName)}</div>
</div>
<div class="row">
  <div class="col" style="font-size:16pt;">名称：${emptyTextFormatter(component.name)}</div>
</div>
<div class="row">
  <div class="col" style="font-size:16pt;">编号：${emptyTextFormatter(component.serialNumber)}</div>
</div>
<div class="row" style="font-weight:bold;">
  <div class="col">长度(mm)：${emptyTextFormatter(component.length)}</div>
  <div class="col">宽度(mm)：${emptyTextFormatter(component.width)}</div>
  <div class="col">高度(mm)：${emptyTextFormatter(component.height)}</div>
</div>
<div class="row" style="${(printConfig?.weight !== printWeightTypeEnum.NONE.V || component?.oneCode) ? '' : 'display:none;'}">
 <div class="col" style="${printConfig?.weight !== printWeightTypeEnum.NONE.V ? '' : 'display:none;'}">单重(kg)：${emptyTextFormatter(component.weight)}</div>
 <div class="col" style="${component?.oneCode ? '' : 'display:none;'}">编码：${component.oneCode}</div>
</div>
<div class="row" style="${(printConfig?.showProductionLine || printConfig?.dateInProduced) ? '' : 'display:none;'}">
  <div class="col" style="${printConfig?.dateInProduced ? '' : 'display:none;'}">生产日期：${emptyTextFormatter(component.printTime)}</div>
  <div class="col" style="${printConfig?.showProductionLine ? '' : 'display:none;'}">生产线：${emptyTextFormatter(productionLineName)}</div>
</div>
<div class="row">
  <div class="col">制造商：${emptyTextFormatter(manufacturerName)}</div>
</div>
<div class="row">
  <div class="col" style="display:flex;">
    <span style="flex:1">${emptyTextFormatter(manufacturerURL)}</span>
    <span style="flex:1">${emptyTextFormatter(manufacturerPhone)}</span>
 </div>
</div>
</div>
`
}

// 分段-简约标签
const BOX_SIMPLE_L_HTML = function ({ component = defComponent, printConfig }) {
  return `
<div class="box-label">
<div class="row">
  <div class="col">
    <span style="font-size: 60px; font-weight: 600;">${emptyTextFormatter(component.serialNumber)}</span>
  </div>
</div>
<div class="row">
  <div class="col" style="flex:2;flex-direction:column;">
    <span style="${printConfig?.dateInProduced ? '' : 'display:none;'}">
      生产日期：${emptyTextFormatter(component.printTime)}
    </span>
    <span style="${component?.oneCode ? '' : 'display:none;'}">
      编码：${component.oneCode}
    </span>
  </div>
  <div class="col qr-content" style="flex:1;">
  </div>
</div>
</div>
`
}

// 分段-定制标签
const BOX_CUSTOM_L_HTML = function ({ component = defComponent, printConfig, manufacturerName = '制造商名称', logo = labelLogo }) {
  return `
  <div class="box-label">
  <div class="row">
    <div class="col" style="justify-content:center;">
      <img src="${logo}" alt="logo" style="height:70%;max-width: 96%;vertical-align: middle;">
    </div>
    <div class="col" style="font-size:10pt;">${emptyTextFormatter(manufacturerName)}</div>
  </div>
  <div class="row row-2">
    <div class="col amplify-content">
      <span class="amplify-no">NO:</span>
      <span class="amplify-text">${emptyTextFormatter(component.serialNumber)}</span>
      <span class="amplify-date" style="${printConfig?.dateInProduced ? '' : 'display:none;'}">生产日期：${emptyTextFormatter(component.printTime)}</span>
    </div>
  </div>
  <div class="row">
    <div class="col" style="${printConfig?.showMonomer ? '' : 'border:none;'}">${emptyTextFormatter(component.projectName)}</div>
    <div class="col" style="${printConfig?.showMonomer ? '' : 'display:none;'}">${emptyTextFormatter(component.monomerName)}</div>
  </div>
  <div class="contains-rows">
    <div class="col" style="flex: 2">
      <div class="row">
        <div class="col" style="${printConfig?.showArea ? '' : 'display:none;'}">区域：${emptyTextFormatter(component.areaName)}</div>
        <div class="col" style="${component?.oneCode ? '' : 'display:none;'}">编码：${component.oneCode}</div>
      </div>
      <div class="row">
        <div class="col">任务数(件)：${emptyTextFormatter(component.quantity)}</div>
        <div class="col">长度(mm)：${emptyTextFormatter(component.length)}</div>
        <div class="col" style="${printConfig?.weight !== printWeightTypeEnum.NONE.V ? '' : 'display:none;'}">单重(kg)：${emptyTextFormatter(component.weight)}</div>
      </div>
      <div class="row">
        <div class="col">${emptyTextFormatter(component.name)}</div>
        <div class="col">${emptyTextFormatter(component.specification)}</div>
      </div>
    </div>
    <div class="col" style="flex: 1">
      <div class="row row-3">
        <div class="col qr-content">
        </div>
      </div>
    </div>
  </div>
  </div>
  `
}

// 分段标签样式
const BOX_STYLE = function ({
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
    .${fClass} .box-label {
      font-family: "微软雅黑";
      font-size: 9pt;
      color: black;
      box-sizing: border-box;
      display: flex;
      flex-direction: column;
    }
    
    .${fClass} .box-label .row {
      display: flex;
      border: 1px solid #000;
      box-sizing: border-box;
      height: ${rowHeight}${unit};
    }
    
    .${fClass} .box-label .row-2 {
      height: ${rowHeight * 2}${unit};
    }
    
    .${fClass} .box-label .row-3 {
      height: ${rowHeight * 3}${unit};
    }
    
    .${fClass} .box-label .row:not(:last-child) {
      border-bottom: none;
    }
  
    .${fClass} .box-label .row > .col {
      height: 100%;
      padding: 0 ${colPadding}${unit};
      box-sizing: border-box;
      word-break: break-all;
      flex: 1;
      display: flex;
      align-items: center;
      justify-content: ${colContent};
    }
      
    .${fClass} .box-label .row > .col:not(:last-child) {
      border-right: 1px solid #000;
    }
  
    .${fClass} .box-label .amplify-content{
      position:relative;
    }
  
    .${fClass} .box-label .amplify-content .amplify-no{
      position: absolute; 
      top: 5px;
      font-size:7pt;
    }
  
    .${fClass} .box-label .amplify-content .amplify-text{
      font-size: 40px; 
      line-height: 36px;
      font-weight: 600; 
    }
  
    .${fClass} .box-label .amplify-content .amplify-date{
      position: absolute; 
      bottom: 5px; 
      right: 10px;
      font-size:7pt;
    }
    
    .${fClass} .box-label .contains-rows {
      display: flex;
      box-sizing: border-box;
      border-left: 1px solid #000;
    }
    
    .${fClass} .box-label .contains-rows .row {
      border-left: none;
    }
    .${fClass} .box-label .contains-rows .col {
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: stretch;
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
      <div class="col col-center" style="font-size:10pt;">${emptyTextFormatter(component.projectName)}</div>
    </div>
    <div class="row">
      <div class="col col-center" style="font-size:10pt;">${emptyTextFormatter(component.monomerName)}</div>
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
          <div class="col">颜色：${emptyTextFormatter(component.color)}</div>
          <div class="col">品牌：${emptyTextFormatter(component.brand)}</div>
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
  [bridgeComponentTypeEnum.BOX.V]: {
    [bridgeLabelTypeEnum.COMMON.V]: BOX_COMMON_L_HTML,
    [bridgeLabelTypeEnum.SIMPLE.V]: BOX_SIMPLE_L_HTML,
    [bridgeLabelTypeEnum.CUSTOM.V]: BOX_CUSTOM_L_HTML
  },
  [bridgeComponentTypeEnum.MACHINE_PART.V]: {
    [bridgeLabelTypeEnum.COMMON.V]: BOX_COMMON_L_HTML,
    [bridgeLabelTypeEnum.SIMPLE.V]: BOX_SIMPLE_L_HTML,
    [bridgeLabelTypeEnum.CUSTOM.V]: BOX_CUSTOM_L_HTML
  },
  [bridgeComponentTypeEnum.AUXILIARY_MATERIAL.V]: {
    [bridgeLabelTypeEnum.COMMON.V]: AUX_MAT_COMMON_L_HTML,
    [bridgeLabelTypeEnum.SIMPLE.V]: '',
    [bridgeLabelTypeEnum.CUSTOM.V]: ''
  }
}

// 产品标签预览样式
export const PRE_LABEL_STYLE = {
  [bridgeComponentTypeEnum.BOX.V]: {
    [bridgeLabelTypeEnum.COMMON.V]: BOX_STYLE({
      fClass: 'pre-com-al',
      qrPosition: { right: '18px', top: '6px', size: 160 },
      rowHeight: 60
    }),
    [bridgeLabelTypeEnum.SIMPLE.V]: BOX_STYLE({
      fClass: 'pre-sim-al',
      qrPosition: { right: '18px', bottom: '8px', size: 160 },
      rowHeight: 210,
      colContent: 'center'
    }),
    [bridgeLabelTypeEnum.CUSTOM.V]: BOX_STYLE({
      fClass: 'pre-cus-al',
      qrPosition: { right: '18px', bottom: '8px', size: 160 },
      rowHeight: 60
    })
  },
  [bridgeComponentTypeEnum.MACHINE_PART.V]: {
    [bridgeLabelTypeEnum.COMMON.V]: BOX_STYLE({
      fClass: 'pre-com-al',
      qrPosition: { right: '18px', bottom: '8px', size: 160 },
      rowHeight: 60
    }),
    [bridgeLabelTypeEnum.SIMPLE.V]: BOX_STYLE({
      fClass: 'pre-sim-al',
      qrPosition: { right: '18px', bottom: '8px', size: 160 },
      rowHeight: 210,
      colContent: 'center'
    }),
    [bridgeLabelTypeEnum.CUSTOM.V]: BOX_STYLE({
      fClass: 'pre-cus-al',
      qrPosition: { right: '18px', bottom: '8px', size: 160 },
      rowHeight: 60
    })
  },
  [bridgeComponentTypeEnum.AUXILIARY_MATERIAL.V]: {
    [bridgeLabelTypeEnum.COMMON.V]: AUX_MAT_STYLE({
      fClass: 'pre-com-al',
      qrPosition: { right: '20px', bottom: '65px', size: 160 },
      rowHeight: 60
    })
  }
}

// 产品标签打印配置预览样式
export const MINI_LABEL_STYLE = {
  [bridgeComponentTypeEnum.BOX.V]: {
    [bridgeLabelTypeEnum.COMMON.V]: BOX_STYLE({
      fClass: 'mini-com-al',
      rowHeight: 40
    }),
    [bridgeLabelTypeEnum.SIMPLE.V]: BOX_STYLE({
      fClass: 'mini-sim-al',
      rowHeight: 140,
      colContent: 'center'
    }),
    [bridgeLabelTypeEnum.CUSTOM.V]: BOX_STYLE({
      fClass: 'mini-cus-al',
      rowHeight: 40
    })
  },
  [bridgeComponentTypeEnum.MACHINE_PART.V]: {
    [bridgeLabelTypeEnum.COMMON.V]: BOX_STYLE({
      fClass: 'mini-com-al',
      rowHeight: 40
    }),
    [bridgeLabelTypeEnum.SIMPLE.V]: BOX_STYLE({
      fClass: 'mini-sim-al',
      rowHeight: 140,
      colContent: 'center'
    }),
    [bridgeLabelTypeEnum.CUSTOM.V]: BOX_STYLE({
      fClass: 'mini-cus-al',
      rowHeight: 40
    })
  }
}

// 产品标签打印样式
export const PRINT_LABEL_STYLE = {
  [bridgeComponentTypeEnum.BOX.V]: {
    [bridgeLabelTypeEnum.COMMON.V]: BOX_STYLE({
      fClass: 'print-com-al',
      rowHeight: 9,
      colPadding: 1,
      unit: 'mm'
    }),
    [bridgeLabelTypeEnum.SIMPLE.V]: BOX_STYLE({
      fClass: 'print-sim-al',
      rowHeight: 32,
      colPadding: 1,
      colContent: 'center',
      unit: 'mm'
    }),
    [bridgeLabelTypeEnum.CUSTOM.V]: BOX_STYLE({
      fClass: 'print-cus-al',
      rowHeight: 9,
      colPadding: 1,
      unit: 'mm'
    })
  },
  [bridgeComponentTypeEnum.MACHINE_PART.V]: {
    [bridgeLabelTypeEnum.COMMON.V]: BOX_STYLE({
      fClass: 'print-com-al',
      rowHeight: 9,
      colPadding: 1,
      unit: 'mm'
    }),
    [bridgeLabelTypeEnum.SIMPLE.V]: BOX_STYLE({
      fClass: 'print-sim-al',
      rowHeight: 32,
      colPadding: 1,
      colContent: 'center',
      unit: 'mm'
    }),
    [bridgeLabelTypeEnum.CUSTOM.V]: BOX_STYLE({
      fClass: 'print-cus-al',
      rowHeight: 9,
      colPadding: 1,
      unit: 'mm'
    })
  },
  [bridgeComponentTypeEnum.AUXILIARY_MATERIAL.V]: {
    [bridgeLabelTypeEnum.COMMON.V]: AUX_MAT_STYLE({
      fClass: 'print-com-al',
      rowHeight: 9,
      colPadding: 1,
      unit: 'mm'
    }),
    [bridgeLabelTypeEnum.SIMPLE.V]: '',
    [bridgeLabelTypeEnum.CUSTOM.V]: ''
  }
}
