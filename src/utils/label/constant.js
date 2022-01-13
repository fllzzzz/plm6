import {
  labelTypeEnum,
  componentTypeEnum
} from '@enum-ms/mes'
import { parseTime } from '@/utils/date'

const defComponent = {
  projectName: '项目名称',
  printTime: parseTime((new Date()).getTime(), '{y}/{m}/{d}'),
  monomerName: '单体名称',
  serialNumber: '编号',
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
const ARTIFACT_COMMON_L_HTML = function ({ component = defComponent, printConfig, manufacturerName = 'XX' }) {
  return `
<div class="artifact-label">
<div class="row">
  <div class="col" style="font-size:10pt;${printConfig?.showMonomer ? '' : 'border:none;'}">${component.projectName}</div>
  <div class="col" style="font-size:10pt;${printConfig?.showMonomer ? '' : 'display:none;'}">${component.monomerName}</div>
</div>
<div class="row row-2">
  <div class="col amplify-content">
    <span class="amplify-no">NO:</span>
    <span class="amplify-text">${component.serialNumber}</span>
    <span class="amplify-date">生产日期：${component.printTime}</span>
  </div>
</div>
<div class="row">
  <div class="col">名称：${component.name}</div>
  <div class="col">数量(件)：${component.quantity}</div>
  <div class="col">单重(kg)：${component.weight}</div>
</div>
<div class="contains-rows">
  <div class="col" style="flex: 2">
    <div class="row">
      <div class="col">长度(mm)：${component.length}</div>
      <div class="col">规格：${component.specification}</div>
    </div>
    <div class="row">
      <div class="col" style="${printConfig?.showArea ? '' : 'display:none;'}">区域：${component.areaName}</div>
    </div>
    <div class="row">
      <div class="col">${manufacturerName}</div>
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

// 构件-简约标签
const ARTIFACT_SIMPLE_L_HTML = function ({ component = defComponent }) {
  return `
<div class="artifact-label">
<div class="row">
  <div class="col">
    <span style="font-size: 60px; font-weight: 600;">${component.serialNumber}</span>
  </div>
</div>
<div class="row">
  <div class="col" style="flex:2;" style="font-size: 60px;">
    生产日期：${component.printTime}
  </div>
  <div class="col qr-content" style="flex:1;">
  </div>
</div>
</div>
`
}

// 构件-定制标签
const ARTIFACT_CUSTOM_L_HTML = function ({ component = defComponent, printConfig, manufacturerName = 'XX', logo = 'https://mes.dev.hzchum.com/files/logo/1642044373620_logo_cmib_black_512.png' }) {
  return `
  <div class="artifact-label">
  <div class="row">
    <div class="col" style="justify-content:center;">
      <img src="${logo}" alt="logo" style="height:70%;vertical-align: middle;">
    </div>
    <div class="col" style="font-size:10pt;">${manufacturerName}</div>
  </div>
  <div class="row row-2">
    <div class="col amplify-content">
      <span class="amplify-no">NO:</span>
      <span class="amplify-text">${component.serialNumber}</span>
      <span class="amplify-date">生产日期：${component.printTime}</span>
    </div>
  </div>
  <div class="row">
    <div class="col" style="${printConfig?.showMonomer ? '' : 'border:none;'}">${component.projectName}</div>
    <div class="col" style="${printConfig?.showMonomer ? '' : 'display:none;'}">${component.monomerName}</div>
  </div>
  <div class="contains-rows">
    <div class="col" style="flex: 2">
      <div class="row">
        <div class="col" style="${printConfig?.showArea ? '' : 'display:none;'}">区域：${component.areaName}</div>
      </div>
      <div class="row">
        <div class="col">数量(件)：${component.quantity}</div>
        <div class="col">长度(mm)：${component.length}</div>
        <div class="col">单重(kg)：${component.weight}</div>
      </div>
      <div class="row">
        <div class="col">${component.name}</div>
        <div class="col">${component.specification}</div>
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

// 构件标签样式
const ARTIFACT_STYLE = function ({
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
    .${fClass} .artifact-label {
      font-family: "微软雅黑";
      font-size: 9pt;
      color: black;
      box-sizing: border-box;
      display: flex;
      flex-direction: column;
    }
    
    .${fClass} .artifact-label .row {
      display: flex;
      border: 1px solid #000;
      box-sizing: border-box;
      height: ${rowHeight}${unit};
    }
    
    .${fClass} .artifact-label .row-2 {
      height: ${rowHeight * 2}${unit};
    }
    
    .${fClass} .artifact-label .row-3 {
      height: ${rowHeight * 3}${unit};
    }
    
    .${fClass} .artifact-label .row:not(:last-child) {
      border-bottom: none;
    }
  
    .${fClass} .artifact-label .row > .col {
      height: 100%;
      padding: 0 ${colPadding}${unit};
      box-sizing: border-box;
      word-break: break-all;
      flex: 1;
      display: flex;
      align-items: center;
      justify-content: ${colContent};
    }
      
    .${fClass} .artifact-label .row > .col:not(:last-child) {
      border-right: 1px solid #000;
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
      font-size: 54px; 
      font-weight: 600; 
      margin-left: 40px;
    }
  
    .${fClass} .artifact-label .amplify-content .amplify-date{
      position: absolute; 
      bottom: 5px; 
      right: 10px;
      font-size:7pt;
    }
    
    .${fClass} .artifact-label .contains-rows {
      display: flex;
      box-sizing: border-box;
      border-left: 1px solid #000;
    }
    
    .${fClass} .artifact-label .contains-rows .row {
      border-left: none;
    }
    .${fClass} .artifact-label .contains-rows .col {
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: stretch;
    }
    </style>
    `
  }
}

// 围护-常规标签
const ENCLOSURE_COMMON_L_HTML = function ({ component = defComponent, printConfig, manufacturerName = 'XX' }) {
  console.log('ENCLOSURE_COMMON_L_HTML')
  return `
  <div class="enclosure-label">
    <div class="content">
      <div class="flex-1">
        <div class="qr-content">
        </div>
      </div>
      <div class="flex-2">
        <div class="row">品名：${component.name}</div>
        <div class="row">厚度(mm)：${component.thickness}</div>
        <div class="row">单长(mm)：${component.length}</div>
        <div class="row">颜色：${component.color}</div>
      </div>
      <div class="flex-2">
        <div class="row">编号：${component.serialNumber}</div>
        <div class="row">总张数：${component.quantity}</div>
        <div class="row">板型：${component.plate}</div>
        <div class="row">生产日期：${component.printTime}</div>
      </div>
    </div>
  </div>
`
}

// 围护-定制标签
const ENCLOSURE_CUSTOM_L_HTML = function ({ component = defComponent, printConfig, manufacturerName = 'XX' }) {
  return `
  <div class="enclosure-label">
    <div class="company">
      <div>LOGO</div>
      <div style="flex:1;text-align:center;">${manufacturerName}</div>
    </div>
    <div class="content">
      <div class="flex-2">
        <div class="row">品名：${component.name}</div>
        <div class="row">厚度(mm)：${component.thickness}</div>
        <div class="row">单长(mm)：${component.length}</div>
        <div class="row">颜色：${component.color}</div>
      </div>
      <div class="flex-2">
        <div class="row">编号：${component.serialNumber}</div>
        <div class="row">总张数：${component.quantity}</div>
        <div class="row">板型：${component.plate}</div>
        <div class="row">生产日期：${component.printTime}</div>
      </div>
      <div class="flex-1">
        <div class="qr-content">
        </div>
      </div>
    </div>
  </div>
`
}

// 围护标签样式
const ENCLOSURE_STYLE = function ({
  fClass = '',
  qrPosition = {},
  rowHeight = 40,
  headerHeight = 60,
  padding = 15,
  border = 1,
  unit = 'px'
}) {
  return {
    fClass,
    qrPosition,
    style: `
    <style>
    .${fClass} .company{
      display:flex;
      align-items: center;
      height: ${headerHeight}${unit};
      font-size: 25pt;
    }

    .${fClass} .enclosure-label {
      font-family: "微软雅黑";
      font-size: 9pt;
      color: black;
      padding:0 ${padding}${unit};
      box-sizing: border-box;
      border:${border}px solid #000;
      border-radius: 15px;
    }

    .${fClass} .enclosure-label .content {
      display: flex;
      justify-content: space-around;
    }

    .${fClass} .enclosure-label .row {
      display: flex;
      height: ${rowHeight}${unit};
      align-items: center;
    }
    
    .${fClass} .enclosure-label .flex-1{
      width:25%;
    }
    
    .${fClass} .enclosure-label .flex-2{
      width: 35%;
      display: flex;
      flex-direction: column;
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
  [componentTypeEnum.ENCLOSURE.V]: {
    [labelTypeEnum.COMMON.V]: ENCLOSURE_COMMON_L_HTML,
    [labelTypeEnum.SIMPLE.V]: '',
    [labelTypeEnum.CUSTOM.V]: ENCLOSURE_CUSTOM_L_HTML
  }
}

export const PRE_LABEL_STYLE = {
  [componentTypeEnum.ARTIFACT.V]: {
    [labelTypeEnum.COMMON.V]: ARTIFACT_STYLE({
      fClass: 'pre-com-al',
      qrPosition: { right: '18px', bottom: '8px', size: 160 },
      rowHeight: 60
    }),
    [labelTypeEnum.SIMPLE.V]: ARTIFACT_STYLE({
      fClass: 'pre-sim-al',
      qrPosition: { right: '18px', bottom: '8px', size: 160 },
      rowHeight: 210,
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
  }
}

export const MINI_LABEL_STYLE = {
  [componentTypeEnum.ARTIFACT.V]: {
    [labelTypeEnum.COMMON.V]: ARTIFACT_STYLE({
      fClass: 'mini-com-al',
      rowHeight: 40
    }),
    [labelTypeEnum.SIMPLE.V]: ARTIFACT_STYLE({
      fClass: 'mini-sim-al',
      rowHeight: 140,
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
      headerHeight: 30,
      rowHeight: 25
    })
  }
}

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
      rowHeight: 32,
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
  }
}
