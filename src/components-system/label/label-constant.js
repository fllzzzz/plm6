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
  specification: 'XX',
  areaName: 'XX'
}

const ARTIFACT_COMMON_L_HTML = function ({ component = defComponent, manufacturerName = 'XX', productionLineName }) {
  return `
<div class="artifact-label">
<div class="row">
  <div class="col">${component.projectName}</div>
  <div class="col">${component.monomerName}</div>
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
      <div class="col">区域：${component.areaName}</div>
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

const ARTIFACT_CUSTOM_L_HTML = function ({ component = defComponent, manufacturerName = 'XX', productionLineName }) {
  return `
  <div class="artifact-label">
  <div class="row">
    <div class="col">LOGO</div>
    <div class="col">${manufacturerName}</div>
  </div>
  <div class="row row-2">
    <div class="col amplify-content">
      <span class="amplify-no">NO:</span>
      <span class="amplify-text">${component.serialNumber}</span>
      <span class="amplify-date">生产日期：${component.printTime}</span>
    </div>
  </div>
  <div class="row">
    <div class="col">${component.projectName}</div>
    <div class="col">${component.monomerName}</div>
  </div>
  <div class="contains-rows">
    <div class="col" style="flex: 2">
      <div class="row">
        <div class="col">区域：${component.areaName}</div>
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

// 构件标签
const ARTIFACT_STYLE = function ({
  fClass = '',
  colContent = 'start',
  rowHeight = 60,
  colPadding = 10,
  unit = 'px'
}) {
  return {
    fClass,
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
      top: 10px;
    }
  
    .${fClass} .artifact-label .amplify-content .amplify-text{
      font-size: 54px; 
      font-weight: 600; 
      margin-left: 40px;
    }
  
    .${fClass} .artifact-label .amplify-content .amplify-date{
      position: absolute; 
      bottom: 10px; 
      right: 10px;
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
      align-items: normal;
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
    [labelTypeEnum.COMMON.V]: ARTIFACT_COMMON_L_HTML,
    [labelTypeEnum.SIMPLE.V]: ARTIFACT_SIMPLE_L_HTML,
    [labelTypeEnum.CUSTOM.V]: ARTIFACT_CUSTOM_L_HTML
  }
}

export const PRE_LABEL_STYLE = {
  [componentTypeEnum.ARTIFACT.V]: {
    [labelTypeEnum.COMMON.V]: ARTIFACT_STYLE({
      fClass: 'pre-com-al',
      rowHeight: 60
    }),
    [labelTypeEnum.SIMPLE.V]: ARTIFACT_STYLE({
      fClass: 'pre-sim-al',
      rowHeight: 210,
      colContent: 'center'
    }),
    [labelTypeEnum.CUSTOM.V]: ARTIFACT_STYLE({
      fClass: 'pre-cus-al',
      rowHeight: 60
    })
  },
  [componentTypeEnum.ENCLOSURE.V]: {
    [labelTypeEnum.COMMON.V]: ARTIFACT_STYLE({
      fClass: 'pre-com-al',
      rowHeight: 60
    }),
    [labelTypeEnum.SIMPLE.V]: '',
    [labelTypeEnum.CUSTOM.V]: ARTIFACT_STYLE({
      fClass: 'pre-cus-al',
      rowHeight: 60
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
    [labelTypeEnum.COMMON.V]: ARTIFACT_STYLE({
      fClass: 'mini-com-al',
      rowHeight: 40
    }),
    [labelTypeEnum.SIMPLE.V]: '',
    [labelTypeEnum.CUSTOM.V]: ARTIFACT_STYLE({
      fClass: 'mini-cus-al',
      rowHeight: 40
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
    [labelTypeEnum.COMMON.V]: ARTIFACT_STYLE({
      fClass: 'print-com-al',
      rowHeight: 9,
      colPadding: 1,
      unit: 'mm'
    }),
    [labelTypeEnum.SIMPLE.V]: '',
    [labelTypeEnum.CUSTOM.V]: ARTIFACT_STYLE({
      fClass: 'print-cus-al',
      rowHeight: 9,
      colPadding: 1,
      unit: 'mm'
    })
  }
}
