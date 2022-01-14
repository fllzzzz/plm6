<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelDetail"
    :visible="crud.detailVisible"
    :title="crud.detailTitle"
    custom-class="print-template-detail"
    :show-close="true"
    width="100%"
    fullscreen
  >
    <template #title>
      <div class="dialog-title">
        <span>打印预览</span>
        <div>
          <el-select v-model="contentZoom" placeholder="显示比例" size="small" class="zoom-select">
            <el-option
              v-for="item in zoomArr"
              :key="item"
              :label="`${item}%`"
              :value="item"
            />
          </el-select>
          <el-select v-model="contentDataLength" @change="handleTableHtmlChange" placeholder="数据填充" size="small" class="zoom-select">
            <el-option
              v-for="item in contentDataArr"
              :key="item"
              :label="`${item}条`"
              :value="item"
            />
          </el-select>
          <common-button size="small" type="danger" :disabled="isBlank(config)" @click="print">打印预览</common-button>
          <common-button size="small" type="success" :disabled="isBlank(config)" @click="exportXSLX">导出 Excel</common-button>
          <common-button size="small" type="" @click="crud.cancelDetail">退出</common-button>
        </div>
      </div>
    </template>
    <div class="dialog-middle">
      <div class="content" :style="contentStyle">
        <template v-if="isNotBlank(config)">
          <el-image
            v-if="logoCfg?.show && logoCfg.url"
            class="logo-info"
            :style="logoStyle"
            :src="logoCfg.url"
            fit="scale-down"
          />
          <div
            v-if="qrCfg?.show"
            class="qr-info"
            :style="qrStyle"
          >
            <qrcode-vue :value="qrContent" :size="150" :margin="1" />
          </div>
          <div v-if="tableFieldsCfg?.show" class="title-info" :style="titleStyle" v-html="titleHtml" />
          <div v-if="headerCfg?.show" class="header-info" :style="headerStyle" v-html="headerHtml" />
          <div v-if="tableCfg && isNotBlank(filterExampleTableData)" class="table-info" :style="tableStyle">
            <div v-html="tableCfg.style" />
            <div v-html="tableHtml" />
          </div>
          <div v-if="footerCfg?.show" class="footer-info" :style="footerStyle" v-html="footerHtml" />
          <div v-if="pageCfg?.show" class="page-info" :style="pageStyle" v-html="pageHtml" />
        </template>
        <template v-else>
          <span class="table-type-tip">* 表格模板错误</span>
        </template>
      </div>
    </div>
  </common-dialog>
</template>

<script setup>
import { ref, reactive, computed } from 'vue'

import { regDetail } from '@compos/use-crud'
import { isBlank, isNotBlank } from '@data-type/index'

import { printTable } from '@/utils/print/table'
import downloadXLSX from '@/utils/print/download'
import example from '@/utils/print/default-template/example'
import { convertColumns, delNotDisplayed, getLastColumns, setting } from '@/utils/print/page-handle'
import { orientEnum, printModeEnum } from '@/utils/print/enum'
import formatFn from '@/utils/print/format/index'
import QrcodeVue from 'qrcode.vue'

const titleHtml = ref('')
const headerHtml = ref('')
const tableHtml = ref('')
const footerHtml = ref('')
const pageHtml = ref('')
const contentZoom = ref(100)
const contentDataLength = ref(10)
const zoomArr = ref([50, 60, 70, 75, 80, 90, 100])
const contentDataArr = ref([0, 10, 20, 30, 50, 100])
const qrContent = ref('欢迎来到初鸣智造钢结构生产执行管理系统')
const headerData = ref({})
const footerData = ref({})
const tableData = ref([])
const config = ref({})
const tableFieldsCfg = reactive({
  columns: {},
  columnRows: {},
  lastColumns: {}
})

const { crud, detail, CRUD } = regDetail()

const qrCfg = computed(() => {
  return config.value?.qrCode
})

const logoCfg = computed(() => {
  return config.value?.logo
})

const titleCfg = computed(() => {
  return config.value?.title
})

const headerCfg = computed(() => {
  return config.value?.header
})

const footerCfg = computed(() => {
  return config.value?.footer
})

const pageCfg = computed(() => {
  return config.value?.page
})

const tableCfg = computed(() => {
  return config.value?.table
})

const pageWidth = computed(() => {
  return orientEnum.TRANSVERSE.V === config.value.orient ? config.value.height : config.value.width
})

const pageHeight = computed(() => {
  return orientEnum.TRANSVERSE.V === config.value.orient ? config.value.width : config.value.height
})

const contentStyle = computed(() => {
  const _config = config.value
  const _contentZoom = contentZoom.value / 100
  if (_config.unit) {
    return {
      transform: `scale(${_contentZoom})`,
      'margin-right': `-${_config.width * (1 - _contentZoom) + _config.unit}`,
      'margin-bottom': `-${_config.height * (1 - _contentZoom) + _config.unit}`,
      width: pageWidth.value + _config.unit,
      height: pageHeight.value + _config.unit,
      padding: `${_config.paddingTB || 0}${_config.unit} ${_config.paddingLR || 0}${_config.unit}`
    }
  } else {
    return {
      transform: `scale(${_contentZoom})`,
      'margin-right': `-${210 * (1 - _contentZoom)}mm`,
      'margin-bottom': `-${297 * (1 - _contentZoom)}mm`,
      width: '210mm',
      height: '297mm',
      padding: `10mm 10mm`
    }
  }
})

const qrStyle = computed(() => {
  const _style = {}
  if (qrCfg.value) {
    const _config = config.value
    const itemConfig = qrCfg.value
    if (isNotBlank(itemConfig.height)) {
      _style['height'] = `${itemConfig.height}${_config.unit}`
    }
    if (isNotBlank(itemConfig.width)) {
      _style['width'] = `${itemConfig.width}${_config.unit}`
    }
    if (isNotBlank(itemConfig.top)) {
      _style['top'] = `${itemConfig.top}${_config.unit}`
    }
    if (isNotBlank(itemConfig.left)) {
      _style['left'] = `${itemConfig.left}${_config.unit}`
    }
  }
  return _style
})

const logoStyle = computed(() => {
  const _style = {}
  if (logoCfg.value) {
    const _config = config.value
    const itemConfig = logoCfg.value
    if (isNotBlank(itemConfig.height)) {
      _style['height'] = `${itemConfig.height}${_config.unit}`
    }
    if (isNotBlank(itemConfig.width)) {
      _style['width'] = `${itemConfig.width}${_config.unit}`
    }
    if (isNotBlank(itemConfig.top)) {
      _style['top'] = `${itemConfig.top}${_config.unit}`
    }
    if (isNotBlank(itemConfig.left)) {
      _style['left'] = `${itemConfig.left}${_config.unit}`
    }
  }
  return _style
})

const titleStyle = computed(() => {
  const _style = {}
  if (titleCfg.value) {
    const _config = config.value
    const itemConfig = titleCfg.value
    if (isNotBlank(itemConfig.size)) {
      _style['font-size'] = `${itemConfig.size}${_config.fontUnit}`
    }
    if (isNotBlank(itemConfig.bold)) {
      _style['font-weight'] = itemConfig.bold
    }
    if (isNotBlank(itemConfig.height)) {
      _style['height'] = `${itemConfig.height}${_config.unit}`
    }
    if (isNotBlank(itemConfig.align)) {
      _style['justify-content'] = setting.flexAlign(itemConfig.align)
    }
    if (isNotBlank(itemConfig.verticleAlign)) {
      _style['align-items'] = setting.verticleAlign(itemConfig.verticleAlign)
    }
  }
  return _style
})

const pageStyle = computed(() => {
  const _style = {}
  if (pageCfg.value) {
    const _config = config.value
    const itemConfig = pageCfg.value
    if (isNotBlank(_config.paddingLR)) {
      _style['padding-left'] = `${_config.paddingLR || 0}${_config.unit}`
      _style['padding-right'] = `${_config.paddingLR || 0}${config.value.unit}`
    }
    if (isNotBlank(itemConfig.bottom)) {
      _style['bottom'] = `${itemConfig.bottom}${_config.unit}`
    }
    if (isNotBlank(itemConfig.size)) {
      _style['font-size'] = `${itemConfig.size}${_config.fontUnit}`
    }
    if (isNotBlank(itemConfig.bold)) {
      _style['font-weight'] = itemConfig.bold
    }
    if (isNotBlank(itemConfig.align)) {
      _style['text-align'] = setting.textAlign(itemConfig.align)
    }
  }
  return _style
})

const headerStyle = computed(() => {
  const _style = {}
  if (headerCfg.value) {
    const _config = config.value
    const itemConfig = headerCfg.value
    if (isNotBlank(itemConfig.width)) {
      _style['width'] = `${itemConfig.width}${_config.unit}`
    }
    if (isNotBlank(itemConfig.height)) {
      _style['height'] = `${itemConfig.height}${_config.unit}`
    }
    if (isNotBlank(itemConfig.size)) {
      _style['font-size'] = `${itemConfig.size}${_config.fontUnit}`
    }
    if (isNotBlank(itemConfig.align)) {
      _style['justify-content'] = setting.flexAlign(itemConfig.align)
    }
    if (isNotBlank(itemConfig.verticleAlign)) {
      _style['align-items'] = setting.verticleAlign(itemConfig.verticleAlign)
    }
  }
  return _style
})

const tableStyle = computed(() => {
  const _style = {}
  if (tableCfg.value) {
    const _config = config.value
    let tHeight = _config.height - _config.paddingTB * 2
    if (_config.title?.show) {
      tHeight -= _config.title.height
    }
    if (_config.header?.show) {
      tHeight -= _config.header.height
    }
    if (_config.footer?.show) {
      tHeight -= _config.footer.height
    }
    _style['max-height'] = `${tHeight}${_config.unit}`
  }
  return _style
})

const filterExampleTableData = computed(() => {
  let _tableData = []
  if (isNotBlank(tableData.value)) {
    const index = contentDataLength.value > tableData.value.length ? tableData.value.length : contentDataLength.value
    _tableData = tableData.value.slice(0, index)
  }
  return _tableData
})

// 详情加载后
CRUD.HOOK.afterToDetail = async (crud) => {
  config.value = isNotBlank(detail.config) && JSON.parse(detail.config)
  setData()
  splicingHtml()
}

function exportXSLX() {
  let param = {
    header: headerData.value,
    table: filterExampleTableData.value,
    footer: footerData.value,
    qrCode: qrContent.value
  }

  const _type = detail.type
  if (_type && formatFn[_type]) { // 数据装换
    param = formatFn[_type](param)
  }
  param.config = JSON.parse(JSON.stringify(config.value))
  downloadXLSX(param)
}

function setData() {
  if (isNotBlank(config.value.table?.fields)) {
    const columnRows = convertColumns(JSON.parse(JSON.stringify(config.value.table.fields)))
    const lastColumns = getLastColumns(columnRows)
    tableData.value = example.getList({
      fields: lastColumns,
      extraFields: config.value.table.extraFields,
      number: 100
    })
  }
  if (isNotBlank(config.value.header?.fields)) {
    headerData.value = example.getOne({
      fields: config.value.header.fields,
      extraFields: config.value.header.extraFields
    })
  }
  if (isNotBlank(config.value.footer?.fields)) {
    footerData.value = example.getOne({
      fields: config.value.footer.fields,
      extraFields: config.value.footer.extraFields
    })
  }
}

function print() {
  let param = {
    header: headerData.value,
    table: filterExampleTableData.value,
    footer: footerData.value,
    qrCode: qrContent.value
  }

  const _type = detail.type
  if (_type && formatFn[_type]) { // 数据装换
    param = formatFn[_type](param)
  }
  param.printMode = printModeEnum.PREVIEW.V
  param.config = JSON.parse(JSON.stringify(config.value))
  printTable(param)
}

function setColumns() {
  if (!tableCfg.value) return
  // 设置column
  tableFieldsCfg.columns = delNotDisplayed(JSON.parse(JSON.stringify(tableCfg.value.fields)))
  tableFieldsCfg.columnRows = convertColumns(tableFieldsCfg.columns)
  tableFieldsCfg.lastColumns = getLastColumns(tableFieldsCfg.columnRows)
}

function splicingHtml() {
  if (isBlank(config.value)) {
    return
  }
  handleTitleHtmlChange()
  handleHeaderHtmlChange()
  handleTableHtmlChange()
  handleFooterHtmlChange()
  handlePageHtmlChange()
}

function handleTitleHtmlChange() {
  titleHtml.value = setting.getTitleHtml(titleCfg.value) // 拼接页码
}

function handleHeaderHtmlChange() {
  setting.setHeaderFieldStyle(config.value) // 设置字段信息
  headerHtml.value = setting.getHeaderHtml(headerData.value, headerCfg.value) // 拼接页码
}

function handleTableHtmlChange() {
  setColumns() // 设置字段
  setting.setTableStyle(config.value, tableFieldsCfg) // 设置表格及其列样式
  tableHtml.value = setting.getTableHtml(filterExampleTableData.value, tableCfg.value, tableFieldsCfg) // 拼接页码
}

function handleFooterHtmlChange() {
  setting.setFooterFieldStyle(config.value) // 设置字段信息
  footerHtml.value = setting.getFooterHtml(footerData.value, config.value) // 拼接页码
}

function handlePageHtmlChange() {
  pageHtml.value = setting.getPageHtml(pageCfg.value) // 拼接页码
}
</script>

<style lang="scss">
  .print-template-detail {
    background: #f0f0f0;
    .el-dialog__header {
      padding-top:10px;
      padding-bottom: 10px;
      background-color: #5c93ce;
      color: #FFFFFF;
    }
  }
</style>
<style lang="scss" scoped>
  ::v-deep(.el-dialog__body) {
    padding: 15px 20px 10px 20px;
    box-sizing: border-box;
  }
  .dialog-title {
    display: flex;
    flex-direction: row;
    align-items: center;
    justify-content: space-between;
    >span {
      font-size: 18px;
    }
    .config-item-box {
      margin-right: 15px;
    }
    .zoom-select {
      width:100px;
      margin-right:10px;
      ::v-deep(.el-input__inner) {
        border: none;
        background-color:#e8f4ff;
      }
    }
  }
  .dialog-middle {
    width: inherit;
    height: calc(100vh - 90px);
    overflow: auto;
    display: flex;
    flex-direction: row;
    justify-content: center;
    align-items: center;
    flex-wrap: wrap;
    padding: 10px;
    text-rendering: optimizeLegibility;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    font-family: AvenirNext,Avenir,-apple-system,BlinkMacSystemFont,Roboto Slab,Droid Serif,Segoe UI,Oxygen-Sans,Ubuntu,Cantarell,Georgia,serif;
    .content {
      overflow: hidden;
      transform-origin: 0 0 0; /*以左上角为起点 */
      box-sizing: border-box;
      // border: 1px solid #333;
      background-color: #FFFFFF;
      box-shadow: 0 0 8px 0 rgba(0,0,0,0.2);
      user-select: none;
      .table-type-tip {
        font-weight:bold;
        font-size:20px;
        color:#ff4949;
      }
      >div {
        border: 1px solidr gba(255,255,255,0.1);
        box-sizing: border-box;
      }
      .logo-info {
        position: absolute;
        border:none;
      }
      // 标题
      .title-info {
        display: flex;
        justify-content: center;
        align-items: flex-start;
        width: 100%;
        box-sizing: border-box;
        text-align: center;
        font-size: 17pt;
        font-weight: bold;
      }
      // 表头信息
      .header-info {
        font-family: lucida sans unicode,lucida grande,Sans-Serif;
        width: 100%;
        display: flex;
        flex-direction: row;
        justify-content: flex-start;
        align-items: center;
        flex-wrap: wrap;
        box-sizing: border-box;
        line-height: 1.15;
      }
      ::v-deep(.header-info >div) {
        display: inline-block;
        margin: 1mm 0;
        box-sizing: border-box;
        overflow : hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      // 表格
      .table-info {
        box-sizing: border-box;
        overflow: hidden;
        width: 100%;
        margin: 1mm 0;
        ::v-deep(.preview-table) {
          font-family: lucida sans unicode,lucida grande,Sans-Serif;
          font-size: 9pt;
          border-collapse: collapse;
          width: 100%;
          .blank-column {
            min-width: 0;
            border: 1px dashed;
            >div{
              display: inline-block;
              min-width: 0;
            }
          }
        }
        ::v-deep(.preview-table th) {
            padding:0;
            line-height: 15pt;
            border:1px solid #000;
            >div {
              padding: 0 1mm;
              box-sizing: border-box;
              min-height: 3mm;
            }
        }
        ::v-deep(.preview-table td) {
            padding:0;
            border:1px solid #000;
            line-height:13pt;
            white-space: pre-wrap;
            >div {
              padding: 0 1mm;
              box-sizing: border-box;
              min-height: 3mm;
            }
        }
        ::v-deep(.preview-table tbody tr:last-child) {
          border-bottom:none
        }
      }

      // footer
      .footer-info{
        font-family: lucida sans unicode,lucida grande,Sans-Serif;
        overflow: hidden;
        width: 100%;
        box-sizing: content-box;
        padding-right: 9mm;
        display: flex;
        flex-direction: row;
        justify-content: flex-start;
        align-items: center;
        flex-wrap: wrap;
        ::v-deep(.tip) {
          display: inline-block;
          white-space: pre-line;
          width: 100%;
          margin: 1mm 0;
          font-size: 9pt;
          color: red;
        }
      }
      ::v-deep(.footer-info >div) {
        display: inline-block;
        margin: 1mm 0;
        // padding-right: 5mm;
        box-sizing: border-box;
        overflow : hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
      }
      .qr-info {
        position: absolute;
        img{
          width: 100%;
          height: 100%;
          vertical-align: middle;
        }
      }
      .page-info {
        box-sizing: border-box;
        width: 100%;
        position: absolute;
        bottom: 0;
        left: 0;
      }
    }
  }
</style>
