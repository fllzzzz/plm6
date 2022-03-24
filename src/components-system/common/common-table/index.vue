<template>
  <!-- :class="`${props.showEmptySymbol ? 'empty-show-symbol-table' : ''}`"-->
  <el-table
    v-bind="$attrs"
    ref="tableRef"
    :data="filterData"
    :stripe="tStripe"
    :border="tBorder"
    :expand-row-keys="props.expandRowKeys"
    :row-key="props.rowKey"
    :row-class-name="rowClassName"
    :row-style="rowStyle"
    :cell-class-name="cellClassName"
    :cell-style="cellStyle"
    :header-row-class-name="headerRowClassName"
    :header-row-style="headerRowStyle"
    :header-cell-class-name="headerCellClassName"
    :header-cell-style="headerCellStyle"
    :span-method="spanMethod"
    :load="load"
    @select="select"
    @select-all="selectAll"
    @selection-change="selectionChange"
    @cell-mouse-enter="cellMouseEnter"
    @cell-mouse-leave="cellMouseLeave"
    @cell-click="cellClick"
    @cell-dblclick="cellDblclick"
    @cell-contextmenu="cellContext"
    @row-click="rowClick"
    @row-contextmenu="rowContextmenu"
    @row-dblclick="rowDblclick"
    @header-click="headerClick"
    @header-contextmenu="headerContextmenu"
    @sort-change="sortChange"
    @filter-change="filterChange"
    @current-change="currentChange"
    @header-dragend="headerDragend"
    @expand-change="expandChange"
  >
    <template #default>
      <slot />
    </template>
    <template #empty>
      <slot name="empty" />
    </template>
    <template #append>
      <slot name="append" />
    </template>
  </el-table>
</template>

<script setup>
import { defineExpose, defineEmits, defineProps, watch, computed, ref, nextTick } from 'vue'
import { mapGetters } from '@/store/lib'
import EO from '@enum'
import { DP } from '@/settings/config'
import { toThousand } from '@/utils/data-type/number'
import { parseTime } from '@/utils/date'
import { projectNameFormatter } from '@/utils/project'
import { cleanArray } from '@/utils/data-type/array'
import { getInfo, setInfo } from '@/utils/el-extra'

import { ElTable } from 'element-plus'
import { addPrefix, addSuffix, emptyTextFormatter, isBlank, isNotBlank, toFixed, toPrecision } from '@/utils/data-type'
import cloneDeep from 'lodash/cloneDeep'

const emit = defineEmits([
  'select',
  'selectAll',
  'selectionChange',
  'cellMouseEnter',
  'cellMouseLeave',
  'cellClick',
  'cellDblclick',
  'cellContext',
  'rowClick',
  'rowContextmenu',
  'rowDblclick',
  'headerClick',
  'headerContextmenu',
  'sortChange',
  'filterChange',
  'currentChange',
  'dragend',
  'expandChange'
])

// default不填写，默认值为null。需要传入undefined
const props = defineProps({
  // 显示的数据
  data: {
    type: Array,
    default: undefined
  },
  /**
   * 数据格式转换
   * 格式：
   * [
   *  ['project', ['parse-project', {lineBreak:true}], ..., { source: 'projects' }],
   *  ['[source].project', 'parse-project']
   *  ...
   * ]
   * 参数1：字段名，string, 必填 数组字段使用“[]”包裹
   * 参数2: 转换类型 ,string | array。 子参数查看对应类型。PS：参数2可填写多个，按填写顺序转换
   * 参数(末尾)：其他信息, object。
   * source: 源数据字段：以上方为例 row.project = format(row.projects)
   */
  dataFormat: {
    type: Array,
    default: undefined
  },
  /**
   * 返回数据源对象（即，在数据源上进行数据格式转换）
   */
  returnSourceData: {
    type: Boolean,
    default: false
  },
  // 空值 显示 符号
  showEmptySymbol: {
    type: Boolean,
    default: true
  },
  emptySymbol: {
    type: String,
    default: '-'
  },
  // Table 的高度， 默认为自动高度。 如果 height 为 number 类型，单位 px；如果 height 为 string 类型，则这个高度会设置为 Table 的 style.height 的值，Table 的高度会受控于外部样式。
  // 是否为斑马纹 table
  stripe: {
    type: Boolean,
    default: undefined
  },
  // 	是否带有纵向边框
  border: {
    type: Boolean,
    default: undefined
  },
  // 空数据时显示的文本内容， 也可以通过 #empty 设置
  emptyText: {
    type: String,
    default: '暂无数据'
  },
  // 合计行第一列的文本
  sumText: {
    type: String,
    default: '合计'
  },

  // 是否默认展开所有行，当 Table 包含展开行存在或者为树形表格时有效
  defaultExpandAll: {
    type: Boolean,
    default: undefined
  },
  // 可以通过该属性设置 Table 目前的展开行，需要设置 row-key 属性才能使用，该属性为展开行的 keys 数组
  expandRowKeys: {
    type: Array,
    default: undefined
  },
  // 行数据的 Key
  rowKey: {
    type: [String, Function]
  },
  // 行的 className 的回调方法，也可以使用字符串为所有行设置一个固定的 className
  rowClassName: {
    type: [Function, String]
  },
  // 行的 style 的回调方法，也可以使用一个固定的 Object 为所有行设置一样的 Style
  rowStyle: {
    type: [Function, Object]
  },
  // 单元格的 className 的回调方法，也可以使用字符串为所有单元格设置一个固定的 className
  cellClassName: {
    type: [Function, String]
  },
  // 单元格的 style 的回调方法，也可以使用一个固定的 Object 为所有单元格设置一样的 Style。
  cellStyle: {
    type: [Function, Object]
  },
  // 表头行的 className 的回调方法，也可以使用字符串为所有表头行设置一个固定的 className。
  headerRowClassName: {
    type: [Function, String]
  },
  // 表头行的 style 的回调方法，也可以使用一个固定的 Object 为所有表头行设置一样的 Style。
  headerRowStyle: {
    type: [Function, Object]
  },
  // 表头单元格的 className 的回调方法，也可以使用字符串为所有表头单元格设置一个固定的 className。
  headerCellClassName: {
    type: [Function, String]
  },
  // 表头单元格的 style 的回调方法，也可以使用一个固定的 Object 为所有表头单元格设置一样的 Style。
  headerCellStyle: {
    type: [Function, Object]
  },
  // 合并行或列的计算方法
  spanMethod: {
    type: Function
  },
  // 加载子节点数据的函数，lazy 为 true 时生效，函数第二个参数包含了节点的层级信息
  load: {
    type: Function
  }
})

const tableRef = ref()

const { tableBorder, tableStripe } = mapGetters(['tableBorder', 'tableStripe'])

const tBorder = computed(() => (isNotBlank(props.border) ? props.border : tableBorder.value))
const tStripe = computed(() => (isNotBlank(props.stripe) ? props.stripe : tableStripe.value))
const tableColumns = computed(() => {
  const columns = []
  if (tableRef.value && tableRef.value.$refs && tableRef.value.$refs.tableHeader.columns) {
    tableRef.value.$refs.tableHeader.columns.forEach((column) => {
      if (column.type === 'default') {
        columns.push(column.property)
      }
    })
  }
  return columns
})

const columnsKV = computed(() => {
  const kv = {}
  tableColumns.value.forEach((c) => {
    kv[c] = true
  })
  return kv
})

const dataFormatKV = computed(() => {
  const kv = {}
  if (props.dataFormat && props.dataFormat.length > 0) {
    props.dataFormat.forEach((df) => {
      kv[df[0]] = df.slice(1)
    })
  }
  return kv
})

const filterData = ref()

const tableLoaded = watch(
  tableRef,
  (val) => {
    if (val) {
      watch(
        [() => props.data, tableColumns],
        ([listData, columns]) => {
          handleData(listData, columns)
        },
        { immediate: true, deep: true }
      )
      nextTick(() => {
        tableLoaded()
      })
    }
  },
  { immediate: true }
)

// 监听表格初始化时，是否展开所有行
watch(
  [() => props.defaultExpandAll, () => props.data],
  ([all, data]) => {
    if (props.expandRowKeys) {
      const keys = props.expandRowKeys
      keys.length = 0
      if (all && Array.isArray(data)) {
        data.forEach((row) => {
          keys.push(row[getRowKey(row, true)])
        })
      }
    }
  },
  { immediate: true }
)

/**
 * @param {array} data 待处理数据
 * @param {array} columns 显示的列
 */
function handleData(data, columns) {
  // 获取格式转化的列字段
  const dfColumns = props.dataFormat ? props.dataFormat.map((df) => df[0]) : []
  // 优化数据列表
  // filterData.value = optimizeList(data, columns, dfColumns)
  const fmList = optimizeList(data, columns, dfColumns)
  if (props.returnSourceData) {
    filterData.value = fmList
  } else {
    /**
     * 不选择重新为filterData.value赋值
     * 是为了避免当表格中有表单内容时，修改了表单内容，导致重新拷贝地址发生变化，从而造成，展开的行收缩等情况。
     * 不触发的条件：不改变rowKey的值以及row的地址
     */
    let sourceChange = false
    // 数组长度大于0 ，格式化后的数组长度与原filter数组长度相同，并且第一个元素的sourceRow相同
    if (
      isNotBlank(filterData.value) &&
      fmList.length === filterData.value.length &&
      fmList[0].sourceRow === filterData.value[0].sourceRow // 判断第一个元素是避免换页
    ) {
      for (let i = 0; i < fmList.length; i++) {
        if (fmList[i].sourceRow !== filterData.value[i].sourceRow) {
          sourceChange = true
          break
        } else {
          if (props.rowKey) {
            // 不改变rowKey,否则监听到rowKey发生变化，会认为当前对象发生改变
            Object.keys(filterData.value[i]).forEach((key) => {
              if (key !== getRowKey(filterData.value[i])) {
                filterData.value[i][key] = undefined
              }
            })
          } else {
            // TODO:未测试
            Object.keys(filterData.value[i]).forEach((key) => {
              filterData.value[i][key] = undefined
            })
          }
          Object.assign(filterData.value[i], fmList[i])
        }
      }
    } else {
      // 长度为0时，也视为变化
      sourceChange = true
    }
    if (sourceChange) {
      filterData.value = fmList
    }
  }
}

// 优化列表数据（通常用于表格显示）
function optimizeList(list, columns, dfColumns = []) {
  // 为空返回“空数组”
  if (isBlank(list) || isBlank(columns)) return []
  // 拷贝对象
  const cloneList = props.returnSourceData ? list : cloneDeep(list)
  // 合并列
  const iterateColumns = Array.from(new Set([...columns, ...dfColumns]))
  // 递归优化
  // optimizeListDeep(list)
  cloneList.forEach((row, rowIndex) => {
    if (row) {
      // 赋予row 数据源
      if (!props.returnSourceData) row.sourceRow = list[rowIndex]
      if (props.showEmptySymbol || dfColumns.length > 0) {
        // 遍历columns
        iterateColumns.forEach((field) => {
          if (field && field.indexOf('[') > -1) {
            recursionFormat(row, field, list[rowIndex], field)
          } else {
            normalFm(row, field, list[rowIndex])
          }
        })
      }
    }
  })
  return cloneList
}

// 递归格式化（当数据中含数组字段时）
function recursionFormat(row, field, data, sliceFields) {
  const keys = sliceFields.split('.')
  let item = data
  let i
  for (i = 0; i < keys.length - 1; i++) {
    const curKey = keys[i]
    // 判断是否符合 '[field]' 数组格式
    const isArray = /^\[[0-9a-zA-Z_]+\]$/.test(curKey)
    if (isArray) {
      const k = curKey.slice(1, curKey.length - 1)
      item = item[k]
      if (item) {
        // 若是数组字段且有值，则遍历数组
        item.forEach((itemInfo) => {
          recursionFormat(row, field, itemInfo, keys.slice(i + 1).join('.'))
        })
      }
      break
    } else {
      // 对象按照常规流程赋值
      item = item[curKey]
      // 对象为空值，结束循环
      if (isBlank(item)) break
    }
  }
  // 如果是最后的字段
  if (i === keys.length - 1) {
    let dfCfg = dataFormatKV.value[field]
    let preData = item[sliceFields]
    if (dfCfg) {
      // 如果数组最后一个值为对象，且不为数组的情况
      const otherInfo = dfCfg[dfCfg.length - 1]
      // 别名，没有则使用field
      if (otherInfo && typeof otherInfo === 'object' && !Array.isArray(otherInfo)) {
        // 实际配置信息范围
        dfCfg = dfCfg.slice(0, dfCfg.length - 1)
        // 获取数据源字段
        const sourceField = otherInfo ? otherInfo.source : void 0
        // 获取实际转换前的值
        if (sourceField) preData = item[sourceField]
      }
      if (sliceFields) {
        for (let j = 0; j < dfCfg.length; j++) {
          // 获取转换后的值
          const fmD = formatDataByType(row, preData, dfCfg[j])
          // 设置转换后的值
          item[sliceFields] = fmD
        }
      }
    }
    // 数组中的值不会属于表格列值，因此不做空值处理
  }
}

// 普通数据格式转换
function normalFm(row, field, data) {
  let dfCfg = dataFormatKV.value[field]
  let preData = getInfo(data, field)
  // 获取未转换的值
  if (dfCfg) {
    // 如果数组最后一个值为对象，且不为数组的情况
    const otherInfo = dfCfg[dfCfg.length - 1]
    // 别名，没有则使用field
    if (otherInfo && typeof otherInfo === 'object' && !Array.isArray(otherInfo)) {
      // 实际配置信息范围
      dfCfg = dfCfg.slice(0, dfCfg.length - 1)
      // 获取数据源字段
      const sourceField = otherInfo ? otherInfo.source : void 0
      // 获取实际转换前的值
      if (sourceField) preData = getInfo(data, sourceField)
    }
    if (field) {
      for (let i = 0; i < dfCfg.length; i++) {
        // 获取转换后的值
        const fmD = formatDataByType(row, preData, dfCfg[i])
        preData = fmD
        // 设置转换后的值
        setInfo(row, field, fmD)
      }
    }
  }
  // 若未显示列中的对象，且值不存在，则设置空
  if (props.showEmptySymbol && columnsKV.value[field] && isBlank(preData)) {
    setInfo(row, field, props.emptySymbol)
  }
}

//
function formatDataByType(row, data, field) {
  const type = Array.isArray(field) ? field[0] : field
  const field1 = Array.isArray(field) && field.length > 1 ? field[1] : void 0
  switch (type) {
    /**
     * to-fixed, to-precision, to-thousand，第二个参数皆为小数精度
     * 共有三种填写方式
     * 普通模式：直接传入小数精度，如：['to-fixed',2]
     * commonKey(ck)模式：传入公共精度的key值，如：['to-fixed','COM_WT__KG']
     * field模式：传入当前对象的字段，如['to-fixed','accountingPrecision'], 单位精度 取值为 row.accountingPrecision
     */
    // 处理小数精度，转换后为string
    case 'to-fixed':
      return toFixed(data, field1)
    case 'to-fixed-ck':
      return toFixed(data, isNotBlank(field1) ? DP[field1] : void 0)
    case 'to-fixed-field':
      return toFixed(data, isNotBlank(field1) ? getInfo(row, field1) : void 0)
    // 处理小数精度，转换后为number
    case 'to-precision':
      return toPrecision(data, field1)
    case 'to-precision-ck':
      return toPrecision(data, isNotBlank(field1) ? DP[field1] : void 0)
    case 'to-precision-field':
      return toPrecision(data, isNotBlank(field1) ? getInfo(row, field1) : void 0)
    // 10000 => 10,000
    case 'to-thousand':
      return toThousand(data, field1)
    case 'to-thousand-ck':
      return toThousand(data, isNotBlank(field1) ? DP[field1] : void 0)
    case 'to-thousand-field':
      return toThousand(data, isNotBlank(field1) ? getInfo(row, field1) : void 0)
    /**
     * 前置文字
     * 例：['prefix', '快乐的']： '小明'  =>  '快乐的小明
     */
    case 'prefix':
      return addPrefix(data, field1)
    /**
     * 前置文字
     * 例：['prefix', '——author：小明']： '嘻嘻'  =>  '嘻嘻——author：小明
     */
    case 'suffix':
      return addSuffix(data, field1)
    /**
     * 空字符串显示
     * 例：
     * 1.'empty-text'
     * 2.['empty-text', '-']
     * 参数2: 空值显示。默认：'-'
     *
     * 通常不需要使用该类型
     * 1.需要显示的字段不在el-table-columns中，例如在expand-columns中
     * 2.该列需要自定义空值
     * 3.未使用showEmptySymbol
     */
    case 'empty-text':
      return emptyTextFormatter(data, field1 || '-')
    /**
     * 分解数组
     * 例：
     * 1.'split': ['小王', '小明']  =>  '小王、小明'
     * 2.['split', '，']：['小王', '小明']  =>  '小王，小明'
     * 参数2：分割字符。默认: '、'
     */
    case 'split':
      return Array.isArray(data) ? data.join(field1 || '、') : data
    /**
     * 时间格式转换
     * 例：
     * 1.'parse-time'： 1647917251993  =>  2022-03-22 10:47:31
     * 2.['parse-time', '{y}-{m}-{d}'] 1647917251993  =>  2022-03-22
     * 参数2：日期格式。默认: '{y}-{m}-{d} {h}:{i}'
     */
    case 'parse-time':
      return parseTime(data, field1 || '{y}-{m}-{d} {h}:{i}')
    /**
     * 项目格式转换
     * 例:
     * 1.'parse-project'
     * 2.['parse-project', {onlyShortName: false, split: '、', lineBreak: false}]
     * 参数2：配置信息,默认：{onlyShortName: false, split: '、', lineBreak: false}
     * onlyShortName:只显示简称
     * split：多个项目时的分割字符
     * lineBreak：合同编号与项目名称之间换行，否则以空格隔开
     */
    case 'parse-project':
      return parseProject(data, Array.isArray(field) ? field.slice(1) : void 0)
    /**
     * 枚举格式转换
     * 例：['parse-enum', matClsEnum, { f: 'L', bit: false, split: '、', extra: '' }]
     * 参数2：枚举对象
     * 参数3：信息配置，默认值为：{ f: 'L', bit: false, split: '、', extra: '' }
     * f: “显示的值”对应的是枚举对象中哪个key值
     * bit：是否为位运算的值
     * split：传入数组或位运算值的情况下，分割字符
     * extra: 额外的值（放在转换信息的末尾）
     */
    case 'parse-enum':
      return parseEnum(data, Array.isArray(field) ? field.slice(1) : void 0)
  }
}

// 项目格式装换
function parseProject(data, cfg) {
  if (isBlank(data)) return
  let p = []
  const config = { onlyShortName: false, split: '、', lineBreak: false }
  if (Array.isArray(data)) {
    p = data
  } else {
    p = [data]
  }
  // 覆盖配置信息
  if (cfg && cfg.length > 0) {
    Object.assign(config, cfg[0] || {}) // 配置信息
  }
  // 格式转化
  if (config.onlyShortName) {
    return p.map((v) => v.shortName).join(config.split)
  } else {
    return p.map((v) => projectNameFormatter(v, null, false)).join(config.split)
  }
}

// 枚举格式装换
function parseEnum(data, field) {
  let text = ''
  const defaultKey = { f: 'L', bit: false, split: '、', extra: '' }
  const cfg = field.length > 1 ? Object.assign(defaultKey, field[1]) : defaultKey
  const fEnum = field[0]
  if (isBlank(data) || isBlank(fEnum)) return

  let enumV = fEnum.V
  if (isBlank(enumV)) {
    // 处理某些页面自定义而没有经过处理的枚举
    enumV = {}
    const KEYS = Object.keys(fEnum)
    KEYS.forEach((key) => {
      const value = fEnum[key].V
      enumV[value] = fEnum[key]
    })
  }
  if (Array.isArray(data)) {
    text = data.map((v) => enumV[v][cfg.f]).join(cfg.split)
  } else if (cfg.bit) {
    text = cleanArray(EO.getBits(fEnum, data, cfg.f)).join(cfg.split)
  } else {
    text = enumV[data][cfg.f] || ''
  }

  if (!text) {
    return
  }
  text += cfg.extra
  return text
}

// 获取表格中的列
// 不能直接 const columns = tableRef.value.$refs.tableHeader.columns.setup时，ref里面还是空的
function getColumns() {
  return tableRef.value.$refs.tableHeader.columns
}

function clearSelection() {
  tableRef.value.clearSelection()
}

// 选中
function toggleRowSelection(row, selected) {
  const sourceRow = getCurrent(row)
  tableRef.value.toggleRowSelection(sourceRow, selected)
}

function toggleAllSelection() {
  tableRef.value.toggleAllSelection()
}

// 展开
function toggleRowExpansion(row, expanded) {
  const sourceRow = getCurrent(row)
  tableRef.value.toggleRowExpansion(sourceRow, expanded)
}

// 解决树形结构打开子节点所有父节点expanded:false收回
function expandParent(row, expanded) {
  if (row.parentArray && row.parentArray.length > 0) {
    row.parentArray.forEach((v) => {
      tableRef.value.store.states.treeData.value[v].expanded = true
    })
  }
  tableRef.value.toggleRowExpansion(row, expanded)
}

// lazy加载情况下已加载过节点刷新

function refreshParent(row) {
  tableRef.value.store.states.treeData.value[row.id].loaded = false
  tableRef.value.store.loadOrToggle(row)
}

function setCurrentRow(row) {
  tableRef.value.setCurrentRow(row)
}

function clearSort() {
  tableRef.value.clearSort()
}

function clearFilter(columnKeys) {
  tableRef.value.clearFilter(columnKeys)
}

function doLayout() {
  tableRef.value.doLayout()
}

function sort(prop, order) {
  tableRef.value.sort(prop, order)
}

// 获取属于row的数据源
function getSource(data) {
  if (props.returnSourceData) return data
  let sourceData
  if (Array.isArray(data)) {
    sourceData = data.map((row) => {
      return row && row.sourceRow ? row.sourceRow : row
    })
  } else {
    sourceData = data && data.sourceRow ? data.sourceRow : data
  }

  return sourceData
}

// 获取当前filterData中的row
function getCurrent(data) {
  if (props.returnSourceData) return data
  let curData
  if (Array.isArray(data)) {
    curData = []
    data.forEach((row) => {
      const curRow = filterData.value.find((fmRow) => fmRow.sourceRow === row)
      curData.push(curRow)
    })
  } else {
    curData = filterData.value.find((fmRow) => fmRow.sourceRow === data)
  }
  return curData
}

function getRowKey(row, boolSource = false) {
  if (typeof props.rowKey === 'function') {
    const sourceRow = boolSource ? row : getSource(row)
    return props.rowKey(sourceRow, row)
  }

  if (typeof props.rowKey === 'string') {
    return props.rowKey
  }
}

// --------------------------- 回调 ------------------------------------------

// 行的 className 的回调方法，也可以使用字符串为所有行设置一个固定的 className
function rowClassName({ row, rowIndex }) {
  if (typeof props.rowClassName === 'function') {
    const sourceRow = getSource(row)
    return props.cellClassName({ row: sourceRow, rowIndex })
  }

  if (typeof props.rowClassName === 'string') {
    return props.rowClassName
  }
}

// 行的 style 的回调方法，也可以使用一个固定的 Object 为所有行设置一样的 Style
function rowStyle({ row, rowIndex }) {
  if (isBlank(props.rowStyle)) return

  if (typeof props.rowStyle === 'function') {
    const sourceRow = getSource(row)
    return props.rowStyle({ row: sourceRow, rowIndex })
  }

  if (typeof props.rowStyle === 'object') {
    return props.rowStyle
  }
}

// 单元格的 className 的回调方法，也可以使用字符串为所有单元格设置一个固定的 className
function cellClassName({ row, column, rowIndex, columnIndex }) {
  if (typeof props.cellClassName === 'function') {
    const sourceRow = getSource(row)
    return props.cellClassName({ row: sourceRow, column, rowIndex, columnIndex })
  }

  if (typeof props.cellClassName === 'string') {
    return props.cellClassName
  }
}

// 单元格的 style 的回调方法，也可以使用一个固定的 Object 为所有单元格设置一样的 Style。
function cellStyle({ row, column, rowIndex, columnIndex }) {
  if (isBlank(props.cellStyle)) return

  if (typeof props.cellStyle === 'function') {
    const sourceRow = getSource(row)
    return props.cellStyle({ row: sourceRow, column, rowIndex, columnIndex })
  }

  if (typeof props.cellStyle === 'object') {
    return props.cellStyle
  }
}

// 表头行的 className 的回调方法，也可以使用字符串为所有表头行设置一个固定的 className。
function headerRowClassName({ row, rowIndex }) {
  if (typeof props.headerRowClassName === 'function') {
    const sourceRow = getSource(row)
    return props.headerRowClassName({ row: sourceRow, rowIndex })
  }

  if (typeof props.headerRowClassName === 'string') {
    return props.headerRowClassName
  }
}

// 表头行的 style 的回调方法，也可以使用一个固定的 Object 为所有表头行设置一样的 Style。
function headerRowStyle({ row, rowIndex }) {
  if (isBlank(props.headerRowStyle)) return

  if (typeof props.headerRowStyle === 'function') {
    const sourceRow = getSource(row)
    return props.headerRowStyle({ row: sourceRow, rowIndex })
  }

  if (typeof props.headerRowStyle === 'object') {
    return props.headerRowStyle
  }
}

// 表头单元格的 className 的回调方法，也可以使用字符串为所有表头单元格设置一个固定的 className。
function headerCellClassName({ row, column, rowIndex, columnIndex }) {
  if (typeof props.headerCellClassName === 'function') {
    const sourceRow = getSource(row)
    return props.headerCellClassName({ row: sourceRow, column, rowIndex, columnIndex })
  }

  if (typeof props.headerCellClassName === 'string') {
    return props.headerCellClassName
  }
}

// 表头单元格的 style 的回调方法，也可以使用一个固定的 Object 为所有表头单元格设置一样的 Style
function headerCellStyle({ row, column, rowIndex, columnIndex }) {
  if (isBlank(props.headerCellStyle)) return

  if (typeof props.headerCellStyle === 'function') {
    const sourceRow = getSource(row)
    return props.headerCellStyle({ row: sourceRow, column, rowIndex, columnIndex })
  }

  if (typeof props.headerCellStyle === 'object') {
    return props.headerCellStyle
  }
}

// 合并行或列的计算方法
function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (typeof props.spanMethod === 'function') {
    const sourceRow = getSource(row)
    return props.spanMethod({ row: sourceRow, column, rowIndex, columnIndex })
  }
}

// 加载子节点数据的函数，lazy 为 true 时生效，函数第二个参数包含了节点的层级信息
function load(row, treeNode, resolve) {
  if (typeof props.load === 'function') {
    const sourceRow = getSource(row)
    return props.load({ row: sourceRow, treeNode, resolve })
  }
}

// --------------------------- 事件 ------------------------------------------
// 当用户手动勾选数据行的 Checkbox 时触发的事件
function select(selection, row) {
  const sourceSelection = getSource(selection)
  const sourceRow = getSource(row)
  emit('select', sourceSelection, sourceRow, selection, row)
}

// 当用户手动勾选全选 Checkbox 时触发的事件
function selectAll(selection) {
  const sourceSelection = getSource(selection)
  emit('selectAll', sourceSelection, selection)
}

// 当选择项发生变化时会触发该事件
function selectionChange(selection) {
  const sourceSelection = getSource(selection)
  emit('selectionChange', sourceSelection, selection)
}

// 当单元格 hover 进入时会触发该事件
function cellMouseEnter(row, column, cell, event) {
  const sourceRow = getSource(row)
  emit('cellMouseEnter', sourceRow, column, cell, event)
}

// 	当单元格 hover 退出时会触发该事件
function cellMouseLeave(row, column, cell, event) {
  const sourceRow = getSource(row)
  emit('cellMouseLeave', sourceRow, column, cell, event)
}

// 当某个单元格被点击时会触发该事件
function cellClick(row, column, cell, event) {
  const sourceRow = getSource(row)
  emit('cellClick', sourceRow, column, cell, event)
}

// 当某个单元格被双击击时会触发该事件
function cellDblclick(row, column, cell, event) {
  const sourceRow = getSource(row)
  emit('cellDblclick', sourceRow, column, cell, event)
}

// 当某个单元格被鼠标右键点击时会触发该事件
function cellContext(row, column, cell, event) {
  const sourceRow = getSource(row)
  emit('cellContext', sourceRow, column, cell, event)
}

// 当某一行被点击时会触发该事件
function rowClick(row, column, event) {
  const sourceRow = getSource(row)
  emit('rowClick', sourceRow, column, event)
}

// 当某一行被鼠标右键点击时会触发该事件
function rowContextmenu(row, column, event) {
  const sourceRow = getSource(row)
  emit('rowContextmenu', sourceRow, column, event)
}

// 当某一行被双击时会触发该事件
function rowDblclick(row, column, event) {
  const sourceRow = getSource(row)
  emit('rowDblclick', sourceRow, column, event)
}

// 当某一列的表头被点击时会触发该事件
function headerClick(column, event) {
  emit('headerClick', column, event)
}

// 当某一列的表头被鼠标右键点击时触发该事件
function headerContextmenu(column, event) {
  emit('headerContextmenu', column, event)
}

// 当表格的排序条件发生变化的时候会触发该事件
function sortChange({ column, prop, order }) {
  emit('sortChange', { column, prop, order })
}

// 参数的值是一个对象， 当表格的筛选条件发生变化的时候会触发该事件，对象的 key 是 column 的 columnKey，对应的 value 为用户选择的筛选条件的数组。
function filterChange(filters) {
  emit('filterChange', filters)
}

// 当表格的当前行发生变化的时候会触发该事件，如果要高亮当前行，请打开表格的 highlight-current-row 属性
function currentChange(currentRow, oldCurrentRow) {
  const sourceCurrentRow = getSource(currentRow)
  const sourceOldCurrentRow = getSource(oldCurrentRow)
  emit('currentChange', sourceCurrentRow, sourceOldCurrentRow, currentRow, oldCurrentRow)
}

// 	当拖动表头改变了列的宽度的时候会触发该事件
function headerDragend(newWidth, oldWidth, column, event) {
  emit('dragend', newWidth, oldWidth, column, event)
}

// 当用户对某一行展开或者关闭的时候会触发该事件（展开行时，回调的第二个参数为 expandedRows；树形表格时第二参数为 expanded）
function expandChange(row, expandedRowsOrExpanded) {
  const sourceRow = getSource(row)
  // 配合组件el-expand-table-column 使用
  const keys = props.expandRowKeys || []
  const index = keys.indexOf(sourceRow[getRowKey()])
  if (index > -1) {
    keys.splice(index, 1)
  } else {
    keys.push(sourceRow[getRowKey()])
  }
  emit('expandChange', sourceRow, expandedRowsOrExpanded)
}

defineExpose({
  getColumns,
  getSource,
  getCurrent,
  clearSelection,
  toggleRowSelection,
  toggleAllSelection,
  toggleRowExpansion,
  expandParent,
  setCurrentRow,
  clearSort,
  clearFilter,
  doLayout,
  sort,
  refreshParent
})
</script>

<style lang="scss">
// 表格空数据显示“-”
// .empty-show-symbol-table td {
//   .cell:empty::before {
//     content: '-';
//     color: gray;
//   }
//   .is-leaf {
//     .cell:empty::before {
//       content: '-';
//       color: gray;
//     }
//   }
// }
</style>
