<template>
  <!-- :class="`${props.showEmptySymbol ? 'empty-show-symbol-table' : ''}`"-->
  <el-table v-bind="$attrs" ref="tableRef" :data="filterData" :stripe="tStripe" :border="tBorder">
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
import { defineExpose, defineProps, watch, computed, ref, nextTick } from 'vue'
import { mapGetters } from '@/store/lib'
import EO from '@enum'
import { DP } from '@/settings/config'
import { toThousand } from '@/utils/data-type/number'
import { parseTime } from '@/utils/date'
import { projectNameFormatter } from '@/utils/project'
import { cleanArray } from '@/utils/data-type/array'
import { getInfo, setInfo } from '@/utils/el-extra'

import { ElTable } from 'element-plus'
import { addPrefix, addSuffix, isBlank, isNotBlank, toFixed, toPrecision } from '@/utils/data-type'
import cloneDeep from 'lodash/cloneDeep'

// default不填写，默认值为null。需要传入undefined
const props = defineProps({
  // 显示的数据
  data: {
    type: Array,
    default: undefined
  },
  // 数据格式转换
  dataFormat: {
    type: Array,
    default: undefined
  },
  /**
   * 返回数据源对象（即，在数据源上进行数据转换）
   * 若不进行数据处理（即：dataFormat为空以及showEmptySymbol为false），则该字段无效，直接返回源数据
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
          keys.push(row[props.rowKey])
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

  if (props.showEmptySymbol || dfColumns.length > 0) {
    // 优化数据列表
    filterData.value = optimizeList(data, columns, dfColumns)
  } else {
    // 不处理则直接返回
    filterData.value = data
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
      // 遍历columns
      iterateColumns.forEach((field) => {
        const dfCfg = dataFormatKV.value[field]
        let preData = getInfo(row, field)
        if (dfCfg) {
          for (let i = 0; i < dfCfg.length; i++) {
            const fmD = formatDataByType(row, preData, dfCfg[0])
            preData = fmD
            setInfo(row, field, fmD)
          }
        }
        // 若未显示列中的对象，且值不存在，则设置空
        if (props.showEmptySymbol && columnsKV.value[field] && isBlank(preData)) {
          setInfo(row, field, props.emptySymbol)
        }
      })
    }
  })
  return cloneList
}

//
function formatDataByType(row, data, field) {
  const type = Array.isArray(field) ? field[0] : field
  const field1 = Array.isArray(field) && field.length > 1 ? field[1] : void 0
  switch (type) {
    case 'to-fixed':
      return toFixed(data, field1)
    case 'to-fixed-ck':
      return toFixed(data, isBlank(field1) ? DP[field1] : void 0)
    case 'to-fixed-field':
      return toFixed(data, isBlank(field1) ? row[field1] : void 0)
    case 'to-precision':
      return toPrecision(data, field1)
    case 'to-precision-ck':
      return toPrecision(data, isBlank(field1) ? DP[field1] : void 0)
    case 'to-precision-field':
      return toPrecision(data, isBlank(field1) ? row[field1] : void 0)
    case 'to-thousand':
      return toThousand(data, field1)
    case 'to-thousand-ck':
      return toThousand(data, field1)
    case 'to-thousand-field':
      return toThousand(data, field1)
    case 'suffix':
      return addSuffix(data, field1)
    case 'prefix':
      return addPrefix(data, field1)
    case 'split':
      return Array.isArray(data) ? data.join(field1 || '、') : data
    case 'parse-time':
      return parseTime(data, field1 || '{y}-{m}-{d} {h}:{i}')
    case 'parse-project':
      return parseProject(row, data, field)
    case 'parse-enum':
      return parseEnum(row, data, field)
  }
}

function parseProject(row, data, field) {
  if (isBlank(data)) return
  let p = []
  let split = '、'
  if (Array.isArray(data)) {
    p = data
  } else {
    p = [data]
  }
  if (field.length > 1) {
    const cfg = field[1]
    split = cfg.split || split
    if (cfg && cfg.onlyShortName) {
      return p.map((v) => v.shortName).join(split)
    } else {
      return p.map((v) => projectNameFormatter(v, null, false)).join(split)
    }
  } else {
    return p.map((v) => projectNameFormatter(v, null, false)).join(split)
  }
}

function parseEnum(row, data, field) {
  let text = ''
  const defaultKey = { f: 'L', bit: false, split: '、', extra: '' }
  const cfg = field.length > 2 ? Object.assign(defaultKey, field[2]) : defaultKey
  const fEnum = field[1]
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

function toggleRowSelection(row, selected) {
  tableRef.value.toggleRowSelection(row, selected)
}

function toggleAllSelection() {
  tableRef.value.toggleAllSelection()
}

function toggleRowExpansion(row, expanded) {
  tableRef.value.toggleRowExpansion(row, expanded)
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

defineExpose({
  getColumns,
  clearSelection,
  toggleRowSelection,
  toggleAllSelection,
  toggleRowExpansion,
  expandParent,
  setCurrentRow,
  clearSort,
  clearFilter,
  doLayout,
  sort
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
