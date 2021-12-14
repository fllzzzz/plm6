<template>
  <el-table
    v-bind="$attrs"
    ref="tableRef"
    :data="props.data"
    :height="props.height"
    :max-height="props.maxHeight"
    :stripe="tStripe"
    :border="tBorder"
    :size="props.size"
    :fit="props.fit"
    :show-header="props.showHeader"
    :highlight-current-row="props.highlightCurrentRow"
    :current-row-key="props.currentRowKey"
    :row-class-name="props.rowClassName"
    :row-style="props.rowStyle"
    :cell-class-name="props.cellClassName"
    :cell-style="props.cellStyle"
    :header-row-class-name="props.headerRowClassName"
    :header-row-style="props.headerRowStyle"
    :header-cell-class-name="props.headerCellClassName"
    :header-cell-style="props.headerCellStyle"
    :row-key="props.rowKey"
    :empty-text="props.emptyText"
    :default-expand-all="props.defaultExpandAll"
    :expand-row-keys="props.expandRowKeys"
    :default-sort="props.defaultSort"
    :tooltip-effect="props.tooltipEffect"
    :show-summary="props.showSummary"
    :sum-text="props.sumText"
    :summary-method="props.summaryMethod"
    :span-method="props.spanMethod"
    :select-on-indeterminate="props.selectOnIndeterminate"
    :indent="props.indent"
    :lazy="props.lazy"
    :load="props.load"
    :tree-props="props.treeProps"
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
import { defineExpose, defineProps, defineEmits, watch, computed, ref } from 'vue'
import { mapGetters } from '@/store/lib'

import { ElTable } from 'element-plus'
import { isNotBlank } from '@/utils/data-type'

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
  // Table 的高度， 默认为自动高度。 如果 height 为 number 类型，单位 px；如果 height 为 string 类型，则这个高度会设置为 Table 的 style.height 的值，Table 的高度会受控于外部样式。
  height: {
    type: [String, Number],
    default: undefined
  },
  // Table 的最大高度。 合法的值为数字或者单位为 px 的高度。
  maxHeight: {
    type: [String, Number],
    default: undefined
  },
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
  // 	Table 的尺寸 medium / small / mini
  size: {
    type: String,
    default: undefined
  },
  // 列的宽度是否自撑开
  fit: {
    type: Boolean,
    default: undefined
  },
  // 是否显示表头
  showHeader: {
    type: Boolean,
    default: undefined
  },
  // 是否要高亮当前行
  heightCurrentRow: {
    type: Boolean,
    default: undefined
  },
  // 当前行的 key，只写属性
  currentRowKey: {
    type: [String, Number],
    default: undefined
  },
  // 行的 className 的回调方法，也可以使用字符串为所有行设置一个固定的 className
  rowClassName: {
    type: [Function, String],
    default: undefined
  },
  // 行的 style 的回调方法，也可以使用一个固定的 Object 为所有行设置一样的 Style
  rowStyle: {
    type: [Function, String],
    default: undefined
  },
  // 	单元格的 className 的回调方法，也可以使用字符串为所有单元格设置一个固定的 className
  cellClassName: {
    type: [Function, String],
    default: undefined
  },
  // 单元格的 style 的回调方法，也可以使用一个固定的 Object 为所有单元格设置一样的 Style
  cellStyle: {
    type: [Function, Object],
    default: undefined
  },
  // 表头行的 className 的回调方法，也可以使用字符串为所有表头行设置一个固定的 className
  headerRowClassName: {
    type: [Function, String],
    default: undefined
  },
  // 表头行的 style 的回调方法，也可以使用一个固定的 Object 为所有表头行设置一样的 Style
  headerRowStyle: {
    type: [Function, Object],
    default: undefined
  },
  // 表头单元格的 className 的回调方法，也可以使用字符串为所有表头单元格设置一个固定的 classNam
  headerCellClassName: {
    type: [Function, String],
    default: undefined
  },
  // 表头单元格的 style 的回调方法，也可以使用一个固定的 Object 为所有表头单元格设置一样的 Style
  headerCellStyle: {
    type: [Function, Object],
    default: undefined
  },
  // 行数据的 Key，用来优化 Table 的渲染； 在使用reserve-selection功能与显示树形数据时，该属性是必填的。 类型为 String 时，支持多层访问：user.info.id，但不支持 user.info[0].id，此种情况请使用 Function
  rowKey: {
    type: [Function, String],
    default: undefined
  },
  // 空数据时显示的文本内容， 也可以通过 #empty 设置
  emptyText: {
    type: String,
    default: '暂无数据'
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
  // 默认的排序列的 prop 和顺序。 它的 prop 属性指定默认的排序的列，order 指定默认排序的顺序
  defaultSort: {
    type: Object,
    default: undefined
  },
  // tooltip effect 属性
  tooltipEffect: {
    type: String,
    default: undefined
  },
  // 是否在表尾显示合计行
  showSummary: {
    type: Boolean,
    default: undefined
  },
  // 合计行第一列的文本
  sumText: {
    type: String,
    default: '合计'
  },
  // 自定义的合计计算方法
  summaryMethod: {
    type: Function,
    default: undefined
  },
  // 合并行或列的计算方法
  spanMethod: {
    type: Function,
    default: undefined
  },
  // 	在多选表格中，当仅有部分行被选中时，点击表头的多选框时的行为。 若为 true，则选中所有行；若为 false，则取消选择所有行
  selectOnIndeterminate: {
    type: Boolean,
    default: undefined
  },
  // 展示树形数据时，树节点的缩进
  indent: {
    type: Number,
    default: undefined
  },
  // 是否懒加载子节点数据
  lazy: {
    type: Boolean,
    default: undefined
  },
  // 加载子节点数据的函数，lazy 为 true 时生效，函数第二个参数包含了节点的层级信息
  load: {
    type: Function,
    default: undefined
  },
  // 渲染嵌套数据的配置选项
  treeProps: {
    type: Object,
    default: undefined
  }
})

const tableRef = ref()

const { tableBorder, tableStripe } = mapGetters(['tableBorder', 'tableStripe'])

const tBorder = computed(() => (isNotBlank(props.border) ? props.border : tableBorder.value))
const tStripe = computed(() => (isNotBlank(props.stripe) ? props.stripe : tableStripe.value))

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

// 获取表格中的列
// 不能直接 const colums = tableRef.value.$refs.tableHeader.columns.setup时，ref里面还是空的
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

// 当用户手动勾选数据行的 Checkbox 时触发的事件
function select(selection, row) {
  emit('select', selection, row)
}

// 当用户手动勾选全选 Checkbox 时触发的事件
function selectAll(selection) {
  emit('selectAll', selection)
}

// 当选择项发生变化时会触发该事件
function selectionChange(selection) {
  emit('selectionChange', selection)
}

// 当单元格 hover 进入时会触发该事件
function cellMouseEnter(row, column, cell, event) {
  emit('cellMouseEnter', row, column, cell, event)
}

// 	当单元格 hover 退出时会触发该事件
function cellMouseLeave(row, column, cell, event) {
  emit('cellMouseLeave', row, column, cell, event)
}

// 当某个单元格被点击时会触发该事件
function cellClick(row, column, cell, event) {
  emit('cellClick', row, column, cell, event)
}

// 当某个单元格被双击击时会触发该事件
function cellDblclick(row, column, cell, event) {
  emit('cellDblclick', row, column, cell, event)
}

// 当某个单元格被鼠标右键点击时会触发该事件
function cellContext(row, column, cell, event) {
  emit('cellContex', row, column, cell, event)
}

// 当某一行被点击时会触发该事件
function rowClick(row, column, event) {
  emit('rowClick', row, column, event)
}

// 当某一行被鼠标右键点击时会触发该事件
function rowContextmenu(row, column, event) {
  emit('rowContextmenu', row, column, event)
}

// 当某一行被双击时会触发该事件
function rowDblclick(row, column, event) {
  emit('rowDblclick', row, column, event)
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
  emit('currentChange', currentRow, oldCurrentRow)
}

// 	当拖动表头改变了列的宽度的时候会触发该事件
function headerDragend(newWidth, oldWidth, column, event) {
  emit('dragend', newWidth, oldWidth, column, event)
}

// 当用户对某一行展开或者关闭的时候会触发该事件（展开行时，回调的第二个参数为 expandedRows；树形表格时第二参数为 expanded）
function expandChange(row, expandedRowsOrExpanded) {
  // 配合组件el-expand-table-column 使用
  const keys = props.expandRowKeys
  const index = keys.indexOf(row[props.rowKey])
  if (index > -1) {
    keys.splice(index, 1)
  } else {
    keys.push(row[props.rowKey])
  }
  emit('expandChange', row, expandedRowsOrExpanded)
}

defineExpose({
  getColumns,
  clearSelection,
  toggleRowSelection,
  toggleAllSelection,
  toggleRowExpansion,
  setCurrentRow,
  clearSort,
  clearFilter,
  doLayout,
  sort
})
</script>
