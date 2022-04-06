<template>
  <div class="app-container">
    <mHeader v-bind="$attrs" ref="header" />
    <common-table
      ref="tableRef"
      v-if="tableRefresh"
      v-loading="crud.loading"
      border
      :data="list"
      :data-format="columnsDataFormat"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :span-method="spanMethod"
      :highlight-current-row="false"
      row-key="id"
    >
      <el-table-column key="index" type="index" label="序号" align="center" width="60" />
      <el-table-column v-if="columns.visible('level1')" prop="level1" label="一级" align="left" min-width="160px">
        <template #header>
          <el-tooltip effect="light" :content="`一级科目（名称-编号）`" placement="top">
            <div>
              <span>一级科目</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <div class="classifyName">
            <span>{{ `${scope.row.fullName[0]}-${scope.row.fullSerialNumber[0]}` }}</span>
          </div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('level2')" prop="level2" label="二级" align="left" min-width="160px">
        <template #header>
          <el-tooltip effect="light" :content="`二级科目（名称-编号）`" placement="top">
            <div>
              <span>二级科目</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <div class="classifyName">
            <template v-if="scope.row.fullName.length > 1">
              <span>{{ `${scope.row.fullName[1]}-${scope.row.fullSerialNumber[1]}` }}</span>
            </template>
          </div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('level3')" prop="level3" label="三级" align="left" min-width="160px">
        <template #header>
          <el-tooltip effect="light" :content="`三级科目（名称-编号）`" placement="top">
            <div style="margin: 0 10px">
              <span>三级科目</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <div class="classifyName">
            <template v-if="scope.row.fullName.length > 2">
              <span>{{ `${scope.row.fullName[2]}-${scope.row.fullSerialNumber[2]}` }}</span>
            </template>
          </div>
        </template>
      </el-table-column>
      <el-table-column key="rdRate" prop="rdRate" :show-overflow-tooltip="true" label="研发费占比" min-width="100px" align="center" />
    </common-table>
  </div>
</template>

<script setup>
import crudApi from '@/api/config/wms/high-tech-rd-fee'
import { highTechRDFeeConfCPM as permission } from '@/page-permission/wms'

import { nextTick, onUnmounted, ref, computed } from 'vue'
import { isNotBlank } from '@data-type'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import mHeader from './module/header'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const tableRefresh = ref(true)
const columnsDataFormat = [['rdRate', ['suffix', ' %']]]
const { maxHeight } = useMaxHeight()

const { crud, columns } = useCRUD(
  {
    title: '高新研发费配置',
    sort: ['sort.asc', 'id.desc'],
    permission: { ...permission },
    crudApi: { ...crudApi },
    optShow: { ...optShow },
    hasPagination: false
  },
  tableRef
)

const list = computed(() => {
  let list = crud.data
  if (crud.query && isNotBlank(crud.query.nameOrCode)) {
    const nameOrCode = crud.query.nameOrCode
    list = list.filter(
      (v) => v.fullName.some((v) => v.indexOf(nameOrCode) > -1) || v.fullSerialNumber.some((v) => v.indexOf(nameOrCode) > -1)
    )
  }
  return mergeCells(list)
})

onUnmounted(() => {
  // TODO:在离开该页面时重新拉取科目列表以获取最新的数据
  // store.dispatch('interfaceCache/fetchSubjectSimple')
})

// 合并单元格
function mergeCells(list) {
  if (list.length === 0) return list
  const row = [[], []]
  const id = [-1, -1]
  list.forEach((v) => {
    for (let i = 0; i < row.length; i++) {
      const newId = v.fullId[i]
      const oldId = id.length > i ? id[i] : undefined
      if (newId === oldId && isNotBlank(oldId)) {
        row[i][row[i].length - 1]++
      } else {
        row[i].push(1)
        id[i] = newId
      }
    }
  })
  row[0].reduce((total, cur) => {
    list[total].name1_rowSpan = cur
    total += cur
    return total
  }, 0)
  row[1].reduce((total, cur) => {
    list[total].name2_rowSpan = cur
    total += cur
    return total
  }, 0)
  setTimeout(() => {
    tableRefresh.value = false
    nextTick(() => {
      tableRefresh.value = true
    })
  }, 1000)
  return list
}

/**
 * columnIndex === 2 时 colspan 的 取值（即二级科目列）
 * index = 0 则 colspan = 0 只存在一级科目，当前列隐藏（被一级占用）
 * index = 1 则 colspan = 2 二级科目，跨两列（占用三级科目列）
 * index = 2 则 colspan = 1 存在三级科目，当前列不做任何操作
 */
const col2Arr = [0, 2, 1]
function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (columnIndex === 1) {
    return {
      rowspan: row.name1_rowSpan || 0,
      colspan: row.fullName.length === 1 ? 3 : 1
    }
  }
  if (columnIndex === 2) {
    return {
      rowspan: row.name2_rowSpan || 0,
      colspan: col2Arr[row.fullName.length - 1]
    }
  }
  if (columnIndex === 3) {
    return {
      rowspan: 1,
      colspan: row.fullName.length < 3 ? 0 : 1
    }
  }
}
</script>

<style lang="scss" scoped>
::v-deep(.el-table) {
  th,
  td {
    padding: 0;
  }
  .el-tooltip {
    line-height: 40px;
  }
  .cell {
    line-height: 30px;
  }
  th:first-child .cell,
  td:first-child .cell {
    padding: 0;
  }
  .el-table__body .el-input__inner,
  .el-table__body .el-textarea__inner {
    border-radius: 0;
  }
}
</style>
