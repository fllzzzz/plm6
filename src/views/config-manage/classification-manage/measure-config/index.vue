<template>
  <div class="app-container">
    <mHeader ref="header" />
    <common-table
      ref="tableRef"
      v-if="tableRefresh"
      v-loading="crud.loading"
      border
      :data="list"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :span-method="spanMethod"
      :highlight-current-row="false"
      style="width: 100%;"
    >
      <el-table-column key="index" type="index" label="序号" align="center" width="60" />
      <el-table-column v-if="columns.visible('level1')" prop="level1" label="一级" align="left" min-width="160px">
        <template #header>
          <el-tooltip
            effect="light"
            :content="`一级科目（名称-编号）`"
            placement="top"
          >
            <div>
              <span>一级科目</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <div class="subjectName">
            <span>{{ `${scope.row.fullName[0]}-${scope.row.fullSerialNumber[0]}` }}</span>
          </div>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('level2')" prop="level2" label="二级" align="left" min-width="160px">
        <template #header>
          <el-tooltip
            effect="light"
            :content="`二级科目（名称-编号）`"
            placement="top"
          >
            <div>
              <span>二级科目</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <div class="subjectName">
            <template v-if="scope.row.fullName.length > 1">
              <span>{{ `${scope.row.fullName[1]}-${scope.row.fullSerialNumber[1]}` }}</span>
            </template>
          </div>
        </template>
      </el-table-column>
      <el-table-column  v-if="columns.visible('level3')" prop="level3" label="三级" align="left" min-width="160px">
        <template #header>
          <el-tooltip
            effect="light"
            :content="`三级科目（名称-编号）`"
            placement="top"
          >
            <div style="margin:0 10px">
              <span>三级科目</span>
              <i class="el-icon-info" />
            </div>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <div class="subjectName">
            <template v-if="scope.row.fullName.length > 2">
              <span>{{ `${scope.row.fullName[2]}-${scope.row.fullSerialNumber[2]}` }}</span>
            </template>
          </div>
        </template>
      </el-table-column>
      <el-table-column  v-if="columns.visible('measureUnit')" key="measureUnit" prop="measureUnit" :show-overflow-tooltip="true" label="计量单位" min-width="100px" align="center">
        <template v-slot="scope">
          <div>{{ emptyTextFormatter(scope.row.measureUnit) }}</div>
        </template>
      </el-table-column>
      <el-table-column  v-if="columns.visible('measurePrecision')" key="measurePrecision" prop="measurePrecision" :show-overflow-tooltip="true" label="小数精度(计量)" min-width="100px" align="center">
        <template v-slot="scope">
          <div>{{ emptyTextFormatter(scope.row.measurePrecision) }}</div>
        </template>
      </el-table-column>
      <el-table-column  v-if="columns.visible('accountingUnit')" key="accountingUnit" prop="accountingUnit" :show-overflow-tooltip="true" label="核算单位" min-width="100px" align="center">
        <template v-slot="scope">
          <div>{{ emptyTextFormatter(scope.row.accountingUnit) }}</div>
        </template>
      </el-table-column>
      <el-table-column  v-if="columns.visible('accountingPrecision')" key="accountingPrecision" prop="accountingPrecision" :show-overflow-tooltip="true" label="小数精度(核算)" min-width="100px" align="center">
        <template v-slot="scope">
          <div>{{ emptyTextFormatter(scope.row.accountingPrecision) }}</div>
        </template>
      </el-table-column>
      <el-table-column key="outboundUnit" prop="outboundUnit" :show-overflow-tooltip="true" label="出库单位" min-width="100px" align="center">
        <template v-slot="scope">
          <div>{{ emptyTextFormatter(measureTypeEnum.VL[scope.row.outboundMethod]) }}</div>
        </template>
      </el-table-column>
    </common-table>
  </div>
</template>

<script setup>
import crudApi from '@/api/config/classification-manage/measure-config'
import { nextTick, onUnmounted, ref } from 'vue'
import { useStore } from 'vuex'
import { isNotBlank, emptyTextFormatter } from '@data-type'
import { measureTypeEnum } from '@enum-ms/wms'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import { computed } from '@vue/reactivity'
import mHeader from './module/header'

const store = useStore()

// crud交由presenter持有
const permission = {
  get: ['config_class_unitConfig:get'],
  edit: ['config_class_unitConfig:edit']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const tableRefresh = ref(true)
const { maxHeight } = useMaxHeight()

const { crud, columns } = useCRUD({
  title: '计量配置',
  sort: ['sort.asc', 'id.desc'],
  permission: { ...permission },
  crudApi: { ...crudApi },
  optShow: { ...optShow },
  hasPagination: false,
  dataPath: null
}, tableRef)

const list = computed(() => {
  let list = crud.data
  if (crud.query && isNotBlank(crud.query.nameOrCode)) {
    const nameOrCode = crud.query.nameOrCode
    list = list.filter(v => v.fullName.some(v => v.indexOf(nameOrCode) > -1) || v.fullSerialNumber.some(v => v.indexOf(nameOrCode) > -1))
  }
  return mergeCells(list)
})

onUnmounted(() => {
  // TODO:在离开该页面时重新拉取科目列表以获取最新的数据
  store.dispatch('interfaceCache/fetchSubjectSimple')
})

// 合并单元格
function mergeCells(list) {
  if (list.length === 0) return list
  const row = [[], []]
  const id = [-1, -1]
  list.forEach(v => {
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
  th, td {
    padding: 0;
  }
  .el-tooltip {
    line-height: 40px;
  }
  .cell {
    line-height: 30px;
  }
  th:first-child .cell, td:first-child .cell {
    padding: 0;
  }
  .el-table__body .el-input__inner, .el-table__body .el-textarea__inner {
    border-radius: 0
  }
}
</style>
