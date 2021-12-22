<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      show-summary
      :summary-method="getSummaries"
      style="width: 100%"
      @sort-change="crud.handleSortChange"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name')"
        key="name"
        prop="name"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="名称"
        min-width="120px"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('material')"
        key="material"
        prop="material"
        sortable="custom"
        :show-overflow-tooltip="true"
        label="材质"
        min-width="80px"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('changeArea')"
        key="changeArea"
        prop="changeArea"
        sortable="custom"
        :label="`涂装表面积(㎡)`"
        align="left"
        min-width="80px"
      >
        <template #header>
          <el-tooltip content="双击可编辑" placement="top" effect="light">
            <span>
              <span>涂装表面积(㎡)</span>
              <span style="margin-left: 5px"><i class="el-icon-edit"></i></span>
            </span>
          </el-tooltip>
        </template>
        <template v-slot="scope">
          <span v-empty-text style="cursor: pointer" @dblclick="toEditArea(scope.row)">{{
            toFixed(scope.row.changeArea, DP.COM_AREA__M2)
          }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('paintCategory')"
        key="paintCategory"
        prop="paintCategory"
        sortable="custom"
        label="油漆类别"
        align="left"
        min-width="80px"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.paintCategory }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('thickness')"
        key="thickness"
        prop="thickness"
        sortable="custom"
        :label="`干膜厚度(μm)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ toFixed(scope.row.thickness, DP.COM_T__MM) }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('volumeSolids')"
        key="volumeSolids"
        prop="volumeSolids"
        sortable="custom"
        :label="`体积固体份(%)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.volumeSolids }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('loss')"
        key="loss"
        prop="loss"
        sortable="custom"
        :label="`损耗(%)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ scope.row.loss }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('measure')"
        key="measure"
        prop="measure"
        sortable="custom"
        :label="`实际用量(L)`"
        align="center"
        min-width="80px"
      >
        <template v-slot="scope">
          <span v-empty-text>{{ toFixed(scope.row.measure, DP.COM_VOLUME__L) }}</span>
        </template>
      </el-table-column>
      <el-table-column label="操作" width="160px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button size="mini" type="primary" icon="el-icon-edit" @click.stop="toEditForm(scope.row)" />
        </template>
      </el-table-column>
    </common-table>
    <edit-area v-model:visible="editAreaVisible" :info="itemInfo" @refresh="crud.toQuery"></edit-area>
    <edit-form v-model:visible="editFormVisible" :info="itemInfo" @refresh="crud.toQuery"></edit-form>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/production-manage/dashboard/painting'
import { ref } from 'vue'

import { DP } from '@/settings/config'
import { toFixed } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import mHeader from './module/header'
import editArea from './module/edit-area.vue'
import editForm from './module/edit-form.vue'

// crud交由presenter持有
const permission = {
  get: [''],
  edit: [''],
  add: [''],
  del: ['']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '涂装计算',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.originChangeArea = v.changeArea
    return v
  })
}

const editAreaVisible = ref(false)
const editFormVisible = ref(false)
const itemInfo = ref({})

function toEditArea(row) {
  itemInfo.value = row
  editAreaVisible.value = true
}

function toEditForm(row) {
  itemInfo.value = row
  editFormVisible.value = true
}

function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (column.property === 'measure') {
      const values = data.map((item) => Number(item[column.property]))
      if (!values.every((value) => isNaN(value))) {
        sums[index] = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
        sums[index] = sums[index].toFixed(DP.COM_VOLUME__L)
      }
    }
  })
  return sums
}
</script>
