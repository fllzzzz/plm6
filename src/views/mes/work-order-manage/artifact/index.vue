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
      :dataFormat="dataFormat"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="70">
        <template #default="{ row, $index }">
          <table-cell-tag :show="row.boolPrinted" name="已打印" type="printed" />
          <span>{{ $index + 1 }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('scheduleTime')"
        :show-overflow-tooltip="true"
        prop="scheduleTime"
        label="排产日期"
        width="110px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('scheduleOrder')"
        :show-overflow-tooltip="true"
        prop="scheduleOrder"
        label="任务工单号"
        min-width="110px"
        align="center"
      >
        <template #default="{ row }">
          <table-cell-tag :show="row.boolIntellect" name="智" />
          <span>{{ row.scheduleOrder }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('userName')"
        :show-overflow-tooltip="true"
        prop="userName"
        label="排产人"
        min-width="110px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('productType')"
        :show-overflow-tooltip="true"
        prop="productType"
        label="类型"
        width="100px"
        align="center"
      >
        <template #default="{ row }">
          <el-tag :type="row.productType & componentTypeEnum.ASSEMBLE.V ? '' : 'success'" effect="plain">
            {{ componentTypeEnum.VL[row.productType] }}
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('workshopName')"
        :show-overflow-tooltip="true"
        prop="workshopName"
        label="车间"
        min-width="110px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('productionLineName')"
        :show-overflow-tooltip="true"
        prop="productionLineName"
        label="生产线"
        min-width="110px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('taskQuantity')"
        :show-overflow-tooltip="true"
        prop="taskQuantity"
        label="任务数（件）"
        min-width="110px"
        align="center"
      />
      <el-table-column
        v-if="columns.visible('taskMete')"
        :show-overflow-tooltip="true"
        prop="taskMete"
        label="任务量（kg）"
        min-width="110px"
        align="center"
      />
      <el-table-column label="操作" width="110px" align="center">
        <template #default="{ row }">
          <common-button type="primary" size="mini" @click="showDetail(row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/work-order-manage/artifact.js'
import { ref } from 'vue'

import { componentTypeEnum, artifactProductLineEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
import mHeader from './module/header'

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
    title: '结构工单',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const dataFormat = ref([['scheduleTime', ['parse-time', '{y}-{m}-{d}']]])

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    v.boolPrinted = Boolean(v.printQuantity)
    v.boolIntellect = v.productionLineTypeEnum === artifactProductLineEnum.INTELLECT.V
    return v
  })
}

const itemInfo = ref()

function showDetail(row) {
  itemInfo.value = Object.assign({}, row)
}
</script>
