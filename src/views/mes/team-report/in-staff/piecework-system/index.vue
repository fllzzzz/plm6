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
      row-key="id"
      :summary-method="getSummaries"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <belonging-info-columns :columns="columns" showWorkshop showProductionLine showTeam />
      <el-table-column
        v-if="columns.visible('processName')"
        key="processName"
        prop="processName"
        :show-overflow-tooltip="true"
        label="工序"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.processName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('wageQuotaType')"
        key="wageQuotaType"
        prop="wageQuotaType"
        :show-overflow-tooltip="true"
        label="单位"
        align="center"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ wageQuotaTypeEnum.V[scope.row.wageQuotaType].meteUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('mate')"
        key="mate"
        prop="mate"
        align="center"
        :show-overflow-tooltip="true"
        label="生产量"
        min-width="100px"
      >
        <template v-slot="scope">
          <span>{{ scope.row.mate }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('price')"
        key="price"
        prop="price"
        align="center"
        :show-overflow-tooltip="true"
        label="工资总额"
        min-width="100px"
      >
        <template v-slot="scope">
          <span v-to-fixed="'YUAN'">{{ scope.row.price }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column v-permission="[...permission.edit, ...permission.del]" label="操作" width="100px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button type="primary" size="mini" @click="showDetail(scope.row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <mDetail v-model:visible="detailVisible" :info="itemInfo"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/team-report/in-staff/piecework-system'
import { ref, provide } from 'vue'

import { wageQuotaTypeEnum } from '@enum-ms/mes'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import mHeader from './module/header'
import mDetail from './module/detail'

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
    title: '编内-计件制',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })

provide('query', crud.query)

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    return v
  })
}

function getSummaries(param) {
  const { columns, data } = param
  const sums = []
  columns.forEach((column, index) => {
    if (index === 0) {
      sums[index] = '合计'
      return
    }
    if (column.property === 'price') {
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
      }
    }
  })
  return sums
}

const detailVisible = ref(false)
const itemInfo = ref({})

function showDetail(row) {
  itemInfo.value = row
  detailVisible.value = true
}
</script>
