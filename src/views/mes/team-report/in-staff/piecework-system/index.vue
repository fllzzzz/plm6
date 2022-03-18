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
      row-key="rowId"
      :summary-method="getSummaries"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <belonging-info-columns :columns="columns" showFactory showWorkshop showProductionLine />
      <template v-for="item in processList" :key="item.id">
        <el-table-column align="center" :label="item.name">
          <template #default="{ row }">
            <span style="cursor: pointer" @click="showDetail(row, item.id)">{{ row.processMap[item.id]?.price }}</span>
          </template>
        </el-table-column>
      </template>
      <!--编辑与删除-->
      <el-table-column v-permission="[...permission.detail]" label="操作" width="100px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button type="primary" size="mini" @click="showDetail(row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <mDetail v-model:visible="detailVisible" :info="itemInfo" />
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/team-report/in-staff/piecework-system'
import { ref, provide, defineProps } from 'vue'

import { teamAttributeEnum } from '@enum-ms/mes'
// import { inStaffPieceworkSystemPM as permission } from '@/page-permission/mes'
import { arr2obj } from '@/utils/convert/type'
import { deepClone } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
// import useWageQuotaMeteConvert from '@compos/mes/use-wage-quota-mete-convert'
import belongingInfoColumns from '@comp-mes/table-columns/belonging-info-columns'
import mHeader from './module/header'
import mDetail from './module/detail'

const props = defineProps({
  organizationType: {
    type: Boolean,
    default: teamAttributeEnum.IN_STAFF.V
  }
})

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const permission = {
  get: []
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
provide('organizationType', props.organizationType)

const processList = ref([])

CRUD.HOOK.beforeRefresh = () => {
  crud.query.organizationType = props.organizationType
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  processList.value = res.data.content.length && res.data.content[0]?.processPrice
  res.data.content = res.data.content.map((v, i) => {
    v.rowId = i + '' + Math.random()
    v.processMap = arr2obj(v.processPrice, 'id')
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
        sums[index] = sums[index].toFixed(2)
      }
    }
  })
  return sums
}

const detailVisible = ref(false)
const itemInfo = ref({})

function showDetail(row, processId) {
  itemInfo.value = deepClone(row)
  if (processId) itemInfo.value.processId = processId
  detailVisible.value = true
}
</script>
