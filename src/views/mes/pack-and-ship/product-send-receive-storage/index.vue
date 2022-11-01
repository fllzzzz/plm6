<template>
  <div class="app-container">
    <mHeader />
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :stripe="false"
      :data-format="dataFormat"
      show-summary
      :summary-method="getSummaries"
      style="width: 100%"
    >
      <el-table-column type="index" prop="index" label="序号" align="center" width="60" />
      <el-table-column key="project" prop="project" v-if="columns.visible('project')" :show-overflow-tooltip="true" label="项目" align="left" />
      <el-table-column key="list" prop="list" v-if="columns.visible('list')" :show-overflow-tooltip="true" label="清单数（件/kg）" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.quantity+' / '+scope.row.totalNetWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column key="into" prop="into" v-if="columns.visible('into')" :show-overflow-tooltip="true" label="入库量（件/kg）" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.intoQuantity+' / '+scope.row.intoWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column key="out" prop="out" v-if="columns.visible('out')" :show-overflow-tooltip="true" label="使用量（件/kg）" align="center" >
        <template v-slot="scope">
          <span>{{ scope.row.outQuantity+' / '+scope.row.outWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column key="stock" prop="stock" v-if="columns.visible('stock')" :show-overflow-tooltip="true" label="库存（件/kg）" align="center">
        <template v-slot="scope">
          <span>{{ scope.row.stockQuantity+' / '+scope.row.stockWeight }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column label="操作" width="130px" align="center" fixed="right">
        <template v-slot="scope">
          <common-button
            size="mini"
            icon="el-icon-view"
            type="primary"
            @click="openDetail(scope.row, 'detail')"
          />
        </template>
      </el-table-column>
    </common-table>
    <pagination />
    <component :is="typeDetail" :showType="showType" v-model="detailVisible" :detailQuery="detailQuery" :detailInfo="currentRow"/>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/pack-and-ship/product-receive-send-storage'
import { ref, nextTick } from 'vue'

import { deliveryInstallListPM as permission } from '@/page-permission/project'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'

import mHeader from './module/header'
import pagination from '@crud/Pagination'
import typeDetail from './module/type-detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const detailVisible = ref(false)
const showType = ref('detail')
const currentRow = ref({})
const detailQuery = ref({})
const { crud, columns } = useCRUD(
  {
    title: '制成品入发存',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: crudApi,
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.product-send-receive-storage',
  paginate: true,
  extraHeight: 40
})

const dataFormat = ref([
  ['project', 'parse-project']
])

function openDetail(row, show) {
  showType.value = show
  currentRow.value = row.sourceRow
  detailQuery.value = {
    // projectId: row.sourceRow.project.id,
    year: crud.query.year
  }
  nextTick(() => {
    detailVisible.value = true
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
    if (column.property === 'list' || column.property === 'into' || column.property === 'out' || column.property === 'stock') {
      const valueKeys = column.property === 'list' ? 'quantity' : column.property + 'Quantity'
      const values = data.map((item) => Number(item.sourceRow?.[valueKeys]))
      let valuesSum = 0
      const valueWeightKeys = column.property === 'list' ? 'totalNetWeight' : column.property + 'Weight'
      const valueWeight = data.map((item) => Number(item.sourceRow?.[valueWeightKeys]))
      let valueWeightSum = 0
      if (!values.every((value) => isNaN(value))) {
        valuesSum = values.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
      if (!valueWeight.every((value) => isNaN(value))) {
        valueWeightSum = valueWeight.reduce((prev, curr) => {
          const value = Number(curr)
          if (!isNaN(value)) {
            return prev + curr
          } else {
            return prev
          }
        }, 0)
      }
      sums[index] = valuesSum + ' / ' + valueWeightSum
    }
  })
  return sums
}
</script>
<style lang="scss" scoped>
::v-deep(.el-table .abnormal-row) {
  background: #f0f9eb;
}
.customer-table {
  ::v-deep(th) {
    border: none;
  }
  ::v-deep(td) {
    border: none;
  }
  ::v-deep(th.is-leaf) {
    border: none;
  }
  &::before {
    width: 0;
  }
}
::v-deep(.el-progress-bar__inner){
  text-align: center;
  max-width: 100%;
}
</style>
