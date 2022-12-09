<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading || !loaded"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      :stripe="false"
      :span-method="spanMethod"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
      <el-table-column
        v-if="columns.visible('workshopName')"
        :show-overflow-tooltip="true"
        align="center"
        prop="workshopName"
        label="车间"
        min-width="120px"
        fixed="left"
      >
        <template #default="{ row }">
          <span>{{ row.workshopName }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('productionLineName')"
        :show-overflow-tooltip="true"
        align="center"
        prop="productionLineName"
        label="生产线"
        min-width="120px"
        fixed="left"
      >
        <template #default="{ row }">
          <span>{{ row.productionLineName }}</span>
        </template>
      </el-table-column>
      <template v-for="item in process" :key="item.id">
        <el-table-column v-if="item.productType & bridgeComponentTypeEnum .BOX.V" :label="item.name" align="center" width="110px">
          <template #default="{ row }">
            <el-tooltip v-if="row.processData[item.id]" content="不合格数 / 检验数" placement="top">
              <span>
                <span class="tc-danger">{{ row.processData[item.id]?.unqualifiedNumber }}</span>
                <span> / </span>
                <span>{{ row.processData[item.id]?.totalNumberInspection }}</span>
              </span>
            </el-tooltip>
          </template>
        </el-table-column>
      </template>
      <el-table-column
        v-if="columns.visible('passRate')"
        :show-overflow-tooltip="true"
        align="center"
        prop="passRate"
        label="通过率"
        width="100px"
        fixed="right"
      >
        <template #default="{ row }">
          <span class="tc-primary">{{ row.passRate }}</span>
        </template>
      </el-table-column>
      <el-table-column label="操作" v-permission="permission.detail" width="100px" align="center" fixed="right">
        <template #default="{ row }">
          <common-button size="mini" type="primary" @click="showDetail(row)">查看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <!-- <pagination /> -->
    <m-detail v-model:visible="detailVisible" :info="itemInfo" :query="crud.query"></m-detail>
  </div>
</template>

<script setup>
import crudApi from '@/api/bridge/QHSE-manage/production-line-report'
import { ref } from 'vue'

import { bridgeQhseProductionLineReportPM as permission } from '@/page-permission/bridge'
import { bridgeComponentTypeEnum } from '@enum-ms/bridge'

import useProcess from '@compos/store/use-process'
import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
// import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mDetail from './module/detail'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '',
    sort: [],
    dataPath: '',
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })

function spanMethod({ row, column, rowIndex, columnIndex }) {
  if (columnIndex === 1) {
    if (rowIndex > 0 && crud.data[rowIndex].workshopId === crud.data[rowIndex - 1].workshopId) {
      return {
        rowspan: 0,
        colspan: 0
      }
    } else {
      return {
        rowspan: row.rowspan,
        colspan: 1
      }
    }
  }
}

const rowspanObj = ref({})
CRUD.HOOK.handleRefresh = (crud, res) => {
  rowspanObj.value = {}
  res.data = res.data.map((v, rowIndex) => {
    v.passRate = v.passRate ? v.passRate + '%' : v.passRate
    v.rowspan = getRowsSpanLength(res.data, v.workshopId)
    return v
  })
}

function getRowsSpanLength(list, target) {
  if (!rowspanObj.value[target]) {
    rowspanObj.value[target] = list.filter((v) => v.workshopId === target).length
  }
  return rowspanObj.value[target]
}

const { loaded, process } = useProcess()

const detailVisible = ref(false)
const itemInfo = ref({})

function showDetail(row) {
  detailVisible.value = true
  itemInfo.value = row
}
</script>
