<template>
  <div class="app-container">
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="table"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      row-key="rowId"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" fixed />
      <el-table-column v-if="columns.visible('date')" prop="date" label="排产日期" align="center" width="140px">
        <template #default="{ row }">
          <span v-parse-time="'{y}-{m}-{d}'">{{ row.date }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('schedulingQuantity')"
        :show-overflow-tooltip="true"
        prop="schedulingQuantity"
        :label="`排产量（件/${unitObj.unit}）`"
        align="left"
      >
        <template #default="{ row }">
          <el-tag type="info" effect="plain" style="width: 95%">
            <span v-empty-text style="color: #409eff">{{ row.schedulingQuantity }}</span> /
            <span v-empty-text>{{ row.totalSchedulingMete }}</span>
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('taskQuantity')"
        :show-overflow-tooltip="true"
        prop="taskQuantity"
        :label="`已下发（件/${unitObj.unit}）`"
        align="left"
      >
        <template #default="{ row }">
          <el-tag type="info" effect="plain" style="width: 95%">
            <span v-empty-text style="color: #67c23a">{{ row.taskQuantity }}</span> /
            <span v-empty-text>{{ row.totalTaskMete }}</span>
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('unschedulingQuantity')"
        :show-overflow-tooltip="true"
        prop="unschedulingQuantity"
        :label="`未下发（件/${unitObj.unit}）`"
        align="left"
      >
        <template #default="{ row }">
          <el-tag type="info" effect="plain" style="width: 95%">
            <span v-empty-text style="color: #f56c6c">{{ row.unschedulingQuantity }}</span> /
            <span v-empty-text>{{ row.unschedulingMete }}</span>
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column
        v-if="checkPermission([...permission.detail])"
        label="操作"
        width="120px"
        align="center"
        fixed="right"
      >
        <template v-slot="scope">
          <common-button size="mini" type="primary" icon="el-icon-s-operation" @click="showDetail(scope.row)" />
        </template>
      </el-table-column>
    </common-table>
    <!-- 详情 -->
    <common-drawer
      v-model:visible="drawerVisible"
      :title="`${parseTime(detailRow.date, '{y}年{m}月{d}日')} ：排产详情`"
      direction="rtl"
      size="100%"
      :before-close="
        () => {
          drawerVisible = false
        }
      "
    >
      <template #content>
        <m-detail :details="detailRow" :query="crud.query" @refresh="crud.toQuery" />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/scheduling-manage/task/machine-part'
import { ref, computed } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'
import { parseTime } from '@/utils/date'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import mDetail from '../components/task-details'
import mHeader from '../components/common-header'

// crud交由presenter持有
const permission = {
  get: ['machinePartTask:get'],
  detail: ['machinePartTask:detail']
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
    title: '零件任务',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi },
    hasPagination: false
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: false })

const detailRow = ref({})
const drawerVisible = ref(false)

function showDetail(row) {
  detailRow.value = Object.assign({}, row)
  drawerVisible.value = true
}

const productType = componentTypeEnum.MACHINE_PART.V

const unitObj = computed(() => {
  return useProductSummaryMeteUnit({
    productType: productType
  })
})

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v, i) => {
    v.rowId = i + '' + Math.random()
    v.schedulingQuantity = v.schedulingQuantity || 0
    v.taskQuantity = v.taskQuantity || 0
    v.unschedulingQuantity = v.schedulingQuantity - v.taskQuantity
    v.productType = productType
    v.totalSchedulingMete = useProductMeteConvert({
      productType: v.productType,
      length: { num: v.totalSchedulingLength, to: unitObj.value.unit, dp: unitObj.value.dp },
      weight: { num: v.totalSchedulingNetWeight, to: unitObj.value.unit, dp: unitObj.value.dp }
    })
    v.totalTaskMete = useProductMeteConvert({
      productType: v.productType,
      length: { num: v.totalTaskLength, to: unitObj.value.unit, dp: unitObj.value.dp },
      weight: { num: v.totalTaskNetWeight, to: unitObj.value.unit, dp: unitObj.value.dp }
    })
    v.unschedulingMete = (v.totalSchedulingMete - v.totalTaskMete).toFixed(unitObj.value.DP)
    return v
  })
}
</script>
