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
      row-key="id"
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
        :label="`已排产量（件/${unitObj.unit}）`"
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
        :label="`未排产量（件/${unitObj.unit}）`"
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
        v-if="checkPermission([...permission.detail, ...permission.download])"
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
        <m-detail :details="detailRow" @refresh="crud.toQuery" />
      </template>
    </common-drawer>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/scheduling-manage/task/machine-part'
import { ref, provide, computed } from 'vue'

import { componentTypeEnum } from '@enum-ms/mes'
import { parseTime } from '@/utils/date'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import useProductMeteConvert from '@compos/mes/use-product-mete-convert'
import useProductSummaryMeteUnit from '@compos/mes/use-product-summary-mete-unit'
import mDetail from '../components/task-details'
import mHeader from './module/header'

// crud交由presenter持有
const permission = {
  get: ['taskAssignDetail:get'],
  print: ['taskAssignDetail:print'],
  detail: ['taskAssignDetail:detail'],
  download: ['taskAssignDetail:download']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

provide('needTableColumns', [
  { label: '编号', width: '120px', field: 'serialNumber' },
  { label: '规格', width: '140px', field: 'specification' }
  // { label: `单重\n(kg)`, width: '80px', field: 'weight', toFixed: true, DP: DP.COM_WT__KG }
])

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
  crud.data = [] // 不清空不更新表格
  res.data.content = res.data.content.map((v) => {
    v.processType = crud.query.processType
    v.schedulingQuantity = v.schedulingQuantity || 0
    v.taskQuantity = v.taskQuantity || 0
    v.unschedulingQuantity = v.schedulingQuantity - v.taskQuantity
    v.productType = productType
    v.totalSchedulingMete = useProductMeteConvert({
      productType: v.productType,
      length: v.totalSchedulingLength,
      L_TO_UNIT: unitObj.value.unit,
      L_DP: unitObj.value.dp,
      weight: v.totalSchedulingNetWeight,
      W_TO_UNIT: unitObj.value.unit,
      W_DP: unitObj.value.dp
    }).convertMete
    v.totalTaskMete = useProductMeteConvert({
      productType: v.productType,
      length: v.totalTaskLength,
      L_TO_UNIT: unitObj.value.unit,
      L_DP: unitObj.value.dp,
      weight: v.totalTaskNetWeight,
      W_TO_UNIT: unitObj.value.unit,
      W_DP: unitObj.value.dp
    }).convertMete
    v.unschedulingMete = (v.totalSchedulingMete - v.totalTaskMete).toFixed(unitObj.value.dp)
    return v
  })
}
</script>
