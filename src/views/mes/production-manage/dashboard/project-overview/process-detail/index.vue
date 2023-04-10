<template>
  <common-dialog
    ref="drawerRef"
    fullscreen
    v-model="dialogVisible"
    :title="`${props.detailData.name}工序生产明细`"
    :before-close="handleClose"
    :close-on-click-modal="false"
    :show-close="false"
  >
    <template #titleAfter>
      <workshop-select
        v-model="workshopId"
        placeholder="请选择车间"
        :factory-id="factoryId"
        style="width: 150px"
        class="filter-item"
        :clearable="true"
        @change="processDetailGet"
      />
      <production-line-select
        v-model="productionLineId"
        :factory-id="factoryId"
        :workshop-id="workshopId"
        :clearable="true"
        class="filter-item"
        style="width: 150px"
        @change="processDetailGet"
      />
      <monomer-select-area-select
        v-model:monomerId="monomerId"
        v-model:areaId="areaId"
        needConvert
        clearable
        areaClearable
        :project-id="projectId"
        style="width: 150px"
        @change="processDetailGet"
      />
      <el-input
        v-model.trim="serialNumber"
        placeholder="编号搜索"
        style="width: 150px"
        class="filter-item"
        clearable
        @keyup.enter="processDetailGet"
      />
      <common-radio-button
        type="enum"
        style="vertical-align: middle"
        v-model="status"
        showOptionAll
        :options="taskTrackingSchedulingStatusEnum.ENUM"
        @change="processDetailGet"
      />
      <common-button class="filter-item" size="mini" type="success" icon="el-icon-search" @click.stop="searchQuery">搜索</common-button>
      <common-button class="filter-item" size="mini" type="warning" icon="el-icon-refresh-left" @click.stop="resetQuery">
        重置
      </common-button>
    </template>
    <template #titleRight>
      <div style="display: flex">
        <print-table
          v-permission="permission.print"
          api-key="mesProjectOverviewList"
          :params="{ ...query, ...commonQuery, processId: props.detailData.id }"
          size="mini"
          type="warning"
          class="filter-item"
        />
        <common-button size="mini" style="margin-left: 8px" @click="handleClose">关 闭</common-button>
      </div>
    </template>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      :data="processDetailData"
      :max-height="maxHeight + 110"
      :show-empty-symbol="false"
      show-summary
      :summary-method="getSummaries"
      style="width: 100%"
    >
      <el-table-column :show-overflow-tooltip="true" prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column :show-overflow-tooltip="true" prop="workshop" label="车间/产线" align="center" min-width="140px">
        <template #default="{ row }">
          <span>{{ row.workshop?.name + '/' + row.productionLine?.name }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="monomer.name" label="单体" align="center" min-width="100px">
        <template #default="{ row }">
          <span>{{ row.monomer ? row.monomer?.name : '/' }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="area.name" label="区域" align="center" min-width="100px">
        <template #default="{ row }">
          <span>{{ row.area ? row.area?.name : '/' }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="serialNumber" label="编号" align="center" />
      <el-table-column :show-overflow-tooltip="true" prop="specification" label="规格" align="center" />
      <el-table-column :show-overflow-tooltip="true" prop="material" label="材质" align="center" />
      <el-table-column :show-overflow-tooltip="true" prop="quantity" label="任务数" align="center" />
      <el-table-column :show-overflow-tooltip="true" prop="taskNetWeight" label="任务量（kg）" align="center">
        <template #default="{ row }">
          <span>{{ weightStatus === weightTypeEnum.NET.V ? row.taskNetWeight : row.taskGrossWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="completeQuantity" label="已生产数" align="center">
        <template #default="{ row }">
          <span :class="row.completeQuantity === row.quantity ? 'tc-success' : 'tc-danger'">{{ row.completeQuantity }}</span>
        </template>
      </el-table-column>
      <el-table-column :show-overflow-tooltip="true" prop="completeNetWeight" label="生产量（kg）" align="center">
        <template #default="{ row }">
          <span :class="row.completeQuantity === row.quantity ? 'tc-success' : 'tc-danger'">{{
            weightStatus === weightTypeEnum.NET.V ? row.completeNetWeight : row.completeGrossWeight
          }}</span>
        </template>
      </el-table-column>
      <!-- <el-table-column :show-overflow-tooltip="true" prop="length" label="长度" align="center" />
      <el-table-column :show-overflow-tooltip="true" prop="netWeight" label="单净重（kg）" align="center" />
      <el-table-column
        v-if="props.detailData.productType !== componentTypeEnum.ASSEMBLE.V"
        :show-overflow-tooltip="true"
        prop="grossWeight"
        label="单毛重（kg）"
        align="center"
      />
      <el-table-column :show-overflow-tooltip="true" prop="quantity" label="需生产数" align="center" />
      <el-table-column :show-overflow-tooltip="true" prop="completeQuantity" label="完成数" align="center">
        <template #default="{ row }">
          <el-tag style="cursor: pointer" @click="showQuantity(row)">{{ row.completeQuantity }}</el-tag>
        </template>
      </el-table-column> -->
    </common-table>
    <!-- 分页 -->
    <el-pagination
      :total="total"
      :current-page="queryPage.pageNumber"
      :page-size="queryPage.pageSize"
      style="margin-top: 8px"
      layout="total, prev, pager, next, sizes"
      @size-change="handleSizeChange"
      @current-change="handleCurrentChange"
    />
  </common-dialog>
  <!-- <detail-drawer v-model:visible="drawerVisible" :query="query" :team-data="teamData" /> -->
</template>

<script setup>
import { getProcessDetail } from '@/api/mes/production-manage/dashboard/project-overview'
import { defineProps, defineEmits, ref, watch, computed } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import { weightTypeEnum } from '@enum-ms/common'
import { taskTrackingSchedulingStatusEnum } from '@enum-ms/mes'
import { mesProjectOverviewPM as permission } from '@/page-permission/mes'
import useVisible from '@compos/use-visible'
import usePagination from '@compos/use-pagination'
import useMaxHeight from '@compos/use-max-height'
import monomerSelectAreaSelect from '@comp-base/monomer-select-area-select'
import productionLineSelect from '@comp-mes/production-line-select'
import workshopSelect from '@comp-mes/workshop-select'
// import detailDrawer from './detail-drawer.vue'

const emit = defineEmits(['update:visible'])
const processDetailData = ref([])
// const drawerVisible = ref(false)
// const teamData = ref({})
const workshopId = ref()
const productionLineId = ref()
const monomerId = ref()
const areaId = ref()
const serialNumber = ref()
const status = ref()

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailData: {
    type: Object,
    default: () => {}
  },
  projectId: {
    type: Number
  },
  weightStatus: {
    type: Number
  }
})

// const lastMonomerId = inject('monomerId')
// const lastAeaId = inject('areaId')
// const lastProductionLineId = inject('productionLineId')

const query = computed(() => {
  return {
    productType: props.detailData.productType,
    processId: props.detailData.id,
    projectId: props.projectId,
    productionLineId: productionLineId.value
  }
})

const commonQuery = computed(() => {
  return {
    workshopId: workshopId.value,
    productionLineId: productionLineId.value,
    monomerId: monomerId.value,
    areaId: areaId.value,
    serialNumber: serialNumber.value,
    status: status.value
  }
})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: processDetailGet })

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: processDetailGet })

watch(
  () => dialogVisible.value,
  (val) => {
    if (val) {
      processDetailGet()
    }
  },
  { deep: true }
)

watch(
  () => commonQuery.value,
  (val) => {
    if (val) {
      processDetailGet()
    }
  }
)

async function processDetailGet() {
  let _list = []
  if (!dialogVisible.value) return
  try {
    const { content = [], totalElements } = await getProcessDetail({
      processId: props.detailData.id,
      monomerId: monomerId.value,
      areaId: areaId.value,
      ...commonQuery.value,
      ...query.value,
      ...queryPage
    })
    setTotalPage(totalElements)
    _list = content
  } catch (e) {
    console.log('获取工序的生产明细失败', e)
  } finally {
    processDetailData.value = _list
  }
}

const { maxHeight } = useMaxHeight({
  paginate: true
})

// 搜索
function searchQuery() {
  processDetailGet()
}
// 重置
function resetQuery() {
  workshopId.value = undefined
  productionLineId.value = undefined
  monomerId.value = undefined
  areaId.value = undefined
  serialNumber.value = undefined
  status.value = undefined
  processDetailGet()
}

// // 点击完成数显示详情
// function showQuantity(row) {
//   drawerVisible.value = true
//   teamData.value = row
// }
// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: ['quantity']
  })
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped></style>
